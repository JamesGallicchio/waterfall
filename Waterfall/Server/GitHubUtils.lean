import Http
import GitHub

namespace Waterfall.Server.GitHub

structure Config where
  ghAppId : String
  ghPrivateKeyPath : System.FilePath
  ghInstallId : Nat
  jwt : IO.Ref String
  token : IO.Ref String


private def parser : Http.Parser (Http.Response String) :=
  Http.Response.parse (Http.Parser.capture (Parser.dropMany Parser.anyToken)
    |>.map (·.toString))

/- Sends given request to GitHub.
   Doesn't check whether the JWT/Token are expired
 -/
def request (c : Config) [ToString T] (req : Http.Request T) (use_jwt := false)
    : IO (Http.Response String) := do
  let auth ← if use_jwt then c.jwt.get else c.token.get
  let res ← IO.Process.run {
    cmd := "curl"
    args := #[
      "-i",
      "--request", toString req.method,
      "--url", s!"{req.url}",
      "--header", "Accept: application/vnd.github+json",
      "--header", s!"Authorization: Bearer {auth}",
      "--header", "X-GitHub-Api-Version: 2022-11-28"
    ]
  }
  match Parser.run parser res with
  | .ok _ res => return res
  | .error e  => IO.throwServerError (toString e)

-- should this be how we do this or maybe something else?
/- Download a specific file from GH -/
def downloadFile (c : Config) (url : String) : IO String := do
  let curl_res ← IO.Process.run {
    cmd := "curl"
    args := #[
      "-i",
      "--request", "GET",
      "--url", s!"{url}",
      "--header", "Accept: application/vnd.github+json",
      "--header", s!"Authorization: Bearer {← c.token.get}",
      "--header", "X-GitHub-Api-Version: 2022-11-28"
    ]
  }
  match Parser.run parser curl_res with
  | .ok _ res =>
    match res.status with
    | ⟨200, _⟩ => return res.body
    | _        => IO.throwServerError curl_res
  | .error e  => IO.throwServerError (toString e)


/- Using a valid JWT, generate a token which can be used for other requests -/
def getInstallTok (c : Config) : ExceptT String IO String := do
  let req :=
    GitHub.«apps/create-installation-access-token»
      (installation_id := c.ghInstallId)
      (body := .obj Lean.RBNode.leaf)
  let res :=
    GitHub.«apps/create-installation-access-token».getResponse
      (← request c (use_jwt := true) req)

  match res with
  | .ok (.«201» res) =>
    let (tok, _exp) := ← do
      let tok ← res.body.getObjValAs? String "token"
      let exp ← res.body.getObjValAs? String "expires_at"
      return (tok,exp)
    return tok

  | .ok (.«401» res) => throw s!"failed to auth: {res.body}"
  | .ok (.«403» res) => throw s!"forbidden: {res.body}"
  | .ok (.«404» res) => throw s!"resource not found: {res.body}"
  | .ok (.«422» res) => throw s!"validation error: {res.body}"
  | .error e         => throw s!"error processing response: {e}"


/- Updates the config to have a new JWT -/
def updateJwt (c : Config) : IO Unit := do
  let res ← IO.Process.run {
    cmd:="jwt",
    args:=
     #[ "encode"
      , "--alg", "RS256"
      , "--exp=+600s"
      , "--iss", c.ghAppId
      , "--secret", "@" ++ c.ghPrivateKeyPath.toString
    ]
  }
  c.jwt.set res

/- Updates the config to have a new token, using a valid JWT -/
def updateToken (c : Config) : IO Unit := do
  match ← getInstallTok c with
  | .error e => IO.println s!"error updating token: {e}"
  | .ok tok  => c.token.set tok


def getHeadCommit (c : Config) (owner repo : String)
    : ExceptT String IO Lean.Json := do
  ExceptT.adapt (fun (e : String) =>
      s!"Error when getting the HEAD commit:\n\
        {e}\n\
        owner: {owner}\n\
        repo: {repo}\n"
    ) do
  let req :=
    GitHub.«repos/get-commit»
      (owner := owner)
      (repo := repo)
      (ref := "HEAD")
  let res := GitHub.«repos/get-commit».getResponse (← request c req)

  match res with
  | .ok (.«200» res) => return res.body
  | .ok (.«404» res) => throw s!"resource not found: {res.body}"
  | .ok (.«422» res) => throw s!"validation error: {res.body}"
  | .ok (.«500» res) => throw s!"internal error: {res.body}"
  | .ok (.«503» res) => throw s!"service unavailable: {res.body}"
  | .error e         => throw s!"error processing response: {e}"

def getHeadCommitRev (c : Config) (owner repo : String)
    : ExceptT String IO String := do
  let head ← getHeadCommit c owner repo
  let sha ← head.getObjValAs? String "sha"
  return sha

def getRepoInfo (c : Config) (owner repo : String)
    : ExceptT String IO Lean.Json :=
  ExceptT.adapt (fun (e : String) =>
      s!"Error when getting repo information:\n\
        {e}\n\
        owner: {owner}\n\
        repo: {repo}\n"
    ) do
  let req := GitHub.«repos/get» (owner := owner) (repo := repo)
  let res := GitHub.«repos/get».getResponse (← request c req)

  match res with
  | .ok (.«200» res) => return res.body
  | .ok (.«301» res) => throw s!"moved permanently: {res.body}"
  | .ok (.«403» res) => throw s!"forbidden: {res.body}"
  | .ok (.«404» res) => throw s!"resource not found: {res.body}"
  | .error e         => throw s!"error processing response: {e}"

def getLakeManifest (c : Config) (owner repo : String)
    (ref := "HEAD")
    (path := "lake-manifest.json")
    : ExceptT String IO Lean.Json :=
  ExceptT.adapt (fun (e : String) =>
      s!"Error when getting the lake manifest:\n\
        {e}\n\
        owner: {owner}\n\
        repo: {repo}\n\
        ref: {ref}\n\
        path: {path}\n"
    ) do
  let req :=
    GitHub.«repos/get-content»
      (owner := owner)
      (repo  := repo)
      (path  := path)
      (ref   := ref)
  let res :=
    GitHub.«repos/get-content».getResponse (← request c req)

  match res with
  | .ok (.«200» res) =>
    match Lean.Json.parse res.body with
    | .error e => throw s!"failed to parse response: {e}"
    | .ok json =>
      let ty ← json.getObjValAs? String "type"
      if ty ≠ "file" then throw s!"`lake-manifest.json` is not a file? {json}"

      let download_url ← json.getObjValAs? String "download_url"
      let lake_manifest ← downloadFile c download_url
      return ← Lean.Json.parse lake_manifest

  | .ok (.«302» res) => throw s!"found: {res.body}"
  | .ok (.«403» res) => throw s!"forbidden: {res.body}"
  | .ok (.«404» res) => throw s!"resource not found: {res.body}"
  | .error e         => throw s!"error processing response: {e}"


def createCommit (c : Config) (owner repo parent : String)
    (message := "Waterfall automated commit")
    (tree := "6ef19b41225c5369f1c104d45d8d85efa9b057b53b14b4b9b939dd74decc5321")
    : ExceptT String IO Lean.Json :=
  ExceptT.adapt (fun (e : String) =>
      s!"Error when getting repo information:\n\
        {e}\n\
        owner: {owner}\n\
        repo: {repo}\n"
    ) do
  let req := GitHub.«git/create-commit» (owner := owner) (repo := repo)
    (body := json%
      { "message": $message
      , "tree": $tree
      , "parents": [$(parent)]
      }
    )
  let res := GitHub.«git/create-commit».getResponse (← request c req)

  match res with
  | .ok (.«201» res) => return res.body
  | .ok (.«404» res) => throw s!"resource not found: {res.body}"
  | .ok (.«422» res) => throw s!"validation failed: {res.body}"
  | .error e         => throw s!"error processing response: {e}"

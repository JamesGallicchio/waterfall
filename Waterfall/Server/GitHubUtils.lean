import Http
import GitHub

namespace Waterfall.Server

structure Config where
  ghAppId : String
  ghPrivateKeyPath : System.FilePath
  ghInstallId : Nat
  jwt : IO.Ref String
  token : IO.Ref String

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
  | .error e => IO.throwServerError (toString e)
where parser : Http.Parser (Http.Response String) :=
  Http.Response.parse (Http.Parser.capture (Parser.dropMany Parser.anyToken)
    |>.map (·.toString))

def getInstallTok (c : Config) : ExceptT String IO String := do
  let req :=
    GitHub.«apps/create-installation-access-token»
      (installation_id := c.ghInstallId)
      (body := .obj Lean.RBNode.leaf)
  let res :=
    GitHub.«apps/create-installation-access-token».getResponse
      (← request c (use_jwt := true) req)

  match res with
  | .error e =>
    throw s!"error processing response: {e}"
  | .ok (.«201» res) =>
    let (tok, _exp) := ← do
      let tok ← (← res.body.getObjVal? "token").getStr?
      let exp ← (← res.body.getObjVal? "expires_at").getStr?
      return (tok,exp)
    return tok
  | .ok (.«401» res) =>
    throw s!"failed to auth: {res.body}"
  | .ok (.«403» res) =>
    throw s!"forbidden: {res.body}"
  | .ok (.«404» res) =>
    throw s!"resource not found: {res.body}"
  | .ok (.«422» res) =>
    throw s!"validation error: {res.body}"

def updateToken (c : Config) : IO Unit := do
  match ← getInstallTok c with
  | .error e => IO.println s!"error updating token: {e}"
  | .ok tok  => c.token.set tok

def getHeadCommit (c : Config) (owner repo : String)
    : ExceptT String IO String := do
  let req :=
    GitHub.«repos/get-commit»
      (owner := owner)
      (repo := repo)
      (ref := "HEAD")
  let res :=
    GitHub.«repos/get-commit».getResponse (← request c req)

  match res with
  | .error e =>
    throw s!"error processing response: {e}"
  | .ok (.«200» res) =>
    let commit := ← do
      let sha ← res.body.getObjVal? "sha"
      return ← sha.getStr?
    return commit
  | .ok (.«404» res) =>
    throw s!"resource not found: {res.body}"
  | .ok (.«422» res) =>
    throw s!"validation error: {res.body}"
  | .ok (.«500» res) =>
    throw s!"internal error: {res.body}"
  | .ok (.«503» res) =>
    throw s!"service unavailable: {res.body}"

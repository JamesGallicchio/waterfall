import Waterfall.Feature
import GitHub
import Socket

namespace Waterfall.GitHub

/-- The primary `RepoHost` for many packages. We implement
  features in GitHub as PRs, with underlying `git` branches. -/
instance Types : RepoHostType where
  repoId := String
  featId := String
  commitId := String

private def getJwt : IO String :=
  IO.Process.run {
    cmd:="./jwt",
    args:=#[
      "encode",
      "--alg", "RS256",
      "--exp=+600s",
      "--iss", "386626",
      "--secret",
      "@/home/jgallicc/Downloads/waterfall-bot.2023-09-06.private-key.pem"]
  }

def request [ToString T] (req : Http.Request T) : IO (Http.Response String) := do
  let jwt ← getJwt
  --IO.println jwt
  let res ← IO.Process.run {
    cmd := "curl"
    args := #[
      "-i",
      "--request",
      toString req.method,
      "--url", toString req.url,
      "--header", "Accept: application/vnd.github+json",
      "--header", s!"Authorization: Bearer {jwt}",
      "--header", "X-GitHub-Api-Version: 2022-11-28"
    ]
  }
  --IO.println s!"got response:\n{res}"
  match Parser.run parser res with
  | .ok _ res => return res
  | .error e => IO.throwServerError (toString e)
where parser : Http.Parser (Http.Response String) :=
  Http.Response.parse (Http.Parser.capture (Parser.dropMany Parser.anyToken) |>.map (·.toString))

def test := do
  let req :=
    GitHub.«apps/create-installation-access-token»
      (installation_id := 41592663)
      (body := .obj Lean.RBNode.leaf)
  let res :=
    GitHub.«apps/create-installation-access-token».getResponse
      (← request req)

  match res with
  | .error e =>
    IO.println s!"error processing response: {e}"
  | .ok (.«201» res) =>
    let (tok, exp) ← IO.ofExcept <| do
      let tok ← (← res.body.getObjVal? "token").getStr?
      let exp ← (← res.body.getObjVal? "expires_at").getStr?
      return (tok,exp)
    IO.println s!"got token {tok}"
    IO.println s!"expires at {exp}"
  | .ok (.«401» res) =>
    IO.println s!"failed to auth: {res.body}"
  | .ok (.«403» res) =>
    IO.println s!"forbidden: {res.body}"
  | .ok (.«404» res) =>
    IO.println s!"resource not found: {res.body}"
  | .ok (.«422» res) =>
    IO.println s!"validation error: {res.body}"

#eval test

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
  let req := GitHub.«apps/list-installations»

  let req := GitHub.«apps/create-installation-access-token» 41592663 (.obj Lean.RBNode.leaf)
  IO.println req.toRequestString
  let res ← request req
  IO.println res.body

--#eval test

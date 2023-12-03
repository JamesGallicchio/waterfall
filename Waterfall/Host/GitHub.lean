import Waterfall.Core
import GitHub
import Socket

namespace Waterfall.GitHub

/-- The primary `RepoHost` for many packages. We implement
  features in GitHub as PRs, with underlying `git` branches. -/
instance Types : RepoHostTypes where
  repoId := String
  featId := String
  commitId := String

def RepoHost : RepoHost Types IO where
  getFeatureInfo := sorry

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

--#eval test

inductive RebaseError : Type u
/-- The feature ID of the feature being rebased was not found -/
| featNotFound
/-- The feature ID of the new parent was not found -/
| parentNotFound
/-- Feature A was being rebased onto feature B,
but A is a parent of B,
and B's head commit is not equal to A's head commit.

Note: if the head commits were equal,
then A can be rebased onto B even if A is a  parent of B. -/
| invalidParent
| other (s : String)

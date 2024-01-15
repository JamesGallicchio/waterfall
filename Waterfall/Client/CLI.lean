import Waterfall.Host.Git

namespace Waterfall.Client.CLI

namespace FeatureTree

open Git.Local
variable (repo : Types.repoId)

inductive Tree (α : Type u) where
| node : α → List (Tree α) → Tree α

structure View where
  tree : Tree (Types.featId)

def renderTree (t : Tree Types.featId) (indent : String) : IO Unit := do
  match t with
  | .node a children =>
    IO.println (indent ++ "- " ++ toString a)
    for h : c in children do
      renderTree c (indent ++ "  |")

partial def createTree (feat : Types.featId) : IO (Tree Types.featId) := do
  let data ← IO.ofExcept <| (← getFeatureData repo feat).mapError (fun _ => ())
  return .node feat (← data.children.mapM createTree)

def run : IO Unit := do
  let curRef ← IO.ofExcept <| ← Git.Local.getCurRef repo
  match curRef.toBranch with
  | none => throw (.userError s!"{curRef} not a branch?")
  | some main =>
  match ← getFeatureData repo main with
  | .error (.invalidFeat e) => throw (.userError s!"{main} not a feature: {e}")
  | .error (.other e) => throw (.userError s!"other: {e}")
  | .ok _ =>
  let tree ← createTree repo main
  renderTree tree ""

#check createFeature "." "main"

end FeatureTree

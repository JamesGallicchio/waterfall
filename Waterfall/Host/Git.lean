import Waterfall.Core.RepoHost

namespace Waterfall.Git

/-! ## Local Git Repo

This section implements waterfall on a local git repo.
There is no attempt to sync anything to a remote.
-/
namespace Local

/-- Store object in the git repo and return the hash of the object
(or an error) -/
def storeObject (root : System.FilePath) (obj : String) : ExceptT String IO String := do
  let child ← IO.Process.spawn {
    cmd := "git"
    args := #["hash-object", "-w", "--stdin"]
    cwd := root
    stdin := .piped
    stdout := .piped
  }
  let (stdin, child) ← child.takeStdin
  stdin.write obj.toUTF8

  let exitCode ← child.wait

  if exitCode != 0 then
    throw s!"git hash-object: exited with {exitCode}"

  let out ← child.stdout.readToEnd
  let hash := out.trim
  if ! hash.all (fun c => c.isDigit || 'a' ≤ c && c ≤ 'f') then
    throw s!"git hash-object: returned not a hash: {hash}"
  if hash.length != 40 then
    throw s!"git hash-object: hash is {hash.length} chars long (expected 40)"

  return hash

/-- Get an object from the git repo -/
def getObject (root : System.FilePath) (hash : String) : ExceptT String IO String := do
  let child ← IO.Process.spawn {
    cmd := "git"
    args := #["cat-file", "blob", hash]
    cwd := root
    stdout := .piped
  }
  let exitCode ← child.wait

  if exitCode != 0 then
    throw s!"git cat-file: exited with {exitCode}"

  let out ← child.stdout.readToEnd

  return out

def RefPart.valid (s : String) : Bool :=
  !String.isEmpty s && s.all (fun c => c.isAlphanum || c ∈ ['-','_'])

def RefPart := { s // RefPart.valid s }
deriving DecidableEq

def Ref := { L : List RefPart // L.length > 0 }
deriving DecidableEq

def Ref.toString (r : Ref) : String :=
  match r with
  | ⟨hd::tl, _⟩ => Subtype.val hd ++ tl.foldr ("/" ++ ·.val ++ ·) ""

instance : ToString Ref := ⟨Ref.toString⟩

def Ref.ofString (s : String) : Except String Ref := do
  let L ← s.splitOn "/" |>.mapM (fun part => do
    if h : _ then return ⟨part,h⟩
    else throw s!"Invalid ref segment: {part}"
  )
  if h : L.length > 0 then
    return ⟨L,h⟩
  else
    throw s!"Invalid ref: {s}"

open Lean in section
instance : ToJson Ref where
  toJson r := toJson r.toString

instance : FromJson Ref where
  fromJson? j := do
    let s ← j.getStr?
    let r ← Ref.ofString s
    return r

end

def Ref.append (r1 r2 : Ref) : Ref :=
  ⟨r1.val ++ r2.val, by simp; apply Nat.le_trans; apply r1.property; apply Nat.le_add_right⟩

instance : Coe RefPart Ref := ⟨(⟨[·], by simp; decide⟩)⟩

def RefPart.static (s : String) (h : RefPart.valid s := by decide) : RefPart := ⟨s, h⟩


def waterfallConfig : Ref :=
  Ref.append (RefPart.static "refs") <|
  RefPart.static "waterfall-config"

def waterfallLock : Ref :=
  Ref.append (RefPart.static "refs") <|
  RefPart.static "waterfall-lock"

def waterfallRef (feat : Ref) : Ref :=
  Ref.append (RefPart.static "refs") <|
  Ref.append (RefPart.static "waterfall") <|
  Ref.append (RefPart.static "heads") <|
  feat

def gitHead (feat : Ref) : Ref :=
  Ref.append (RefPart.static "refs") <|
  Ref.append (RefPart.static "heads") <|
  feat


/-- Update a reference in the git repo.

If `hash` is none, we attempt to delete `ref`.
If `oldHash` is none, we expect `ref` to be currently nonexistent.

**Note:** ALWAYS checks that the old value is correct.
This is the only way to avoid race conditions.
 -/
def updateRef (root : System.FilePath) (ref : Ref) (hash : Option String) (oldHash : Option String)
  : ExceptT String IO Unit := do
  let args := match hash with
    | none => #["-d", ref.toString, oldHash.getD ""]
    | some hash => #[ref.toString, hash, oldHash.getD ""]

  let child ← IO.Process.spawn {
    cmd := "git"
    args := #["update-ref"] ++ args
    cwd := root
  }
  let exitCode ← child.wait

  if exitCode != 0 then
    throw s!"git update-ref: exited with {exitCode} setting {ref} to {hash} from {oldHash}"

/-- Get the hash that `ref` points to in the git repo,
or `none` if it is undefined. -/
def getRef (root : System.FilePath) (ref : Ref) : ExceptT String IO (Option String) := do
  let child ← IO.Process.spawn {
    cmd := "git"
    args := #[
        "rev-parse"
      , "--verify" -- ensures the first argument resolves to a hash
      , "-q"       -- on failure, exits with code 1 but no stderr output
      , ref.toString]
    cwd := root
    stdout := .piped
  }
  let exitCode ← child.wait

  if exitCode = 0 then
    return (← child.stdout.readToEnd).trim
  else if exitCode = 1 then
    return none
  else
    throw s!"git rev-parse: exited with {exitCode}"

def withWaterfallLock (root : System.FilePath) (f : Unit → IO α) : IO α := do
  let num := toString (← IO.rand 1 1000000000)

  while !(← updateRef root waterfallConfig (some num) none).isOk do
    IO.sleep 200

  let res ← f ()

  match ← updateRef root waterfallConfig none (some num) with
    | .ok () => pure ()
    | .error e =>
      IO.eprint s!"waterfall lockfile touched during withWaterfallLock: {e}"

  return res


def checkIsCommitAncestor (root : System.FilePath) (parent child : String) : IO Bool := do
  let child ← IO.Process.spawn {
    cmd := "git"
    args := #["merge-base", "--is-ancestor", parent, child]
    cwd := root
  }
  let exitCode ← child.wait

  return exitCode = 0


@[reducible] def RepoTypes : RepoHostTypes where
  repoId := System.FilePath
  featId := Ref
  commitId := String

def getFeatureInfo (repo : System.FilePath) (feat : Ref)
    : ExceptT (RepoHost.GetFeatureInfo.Error RepoTypes) IO (FeatureInfo RepoTypes) := do
  match ← ExceptT.adapt .other <|
    getRef repo (waterfallRef feat)
  with
  | none =>
    throw (.invalidFeat feat)
  | some hash =>
  let data ← ExceptT.adapt .other <| getObject repo hash
  let json ← ExceptT.adapt .other <| Lean.Json.parse data
  let info ← ExceptT.adapt .other <| Lean.fromJson? json
  return info

def checkIsFeatAncestor (repo : System.FilePath) (parent child : Ref)
    : ExceptT (RepoHost.GetFeatureInfo.Error RepoTypes) IO Bool :=
  aux child 100
where aux (feat fuel) := do
  match fuel with
  | 0 => throw (.other s!"checkIsFeatAncestor: deep feature traversal detected starting from {child} (possible infinite loop detected?)")
  | fuel+1 =>
  if parent = feat then
    return true
  else match (← getFeatureInfo repo feat).parent with
  | none => return false
  | some feat' =>
    aux feat' fuel

def createFeature (repo : System.FilePath) (name : String) (parent : Ref)
    : ExceptT (RepoHost.CreateFeature.Error RepoTypes) IO Ref := do
  let parentInfo ← ExceptT.adapt fromFt <| getFeatureInfo repo parent
  let info : FeatureInfo RepoTypes := {
    parent := some parent
    base := parentInfo.head
    head := parentInfo.head
  }
  let json := Lean.toJson info
  let data := json.compress
  let hash ← ExceptT.adapt .other <| storeObject repo data
  let newRef ← ExceptT.adapt .invalidName <| Ref.ofString name
  ExceptT.adapt .invalidName <| updateRef repo (waterfallRef newRef) hash none
  return newRef
where
  fromFt : RepoHost.GetFeatureInfo.Error RepoTypes → RepoHost.CreateFeature.Error RepoTypes
  | .invalidFeat feat => .invalidFeat feat
  | .other err => .other err

def rebase (repo : System.FilePath) (child : Ref) (parent : Option Ref)
    : ExceptT (RepoHost.RebaseError RepoTypes) IO Unit := do
  let childInfo ← ExceptT.adapt fromFt <| getFeatureInfo repo child
  match childInfo.parent with
  | none => throw (.other s!"Rebase: Cannot rebase root feature {child}")
  | some curParent =>

  let newParent := parent.getD curParent

  -- check that the new parent is not a child of `child`
  if ← ExceptT.adapt fromFt <|
    checkIsFeatAncestor repo (parent := child) (child := newParent)
  then
    throw (.other s!"Rebase: cycles not allowed: {child} is a (transitive) parent of {newParent}")

  let parentInfo ← ExceptT.adapt fromFt <| getFeatureInfo repo newParent
  let oldBase := childInfo.base
  let newBase := parentInfo.head

  -- Make sure the old base is an ancestor of the new base, so that we can
  -- "rebase" by merging the new base into the head.
  if ← checkIsCommitAncestor repo (parent := oldBase) (child := newBase) then
    throw (.other s!"Rebase: this rebase would require reverting some changes. This is not yet supported.")

  sorry
where
  fromFt : RepoHost.GetFeatureInfo.Error RepoTypes → RepoHost.RebaseError RepoTypes
  | .invalidFeat feat => .invalidFeat feat
  | .other err => .other err

def RepoHost : RepoHost RepoTypes IO where
  getFeatureInfo := getFeatureInfo
  createFeature := createFeature
  rebase := rebase

end Local

def RepoTypes : RepoHostTypes where
  repoId := String
  featId := String
  commitId := String

def RepoHost : RepoHost RepoTypes IO where
  getFeatureInfo := sorry
  createFeature := sorry
  rebase := sorry

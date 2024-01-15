import Waterfall.Core.RepoHost
import Std

namespace Waterfall.Git

/-! ## Local Git Repo

This section implements waterfall on a local git repo.
There is no attempt to sync anything to a remote.
-/
namespace Local

/-! #### Git stuff -/

def Hash := String
deriving ToString

/-- Store object in the git repo and return the hash of the object
(or an error) -/
def storeObject (root : System.FilePath) (obj : String) : ExceptT String IO Hash := do
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
def getObject (root : System.FilePath) (hash : Hash) : ExceptT String IO String := do
  let out ← IO.Process.output {
    cmd := "git"
    args := #["cat-file", "blob", hash]
    cwd := root
  }

  if out.exitCode != 0 then
    throw s!"git cat-file: exited with {out.exitCode}:\n{out.stderr}"

  return out.stdout

def RefPart.valid (s : String) : Bool :=
  !String.isEmpty s && s.all (fun c => c.isAlphanum || c ∈ ['-','_'])

def RefPart := { s // RefPart.valid s }
deriving DecidableEq, Repr, Hashable

def Ref := { L : List RefPart // L.length > 0 }
deriving DecidableEq, Repr, Hashable

def Ref.toString (r : Ref) : String :=
  match r with
  | ⟨hd::tl, _⟩ => Subtype.val hd ++ tl.foldr ("/" ++ ·.val ++ ·) ""

instance : ToString Ref := ⟨Ref.toString⟩

def Ref.ofString (s : String) : Except String Ref := do
  let L ← s.splitOn "/" |>.mapM (fun part => do
    if h : RefPart.valid part then return ⟨part,h⟩
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

instance : Coe RefPart Ref := ⟨(⟨[·], by simp⟩)⟩

def RefPart.static (s : String) (h : RefPart.valid s := by decide) : RefPart := ⟨s, h⟩
def Ref.static (s : String) (h : (Ref.ofString s).isOk := by decide) : Ref :=
  match Ref.ofString s, h with
  | .ok ref, _ => ref
  | .error _, h => by contradiction

instance : Inhabited Ref := ⟨Ref.static "a"⟩

/-- Update a reference in the git repo.

If `hash` is none, we attempt to delete `ref`.
If `oldHash` is none, we expect `ref` to be currently nonexistent.

Returns true if the operation succeeds,
false if the old hash was wrong,
and error if a different error occurs.

**Note:** ALWAYS checks that the old value is correct.
This is the only way to avoid race conditions.
 -/
def updateRef (root : System.FilePath) (ref : Ref) (hash : Option Hash) (oldHash : Option Hash)
  : ExceptT String IO Bool := do
  let args := match hash with
    | none => #["-d", ref.toString, oldHash.getD ""]
    | some hash => #[ref.toString, hash, oldHash.getD ""]

  let out ← IO.Process.output {
    cmd := "git"
    -- TODO: reflog not an ideal way to keep git gc from deleting waterfall objects
    args := #["update-ref", "--create-reflog"] ++ args
    cwd := root
  }

  if out.exitCode = 0 then
    return true
  else
    if out.stderr.containsSubstr "but expected"
      || out.stderr.containsSubstr "already exists"
      || out.stderr.containsSubstr "unable to resolve" then
      return false
    else
      throw s!"git update-ref: setting {ref} to {hash} from {oldHash}:\n{out.stderr}"

/-- Get the hash that `ref` points to in the git repo,
or `none` if it is undefined. -/
def getRef (root : System.FilePath) (ref : Ref) : ExceptT String IO (Option Hash) := do
  let out ← IO.Process.output {
    cmd := "git"
    args := #[
        "rev-parse"
      , "--verify" -- ensures the first argument resolves to a hash
      , "-q"       -- on failure, exits with code 1 but no stderr output
      , ref.toString]
    cwd := root
    stdout := .piped
  }

  if out.exitCode = 0 then
    return some out.stdout.trim
  else if out.exitCode = 1 then
    return none
  else
    throw s!"git rev-parse: exited with {out.exitCode}:\n{out.stderr}"

def checkIsCommitAncestor (root : System.FilePath) (parent child : String) : IO Bool := do
  let child ← IO.Process.spawn {
    cmd := "git"
    args := #["merge-base", "--is-ancestor", parent, child]
    cwd := root
  }
  let exitCode ← child.wait

  return exitCode = 0

def getCurRef (root : System.FilePath) : ExceptT String IO Ref := do
  let out ← IO.Process.output {
    cmd := "git"
    args := #["symbolic-ref", "HEAD"]
    cwd := root
  }

  if out.exitCode != 0 then
    throw s!"git cat-file: exited with {out.exitCode}:\n{out.stderr}"

  Ref.ofString out.stdout.trim

def Ref.toBranch : Ref → Option Ref
| ⟨⟨"refs", _⟩ :: ⟨"heads", _⟩ :: rest,_⟩ =>
  if h : _ then
    some ⟨rest, h⟩
  else none
| _ => none



/-! #### Waterfall implementation in Git
-/

@[reducible] def Types : RepoHostTypes where
  repoId := System.FilePath
  featId := Ref
  commitId := String

def waterfallConfig : Ref :=
  Ref.append (RefPart.static "waterfall") <|
  RefPart.static "config"

def waterfallLock (root : System.FilePath) : System.FilePath :=
  root / ".git" / "waterfall" / "waterfall.lock"

def waterfallRef (feat : Types.featId) : Ref :=
  Ref.append (RefPart.static "waterfall") <|
  Ref.append (RefPart.static "feats") <|
  feat

def gitHead (feat : Types.featId) : Ref :=
  Ref.append (RefPart.static "refs") <|
  Ref.append (RefPart.static "heads") <|
  feat

def withWaterfallLock (root : System.FilePath) (f : Unit → IO α) : IO α := do
  let handle ← IO.FS.Handle.mk (waterfallLock root) .write
  IO.FS.Handle.lock handle

  let res ← f ()

  IO.FS.Handle.unlock handle
  return res

structure RepoConfig where
  rootFeat : Types.featId
deriving Lean.ToJson, Lean.FromJson

def getConfig (repo : System.FilePath)
  : ExceptT String IO (Hash × RepoConfig) := do
  match ← getRef repo waterfallConfig with
  | none =>
    throw s!"{waterfallConfig} not defined?"
  | some hash =>
  let data ← getObject repo hash
  let json ← Lean.Json.parse data
  let info ← Lean.fromJson? json
  return (hash, info)

structure FeatureData extends FeatureInfo Types where
  children : List Types.featId
deriving Lean.ToJson, Lean.FromJson, Repr

def getFeatureData (repo : System.FilePath) (feat : Ref)
    : ExceptT (RepoHost.GetFeatureInfo.Error Types) IO (Hash × FeatureData) := do
  match ← ExceptT.adapt .other <|
    getRef repo (waterfallRef feat)
  with
  | none =>
    throw (.invalidFeat feat)
  | some hash =>
  let data ← ExceptT.adapt .other <| getObject repo hash
  let json ← ExceptT.adapt .other <| Lean.Json.parse data
  let info ← ExceptT.adapt .other <| Lean.fromJson? json
  return (hash, info)

def tryUpdateFeatureData (repo : System.FilePath) (id : Types.featId) (data : FeatureData) (old : Option Hash)
    : ExceptT String IO Bool := do
  let json := Lean.toJson data
  let data := json.compress
  let hash ← storeObject repo data
  updateRef repo (waterfallRef id) hash old

partial def updateFeatureData (repo : System.FilePath) (id : Types.featId)
      (f : Option FeatureData → ExceptT (RepoHost.CreateFeature.Error Types) IO (Option FeatureData))
    : ExceptT (RepoHost.CreateFeature.Error Types) IO Unit := do
  while true do
    let (hash, data) ← ExceptT.adapt fromFt <| getFeatureData repo id
    match ← f data with
    | none =>
    if ← ExceptT.adapt .other <| updateRef repo (waterfallRef id) none hash then
      return
    | some newData =>
    if ← ExceptT.adapt .other <| tryUpdateFeatureData repo id newData hash then
      return

where
  fromFt : RepoHost.GetFeatureInfo.Error Types → RepoHost.CreateFeature.Error Types
  | .invalidFeat feat => .invalidFeat feat
  | .other err => .other err

def initWaterfall (root : System.FilePath) (main : Ref) : IO Unit :=
  withWaterfallLock root fun () => do
  match ← IO.ofExcept <| ← getRef root (gitHead main) with
  | none => throw (.userError s!"branch {main} doesn't have an entry in refs/heads")
  | some headCommit =>
  let mainData := {
      parent := none
      base := headCommit, head := headCommit
      children := []
    }
  let .true ← IO.ofExcept <| ← tryUpdateFeatureData root main mainData none
    | throw (.userError s!"feature {main} already exists")
  let config : RepoConfig := {
    rootFeat := main
  }
  let configHash ← IO.ofExcept <| ← storeObject root (Lean.toJson config).compress
  let .true ← IO.ofExcept <| ← updateRef root waterfallConfig configHash none
    | throw (.userError s!"config ref {waterfallConfig} already exists")

def getFeatureInfo (repo : System.FilePath) (feat : Ref)
    : ExceptT (RepoHost.GetFeatureInfo.Error Types) IO (FeatureInfo Types) :=
  getFeatureData repo feat |>.map (·.2.toFeatureInfo)

def checkIsFeatAncestor (repo : System.FilePath) (parent child : Ref)
    : ExceptT (RepoHost.GetFeatureInfo.Error Types) IO Bool :=
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
    : ExceptT (RepoHost.CreateFeature.Error Types) IO Ref := do
  let newRef ← ExceptT.adapt .invalidName <| Ref.ofString name
  let (_,{head,..}) ← ExceptT.adapt fromFt <| getFeatureData repo parent
  let childData : FeatureData := {
      parent := some parent
      base := head
      head := head
      children := []
    }
  let .true ← ExceptT.adapt .other <| tryUpdateFeatureData repo newRef childData none
    | throw (.other s!"{newRef} already exists")
  -- this runs in a loop until it successfully updates parent
  updateFeatureData repo parent (fun
    | none => throw (.other s!"{parent} does not exist")
    | some parentData => do
      return some {
        parentData with children := parentData.children.insert newRef
      }
  )
  return newRef
where
  fromFt : RepoHost.GetFeatureInfo.Error Types → RepoHost.CreateFeature.Error Types
  | .invalidFeat feat => .invalidFeat feat
  | .other err => .other err

def rebase (repo : System.FilePath) (child : Ref) (parent : Option Ref)
    : ExceptT (RepoHost.RebaseError Types) IO Unit := do
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
  fromFt : RepoHost.GetFeatureInfo.Error Types → RepoHost.RebaseError Types
  | .invalidFeat feat => .invalidFeat feat
  | .other err => .other err

def RepoHost : RepoHost Types IO where
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

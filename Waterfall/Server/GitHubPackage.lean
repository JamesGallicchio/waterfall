import Waterfall.Server.GitHubUtils

/- This isn't written out the best and probably would make more sense to rewrite
    to use features but i figured this is a good proof-of-concept for now : )
    - thea
 -/
namespace Waterfall.Server.GitHub

def isSupportedLakeVersion : Nat → Bool := fun x => x = 7
def waterfallVersion : Nat := 0

structure Package.Ident where
  owner : String
  name  : String      -- ending should not be `.git`
deriving Repr, DecidableEq, Hashable

-- todo: Can github repos have `/` in them? how is that represented ???
def Package.Ident.ofURL (url : String) : Except String Package.Ident := do
  let url := url.dropRightWhile (fun | '/' => true | _ => false)
  let url := url.dropSuffix? ".git" |>.getD url
  let components := url.splitOn "/"
  if let repo :: owner :: _ := components.reverse
  then return ⟨owner.toString, repo.toString⟩
  else throw s!"Could not parse URL {url}!"

def Package.Ident.toString : Package.Ident → String
  | ⟨owner, name⟩ => s!"{owner}/{name}"
instance : ToString Package.Ident := ⟨Package.Ident.toString⟩

def Package.Ident.toJson : Package.Ident → Lean.Json
  | ⟨owner, name⟩ => .mkObj [("owner", .str owner), ("name", .str name)]

def Package.Ident.ofJson (json : Lean.Json) : Except String Package.Ident := do
  let owner ← json.getObjValAs? String "owner"
  let name  ← json.getObjValAs? String "name"
  return ⟨owner, name⟩

/- Information for a related package -/
structure Package.Related where
  ident     : Package.Ident
  rev       : String
  inherited : Bool
deriving Repr, DecidableEq, Hashable

def Package.Related.toString : Package.Related → String
  | ⟨id, rev, inh⟩ => s!"{id} @ {rev}    (inherited: {inh})"
instance : ToString Package.Related := ⟨Package.Related.toString⟩

def Package.Related.toJson : Package.Related → Lean.Json
  | ⟨id, rev, inh⟩ =>
    .mkObj [("id", id.toJson), ("rev", .str rev), ("inherited", .bool inh)]

def Package.Related.ofJson (json : Lean.Json)
    : Except String Package.Related := do
  let id  ← Package.Ident.ofJson (← json.getObjVal? "id")
  let rev ← json.getObjValAs? String "rev"
  let inh ← json.getObjValAs? Bool "inherited"
  return ⟨id, rev, inh⟩

-- todo use features
structure Package where
  ident    : Package.Ident
  rev      : String
  parents  : Array Package.Related
  children : List Package.Related      -- should this come with a rev?

def Package.toString : Package → String
  | ⟨id, rev, _, _⟩ => s!"{id} @ {rev}"
instance : ToString Package := ⟨Package.toString⟩

def Package.toJson : Package → Lean.Json
  | ⟨id, rev, parents, children⟩ =>
    .mkObj [ ("id", id.toJson)
           , ("rev", .str rev)
           , ("parents", .arr (parents.map Related.toJson))
           , ("children", .arr (.mk (children.map Related.toJson)))
           ]

def Package.ofJson (json : Lean.Json)
    : Except String Package := do
  let id       ← Package.Ident.ofJson (← json.getObjVal? "id")
  let rev      ← json.getObjValAs? String "rev"
  let parents  ← (← (← json.getObjVal? "parents").getArr?).mapM (Related.ofJson)
  let children ← (← (← json.getObjVal? "children").getArr?).mapM (Related.ofJson)
  return ⟨id, rev, parents, children.toList⟩

namespace Package

/- Given an owner/repo, create a package with parents populated. Children is
   left blank.
 -/
def assemble (c : Config) (owner repo : String) (ref? : Option String := none)
    : ExceptT String IO Package := do
  let ref ←
    match ref? with
    | some ref => pure ref
    | none     => getHeadCommitRev c owner repo
  let lake ← getLakeManifest c owner repo (ref := ref)

  let lake_version ← lake.getObjValAs? Nat "version"
  if ¬isSupportedLakeVersion lake_version then
    throw s!"Unsupported lake version {lake_version}."

  let packages ← lake.getObjValAs? (Array Lean.Json) "packages"
  let packages ← packages.mapM (fun json => do
      let id ← Package.Ident.ofURL (← json.getObjValAs? String "url")
      let rev ← json.getObjValAs? String "rev"
      let der ← json.getObjValAs? Bool "inherited"
      return ⟨id, rev, der⟩
    )

  return { ident    := ⟨owner, repo⟩
         , rev      := ref
         , parents  := packages
         , children := []
         }


-- todo better name??
/- A list for all the packages that waterfall is tracking -/
structure Tracking where
  listing : List Package
  failed  : (Package.Ident × String) → Bool

/- These are common repos that we don't want to lookup and will save unnecessary
      requests.
 -/
def Tracking.defaultNoTrack : (Package.Ident × String) → Bool :=
  fun (id, _rev) =>
    id.owner = "leanprover"
    || id = ⟨"mhuisi", "lean4-cli"⟩
    || id = ⟨"gebner", "quote4"⟩
    || id = ⟨"fgdorais", "lean4-unicode-basic"⟩
    || id = ⟨"fgdorais", "lean4-parser"⟩

instance : Inhabited Tracking := ⟨⟨[], Tracking.defaultNoTrack⟩⟩

namespace Tracking

def toJson : Tracking → Lean.Json
  | ⟨listing, _failed⟩ =>
    .mkObj [ ("version", waterfallVersion)
           , ("packages", .arr (.mk <| listing.map Package.toJson))
           ]

def ofJson (json : Lean.Json)
    : Except String Tracking := do
  let version ← json.getObjValAs? Nat "version"
  if version ≠ waterfallVersion then
    throw s!"Excepted waterfall version {waterfallVersion} but got {version}!"

  let pkgs ← (← (← json.getObjVal? "packages").getArr?).mapM (Package.ofJson)
  return ⟨pkgs.toList, Tracking.defaultNoTrack⟩


@[inline] def find? (t : Tracking) (id : Package.Ident) : Option Package :=
  t.listing.find? (fun p => p.ident = id)

@[inline] def find_with_rev? (t : Tracking) (id : Package.Ident) (rev : String)
    : Option Package :=
  t.listing.find? (fun p => p.ident = id && p.rev = rev)

@[inline] def has_package (t : Tracking) (id : Package.Ident) (rev : String) :=
  find_with_rev? t id rev |>.isSome

/- Updates the known children of candidates with the given function -/
def refreshChildren
    (t : Tracking)
    (f : Package → List Package.Related)
    : Tracking :=
  { t with listing := t.listing.map (fun p => { p with children := f p }) }

end Tracking


structure Repo.State where
  config     : Config
  tracking   : Tracking

abbrev Repo := ExceptT String (StateT Repo.State IO)

namespace Repo

@[inline] def State.has_package (s : State) : Package.Ident → String → Bool :=
  s.tracking.has_package

def has_package (id : Package.Ident) (rev : String) : Repo Bool := do
  return (← get).has_package id rev

-- todo: probs could write this better idk
def add_package (pkg : Package) : Repo Unit :=
  fun s => do
    let f := fun p => (fun ⟨i, r, _⟩ => i == p.ident && r == p.rev)
    let tracking :=
      s.tracking.refreshChildren (fun p =>
        match pkg.parents.find? (f p) with
        | some rel => ⟨pkg.ident, pkg.rev, rel.inherited⟩ :: p.children
        | none     => p.children
      )
    let tracking := { tracking with listing := pkg :: tracking.listing }
    return (pure (), { s with tracking })

partial def add_new_package
    (id : Package.Ident)
    (ref? : Option String := none)
    : Repo Unit :=
  fun s => do
    if s.tracking.failed (id, ref?.getD "HEAD") ||
       s.has_package id (ref?.getD "HEAD")
    then return (pure (), s)
    else
      match ← Package.assemble s.config id.owner id.name ref? with
      | .error err => return (throw err, s)
      | .ok pkg    =>
        let (e, s) ← pkg.parents.mapM (fun rel =>
            ExceptT.tryCatch
              (add_new_package rel.ident (ref? := rel.rev))
              (fun err => do
                IO.println s!"Error when adding package {rel}\n{err}\nSkipping"
                let s ← get
                let tracking := { s.tracking with failed :=
                    fun (id, rev) =>
                      (id = rel.ident && rev == rel.rev)
                      || (s.tracking.failed (id, rev))
                  }
                set { s with tracking }
                return ()
              )
          ) s
        match e with
        | .error err => return (throw err, s)
        | .ok _      => add_package pkg s


/- Finds packages that are downstream from an updating package and are also
    eligible to do so (not inherited).
 -/
def update_candidates (pkg : Package)
    : List Package.Ident :=
  pkg.children.filterMap (fun child =>
    if ¬child.inherited then some child.ident else none
  )

/- Adds the new version of the package, removing the old one. Notably the
    children reference the old revision of the new package. This will be 'fixed'
    when the child package makes an update.
 -/
def update_package (id : Package.Ident) (rev : String) : Repo (List Ident) :=
  fun s => do
    match s.tracking.find? id with
    | none     => return (throw s!"Could not find package {id}", s)
    | some pkg =>
      let candidates := update_candidates pkg
      let tracking := { s.tracking with
        listing :=
          s.tracking.listing.filter (
            fun p => p.ident ≠ id && p.rev ≠ pkg.rev -- remove old rev
          )
        }
      let (e, s) ← add_package { pkg with rev } { s with tracking }
      match e with
      | .error err =>
        return (throw s!"Something went wrong when updating {err}", s)
      | .ok () =>
        return (pure candidates, s)

def load (path : System.FilePath := "waterfall.json") : Repo Unit := do
  if ¬(← path.pathExists) then
    throw s!"Could not find saved waterfall repository: {path}"
  let contents ← IO.FS.readFile path
  let json ← Lean.Json.parse contents
  set { (← get) with tracking := ← Tracking.ofJson json }

def save (path : System.FilePath := "waterfall.json") : Repo Unit := do
  IO.FS.writeFile path ((← get).tracking.toJson.pretty)

end Repo

end Package



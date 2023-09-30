import Lean
import Cli

open Cli

def getManifestCommitCmd : Cmd := `[Cli|
  "get-manifest-commit" VIA run; ["0.0.1"]
  "Prints the commit of a package as specified in the manifest"

  ARGS:
    package : String; "Package name to look for"
    path : String; "Path to the manifest file"
]
where run (p : Parsed) : IO UInt32 := do
  let name : String := p.positionalArg! "package" |>.as! String
  let file : System.FilePath := p.positionalArg! "path" |>.as! String
  if !(← file.pathExists) then
    return 1

  let contents ← IO.FS.readFile file

  match Lean.Json.parse contents with
  | .error e =>
    IO.println e
    return 1
  | .ok json =>
    let packs ← IO.ofExcept <| json.getObjValAs? (List Lean.Json) "packages"
    match
      ← packs.findSomeM? fun js => IO.ofExcept do
        let x ← js.getObjVal? "git"
        let pname ← x.getObjValAs? String "name"
        if pname = name then
          let z ← x.getObjValAs? String "rev"
          return some z
        else return none
    with
    | none =>
      IO.println s!"No package found with name `{name}` in manifest file"
      return 1
    | some rev =>
      IO.println rev
      return 0

def main (args : List String) : IO UInt32 :=
  getManifestCommitCmd.validate args

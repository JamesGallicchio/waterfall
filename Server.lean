-- import Waterfall.Server.GitHubUtils
-- import Waterfall.Server.GithubPackage
import Waterfall.Server.WebhookHandler

open Waterfall.Server GitHub

open Cli in
def startServerCmd : Cmd := `[Cli|
  "" VIA run; ["0.0.1"]
  "Starts the Waterfall GH bot server"

  FLAGS:
    port : Nat;                   "Localhost port to start server on"
    "gh-app-id" : String;         "The bot's GitHub App ID"
    "pkey" : String;              "Path to private key file (.pem)"
    "gh-install-id" : Nat;         "The ID of an installation of the bot"
  EXTENSIONS:
    require! #["port", "gh-app-id", "pkey", "gh-install-id"]
]
where run (p : Parsed) : IO UInt32 := do
  let some port :=
      let raw := p.flag! "port" |>.as! Nat
      if h : _ then
        some (⟨raw,h⟩ : UInt16)
      else none
    | IO.throwServerError "port is not in range"; return 1

  let ghAppId                    := p.flag! "gh-app-id" |>.as! String
  let pkeyPath : System.FilePath := p.flag! "pkey" |>.as! String
  let ghInstallId                := p.flag! "gh-install-id" |>.as! Nat

  if !(← pkeyPath.pathExists) then
    IO.throwServerError s!"path {pkeyPath} does not exist"
    return 1

  let c := {
      ghAppId
      ghPrivateKeyPath := pkeyPath
      ghInstallId
      jwt   := ← IO.mkRef ""
      token := ← IO.mkRef ""
    }

  -- since we put bogus info in the jwt and token ref, update it
  updateJwt c
  updateToken c

  let tracking : Package.Tracking := ⟨[], Package.Tracking.defaultNoTrack⟩
  let s : Package.Repo.State := ⟨c, tracking⟩

  let waterfall := ⟨"JamesGallicchio", "waterfall"⟩
  let waterfall_test := ⟨"T-Brick", "waterfall-test"⟩

  let (e, s) ← Package.Repo.add_new_package waterfall_test (ref? := none) s
  IO.println e
  -- IO.println <| "\n".intercalate <| s.tracking.listing.map toString

  let _ ← Package.Repo.save (path := "waterfall.json") s

  let (e, s) ← Package.Repo.update_package waterfall "oogabooga" s
  IO.println e    -- should see `waterfall-test` as an update candidate

  let _ ← WebhookHandler.openServer (port := port) s

  return 0

def main (args : List String) : IO UInt32 :=
  startServerCmd.validate args

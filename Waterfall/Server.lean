import Http
import GitHub
import Socket
import Cli

namespace Waterfall.Server

structure Config where
  ghAppId : String
  ghPrivateKeyPath : System.FilePath
  jwt : IO.Ref String

private def updateJwt (c : Config) : IO Unit := do
  let res ← IO.Process.run {
    cmd:="./jwt",
    args:=#[
      "encode",
      "--alg", "RS256",
      "--exp=+600s",
      "--iss", c.ghAppId,
      "--secret", "@" ++ c.ghPrivateKeyPath.toString]
  }
  c.jwt.set res

def request (c : Config) [ToString T] (req : Http.Request T) : IO (Http.Response String) := do
  let res ← IO.Process.run {
    cmd := "curl"
    args := #[
      "-i",
      "--request",
      toString req.method,
      "--url", toString req.url,
      "--header", "Accept: application/vnd.github+json",
      "--header", s!"Authorization: Bearer {← c.jwt.get}",
      "--header", "X-GitHub-Api-Version: 2022-11-28"
    ]
  }
  match Parser.run parser res with
  | .ok _ res => return res
  | .error e => IO.throwServerError (toString e)
where parser : Http.Parser (Http.Response String) :=
  Http.Response.parse (Http.Parser.capture (Parser.dropMany Parser.anyToken) |>.map (·.toString))

def getInstallTok (c : Config) (iid : Int) : ExceptT String IO String := do
  let req :=
    GitHub.«apps/create-installation-access-token»
      (installation_id := iid)
      (body := .obj Lean.RBNode.leaf)
  let res :=
    GitHub.«apps/create-installation-access-token».getResponse
      (← request c req)

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


private def SIZE_MAX_PACK   : USize := .ofNat <| 64 * 2^10 -- 64 KB
private def SIZE_PACK_CHUNK : USize := .ofNat <|  4 * 2^10 --  4 KB

open Socket

def startServer (c : Config) (port : UInt16) : IO Unit := do
  let sock ← Socket.mk .inet .stream
  IO.println "Made socket"
  let sa := SockAddr4.v4 (.mk 127 0 0 1) port
  sock.bind sa
  IO.println s!"Bound to socket; now listening on port {port}."
  sock.listen 1024
  while true do
    let (client, sa') ← sock.accept

    IO.println s!"Accepting connection from {sa'.addr}"

    let mut done := false
    let mut acc : ByteArray := .empty
    while !done do
      let pkt ← client.recv SIZE_PACK_CHUNK
      if pkt.size.toUSize < SIZE_PACK_CHUNK then
        done := true
        acc := acc ++ pkt
      else
        acc := acc ++ pkt
        done := acc.size.toUSize >= SIZE_MAX_PACK

    IO.println <| s!"Received {acc.size} bytes"

    let str : String := String.fromUTF8Unchecked acc
    match
      Http.Request.parse (
        Http.Parser.capture <| Parser.dropMany Parser.anyToken)
      |>.run str
    with
    | .error e =>
      IO.println "failed to parse HTTP request!"
      IO.println e
    | .ok _s req =>
    match Lean.Json.parse req.body.toString with
    | .error e =>
      IO.println "failed to parse JSON body!"
      IO.println e
    | .ok json =>
    IO.println (json.pretty)

open Cli in
def startServerCmd : Cmd := `[Cli|
  "" VIA run; ["0.0.1"]
  "Starts the Waterfall GH bot server"

  FLAGS:
    port : Nat; "Localhost port to start server on"
    "gh-app-id" : String; "The bot's GitHub App ID"
    "pkey" : String; "Path to private key file (.pem)"
  EXTENSIONS:
    require! #["port", "gh-app-id", "pkey"]
]
where run (p : Parsed) : IO UInt32 := do
  let some port :=
      let raw := p.flag! "port" |>.as! Nat
      if h : _ then
        some (⟨raw,h⟩ : UInt16)
      else none
    | IO.throwServerError "port is not in range"; return 1

  let ghAppId := p.flag! "gh-app-id" |>.as! String
  let pkeyPath : System.FilePath :=
    p.flag! "pkey" |>.as! String

  if !(← pkeyPath.pathExists) then
    IO.throwServerError s!"path {pkeyPath} does not exist"
    return 1

  let c := {
      ghAppId
      ghPrivateKeyPath := pkeyPath
      jwt := ← IO.mkRef ""
    }

  -- since we put bogus info in the jwt ref, update it
  updateJwt c

  startServer
    (c := c)
    (port := port)

  return 0

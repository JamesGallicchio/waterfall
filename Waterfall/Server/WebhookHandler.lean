import Waterfall.Server.Event
import Waterfall.Server.GitHubPackage

import Http
import Socket
import Cli

namespace Waterfall.Server.WebhookHandler

private def SIZE_MAX_PACK   : USize := .ofNat <| 64 * 2^10 -- 64 KB
private def SIZE_PACK_CHUNK : USize := .ofNat <|  4 * 2^10 --  4 KB

open Socket GitHub

def handlePush (json : Lean.Json) : Package.Repo Unit := do
  IO.println "Push detected!!"

  let repository ← json.getObjVal? "repository"
  let full_name ← repository.getObjValAs? String "full_name"
  let ident ← Package.Ident.ofURL full_name

  let before ← json.getObjValAs? String "before"
  let after  ← json.getObjValAs? String "after"


  if ← Package.Repo.has_package ident before then
    let needs_pr ← Package.Repo.update_package ident after
    IO.println needs_pr
  else return () -- we aren't tracking this package so we don't care

def openServer (port : UInt16) : Package.Repo Unit := do
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
        match req.headers.find? (.custom (.ofString "X-Github-Event")) with
        | none       => IO.println "Received unknown packet"
        | some ["push"] => handlePush json
        | some event => IO.println s!"Received unknown event(s) {event}"

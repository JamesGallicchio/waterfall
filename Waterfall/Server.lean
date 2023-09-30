import Std
import Socket
import Http

namespace Waterfall.Server
open Socket

private def SIZE_MAX_PACK   : USize := .ofNat <| 64 * 2^10 -- 64 KB
private def SIZE_PACK_CHUNK : USize := .ofNat <|  4 * 2^10 --  4 KB

def main (port : UInt16) : IO Unit := do
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
    IO.println str
    match
      Http.Request.parse (Parser.takeMany Parser.anyToken)
      |>.run str
    with
    | .error e =>
      IO.println e
    | .ok _ req =>
      IO.println req.toRequestString

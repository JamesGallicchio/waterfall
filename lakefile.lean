import Lake
open Lake DSL

package «waterfall» {
  -- add package configuration options here
}

lean_lib «Waterfall» {
  -- add library configuration options here
}

@[default_target]
lean_exe server {
  root := `Server
}

@[default_target]
lean_exe client {
  root := `Client
}

require Cli from git "http://github.com/mhuisi/lean4-cli.git" @ "nightly"
require Std from git "https://github.com/leanprover/std4" @ "main"
require socket from git "https://github.com/hargoniX/socket.lean" @ "main"
require Http from git "https://github.com/JamesGallicchio/http" @ "main"

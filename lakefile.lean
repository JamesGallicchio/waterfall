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
lean_exe waterfall {
  root := `CLI
}

require Cli from git "http://github.com/mhuisi/lean4-cli.git" @ "nightly"
require «lean-openapi» from git "http://github.com/JamesGallicchio/lean-openapi" @ "main"
require socket from git "http://github.com/hargonix/Socket.lean" @ "main"

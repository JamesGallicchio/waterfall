import Lake
open Lake DSL

package «waterfall» {
  -- add package configuration options here
}

lean_lib «Waterfall» {
  -- add library configuration options here
}

@[default_target]
lean_exe «waterfall» {
  root := `Main
}

import Waterfall

def main (args : List String) : IO Unit :=
  Waterfall.Server.main args[0]!.toNat!.toUInt16

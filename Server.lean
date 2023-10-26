import Waterfall.Server

def main (args : List String) : IO UInt32 :=
  Waterfall.Server.startServerCmd.validate args

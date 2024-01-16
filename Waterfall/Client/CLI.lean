import Waterfall.Host.Git
import Cli

namespace Waterfall.Client.CLI

open Git.Local

def printStatus : IO Unit := do
  let feat := Ref.branchToFeat (← IO.ofExcept <| ← getCurRef ".")
  
  sorry

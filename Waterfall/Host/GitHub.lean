import Waterfall.Core
import GitHub
import Socket

namespace Waterfall.GitHub

/-- The primary `RepoHost` for many packages. We implement
  features in GitHub as PRs, with underlying `git` branches. -/
instance Types : RepoHostTypes where
  repoId := String
  featId := String
  commitId := String

def RepoHost : RepoHost Types IO where
  getFeatureInfo := sorry

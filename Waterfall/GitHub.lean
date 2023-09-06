import Waterfall.Feature

namespace Waterfall

/-- The primary `RepoHost` for many packages. We implement
  features in GitHub as PRs, with underlying `git` branches. -/
instance GitHub : RepoHost where
  repoId := String
  featId := String
  commitId := String


/-! Defines features and the feature tree. -/

namespace Waterfall

/-- A repository host is the abstract way we get information
  about repositories and features in the world.

  The canonical implementation is [GitHub].
-/
class RepoHost.{u} where
  (repoId featId commitId : Type u)

structure FeatureInfo (host : RepoHost) where
  parent : host.featId
  (base head : host.commitId)

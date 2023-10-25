
/-! Defines features and the feature tree. -/

namespace Waterfall

/-- A repository host is the abstract way we get information
  about repositories and features in the world.

  The canonical implementation is [GitHub].
-/
structure RepoHostTypes.{u} where
  (repoId featId commitId : Type u)

structure FeatureInfo (tys : RepoHostTypes) where
  parent : Option tys.featId
  (base head : tys.commitId)

structure RepoHost.{u} (tys : RepoHostTypes.{u}) (m : Type u → Type u) [Monad m] where
  getFeatureInfo : tys.featId → m (FeatureInfo tys)

import Waterfall.Core.RepoHost

namespace Waterfall.Server

inductive Event (ty : RepoHostTypes)
/-- new feature created -/
| featNew (repo : ty.repoId) (feat : ty.featId) (info : FeatureInfo ty)
/-- feature info updated -/
| featUpd (repo : ty.repoId) (feat : ty.featId) (old new : FeatureInfo ty)
/-- existing feature released into parent -/
| featRel (repo : ty.repoId) (feat : ty.featId) (info : FeatureInfo ty) (h : info.parent.isSome)

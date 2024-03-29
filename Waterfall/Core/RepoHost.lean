
import Lean

/-! Waterfall repositories are defined by the operations
that can be done on them.
A given implementation might be built on any version control system,
so we must stay pretty general.
-/

namespace Waterfall

structure RepoHostTypes.{u} where
  (repoId featId commitId : Type u)

structure FeatureInfo (tys : RepoHostTypes) where
  parent : Option tys.featId
  (base head : tys.commitId)

namespace FeatureInfo

instance [Repr tys.featId] [Repr tys.commitId] : Repr (FeatureInfo tys) where
  reprPrec info prec := .group (.nest 1 <| "{" ++ .line ++
      .group ("parent := " ++ reprPrec info.parent prec ++ ",") ++ .line ++
      .group ("base := " ++ reprPrec info.base prec ++ ",") ++ .line ++
      .group ("head := " ++ reprPrec info.head prec ++ ",") ++ .line ++
    "}")

open Lean in
instance [ToJson tys.featId] [ToJson tys.commitId]
  : ToJson (FeatureInfo tys) where
  toJson info := Json.mkObj [
    ("parent", toJson info.parent),
    ("base", toJson info.base),
    ("head", toJson info.head)
  ]

open Lean in
instance [FromJson tys.featId] [FromJson tys.commitId]
  : FromJson (FeatureInfo tys) where
  fromJson? data := do
    let parent ← data.getObjValAs? _ "parent"
    let base ← data.getObjValAs? _ "base"
    let head ← data.getObjValAs? _ "head"
    return {parent,base,head}

end FeatureInfo

namespace RepoHost

variable (tys : RepoHostTypes) (m) [Monad m]

inductive GetFeatureInfo.Error  : Type u
| invalidFeat (feat : tys.featId)
| other (err : String)

instance [ToString tys.featId] : ToString (GetFeatureInfo.Error tys) where
  toString
    | .invalidFeat feat => s!"invalid feature: {feat}"
    | .other err => s!"{err}"

abbrev GetFeatureInfo :=
  (repo : tys.repoId) →
  (feat : tys.featId) →
  ExceptT (GetFeatureInfo.Error tys) m (FeatureInfo tys)

inductive CreateFeature.Error : Type u
| invalidName (err : String)
| invalidFeat (feat : tys.featId)
| invalidCommit (commit : tys.commitId)
| other (err : String)

instance [ToString tys.repoId] [ToString tys.featId] [ToString tys.commitId] : ToString (CreateFeature.Error tys) where
  toString
    | .invalidName n => s!"invalid feature: {n}"
    | .invalidFeat feat => s!"invalid feature: {feat}"
    | .invalidCommit c => s!"invalid feature: {c}"
    | .other err => s!"{err}"

inductive RebaseError : Type u
| invalidFeat (feat : tys.featId)
| other (s : String)

end RepoHost

/-- A repository host is the abstract way we get information
  about repositories and features in the world.

  The canonical implementation is [Git].
-/
structure RepoHost.{u} (tys : RepoHostTypes.{u}) (m : Type u → Type u) [Monad m] where
  getFeatureInfo : RepoHost.GetFeatureInfo tys m
  /-- Create a new feature with name `name` as a child of `parent`. -/
  createFeature
    : (repo : tys.repoId) → (name : String) → (parent : tys.featId) →
      ExceptT (RepoHost.CreateFeature.Error tys) m tys.featId
  /-- Rebase `child` onto `parent`. If `parent` is `none`,
    then `child`'s current parent is used.

    After this operation, the base commit for `child`
    should be the head commit of `parent`. -/
  rebase
    : (repo : tys.repoId) → (child : tys.featId) → (parent : Option tys.featId) →
      ExceptT (RepoHost.RebaseError tys) m PUnit

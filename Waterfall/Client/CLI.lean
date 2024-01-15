import Waterfall.Host.Git

namespace Waterfall.Client.CLI

namespace FeatureTree

open Git.Local
variable (repo : Types.repoId)

inductive Tree (α : Type u) where
| node : α → List (Tree α) → Tree α
deriving Inhabited

namespace Tree
def select : Tree (α × Bool) → Tree (α × Bool)
| .node (x,_) cs => .node (x,true) cs
def data : Tree α → α
| .node x _ => x
end Tree

structure View where
  tree : Tree (Types.featId × Bool)
deriving Inhabited

namespace View

def mapSelected [Monad m]
    (f : Tree (Types.featId × Bool) → m (Tree (Types.featId × Bool)))
    (v : View) : ExceptT Unit m View := do
  return { tree := ← aux v.tree }
where
  aux  : Tree _ → m (Except Unit (Tree _))
  | .node (feat,s) c => do
      if s then
        return .ok (← f (.node (feat,s) c))
      else
        match ← auxL c with
        | .error () => return .error ()
        | .ok c' => return .ok (.node (feat,s) c')
  auxL : List (Tree _) → m (Except Unit <| List <| Tree _)
  | [] => return .error ()
  | hd :: tl => do
    match ← aux hd with
    | .ok hd' => return .ok (hd' :: tl)
    | .error () =>
    match ← auxL tl with
    | .ok tl' => return .ok (hd :: tl')
    | .error () => return .error ()

def up (v : View) : View :=
  { tree := match v.tree with
  | .node (feat,true) children => .node (feat,true) children
  | .node (feat,false) children =>
    match aux children with
    | .ok (selectParent,children') =>
        .node (feat,selectParent) children'
    | .error () => panic! "view has nothing selected?"
  }
where
  /-- returns .ok (selectAbove, children') if find selected,
      otherwise .error () -/
  aux : List (Tree _) → Except Unit (Bool × List (Tree _))
    | [] => .error ()
    | .node (f, s) c :: rest =>
      if s then
        .ok (true, .node (f, false) c :: rest)
      else
        match aux c with
        | .ok (selectAbove, c') =>
          .ok (false, .node (f, selectAbove) c' :: rest)
        | .error () =>
        match aux rest with
        | .error () => .error ()
        | .ok (selectAbove, rest') =>
          .ok (false, .node (f, selectAbove) c :: rest')

def down (v : View) : View :=
  { tree := match v.tree with
  | .node (feat,true) children => .node (feat,true) children
  | .node (feat,false) children =>
    match aux children with
    | .ok children' =>
        .node (feat,false) children'
    | .error () => panic! "view has nothing selected?"
  }
where
  /-- returns .ok (children') if find selected,
      otherwise .error () -/
  aux : List (Tree _) → Except Unit (List (Tree _))
    | [] => .error ()
    | .node (f, s) c :: rest =>
      if s then
        match rest with
        | [] => .ok [.node (f,true) c]
        | .node (f2,_) c2 :: rest =>
            .ok (.node (f, false) c :: .node (f2, true) c2 :: rest)
      else
        match aux c with
        | .ok c' =>
            .ok (.node (f, false) c' :: rest)
        | .error () =>
        match aux rest with
        | .ok rest' =>
            .ok (.node (f, false) c :: rest')
        | .error () => .error ()

def left (v : View) : View :=
  { tree := match v.tree with
  | .node (feat,true) children => .node (feat,true) children
  | .node (feat,false) children =>
    match aux children with
    | .ok (selectParent,children') =>
        .node (feat,selectParent) children'
    | .error () => panic! "view has nothing selected?"
  }
where
  /-- returns .ok (selectAbove, children') if find selected,
      otherwise .error () -/
  aux : List (Tree _) → Except Unit (Bool × List (Tree _))
    | [] => .error ()
    | .node (f, s) c :: rest =>
      if s then
        .ok (true, .node (f, false) c :: rest)
      else
        match aux c with
        | .ok (selectAbove, c') =>
          .ok (false, .node (f, selectAbove) c' :: rest)
        | .error () =>
        match aux rest with
        | .error () => .error ()
        | .ok (selectAbove, rest') =>
          .ok (selectAbove, .node (f, false) c :: rest')

def right (v : View) : View :=
  match mapSelected (m := Id) (fun tree =>
    match tree with
    | .node (f,_) (c1 :: rest) => .node (f,false) (c1.select :: rest)
    | _ => tree
    ) v with
  | .error () => panic! "nothing selected"
  | .ok v => v

end View

def withTails : List α → List (α × List α)
| [] => []
| hd :: tl => (hd,tl) :: withTails tl

@[simp] theorem mem_withTails (x : α × List α) (L : List α)
  : x ∈ withTails L ↔ (x.1 :: x.2) <:+ L := by
  rcases x with ⟨hd,tl⟩; simp
  induction L <;> simp_all [withTails]
  next hd' tl' ih =>
  rw [List.suffix_cons_iff]
  simp

def renderView (v : View) : IO Unit :=
  renderTree v.tree "" ""
where renderTree (t : Tree _) (firstLineIndent : String) (restLinesIndent : String) : IO Unit := do
  match t with
  | .node (a,isSelected) children =>
    IO.println (firstLineIndent ++ " " ++
      if isSelected then "[" ++ toString a ++ "]"
      else toString a)
    for h : (c,last) in (withTails children).map fun (a,tl) => (a,tl.isEmpty) do
      have : c ∈ children := by
        simp at h; rcases h with ⟨⟨c',tl⟩,sub,rfl,_⟩
        simp_all; apply sub.subset; simp
      if last then
        renderTree c (restLinesIndent ++ "  └") (restLinesIndent ++ "   ")
      else
        renderTree c (restLinesIndent ++ "  ├") (restLinesIndent ++ "  │")

partial def createTree (feat : Types.featId) : IO (Tree Types.featId) := do
  let (_,data) ← IO.ofExcept <| (← getFeatureData repo feat).mapError (fun _ => ())
  return .node feat (← data.children.mapM createTree)

def initView : IO View := do
  let (_,{rootFeat,..}) ← IO.ofExcept <| ← Git.Local.getConfig repo
  return { tree := .node (rootFeat,true) [] }

def expandSelected (v : View) : IO View := do
  match ← v.mapSelected (fun
    | .node (f,s) cs => do
      let (_,{children,..}) ← IO.ofExcept <| ← getFeatureData repo f
      let mut cs := cs
      for c in children do
        cs := @List.insert _ ⟨(·.data = ·.data)⟩ (.node (c,false) []) cs
      return .node (f,s) cs
  ) with
  | .error () =>
    panic! "nothing selected"
  | .ok v =>
    return v

def run : IO Unit := do
  let mut view ← initView repo
  while true do
    renderView view
    sorry

end FeatureTree

brainstorm zone

## features
Commits and branches are very low-level abstractions -- too low-level for our needs.
So, first we need a high-level abstraction for version control: **features**.

We start with a special root feature, which represents the repo's initial commit.
Then, every non-root feature has:
- a parent feature
- a base commit, which is somewhere in the parent's history
- 0 or more commits on top of the base commit (i.e. a head commit)

The idea is that a child feature extends the parent in some way,
and hopes to eventually be released/incorporated into the parent.
Since each feature only has one (or zero) parents,
the features form a tree.

Features are sort of like a combination of **a branch and a GitHub PR**.
Like a branch, features have a head commit.
And, most branches have parents (even if git has no way to track this), which features track explicitly.
Then, like PRs, they are trying to be upstreamed into their parent.

Rebases are the canonical way to move changes around.
If a feature's base commit becomes out of date (something was pushed to the parent),
we can straightforwardly rebase onto the parent's new head.
Or perhaps we want to change the parent feature;
this is also a simple rebase, since we kept track of the feature's base commit.

How about releases? To get changes from a child feature C into a parent feature P, we do the following magic trick:
1. Rebase C's changes onto P's head commit*
2. Move P's head commit to C's head commit
3. Set C's base commit to C's head commit, and its change-set to empty.

At this point, C's changes have made it upstream, and C is an empty shell of its former change-containing self.
If desired, this is a great time to make C's children instead be children of P.
Then we can archive/delete/forget about C.

\* If this step fails, or if the new head fails to compile, the release should be aborted.
In practice waterfall implements a merge queue for releases,
so that it can ensure this step succeeds before actually changing anything.

## dependencies
Package dependencies are an interesting special case for VCS,
because packages are guaranteed to have disjoint source code.
A git-diff for package A is guaranteed not to conflict
(at least at the source level) with a git-diff for package B.

This means we can express more complicated dependencies *across* repos than we can *within* repos.
In particular, we can have multiple parent features -- one from each dependency.

# waterfall
infra for doing version control across Lean packages

Some opinions:
1. Dependencies should always be up to date.
2. Main should always compile, see [e.g. bors](https://bors.tech/essay/2017/02/02/pitch/).
3. Upstream packages should be *allowed to care* about downstream packages.

(1) is easy to solve -- automatically update downstream packages when an upstream updates.
But what if the upstream change breaks the downstream package?
To maintain (2), we need to **patch the downstream**.
And, given (3), ideally we would write and test this downstream patch *before* releasing the upstream changes to main.

That's where waterfall comes in.
It attempts to straddle the divide between monorepos and multirepos,
by re-building the monorepo workflow atop a decentralized multirepo like GitHub.

## current plan
Right now I am hacking together a first-order approximation via GitHub Actions.
The goal is to test out some GitHub UI and see what features we really need.

Long term my plan is to build a proper web service that integrates with GitHub,
but is able to support workflows that we can't build in GH Actions alone.

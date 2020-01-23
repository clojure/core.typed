# Historical repository organization

core.typed has had many homes over the years. Unfortunately, the
git history and historical Maven coordinates can be rather confusing,
so this document serves explain it for posterity.

Author: Ambrose Bonnaire-Sergeant

## Original home (21 Nov 2011 - Jan 2013)

Typed Clojure started life in [frenchy64/typed-clojure](https://github.com/frenchy64/typed-clojure).
The original commit is [d1edc56c](https://github.com/frenchy64/typed-clojure/commit/d1edc56cc5b8ed6c2e380bff2d7c28d43968bf1b).

The Clojars coordinates was [typed/typed](https://clojars.org/typed).

The [first release](https://clojars.org/typed/versions/0.1) was `[typed "0.1"]`, and the
main namespace was called [typed.core](https://github.com/frenchy64/typed-clojure/blob/829f1d80e598e21be5cdb809d8f741c525b5332e/src/typed/core.clj)
and was 9,339 lines long.

The final released version was 0.1.6 ([commit](https://github.com/frenchy64/typed-clojure/commit/4351318ffc1ab0c2b91550c5c165db27fbdcb474),
released for Clojure 1.5.0-RC1.

The final relevant master branch commit was [1c9a99ac](https://github.com/frenchy64/typed-clojure/commit/1c9a99ac7387545caed545bdcfa4c183c31fdbe8),
for `0.1.7-SNAPSHOT`.

## Early Contrib + Clojars (Feb 2013 - March 2013)

I [offered](https://groups.google.com/d/msg/clojure-dev/ryYzI1Bj0p0/YNEvFxx4ESIJ) Typed Clojure up for contrib,
and that became the [clojure/core.typed](https://github.com/clojure/core.typed) repository.

At that point, there were two first commits, joined by a merge commit.

```
*   1c9a99ac Merge branch 'master' of github.com:clojure/core.typed (clojure/core.typed)
|\  
| * c7e5b19d First commit. (clojure/core.typed first commit)
* 2d95c459 Further along CLJS compiler (frenchy64/typed-clojure)
| ...
* (all of frenchy64/typed-clojure)
| ...
* d1edc56c Some sketches with syntax (frenchy64/typed-clojure first commit)
```

The left branch is the entire `frenchy64/typed-clojure` history, starting at
[d1edc56c](https://github.com/clojure/core.typed/commit/d1edc56c).

The right branch starts at the "real" first commit of `clojure/core.typed`
[c7e5b19d](https://github.com/clojure/core.typed/commit/c7e5b19d), which
just included a `README.md` file.

The new Clojars coordinate was [core.typed/core.typed](https://clojars.org/core.typed).
The first released version was 0.1.7, with a new main namespace
[clojure.core.typed](https://github.com/clojure/core.typed/blob/4da62ed2604cb6cc338e9d69476915599a2e222f/src/main/clojure/clojure/core/typed.clj)
at 711 lines long.

The final released version at this coordinate was `[core.typed "0.1.8"]`.

## Contrib + org.clojure Maven Central coordinate (March 2013 - November 2018)

Then, core.typed was setup to release to Maven central via the contrib
build process under the [org.clojure/core.typed](https://search.maven.org/artifact/org.clojure/core.typed)
coordinate.

This housed versions `0.1.9` ([commit](https://github.com/clojure/core.typed/commit/28669b31)) to `0.6.0`.

## Split repositories (November 2018 - January 2020)

As explained [here](https://groups.google.com/d/msg/clojure-dev/ij9Ynpzzaaw/hPpg65LYAwAJ), core.typed
was split into many different repositories.

## Back to Monorepo (January 2020 -)

core.typed was merged back into a single repository, continuing on from 
the original `clojure/core.typed` master branch from commit
[241a703d](https://github.com/clojure/core.typed/commit/241a703d).

To perform the merge, I created a fresh repository with a single
commit [f95bdf27](https://github.com/clojure/core.typed/commit/f95bdf27).

Then, in the fresh repository, I ran the following script to merge the 6 individual repos:

```
#!/bin/bash

set -e

git subtree add -P module-analyzer.jvm https://github.com/clojure/core.typed.analyzer.jvm.git 31892d2a3e1083d0cd495c39e4704eacf14e0d96
git subtree add -P module-annotator.jvm https://github.com/clojure/core.typed.annotator.jvm.git 25d2d433c983b16445f8da9d9cc80789e9673bdf
git subtree add -P module-checker.jvm https://github.com/clojure/core.typed.checker.jvm.git 33f3e269a1de640543a6c64fc33fa6faaf071e7a
git subtree add -P module-runtime.jvm https://github.com/clojure/core.typed.runtime.jvm.git c0a07a2dd2d1d67196f38b517c47905531179842
git subtree add -P module-lang.jvm https://github.com/typedclojure/core.typed.lang.jvm.git da7a11930fbbc237376cd14550231c90cab05146
git subtree add -P module-lib.clojure https://github.com/typedclojure/core.typed.lib.clojure.git b090c2f3d94714e8d248aad0b3e1b2305dd58eb1
```

Then, I merged this fresh repository with
[241a703d](https://github.com/clojure/core.typed/commit/241a703d).
I actually intended to run the script directly on
[241a703d](https://github.com/clojure/core.typed/commit/241a703d),
and the extra merge was a mistake.

I intended this to have introduced 6 additional new orphan "first commits".
Unfortunately, it 
introduced a [7th](https://github.com/clojure/core.typed/commit/f95bdf27),
the first commit for my fresh repository above.

You can see all orphaned commits (9 at this point!) with this command:

```
git rev-list --max-parents=0 9a47983d2b51befde6d200d874c0f238c5226d4f | xargs git log
```

So there are 9 "first commits" in core.typed:
- 1 for the original `frenchy64/typed` repository (d1edc56c)
- 1 for the move to `clojure/core.typed` repository (c7e5b19d)
- 6 for adding back the histories of each split repository (5fd81305, 5f97f21f, e012296a, 0e44e951, eb0691b0, 216a3e84)
- 1 for a mistake carried over from merging a dry-run of the previous operation (f95bdf27)

All 9 "first commits" are visualized below.

```
...
*   f533b942 Merge remote-tracking branch 'monorepo/master' into monorepo
|\  
| * d02b533a skip mvn tests
| ...
| *   277faedb Add 'module-lib.clojure/' from commit 'b090c2f3d94714e8d248aad0b3e1b2305dd58eb1'
| |\  
| | * b090c2f3 move to github actions
| | ...
| | * 5fd81305 init
| *   e299b1d7 Add 'module-lang.jvm/' from commit 'da7a11930fbbc237376cd14550231c90cab05146'
| |\  
| | * da7a1193 move to github actions
| | ...
| | * 5f97f21f initial commit
| *   e5c4dc9a Add 'module-runtime.jvm/' from commit 'c0a07a2dd2d1d67196f38b517c47905531179842'
| |\  
| | * c0a07a2d move to github actions
| | ...
| | * e012296a CTYP-145: now also producing a zero-dependency "core.typed.rt" artifact that contains only annotation machinery, suitable for use in libraries
| *   0c8b10c9 Add 'module-checker.jvm/' from commit '33f3e269a1de640543a6c64fc33fa6faaf071e7a'
| |\  
| | * 33f3e269 download latest clj in circleci
| | ...
| | * 0e44e951 core.typed depends on core.typed.rt
| *   7cd0fbfa Add 'module-annotator.jvm/' from commit '25d2d433c983b16445f8da9d9cc80789e9673bdf'
| |\  
| | * 25d2d433 move to github actions
| | ...
| | * eb0691b0 create module-infer and module-analyzer-jvm maven submodules
| *   7ed25546 Add 'module-analyzer.jvm/' from commit '31892d2a3e1083d0cd495c39e4704eacf14e0d96'
| |\  
| | * 31892d2a move to github actions
| | ...
| | * 216a3e84 create module-infer and module-analyzer-jvm maven submodules
| * f95bdf27 cloning file
* 241a703d prepare for monorepo merge
...
*   1c9a99ac Merge branch 'master' of github.com:clojure/core.typed (clojure/core.typed)
|\  
| * c7e5b19d First commit. (clojure/core.typed first commit)
* 2d95c459 Further along CLJS compiler (frenchy64/typed-clojure)
| ...
* d1edc56c Some sketches with syntax (frenchy64/typed-clojure first commit)
```

### Merge core.typed.checker.js into monorepo (January 2020)

```
git subtree add -P module-checker.js https://github.com/clojure/core.typed.checker.js.git cf4cd218ba1b721fb1105129849ae1c74dc30ebc
```

```
...
*   497087b8 (frenchy64/merge-cljs-checker) Add 'module-checker.js/' from commit 'cf4cd218ba1b721fb1105129849ae1c74dc30ebc'
|\
| * cf4cd218 readme
| ...
| * b03896d1 Move Clojurescript typechecker into own mvn module
* e6659d6a history
...
```

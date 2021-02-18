# Preliminaries
 
Before contributing to any Typed Clojure asset, please read the [Typed Clojure Contributor Code of Conduct](CODE_OF_CONDUCT.md).

This is a [Clojure contrib] project.

Under the Clojure contrib [guidelines], this project cannot accept
pull requests. All patches must be submitted via [JIRA].
Pull requests are used for developing patches in core.typed, see the Contribution Guide below.

See [Contributing] and the [FAQ] on the Clojure development [wiki] for
more information on how to contribute.

[Clojure contrib]: https://clojure.org/community/contrib_libs
[Contributing]: https://clojure.org/community/contributing
[JIRA]: https://clojure.atlassian.net/browse/CTYP

# Contribution Guide

This guide should be followed to develop patches for core.typed.

## Idea

If you have an idea that you would like to discuss, first post it on the [core.typed mailing list](https://groups.google.com/forum/#!topic/clojure-core-typed).

For example, see [this post](https://groups.google.com/forum/#!topic/clojure-core-typed/ZEK2xbqBcb).

## JIRA Issue

JIRA is the main bug tracker for core.typed, so all issues should be posted there.

For example, [CTYP-305](http://dev.clojure.org/jira/browse/CTYP-305) is a good template to follow.

Here is the JIRA syntax to generate that issue:

```
h1. Problem

The AST rewriting algorithm deletes the contract check for casts.

{code}
(cf (cast Int nil))
;=> Int
{code}

This passes static type checking, but does not run the code.

h1. Solution

This happens because a t/cast expression is always rewritten to return `nil`. The AST rewriting algorithm should be fixed to preserve the original contract check.

*Pull request*: [92|https://github.com/typedclojure/core.typed/pull/92]
*Commit*: [a05205be|https://github.com/clojure/core.typed/commit/a05205be2a3869a7e7584fc5f0b08d1690a8a990]
*Version*: 0.3.20
```

The `Problem` section mostly describes the symptoms of the problem. Here a particular expression
has some strange behaviour.

The `Solution` section describes the (attempted) solution and reveals more information about the original problem.
This can be left blank initially.

The `Pull request` section is a link to a pull request that runs the continuous integration tests.
*Do not* send a pull request to `clojure/core.typed`; please send to `typedclojure/core.typed`, which
is hooked into TravisCI.

The `Commit` section will link to the final commit/s in the `clojure/core.typed` repository
that fix this issue. A maintainer will typically update this value before resolving the issue.

The `Version` section is the minimum `core.typed` release that will contain these changes.

Here's a handy template with a hole for the symptoms.

```
h1. Problem

<Describe symptoms>

h1. Solution

*Pull request*:
*Commit*:
*Version*:
```

## Leiningen vs Maven

core.typed uses a hybrid of `lein` and `Maven` for development.

To open a REPL, run `lein repl`. The port the REPL opens on is hardcoded and can be changed in `project.clj`.

The Maven project structure builds two JAR's.

* `org.clojure/core.typed.rt` (`module-rt/pom.xml)
* `org.clojure/core.typed` (`module-check/pom.xml)

The larger `core.typed` project depends on `core.typed.rt`.

The `pom.xml` at the top level links both of these POM files.
The JAR dependencies are specified in the following files:

* `pom.xml`: contains dependencies common to `core.typed.rt` and `core.typed`
* `module-rt/pom.xml`: only inherits dependencies from `pom.xml`
* `module-check/pom.xml`: inherits dependencies from `pom.xml` and defines the dependencies needed for the type checker

In Leiningen, all dependencies are simply defined in `project.clj`. Be careful when changing
dependencies: they should be changed in *both* `project.clj` and the appropriate `pom.xml` files.

To run the unit tests, use `mvn test`.

To install a local version of the current branch under the artifact specified in `pom.xml`
  (eg. `0.3.20-SNAPSHOT` for `<version>0.3.20-SNAPSHOT</version>`)
  run `mvn install`.

To skip the unit tests, run `mvn install -DskipTests=true`.

You should ignore the project version in `project.clj`.

## Commits and useful Git/Github information

If you are familiar with git, you might only want to skim this section.

*dev and prod repositories*:

There are two useful Git repositories to work with, the canonical `prod` repository `clojure/core.typed`,
and the internal `dev` repository; only `dev` accepts pull requests.

These remote aliases might be useful in your fork.

```
git remote add dev https://github.com/typedclojure/core.typed.git
git remote add prod https://github.com/clojure/core.typed.git
```

If you prefer ssh, use the following commands.

```
git remote add dev git@github.com:typedclojure/core.typed.git
git remote add prod git@github.com:clojure/core.typed.git
```

*Forking*:

Always fork the `dev` repository on Github to easily send pull requests only to `dev`, not `prod`. The `master` branch
on `dev` and `prod` should be identical.

*Branching*:

When starting work on an issue, you should create a new branch from `master`.

For example, here is one way to start working on `CTYP-305`: by creating a
new branch called `ctyp-305`.

```
git checkout -b ctyp-305 master
```

*Pull request*:

Send a pull request to [dev](https://github.com/typedclojure/core.typed) to trigger the TravisCI tests.

Push

```
git rebase master
```

*Squashing commits*:



*Commit messages*:
[a05205be](https://github.com/clojure/core.typed/commit/a05205be2a3869a7e7584fc5f0b08d1690a8a990)

Example commit message.

```
CTYP-305: Fix bad rewrite for `t/cast` expressions
```

# core.typed

<a href='http://typedclojure.org'><img src='images/part-of-typed-clojure-project.png'></a>

Optional typing in Clojure, as a library.

## Usage Overview

core.typed is separated into modules. You'll want the full type checker at development
time, and the runtime dependencies in production.

In Clojure CLI's `deps.edn`, this will look something like this:

```clojure
{:deps {org.clojure/core.typed.runtime.jvm {:mvn/version "0.7.2"}}
 :aliases {:dev {:extra-deps {org.clojure/core.typed.checker.jvm {:mvn/version "0.7.2"}}}}}
```

You can then start a dev repl with `clj -A:dev`.

In Leiningen's `project.clj`, something like this:

```clojure
(defproject a-project "0.0.1-SNAPSHOT"
  :dependencies [[org.clojure/core.typed.runtime.jvm "0.7.2"]]
  :profiles {:dev {:dependencies [[org.clojure/core.typed.checker.jvm "0.7.2"]]}})
```

Then, `lein repl` will automatically activate the `:dev` profile. Verify the type
checker is not included in uberjars with `lein with-profile uberjar deps :tree`.

## Releases and Dependency Information

Latest stable release is 0.7.2.

See modules for specific version coordinates:

* [core.typed.checker.jvm](module-checker.jvm/README.md): The type checker
* [core.typed.runtime.jvm](module-runtime.jvm/README.md): Runtime dependencies
* [core.typed.annotator.jvm](module-annotator.jvm/README.md): Automatic annotator
* [core.typed.analyzer.jvm](module-analyzer.jvm/README.md): Analyzer
* [core.typed.lib.clojure](module-lib.clojure/README.md): Base type annotations
* [core.typed.lang.jvm](module-lang.jvm/README.md): Extensible languages

## Compatibility

`core.typed` supports Clojure 1.10.1 and JDK 1.8+.

## Mailing List and IRC

Use the core.typed [mailing list](https://groups.google.com/forum/?fromgroups#!forum/clojure-core-typed) for core.typed discussion.

## Documentation

See [wiki](https://github.com/clojure/core.typed/wiki).

[API Reference](http://clojure.github.io/core.typed/)

[Ambrose's blog](http://frenchy64.github.io/)

## Leiningen Plugin

[lein-typed](https://github.com/frenchy64/lein-typed)

## Vim Plugin

[vim-typedclojure](https://github.com/typedclojure/vim-typedclojure)

## Emacs Mode

[typed-clojure-mode](https://github.com/typedclojure/typed-clojure-mode)

## Examples

[core.async Rock-paper-scissors](https://github.com/clojure/core.typed/blob/master/module-check/src/test/clojure/clojure/core/typed/test/rps_async.clj)

## Developer Information

- [Typed Clojure Contributor Code of Conduct](CODE_OF_CONDUCT.md)
- [Contributing guidelines](CONTRIBUTING.md)
- [GitHub Project](https://github.com/clojure/core.typed)
- [Bug Tracker](http://dev.clojure.org/jira/browse/CTYP)
- [Continuous Integration](http://build.clojure.org/job/core.typed/)
- [Compatibility Test Matrix](http://build.clojure.org/job/core.typed-test-matrix/)

<!---
## Future work

* Equality filters for occurrence typing
* Unify AST with ClojureScript
* Namespace dependency management
* Track changes to Typed Racket
  * https://github.com/plt/racket/compare/6105ce8b2087...71d6189132ce
-->

## Contributors

Thanks to the following people for contributing to core.typed:

* Stephen Compall (S11001001)
* Andrew Brehaut (brehaut)
* Christopher Spencer (cspencer)
* Reid McKenzie (arrdem)
* Di Xu (xudifsd)
* Nicola Mometto (Bronsa)
* Chas Emerick (cemerick)
* Jake Goulding (shepmaster)
* Andy Fingerhut
* Aravind K N (arav93)
* Allen Rohner (arohner)
* Minori Yamashita (ympbyc)
* Kyle Kingsbury (aphyr)
* Nathan Sorenson
* Tobias Kortkamp (t6)
* Alejandro Gomez (dialelo)
* Piotr Jarzemski (kazuhiro)

## Sponsors

Development is sponsored (via [crowdfunding](http://www.indiegogo.com/projects/typed-clojure)) by

<div>
  <div>
    <a href="http://brickalloy.com/">
      <img src="http://typedclojure.org/images/sponsors/brick_alloy_2_37.png" alt="Brick Alloy">
    </a>
  </div>
  <div class="col-md-2">
    <a href="http://cognitect.com/">
      <img src="http://typedclojure.org/images/sponsors/cognitect_black_1_27.png" alt="Cognitect">
    </a>
  </div>
  <div>
    <a href="http://www.circleci.com/">
      <img src="http://typedclojure.org/images/sponsors/circleci_logoweb.jpg" alt="CircleCI">
    </a>
  </div>
  <div class="col-md-2">
    <a href="https://www.hackerschool.com/">
      <img src="http://typedclojure.org/images/sponsors/hackerschool.png" alt="Hacker School">
    </a>
  </div>
</div>
<div>
  <div>
    <a href="http://snowplowanalytics.com/">
      <img src="http://typedclojure.org/images/sponsors/snowplow-logo.png" alt="Snowplow Analytics">
    </a>
  </div>
  <div>
    <a href="http://leonidasoy.fi/">
      <img src="http://typedclojure.org/images/sponsors/leonidas.png" alt="Leonidas">
    </a>
  </div>
  <div>
    <a href="http://getprismatic.com/">
      <img src="http://typedclojure.org/images/sponsors/prismatic-logo.png" alt="Prismatic">
    </a>
  </div>
</div>
<div>
  <div style="background-color: #555;">
    <a href="http://www.thortech-solutions.com/">
      <img src="http://typedclojure.org/images/sponsors/thortech.png" alt="ThorTech Solutions">
    </a>
  </div>
  <div>
    <a href="http://sonian.com/">
      <img src="http://typedclojure.org/images/sponsors/sonian.png" alt="Sonian">
    </a>
  </div>
  <div>
    <a href="https://twitter.com/srseverance">Shannon Severance</a>
  </div>
</div>
<div>
  <div>
    <a href="http://cursiveclojure.com/">
      <img src="http://typedclojure.org/images/sponsors/cursive.png" alt="Cursive Clojure">
    </a>
  </div>
</div>
</div>


## YourKit

YourKit is kindly supporting core.typed and other open source projects with its full-featured Java Profiler.
YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:

* <a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
* <a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).

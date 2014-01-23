# core.typed

<a href='http://typedclojure.org'><img src='images/part-of-typed-clojure-project.png'></a>

Gradual typing in Clojure, as a library.

## Releases and Dependency Information

Latest stable release is 0.2.25.

Leiningen dependency information:

```clojure
[org.clojure/core.typed "0.2.25"]

...
; for very recent releases
:repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}

; for slim jars, follow version string with: :classifier "slim"
```

Maven dependency information:

```XML
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>core.typed</artifactId>
  <version>0.2.25</version>
  <!-- slim jar -->
  <!-- <classifier>slim</classifier> -->
</dependency>
```

The default jars contain AOT files for faster loading. If jar size is a concern, consider
using the slim jar in production.

## Crowdfunding

[Support full-time core.typed development](http://www.indiegogo.com/projects/typed-clojure/)

## [Talk] Clojure Conj 2012

[Video](http://www.youtube.com/watch?v=wNhK8t3uLJU)

## Mailing List and IRC

Use the core.typed [mailing list](https://groups.google.com/forum/?fromgroups#!forum/clojure-core-typed) for core.typed discussion, 
or try #typed-clojure on Freenode (the main developer is ambrosebs).

[IRC Logs](http://logs.lazybot.org/irc.freenode.net/%23typed-clojure) (Thanks to [Anthony Grimes](https://twitter.com/IORayne)!)

## Documentation

See [wiki](https://github.com/clojure/core.typed/wiki).

[API Reference](http://clojure.github.io/core.typed/)

[Ambrose's blog](http://frenchy64.github.io/)

## Leiningen Plugin

[lein-typed](https://github.com/frenchy64/lein-typed)

## Quickstart

`(clojure.core.typed/ann v t)` gives var `v` the static type `t`.

`(clojure.core.typed/ann-form f t)` ensures form `f` is of the static type `t`.

`(clojure.core.typed/check-ns)` type checks the current namespace.

`(clojure.core.typed/cf t)` type checks the form `t`.

<!---
## Examples

(These don't completely type check yet)

* [clojure.core.typed.test.rbt](https://github.com/frenchy64/typed-clojure/blob/master/test/typed/test/rbt.clj) for examples of mutually recursive types and heterogenous maps
* [typed.test.core-logic](https://github.com/frenchy64/typed-clojure/blob/master/test/typed/test/core_logic.clj) for examples of typing (tightly coupled) datatypes and protocols
* [typed.test.example](https://github.com/frenchy64/typed-clojure/blob/master/test/typed/test/example.clj) for a few little examples of simple usage
-->

## Developer Information

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

* Stephen Compall (S11001001)
* Andrew Brehaut (brehaut)
* Christopher Spencer (cspencer)
* Reid McKenzie (arrdem)

## YourKit

YourKit is kindly supporting core.typed and other open source projects with its full-featured Java Profiler.
YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:

* <a href="http://www.yourkit.com/java/profiler/index.jsp">YourKit Java Profiler</a> and
* <a href="http://www.yourkit.com/.net/profiler/index.jsp">YourKit .NET Profiler</a>.

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl.html).

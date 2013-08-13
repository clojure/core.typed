# core.typed

Gradual typing in Clojure, as a library.

## Releases and Dependency Information

Latest stable release is 0.1.24.

Leiningen dependency information:

```clojure
[org.clojure/core.typed "0.1.24"]
```

Maven dependency information:

```XML
<dependency>
  <groupId>org.clojure</groupId>
  <artifactId>core.typed</artifactId>
  <version>0.1.24</version>
</dependency>
```

## [Talk] Clojure Conj 2012

[Video](http://www.youtube.com/watch?v=wNhK8t3uLJU)

## Mailing List

Use the core.typed [mailing list](https://groups.google.com/forum/?fromgroups#!forum/clojure-core-typed) for core.typed discussion, 
or try #clojure on IRC.

## Documentation

See [wiki](https://github.com/clojure/core.typed/wiki).

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

## Future work

* Equality filters for occurrence typing
* Unify AST with ClojureScript
* Namespace dependency management
* Track changes to Typed Racket
  * https://github.com/plt/racket/compare/6105ce8b2087...71d6189132ce

## Contributors

Stephen Compall (S11001001)
Andrew Brehaut (brehaut)

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl.html).

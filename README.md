Leiningen dependency (Clojars):

`[typed "0.1.6"]`

# core.typed

Gradual typing in Clojure, as a library.

# [Talk] Clojure Conj 2012

[Video](http://www.youtube.com/watch?v=wNhK8t3uLJU)

# Documentation

See [wiki](https://github.com/clojure/core.typed/wiki).

# Quickstart

`(clojure.core.typed/ann v t)` gives var `v` the static type `t`.

`(clojure.core.typed/ann-form f t)` ensures form `f` is of the static type `t`.

`(clojure.core.typed/check-ns)` type checks the current namespace.

`(clojure.core.typed/cf t)` type checks the form `t`.

<!---
# Examples

(These don't completely type check yet)

* [clojure.core.typed.test.rbt](https://github.com/frenchy64/typed-clojure/blob/master/test/typed/test/rbt.clj) for examples of mutually recursive types and heterogenous maps
* [typed.test.core-logic](https://github.com/frenchy64/typed-clojure/blob/master/test/typed/test/core_logic.clj) for examples of typing (tightly coupled) datatypes and protocols
* [typed.test.example](https://github.com/frenchy64/typed-clojure/blob/master/test/typed/test/example.clj) for a few little examples of simple usage
-->

# Future work

* Equality filters for occurrence typing
* Rest type checking in fn definition
* Type check defprotocol definitions
* Unify AST with ClojureScript
* Namespace dependency management

# Contributors

Stephen Compall (S11001001)

# License

core.typed is released under the same license as Clojure: Eclipse Public License v 1.0.

See `LICENSE`.

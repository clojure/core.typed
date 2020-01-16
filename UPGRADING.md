# Upgrading from 0.7.x to monorepo

On 15th January 2020, core.typed moved back to a monorepo.
All Maven coordinates are unchanged, however any tools.deps
Git dependencies will need to be updated to the corresponding
subdirectory under the monorepo.

# Upgrading from 0.6.x to split repository

## Update packages

- update `[org.clojure/core.typed "0.6.x"]` to `[org.clojure/core.typed.checker.jvm "0.7.0"]`
  - follow updates at [core.typed.checker.jvm](https://github.com/clojure/core.typed.checker.jvm)
- update `[org.clojure/core.typed.rt "0.6.x"]` to `[org.clojure/core.typed.runtime.jvm "0.7.0"]`
  - follow updates at [core.typed.runtime.jvm](https://github.com/clojure/core.typed.runtime.jvm)
- update `[org.clojure/core.typed.infer "0.6.x"]` to `[org.clojure/core.typed.annotator.jvm "0.7.0"]`
  - follow updates at [core.typed.annotator.jvm](https://github.com/clojure/core.typed.annotator.jvm)
- update `[org.clojure/core.typed.analyzer.jvm "0.6.x"]` to `[org.clojure/core.typed.analyzer.jvm "0.7.0"]`
  - follow updates at [core.typed.analyzer.jvm](https://github.com/clojure/core.typed.analyzer.jvm): Analyzer

# Upgrading from 0.2.x

## Renamed macros

- `clojure.core.typed/def-alias` renamed `clojure.core.typed/defalias`
- `clojure.core.typed/loop>` renamed to `clojure.core.typed/loop`
- `clojure.core.typed/dotimes>` renamed to `clojure.core.typed/dotimes`

## Renamed and changed macros

- `clojure.core.typed/for>` renamed `clojure.core.typed/for`
  - return type annotation now goes after the binder: `(for [a :- t, i] :- r, i)`
  - Deprecated binder syntax no longer works: `[[a :- t] i]`
- `clojure.core.typed/doseq>` renamed `clojure.core.typed/doseq`
  - Deprecated binder syntax no longer works: `[[a :- t] i]`
- `clojure.core.typed/fn>` renamed `clojure.core.typed/fn`
  - Return type now goes after the binder: `(fn [a :- t] :- t, b)`
- `clojure.core.typed/def>` renamed `clojure.core.typed/def`
  - Note it is impossible to refer to a var called `def`
- `clojure.core.typed/defprotocol>` renamed `clojure.core.typed/defprotocol`
  - New syntax cannot be used in conjunction with `ann-protocol`
- `clojure.core.typed/defn>` renamed `clojure.core.typed/defn`
  - Return type now goes after the binder: `(defn name [a :- t] :- t, b)`

## Changed macros

- `clojure.core.typed/ann`
  - `:nocheck` metadata renamed to `:no-check`

## Renamed types

- `All` renamed `clojure.core.typed/All`
- `Any` renamed `clojure.core.typed/Any`
- `Difference` renamed `clojure.core.typed/Difference`
- `Fn` renamed `clojure.core.typed/IFn`
- `HMap` renamed `clojure.core.typed/HMap`
- `HSeq` renamed `clojure.core.typed/HSeq`
- `HSequential` renamed `clojure.core.typed/HSequential`
- `HVec` renamed `clojure.core.typed/HVec`
- `I` renamed `clojure.core.typed/I`
- `Nothing` renamed `clojure.core.typed/Nothing`
- `predicate` renamed `clojure.core.typed/Pred`
- `Rec` renamed `clojure.core.typed/Rec`
- `TFn` renamed `clojure.core.typed/TFn`
- `U` renamed `clojure.core.typed/U`
- `Value` renamed `clojure.core.typed/{Value,Val}`

## Renamed and changed types

- `Seq*` renamed to `clojure.core.typed/HSeq`
  - HSeq takes one argument: `(Seq* Any)` -> `(HSeq [Any])`
- `Vector*` renamed to `clojure.core.typed/HVec`
  - HSeq takes one argument: `(Vector* Any)` -> `(HVec [Any])`

## Changed type syntax

- `[& {} -> ]` optional keyword argument syntax is now `[& :optional {} -> ]`
- `(HMap {})` mandatory key syntax is now `(HMap :mandatory {})`

## Removed type syntax

- `:kind` option for `TFn` and `All` binders is removed
  - eg. (TFn [[x :kind ...]])

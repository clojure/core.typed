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

## Changed macros

- `clojure.core.typed/ann`
  - `:nocheck` metadata renamed to `:no-check`

## Renamed types

- `Any` renamed `clojure.core.typed/Any`
- `Nothing` renamed `clojure.core.typed/Nothing`
- `Fn` renamed `clojure.core.typed/IFn`

## Changed type syntax

- `[& {} -> ]` optional keyword argument syntax is now `[& :optional {} -> ]`
- `(HMap {})` mandatory key syntax is now `(HMap :mandatory {})`

## Removed type syntax

- `:kind` option for `TFn` and `All` binders is removed
  - eg. (TFn [[x :kind ...]])

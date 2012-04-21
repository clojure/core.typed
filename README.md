# Typed Clojure

Clojure with a type system, as a library.

# Research Proposal

Typed Clojure will be the subject of my dissertation.

[Final Draft Project Proposal](https://github.com/downloads/frenchy64/papers/research-proposal-final-draft.pdf)
[Early Draft Literary Review](https://github.com/downloads/frenchy64/papers/lit-review-draft.pdf)

# License

Typed Clojure is released under the same license as Clojure: Eclipse Public License v 1.0.

See `LICENSE`.

# Download

Leiningen:

[typed "0.1-alpha2]

# Immediate Future work

- Complete type checking algorithm
- Type variables

# Usage

There are two main macros: `+T` and `require-typed`.

```clojure
  ...
  (:require [typed.core :refer [+T require-typed check-namespace]])
  ...
```

`+T` is for top level annotation. 

```clojure
(+T double-num [Number -> Number])
(defn double-num [n]
  (add-twice n))

(+T my-map (Mapof Number Number))
(def my-map {1 2 3 4})
```

`require-typed` informs the type checker about typed dependencies.

Here is an example of two typed modules, the first requiring the second.

```clojure
(ns typed.example.typed
  (:require [typed.core :refer [+T require-typed]]
            [typed.example.typed2 :refer [add-twice]]))

(require-typed typed.example.typed2)

(+T double-num [Number -> Number])
(defn double-num [n]
  (add-twice n))

(+T my-map (Mapof Number Number))
(def my-map {1 2 3 4})
```

```clojure
(ns typed.example.typed2
  (:require [typed.core :refer [+T]]))

(+T add-twice [Number -> Number])
(defn add-twice [n]
  (+ n n))
```

`check-namespace` invokes the type checker. Use it at the REPL.

```clojure
(check-namespace 'typed.example.typed)
```

Turn on debug mode for some feedback.

```clojure
typed.core=> (reset! typed.core/debug-mode true)
true
typed.core=> 
  (check-namespace 'typed.example.typed)
Overwriting type for typed.core/add-ns-dep : #typed.core.Fun{:arities (#typed.core.arity{:dom (#typed.core.ClassType{:the-class clojure.lang.Symbol} #typed.core.ClassType{:the-class clojure.lang.Symbol}), :rng #typed.core.NilType{}, :rest-type nil, :flter nil, :type-params nil})} from (Fun [clojure.lang.Symbol clojure.lang.Symbol -> nil])
....
...
invoke: #'clojure.core/in-ns
invoke: ??
invoke: #'clojure.core/refer
invoke: #'clojure.core/require
invoke: #'typed.core/add-ns-dep
invoke: #'clojure.core/symbol
invoke: #'clojure.core/name
invoke: #'clojure.core/ns-name
invoke: #'typed.core/*add-type-ann-fn*
invoke: #'clojure.core/symbol
invoke: #'clojure.core/name
invoke: #'clojure.core/ns-name
invoke: #'clojure.core/name
def: #'typed.example.typed/double-num
invoke: #'typed.example.typed2/add-twice
invoke: #'typed.core/*add-type-ann-fn*
invoke: #'clojure.core/symbol
invoke: #'clojure.core/name
invoke: #'clojure.core/ns-name
invoke: #'clojure.core/name
def: #'typed.example.typed/my-map
nil
```

# Type Syntax

## Protocols

Simply refer to protocols as usual.

```clojure

(defprotocol A
 ...)

; takes a type that is a subtype of protocol A and return a Long
(+T p1 [A -> Long])
(defn p1 [a]
  2)
```

## Classes

Same as protocols

## Vectors

```clojure
; (Vectorof n) is a subtype of clojure.lang.IPersistentVector
(+T v1 [(Vectorof Double) -> Boolean])
(defn v1 [v]
  true)
```

## Sequentials

; (Sequentialof n) is a subtype of clojure.lang.Sequential
```clojure
(+T s1 [(Sequentialof Double) -> Boolean])
(defn s1 [s]
  true)
```

## Functions


```clojure
; A function that takes a Number and returns a Number.
(+T a1 (Fun [Number -> Number]))
(defn a1 [n]
  n)

; Shorthand for single arity functions: 
(+T a2 [Number -> Number])
(defn a2 [n]
  n)

; A function that has 1 fixed parameter (Number) and any number
; of Number parameters, and returns a Number.
(+T a3 [Number & Number * -> Number])
(defn a3 [n & ns]
  (+ n (count ns)))

;Multiple arity:
(+T a4
    (Fun [Number Boolean -> Number]
         [Boolean -> Double]))
(defn a4
  ([n b] (+ n (if b 1 2)))
  ([b] (if b 1.1 2.2)))

```

## Examples

See `typed.base` for some examples, and how to add your own annotations to clojure.core functions.


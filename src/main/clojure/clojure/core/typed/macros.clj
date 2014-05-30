(ns clojure.core.typed.macros
  (:refer-clojure :exclude [type defprotocol fn loop dotimes let for doseq])
  (:require [clojure.core :as core]
            [clojure.core.typed.internal :as internal]
            [clojure.core.typed.special-form :as spec]))

(alias 't 'clojure.core.typed)

(defmacro
  ^{:forms '[(def name docstring? :- type? expr)]}
  def
  "Like clojure.core/def with optional type annotations

  NB: it is impossible to refer a var called `def` as it is a
  special form. Use an alias prefix (eg. `t/def`).

  If an annotation is provided, a corresponding `ann` form
  is generated, otherwise it expands identically to clojure.core/def

  eg. ;same as clojure.core/def
      (def vname 1)
      
      ;with Number `ann`
      (def vname :- Number 1)

      ;doc
      (def vname
        \"Docstring\"
        :- Long
        1)"
  [name & fdecl]
  (core/let [[docstring fdecl] (internal/take-when string? fdecl)
             [provided? t body] (if (#{:-} (first fdecl))
                                  (core/let [_ (assert (#{3} (count fdecl))
                                                       "Bad arguments to clojure.core.typed/def")
                                             [colon t body] fdecl]
                                    [true t body])
                                  (core/let [_ (assert (#{1} (count fdecl))
                                                       "Bad arguments to clojure.core.typed/def")
                                             [body] fdecl]
                                    [false nil body]))]
    `(do ~@(when provided?
             [`(ann ~name ~t)])
         ~(list* 'def name 
                 (concat
                   (when docstring [docstring])
                   [body])))))

(defmacro 
  ^{:forms '[(fn name? [param :- type* & param :- type * ?] :- type? exprs*)
             (fn name? ([param :- type* & param :- type * ?] :- type? exprs*)+)]}
  fn
  "Like clojure.core/fn, but with optional annotations.

  eg. ;these forms are equivalent
      (fn [a] b)
      (fn [a :- Any] b)
      (fn [a :- Any] :- Any b)
      (fn [a] :- Any b)

      ;annotate return
      (fn [a :- String] :- String body)

      ;named fn
      (fn fname [a :- String] :- String body)

      ;rest parameter
      (fn [a :- String & b :- Number *] body)

      ;dotted rest parameter
      (fn [a :- String & b :- Number ... x] body)

      ;multi-arity
      (fn fname 
        ([a :- String] :- String ...)
        ([a :- String, b :- Number] :- String ...))"
  [& forms]
  (core/let [{:keys [fn ann]} (internal/parse-fn* false forms)]
    `(do ~spec/special-form
         ::t/fn
         {:ann '~ann}
         ~fn)))

(defmacro 
  ^{:forms '[(loop [binding :- type?, init*] exprs*)]}
  loop
  "Like clojure.core/loop, and supports optional type annotations.
  Arguments default to a generalised type based on the initial value.

  eg. (loop [a :- Number 1
             b :- (U nil Number) nil]
        ...)"
  [bindings & exprs]
  (core/let [{:keys [ann loop]} (internal/parse-loop* `(~bindings ~@exprs))]
    `(do ~spec/special-form
         ::t/loop
         {:ann '~ann}
         ~loop)))

(defmacro 
  ^{:forms '[(let [binding :- type?, init*] exprs*)]}
  let
  "Like clojure.core/let but supports optional type annotations.

  eg. (let [a :- Type, b
            a2 1.2]
        body)"
  [bvec & forms]
  (core/let [{:keys [let]} (internal/parse-let* (cons bvec forms))]
    let))

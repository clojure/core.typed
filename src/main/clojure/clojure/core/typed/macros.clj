;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.macros
  (:refer-clojure :exclude [type defprotocol fn loop dotimes let for doseq
                            defn atom ref])
  (:require [clojure.core :as core]
            [clojure.core.typed.special-form :as spec]))

;also defined in clojure.core.typed
(core/defn dynaload
  [s]
  (core/let [ns (namespace s)]
    (assert ns)
    (require (symbol ns))
    (core/let [v (resolve s)]
      (if v
        @v
        (throw (RuntimeException. (str "Var " s " is not on the classpath")))))))

(core/defn core-kw [kw]
  (keyword "clojure.core.typed"
           (name kw)))

(core/defn parse-colon
  "Returns a vector of [provided? t args]"
  [fdecl name]
  (if (#{:-} (first fdecl))
    (core/let [[colon t & body] fdecl]
      [true t body])
    [false nil fdecl]))

(core/let [take-when (delay (dynaload 'clojure.core.typed.internal/take-when))]
  (defmacro
    ^{:forms '[(def name docstring? :- type? expr)]}
    def
    "Like clojure.core/def with optional type annotations

    NB: in Clojure it is impossible to refer a var called `def` as it is a
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
    (core/let [[docstring fdecl] (@take-when string? fdecl)
               [provided? t [body :as args]] (parse-colon fdecl 'def)]
      (assert (= 1 (count args)) "Wrong arguments to def")
      `(def ~(vary-meta name #(merge
                                %
                                (when docstring
                                  {:doc docstring})))
         ~(if provided?
            `(ann-form ~body ~t)
            body)))))

(core/let [parse-fn* (delay (dynaload 'clojure.core.typed.internal/parse-fn*))]
  (core/defn expand-typed-fn [form]
    (core/let [{:keys [poly fn ann]} (@parse-fn* form)]
      `(do ~spec/special-form
           ~(core-kw :fn)
           {:ann '~ann
            :poly '~poly}
           ~fn))))

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
        ([a :- String, b :- Number] :- String ...))

      ; polymorphic binder
      (fn :forall [x y z]
        fname 
        ([a :- String] :- String ...)
        ([a :- String, b :- Number] :- String ...))
  "
  [& forms]
  (expand-typed-fn &form))

(core/let [parse-loop* (delay (dynaload 'clojure.core.typed.internal/parse-loop*))]
  (defmacro 
    ^{:forms '[(loop [binding :- type?, init*] exprs*)]}
    loop
    "Like clojure.core/loop, and supports optional type annotations.
    Arguments default to a generalised type based on the initial value.

    eg. (loop [a :- Number 1
               b :- (U nil Number) nil]
          ...)"
    [bindings & exprs]
    (core/let [{:keys [ann loop]} (@parse-loop* `(~bindings ~@exprs))]
      `(do ~spec/special-form
           ~(core-kw :loop)
           {:ann '~ann}
           ~loop))))

(core/let [parse-let* (delay (dynaload 'clojure.core.typed.internal/parse-let*))]
  (defmacro 
    ^{:forms '[(let [binding :- type?, init*] exprs*)]}
    let
    "Like clojure.core/let but supports optional type annotations.

    eg. (let [a :- Type, b
              a2 1.2]
          body)"
    [bvec & forms]
    (core/let [{:keys [let]} (@parse-let* (cons bvec forms))]
      let)))

(defmacro ann-form
  "Annotate a form with an expected type."
  [form ty]
  `(do ~spec/special-form
       ~(core-kw :ann-form)
       {:type '~ty}
       ~form))

(core/let [parse-defprotocol* (delay (dynaload 'clojure.core.typed.internal/parse-defprotocol*))]
  (defmacro defprotocol
    "Like defprotocol, but with optional type annotations.

    Omitted annotations default to Any. The first argument
    of a protocol cannot be annotated.

    Add a binder before the protocol name to define a polymorphic
    protocol. A binder before the method name defines a polymorphic
    method, however a method binder must not shadow type variables
    introduced by a protocol binder.

    Return types for each method arity can be annotated.

    Unlike clojure.core/defprotocol, successive methods can
    have the same arity. Semantically, providing multiple successive
    methods of the same arity is the same as just providing the left-most
    method. However the types for these methods will be accumulated into
    a Fn type.
    
    eg. ;annotate single method
    (defprotocol MyProtocol
      (a [this a :- Integer] :- Number))

    ;polymorphic protocol
    (defprotocol [[x :variance :covariant]]
      MyProtocol
      (a [this a :- Integer] :- Number))

    ;multiple types for the same method
    (defprotocol [[x :variance :covariant]]
      MyProtocol
      (a [this a :- Integer] :- Integer
         [this a :- Long] :- Long
         [this a :- Number] :- Number))

    ;polymorphic method+protocol
    (defprotocol [[x :variance :covariant]]
      MyProtocol
      ([y] a [this a :- x, b :- y] :- y))
    "
    [& body]
    (core/let [{:keys [ann-protocol defprotocol]} (@parse-defprotocol* body)]
      `(do ~ann-protocol
           (tc-ignore
             ~defprotocol)))))

(defmacro tc-ignore 
  "Ignore forms in body during type checking"
  [& body]
  `(do ~spec/special-form
       ~(core-kw :tc-ignore)
       {}
       (do ~@(or body [nil]))))

(defmacro when-let-fail 
  "Like when-let, but fails if the binding yields a false value."
  [b & body]
  `(if-let ~b
     (do ~@body)
     (throw (ex-info (str "Expression was nil or false") {:form '~(second b)}))))

(defmacro atom
  "Like atom, but with optional type annotations.
  
  Same as (atom (ann-form init t) args*)
  
  eg. (atom 1) : (Atom1 (Value 1))
      (atom :- Num, 1) : (Atom1 Num)"
  [& args]
  (core/let [[provided? t args] (parse-colon args 'atom)
             [init & args] args]
    `(core/atom ~(if provided?
                   `(ann-form ~init ~t)
                   init)
                ~@args)))

(defmacro ref
  "Like ref, but with optional type annotations.
  
  Same as (ref (ann-form init t) args*)
  
  eg. (ref 1) : (Ref1 (Value 1))
      (ref :- Num, 1) : (Ref1 Num)"
  [& args]
  (core/let [[provided? t args] (parse-colon args 'ref)
             [init & args] args]
    `(core/ref ~(if provided?
                  `(ann-form ~init ~t)
                  init)
               ~@args)))

(core/let [parse-defn* (delay (dynaload 'clojure.core.typed.internal/parse-defn*))]
  (defmacro
    ^{:forms '[(defn kw-args? name docstring? attr-map? [param :- type *] :- type exprs*)
               (defn kw-args? name docstring? attr-map? ([param :- type *] :- type exprs*)+)]}
    defn
    "Like defn, but expands to clojure.core.typed/fn. If a polymorphic binder is
    supplied before the var name, expands to clojure.core.typed/pfn.

    eg. (defn fname [a :- Number, b :- (U Symbol nil)] :- Integer ...)

    ;annotate return
    (defn fname [a :- String] :- String ...)

    ;multi-arity
    (defn fname 
      ([a :- String] :- String ...)
      ([a :- String, b :- Number] :- Long ...))

    ;polymorphic function
    (defn :forall [x y]
      fname 
      ([a :- x] :- (Coll y) ...)
      ([a :- Str, b :- y] :- y ...))"
    [& args]
    (core/let [{:keys [name args]} (@parse-defn* args)]
      `(def ~name (fn ~@args)))))

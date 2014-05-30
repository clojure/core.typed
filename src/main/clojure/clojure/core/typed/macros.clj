(ns clojure.core.typed.macros
  (:refer-clojure :exclude [type defprotocol fn loop dotimes let for doseq])
  (:require [clojure.core :as core]
            [clojure.core.typed.internal :as internal]
            [clojure.core.typed.util-vars :as vs]
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
                                    [false nil body]))
             def (list* 'def name 
                        (concat
                          (when docstring [docstring])
                          [body]))]
    (if vs/*currently-checking-clj*
      `(do ~@(when provided?
               [`(ann ~name ~t)])
           ~def)
      def)))

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
    (if vs/*currently-checking-clj*
      `(do ~spec/special-form
           ::t/fn
           {:ann '~ann}
           ~fn)
      fn)))

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
    (if vs/*currently-checking-clj*
      `(do ~spec/special-form
           ::t/loop
           {:ann '~ann}
           ~loop)
      loop)))

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

(defmacro ann-form 
  "Annotate a form with an expected type."
  [form ty]
  (if vs/*currently-checking-clj*
    `(do ~spec/special-form
         ::t/ann-form
         {:type '~ty}
         ~form)
    form))

(defmacro defprotocol [& body]
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
  (let [{:keys [ann-protocol defprotocol]} (internal/parse-defprotocol* body)]
    `(do ~ann-protocol
         (tc-ignore
           ~defprotocol))))

(defmacro tc-ignore 
  "Ignore forms in body during type checking"
  [& body]
  (if vs/*currently-checking-clj*
    `(do ~spec/special-form
         ::t/tc-ignore
         ~@(or body [nil]))
    (case (count body)
      0 nil
      1 (first body)
      `(do ~@body))))

(defmacro when-let-fail 
  "Like when-let, but fails if the binding yields a false value."
  [b & body]
  `(if-let ~b
     (do ~@body)
     (throw (ex-info (str "Expression was nil or false") {:form '~(second b)}))))

(defmacro atom>
  "Like atom, but creates an Atom1 of type t.
  
  Same as (atom (ann-form init t) args*)
  
  eg. (atom> Number 1)
      (atom> (Vec Any) [])"
  [t init & args]
  `(atom (ann-form ~init ~t) ~@args))

(defmacro ref>
  "Like ref, but creates a Ref1 of type t.
  
  Same as (ref (ann-form init t) args*)
  
  eg. (ref> Number 1)
      (ref> (Vec Any) [])"
  [t init & args]
  `(ref (ann-form ~init ~t) ~@args))


(ns 
  ^{:doc "This namespace contains typed wrapper macros, type aliases
and functions for type checking Clojure code. check-ns is the interface
for checking namespaces, cf for checking individual forms."}
  clojure.core.typed
  (:refer-clojure :exclude [type defprotocol #_letfn fn loop dotimes let for doseq
                            #_def #_filter #_remove])
  (:require [clojure.core :as core]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.util-vars :as uvars]
            [clojure.core.typed.profiling :as p]
            [clojure.core.typed.parse-ast :as ast]
            [clojure.core.typed.internal :as internal]
            [clojure.java.io :as io]))

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

;at the top because the rest of this namespace uses this macro
(defmacro 
  ^{:forms '[(fn name? :- type? [param :- type* & param :- type * ?] exprs*)
             (fn name? (:- type? [param :- type* & param :- type * ?] exprs*)+)]}
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
    `(do ::special-form
         ::fn
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
    `(do ::special-form
         ::loop
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

(defmacro ann-form 
  "Annotate a form with an expected type."
  [form ty]
  `(do ::special-form
       ::ann-form
       {:type '~ty}
       ~form))

;=============================================================
; # core.typed
;
; This is the main namespace for core.typed. This project is
; split into many internal namespaces. Here are some of the main ones:
;
; c.c.typed.base-env
;   The base global type environment. All base Var Annotations,
;   Java method annotations, Class overriding and other annotations
;   live here.
;
; c.c.typed.type-{rep,ctors}, c.c.parse-unparse,
; c.c.typed.fold-{rep,default}
;   Internal type representation and operations.
;   
; c.c.typed.check
;   The type checker.
;
; c.c.typed.cs-gen
;   Polymorphic local type inference algorithm.

;=============================================================
; Query functions

; Usually query functions need to force core.typed to fully load.
; To be as lazy as possible, we use `ns-resolve` to grab the Vars
; we need.

(declare load-if-needed)

;(ann method-type [Symbol -> nil])
(defn method-type
  "Given a method symbol, print the core.typed types assigned to it.
  Intended for use at the REPL."
  [mname]
  (load-if-needed)
  (let [type-reflect (impl/v 'clojure.reflect/type-reflect)
        unparse-type (impl/v 'clojure.core.typed.parse-unparse/unparse-type)
        Method->Type (impl/v 'clojure.core.typed.check/Method->Type)
        ms (->> (type-reflect (Class/forName (namespace mname)))
             :members
             (core/filter #(and (instance? clojure.reflect.Method %)
                           (= (str (:name %)) (name mname))))
             set)
        _ (assert (seq ms) (str "Method " mname " not found"))]
    (println "Method name:" mname)
    (flush)
    (core/doseq [m ms]
      (println (unparse-type
                 (Method->Type m)))
      (flush))))


;=============================================================
; Special functions

(defn print-filterset
  "During type checking, print the filter set attached to form, 
  preceeded by literal string debug-string.
  Returns nil.
  
  eg. (let [s (seq (get-a-seqable))]
        (print-filterset \"Here now\" s))"
  [debug-string frm]
  frm)

(defn ^:skip-wiki
  inst-poly 
  "Internal use only. Use inst."
  [inst-of types-syn]
  inst-of)

(defn ^:skip-wiki
  inst-poly-ctor 
  "Internal use only. Use inst-ctor"
  [inst-of types-syn]
  inst-of)

(defmacro inst 
  "Instantiate a polymorphic type with a number of types.
  
  eg. (inst foo-fn t1 t2 t3 ...)"
  [inst-of & types]
  `(inst-poly ~inst-of '~types))

(defmacro inst-ctor
  "Instantiate a call to a constructor with a number of types.
  First argument must be an immediate call to a constructor.
  Returns exactly the instantiatee (the first argument).
  
  eg. (inst-ctor (PolyCtor. a b c)
                 t1 t2 ...)"
  [inst-of & types]
  `(inst-poly-ctor ~inst-of '~types))

(defn ^:skip-wiki
  fn>-ann 
  "Internal use only. Use fn>."
  [fn-of param-types-syn]
  fn-of)

(defn ^:skip-wiki
  pfn>-ann 
  "Internal use only. Use pfn>."
  [fn-of polys param-types-syn]
  fn-of)

(defn ^:skip-wiki
  loop>-ann 
  "Internal use only. Use loop>"
  [loop-of bnding-types]
  loop-of)

(defmacro ^:deprecated dotimes>
  "DEPRECATED: Use clojure.core.typed/dotimes

  Like dotimes.
  
  eg. (dotimes> [_ 100]
        (println \"like normal\"))"
  [bindings & body]
  (@#'core/assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# (long ~n)]
       (loop> [~i :- ~'clojure.core.typed/AnyInteger 0]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))

(defmacro dotimes
  "Like clojure.core/dotimes, but with optional annotations.

  If annotation for binding is omitted, defaults to Int.
  
  eg. (dotimes [_ 100]
        (println \"like normal\"))

      (dotimes [x :- Number, 100.123]
        (println \"like normal\" x))"
  [bindings & body]
  (@#'core/assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# (long ~n)]
       (loop [~i :- ~'clojure.core.typed/Int 0]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))

(defmacro ^:deprecated for>
  "DEPRECATED: use clojure.core.typed/for

  Like for but requires annotation for each loop variable: [a [1 2]] becomes [a :- Long [1 2]]
  Also requires annotation for return type.
  
  eg. (for> :- Number
        [a :- (U nil AnyInteger) [1 nil 2 3]
         :when a]
        (inc a))"
  [tk ret-ann seq-exprs body-expr]
  (@#'core/assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (assert (#{:-} tk) "Must provide return type annotation for for>.")
  (let [normalise-args
        ; change [a :- b c] to [[a :- b] c]
        (fn [seq-exprs]
          (loop [flat-result ()
                 seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result
              (keyword? (first seq-exprs)) (recur (concat flat-result (take 2 seq-exprs))
                                                  (drop 2 seq-exprs))
              (and (vector? (first seq-exprs))
                   (#{:-} (-> seq-exprs first second))) (do
                                                          (prn "DEPRECATED WARNING: for> syntax has changed, use [b :- t i] for clauses")
                                                          (recur (concat flat-result (take 2 seq-exprs))
                                                                 (drop 2 seq-exprs)))
              :else (do (assert (#{:-} (second seq-exprs))
                                "Incorrect syntax in for>.")
                        (recur (concat flat-result [(vec (take 3 seq-exprs))
                                                    (nth seq-exprs 3)])
                               (drop 4 seq-exprs))))))

        ; normalise seq-exprs to be flat pairs
        seq-exprs (normalise-args seq-exprs)

        to-groups (fn [seq-exprs]
                    (@#'core/reduce1 (fn [groups [k v]]
                                               (if (keyword? k)
                                                 (conj (pop groups) (conj (peek groups) [k v]))
                                                 (conj groups [k v])))
                                             [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))
        emit-bind (fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (let [_ (assert (and (vector? bind)
                                         (#{3} (count bind))
                                         (#{:-} (second bind))) 
                                    "Binder must be of the form [lhs :- type]")
                          bind-ann (nth bind 2)
                          bind (nth bind 0)
                          giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     (= k :let) `(core/let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                      `(core/let [iterys# ~(emit-bind next-groups)
                                                  fs# (seq (iterys# ~next-expr))]
                                         (if fs#
                                           (concat fs# (~giter (rest ~gxs)))
                                           (recur (rest ~gxs))))
                                     :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))]
                      (if next-groups
                        #_"not the inner-most loop"
                        `(ann-form
                           (fn ~giter [~gxs]
                             (lazy-seq
                               (loop> [~gxs :- (~'clojure.core.typed/Option (~'clojure.lang.Seqable ~bind-ann)) ~gxs]
                                 (when-first [~bind ~gxs]
                                   ~(do-mod mod-pairs)))))
                           [(~'clojure.core.typed/Option (~'clojure.lang.Seqable ~bind-ann)) ~'-> (~'clojure.core.typed/Seq ~ret-ann)])
                        #_"inner-most loop"
                        (let [gi (gensym "i__")
                              gb (gensym "b__")
                              do-cmod (fn do-cmod [[[k v :as pair] & etc]]
                                        (cond
                                          (= k :let) `(core/let ~v ~(do-cmod etc))
                                          (= k :while) `(when ~v ~(do-cmod etc))
                                          (= k :when) `(if ~v
                                                         ~(do-cmod etc)
                                                         (recur
                                                           (unchecked-inc ~gi)))
                                          (keyword? k)
                                            (err "Invalid 'for' keyword " k)
                                          :else
                                            `(do (chunk-append ~gb 
                                                               ; put an ann-form here so at least one error message
                                                               ; points to code the user can recognise.
                                                               (ann-form ~body-expr
                                                                         ~ret-ann))
                                                 (recur (unchecked-inc ~gi)))))]
                          `(ann-form
                             (fn ~giter [~gxs]
                               (lazy-seq
                                 (loop> [~gxs :- (~'clojure.core.typed/Option (~'clojure.lang.Seqable ~bind-ann)) ~gxs]
                                        (when-let [~gxs (seq ~gxs)]
                                          (if (chunked-seq? ~gxs)
                                            (core/let [c# (chunk-first ~gxs)
                                                       size# (int (count c#))
                                                       ~gb (ann-form (chunk-buffer size#)
                                                                     (~'clojure.lang.ChunkBuffer ~ret-ann))]
                                              (if (loop> [~gi :- ~'clojure.core.typed/AnyInteger, (int 0)]
                                                         (if (< ~gi size#)
                                                           (core/let [;~bind (.nth c# ~gi)]
                                                                      ~bind (nth c# ~gi)]
                                                             ~(do-cmod mod-pairs))
                                                           true))
                                                (chunk-cons
                                                  (chunk ~gb)
                                                  (~giter (chunk-rest ~gxs)))
                                                (chunk-cons (chunk ~gb) nil)))
                                            (core/let [~bind (first ~gxs)]
                                              ~(do-mod mod-pairs)))))))
                             [(~'clojure.core.typed/Option (~'clojure.lang.Seqable ~bind-ann)) ~'->
                              (~'clojure.core.typed/Seq ~ret-ann)])))))]
    `(core/let [iter# ~(emit-bind (to-groups seq-exprs))]
        (iter# ~(second seq-exprs)))))

(defmacro for
  "Like clojure.core/for with optional type annotations.

  All types default to Any.

  The :let option uses clojure.core.typed/let.
  
  eg. (for [a :- (U nil AnyInteger) [1 nil 2 3]
            :when a]
        :- Number
        (inc a))"
  [seq-exprs & maybe-ann-body-expr]
  (@#'core/assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [[ret-ann body-expr] (if (#{:-} (first maybe-ann-body-expr))
                              (let [_ (assert (#{3} (count maybe-ann-body-expr))
                                              (str "Wrong arguments to for: " maybe-ann-body-expr))
                                    [colon t body] maybe-ann-body-expr]
                                [t body])
                              (let [_ (assert (#{1} (count maybe-ann-body-expr))
                                              (str "Wrong arguments to for: " maybe-ann-body-expr))
                                    [body] maybe-ann-body-expr]
                                ['Any body]))
        normalise-args
        ; change [a :- b c] to [[a :- b] c]
        (fn [seq-exprs]
          (loop [flat-result []
                 seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result

              ;for options (:let, :while etc)
              (keyword? (first seq-exprs)) (let [_ (assert (#{2} (count (take 2 seq-exprs)))
                                                           (str "for option missing " (first seq-exprs)))
                                                 [k v & rst] seq-exprs]
                                             (recur (conj flat-result k v)
                                                    rst))
              :else (if (#{:-} (second seq-exprs))
                      (let [_ (assert (#{4} (count (take 4 seq-exprs)))
                                      (str "for parameter missing after ':-'"))
                            [b colon t init & rst] seq-exprs]
                        (recur (conj flat-result [b colon t] init)
                               rst))
                      (let [_ (assert (#{2} (count (take 2 seq-exprs)))
                                      (str "for binding needs initial values"))
                            [b init & rst] seq-exprs]
                        (recur (conj flat-result [b :- 'Any] init)
                               rst))))))

        ; normalise seq-exprs to be flat pairs
        seq-exprs (normalise-args seq-exprs)

        to-groups (fn [seq-exprs]
                    (reduce (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))
        emit-bind (fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (let [_ (assert (and (vector? bind)
                                         (#{3} (count bind))
                                         (#{:-} (second bind))) 
                                    "Binder must be of the form [lhs :- type]")
                          bind-ann (nth bind 2)
                          bind (nth bind 0)
                          giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     ;typed let
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                      `(let [iterys# ~(emit-bind next-groups)
                                             fs# (seq (iterys# ~next-expr))]
                                         (if fs#
                                           (concat fs# (~giter (rest ~gxs)))
                                           (recur (rest ~gxs))))
                                     :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))]
                      (if next-groups
                        #_"not the inner-most loop"
                        `(fn ~giter [~gxs :- (Option (Seqable ~bind-ann))]
                           :- (Seq ~ret-ann)
                           (lazy-seq
                             (loop [~gxs :- (Option (Seqable ~bind-ann)) ~gxs]
                               (when-first [~bind ~gxs]
                                 ~(do-mod mod-pairs)))))
                        #_"inner-most loop"
                        (let [gi (gensym "i__")
                              gb (gensym "b__")
                              do-cmod (fn do-cmod [[[k v :as pair] & etc]]
                                        (cond
                                          ; typed let
                                          (= k :let) `(let ~v ~(do-cmod etc))
                                          (= k :while) `(when ~v ~(do-cmod etc))
                                          (= k :when) `(if ~v
                                                         ~(do-cmod etc)
                                                         (recur
                                                           (unchecked-inc ~gi)))
                                          (keyword? k)
                                            (err "Invalid 'for' keyword " k)
                                          :else
                                            `(do (chunk-append ~gb 
                                                               ; put an ann-form here so at least one error message
                                                               ; points to code the user can recognise.
                                                               (ann-form ~body-expr
                                                                         ~ret-ann))
                                                 (recur (unchecked-inc ~gi)))))]
                          `(fn ~giter [~gxs :- (Option (Seqable ~bind-ann))]
                             :- (Seq ~ret-ann)
                             (lazy-seq
                               (loop [~gxs :- (Option (Seqable ~bind-ann)) ~gxs]
                                 (when-let [~gxs (seq ~gxs)]
                                   (if (chunked-seq? ~gxs)
                                     (let [c# (chunk-first ~gxs)
                                           size# (int (count c#))
                                           ~gb (ann-form (chunk-buffer size#)
                                                         (~'clojure.lang.ChunkBuffer ~ret-ann))]
                                       (if (loop [~gi :- Int, (int 0)]
                                             (if (< ~gi size#)
                                               (let [;~bind (.nth c# ~gi)]
                                                     ~bind (nth c# ~gi)]
                                                 ~(do-cmod mod-pairs))
                                               true))
                                         (chunk-cons
                                           (chunk ~gb)
                                           (~giter (chunk-rest ~gxs)))
                                         (chunk-cons (chunk ~gb) nil)))
                                     (let [~bind (first ~gxs)]
                                       ~(do-mod mod-pairs)))))))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
        (iter# ~(second seq-exprs)))))

(defmacro ^:deprecated doseq>
  "DEPRECATED: use clojure.core.typed/doseq

  Like doseq but requires annotation for each loop variable: 
  [a [1 2]] becomes [a :- Long [1 2]]
  
  eg.
  (doseq> [a :- (U nil AnyInteger) [1 nil 2 3]
           :when a]
     (inc a))"
  [seq-exprs & body]
  (@#'core/assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [normalise-args
        ; change [a :- b c] to [[a :- b] c]
        (fn [seq-exprs]
          (loop [flat-result ()
                 seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result
              (keyword? (first seq-exprs)) (recur (concat flat-result (take 2 seq-exprs))
                                                  (drop 2 seq-exprs))
              (and (vector? (first seq-exprs))
                   (#{:-} (-> seq-exprs first second))) (do
                                                          (prn "DEPRECATED WARNING: doseq> syntax has changed, use [b :- t i] for clauses")
                                                          (recur (concat flat-result (take 2 seq-exprs))
                                                                 (drop 2 seq-exprs)))
              :else (do (assert (#{:-} (second seq-exprs))
                                "Incorrect syntax in doseq>")
                        (recur (concat flat-result [(vec (take 3 seq-exprs))
                                                    (nth seq-exprs 3)])
                               (drop 4 seq-exprs))))))

        ; normalise seq-exprs to be flat pairs
        seq-exprs (normalise-args seq-exprs)
        step (fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (let [k (first exprs)
                       v (second exprs)]
                   (if (keyword? k)
                     (let [steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)]
                       (cond
                         (= k :let) [needrec `(core/let ~v ~subform)]
                         (= k :while) [false `(when ~v
                                                ~subform
                                                ~@(when needrec [recform]))]
                         (= k :when) [false `(if ~v
                                               (do
                                                 ~subform
                                                 ~@(when needrec [recform]))
                                               ~recform)]))
                     ;; k is [k :- k-ann]
                     (let [_ (assert (and (vector? k)
                                          (#{3} (count k))
                                          (#{:-} (second k))) 
                                     "Binder must be of the form [lhs :- type]")
                           k-ann (nth k 2)
                           k (nth k 0)
                           ; k is the lhs binding
                           seq- (gensym "seq_")
                           chunk- (with-meta (gensym "chunk_")
                                             {:tag 'clojure.lang.IChunk})
                           count- (gensym "count_")
                           i- (gensym "i_")
                           recform `(recur (next ~seq-) nil 0 0)
                           steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)
                           recform-chunk 
                             `(recur ~seq- ~chunk- ~count- (unchecked-inc ~i-))
                           steppair-chunk (step recform-chunk (nnext exprs))
                           subform-chunk (steppair-chunk 1)]
                       [true
                        `(loop> [~seq- :- (~'U nil (Seq ~k-ann)) (seq ~v), 
                                 ~chunk- :- (~'U nil (~'clojure.lang.IChunk ~k-ann)) nil
                                 ~count- :- (~'U Integer Long) 0,
                                 ~i- :- (~'U Integer Long) 0]
                           (if (and (< ~i- ~count-)
                                    ;; FIXME review this
                                    ;; core.typed thinks chunk- could be nil here
                                    ~chunk-)
                             (core/let [;~k (.nth ~chunk- ~i-)
                                        ~k (nth ~chunk- ~i-)]
                               ~subform-chunk
                               ~@(when needrec [recform-chunk]))
                             (when-let [~seq- (seq ~seq-)]
                               (if (chunked-seq? ~seq-)
                                 (core/let [c# (chunk-first ~seq-)]
                                   (recur (chunk-rest ~seq-) c#
                                          (int (count c#)) (int 0)))
                                 (core/let [~k (first ~seq-)]
                                   ~subform
                                   ~@(when needrec [recform]))))))])))))]
    (nth (step nil (seq seq-exprs)) 1)))

(defmacro doseq
  "Like clojure.core/doseq with optional annotations.

  :let option uses clojure.core.typed/let
  
  eg.
  (doseq [a :- (U nil AnyInteger) [1 nil 2 3]
          :when a]
     (inc a))"
  [seq-exprs & body]
  (@#'core/assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [normalise-args
        ; change [a :- b c] to [[a :- b] c]
        (fn [seq-exprs]
          (loop [flat-result []
                 seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result

              ;for options (:let, :while etc)
              (keyword? (first seq-exprs)) (let [_ (assert (#{2} (count (take 2 seq-exprs)))
                                                           (str "for option missing " (first seq-exprs)))
                                                 [k v & rst] seq-exprs]
                                             (recur (conj flat-result k v)
                                                    rst))
              :else (if (#{:-} (second seq-exprs))
                      (let [_ (assert (#{4} (count (take 4 seq-exprs)))
                                      (str "for parameter missing after ':-'"))
                            [b colon t init & rst] seq-exprs]
                        (recur (conj flat-result [b colon t] init)
                               rst))
                      (let [_ (assert (#{2} (count (take 2 seq-exprs)))
                                      (str "for binding needs initial values"))
                            [b init & rst] seq-exprs]
                        (recur (conj flat-result [b :- 'Any] init)
                               rst))))))

        ; normalise seq-exprs to be flat pairs
        seq-exprs (normalise-args seq-exprs)
        step (fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (let [k (first exprs)
                       v (second exprs)]
                   (if (keyword? k)
                     (let [steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)]
                       (cond
                         ;typed let
                         (= k :let) [needrec `(let ~v ~subform)]
                         (= k :while) [false `(when ~v
                                                ~subform
                                                ~@(when needrec [recform]))]
                         (= k :when) [false `(if ~v
                                               (do
                                                 ~subform
                                                 ~@(when needrec [recform]))
                                               ~recform)]))
                     ;; k is [k :- k-ann]
                     (let [_ (assert (and (vector? k)
                                          (#{3} (count k))
                                          (#{:-} (second k))) 
                                     "Binder must be of the form [lhs :- type]")
                           k-ann (nth k 2)
                           k (nth k 0)
                           ; k is the lhs binding
                           seq- (gensym "seq_")
                           chunk- (with-meta (gensym "chunk_")
                                             {:tag 'clojure.lang.IChunk})
                           count- (gensym "count_")
                           i- (gensym "i_")
                           recform `(recur (next ~seq-) nil 0 0)
                           steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)
                           recform-chunk 
                             `(recur ~seq- ~chunk- ~count- (unchecked-inc ~i-))
                           steppair-chunk (step recform-chunk (nnext exprs))
                           subform-chunk (steppair-chunk 1)]
                       [true
                        `(loop [~seq- :- (~'U nil (Seq ~k-ann)) (seq ~v), 
                                ~chunk- :- (~'U nil (~'clojure.lang.IChunk ~k-ann)) nil
                                ~count- :- (~'U Integer Long) 0,
                                ~i- :- (~'U Integer Long) 0]
                           (if (and (< ~i- ~count-)
                                    ;; FIXME review this
                                    ;; core.typed thinks chunk- could be nil here
                                    ~chunk-)
                             (let [;~k (.nth ~chunk- ~i-)
                                   ~k (nth ~chunk- ~i-)]
                               ~subform-chunk
                               ~@(when needrec [recform-chunk]))
                             (when-let [~seq- (seq ~seq-)]
                               (if (chunked-seq? ~seq-)
                                 (let [c# (chunk-first ~seq-)]
                                   (recur (chunk-rest ~seq-) c#
                                          (int (count c#)) (int 0)))
                                 (let [~k (first ~seq-)]
                                   ~subform
                                   ~@(when needrec [recform]))))))])))))]
    (nth (step nil (seq seq-exprs)) 1)))



(defmacro pfn> 
  "Define a polymorphic typed anonymous function.
  (pfn> name? [binder+] :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (pfn> name? [binder+] (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [& forms]
  (let [{:keys [poly fn parsed-methods]} (internal/parse-fn> true forms)]
    `(pfn>-ann ~fn '~poly '~parsed-methods)))


(defmacro when-let-fail 
  "Like when-let, but fails if the binding yields a false value."
  [b & body]
  `(if-let ~b
     (do ~@body)
     (throw (ex-info (str "Expression was nil or false") {:form '~(second b)}))))

(defmacro 
  ^{:forms '[(fn> name? :- type? [param :- type* & param :- type * ?] exprs*)
             (fn> name? (:- type? [param :- type* & param :- type * ?] exprs*)+)]}
  ^:deprecated
  fn> 
  "DEPRECATED: use clojure.core.typed/fn

  Like fn, but with annotations. Annotations are mandatory
  for parameters, with optional annotations for return type.
  If fn is named, return type annotation is mandatory.

  Suggested idiom: use commas between parameter annotation triples.

  eg. (fn> [a :- Number, b :- (U Symbol nil)] ...)

      ;annotate return
      (fn> :- String [a :- String] ...)

      ;named fn
      (fn> fname :- String [a :- String] ...)

      ;multi-arity
      (fn> fname 
        (:- String [a :- String] ...)
        (:- Long   [a :- String, b :- Number] ...))"
  [& forms]
  (let [{:keys [fn parsed-methods]} (internal/parse-fn> false forms)]
    `(fn>-ann ~fn '~parsed-methods)))


(defn- defn>-parse-typesig 
  "Helper for parsing type signatures out of defn> forms"
  [forms]
  (if (= :- (first forms))
    (let [ret (second forms)
          args (take-nth 3 (drop 2 (first (drop 2 forms))))]
      `[~@args ~'-> ~ret])
    `(~'Fn ~@(map defn>-parse-typesig forms))))

(defmacro
  ^{:forms '[(defn> name docstring? :- type [param :- type *] exprs*)
             (defn> name docstring? (:- type [param :- type *] exprs*)+)]}
  defn>
  "Like defn, but with annotations. Annotations are mandatory for
  parameters and for return type.

  eg. (defn> fname :- Integer [a :- Number, b :- (U Symbol nil)] ...)

  ;annotate return
  (defn> fname :- String [a :- String] ...)

  ;multi-arity
  (defn> fname 
    (:- String [a :- String] ...)
    (:- Long   [a :- String, b :- Number] ...))"
  [name & fdecl]
  (let [[docstring fdecl] (internal/take-when string? fdecl)
        signature (defn>-parse-typesig fdecl)]
    `(do (ann ~name ~signature)
         ~(list* 'def name 
                 (concat
                   (when docstring [docstring])
                   [`(fn> ~name ~@fdecl)])))))

(defmacro
  ^{:forms '[(def> name docstring? :- type expr)]}
  ^:deprecated
  def>
  "DEPRECATED: use clojure.core.typed/def

  Like def, but with annotations.

  eg. (def> vname :- Long 1)

  ;doc
  (def> vname
    \"Docstring\"
    :- Long
    1)"
  [name & fdecl]
  (let [[docstring fdecl] (internal/take-when string? fdecl)
        _ (assert (and (#{3} (count fdecl))
                       (#{:-} (first fdecl)))
                  (str "Bad def> syntax: " fdecl))
        [_ tsyn body] fdecl]
    `(do (ann ~name ~tsyn)
         ~(list* 'def name 
                 (concat
                   (when docstring [docstring])
                   [body])))))


(defmacro 
  ^{:forms '[(letfn> [fn-spec-or-annotation*] expr*)]}
  letfn>
  "Like letfn, but each function spec must be annotated.

  eg. (letfn> [a :- [Number -> Number]
               (a [b] 2)

               c :- [Symbol -> nil]
               (c [s] nil)]
        ...)"
  [fn-specs-and-annotations & body]
  (let [bindings fn-specs-and-annotations
        ; (Vector (U '[Symbol TypeSyn] LetFnInit))
        normalised-bindings
        (loop [[fbnd :as bindings] bindings
               norm []]
          (cond
            (empty? bindings) norm
            (symbol? fbnd) (do
                             (assert (#{:-} (second bindings))
                                     "letfn> annotations require :- separator")
                             (assert (<= 3 (count bindings)))
                             (recur 
                               (drop 3 bindings)
                               (conj norm [(nth bindings 0)
                                           (nth bindings 2)])))
            (list? fbnd) (recur
                           (next bindings)
                           (conj norm fbnd))
            :else (throw (Exception. (str "Unknown syntax to letfn>: " fbnd)))))
        {anns false inits true} (group-by list? normalised-bindings)
        ; init-syn unquotes local binding references to be compatible with hygienic expansion
        init-syn (into {}
                   (for [[lb type] anns]
                     [lb `'~type]))]
    `(core/letfn ~(vec inits)
       ;unquoted to allow bindings to resolve with hygiene
       ~init-syn
       ;preserve letfn empty body
       ~@(or body [nil]))))

(comment
  (letfn :- Type
    [(a (:- RetType [b :- Type] b)
        (:- Any [b :- Type, c :- Type] c))]
    )
  (let :- Type
    [f :- Type, foo]
    (f 1))
  (do :- Type
      )

  (fn ([a :- Type & r Type ... a] :- Type))

  (pfn [x] 
    (:- Type [a :- Type & r Type ... a]))

  (for :- Type
    [a :- Type, init]
    )

  (dotimes
    [a :- Type, init]
    )

  (loop :- Type
    [a :- Type, init]
    )

  (doseq
    [a :- Type, init]
    )

  (deftype [[x :variance :covariant]]
    Name 
    [x :- Foo, y :- Bar]
    )

  (defrecord [[x :variance :covariant]]
    Name 
    [x :- Foo, y :- Bar]
    )

  (definterface Name)

  (reify)
)

;(defmacro 
;  ^{:see-also '[filter-identity]}
;  filter 
;  "The same as clojure.core/filter, but supports inline annotations
;  to help instantiate negative predicates.
;
;  # Positive predicates
;  
;  Simple positive predicates like `number?` or `symbol?` that have a
;  :then filter of the form `(is x 0) do not require annotation:
;    
;    (filter number? [1 2 3 nil 'a])
;    ; (Seq Number)
;
;  # Negative predicates
;
;  If the predicate's :then filter has the form `(! x 0)`, like 
;  for example `identity`, this macro can help instantiate the expression.
;
;  2 type annotations are needed:
;   :in      the member type of the input
;   :remove  a type the predicate removes from the input collection
;
;    (filter :in (U nil Number), :remove nil
;            identity [1 2 nil])
;    ; Number"
;  [& args]
;  (let [[flat-opt tail] (split-at (- (count args) 2) args)
;        _ (assert (even? (count flat-opt)) (str "Uneven keyword arguments to filter"))
;        _ (assert (#{2} (count tail)) "Wrong arguments to filter")
;        {:as opt} flat-opt
;        extra (seq (set/difference (set (keys opt)) #{:in :remove}))
;        _ (assert (not extra) (str "Unsupported options to filter: " (set extra)))
;        has-in (contains? opt :in)
;        has-remove (contains? opt :remove)
;        _ (assert (or (and has-in has-remove)
;                      (and (not has-in)
;                           (not has-remove)))
;                  "Must provide both :in and :remove if supplying one.")]
;    (if (and has-in has-remove)
;      `((inst core/filter ~(:in opt) ~(:remove opt)) ~@tail)
;      `(core/filter ~@tail))))
;
;(defmacro filter-identity 
;  "Semantically the same as (filter identity coll).
;
;  Expands out to the equivalent of
; 
;    ((inst filter t (U nil false)) 
;     (inst identity t) 
;     coll)
;
;  The type t is the member type of the collection argument.
;  
;  eg. 
;      (filter-identity :- (U nil Number) [1 2 nil 3])
;      ; (Seq Number)
;
;      (filter-identity :- (U nil Number false) [1 2 nil 3 false])
;      ; (Seq Number)"
;  [colon t coll]
;  (assert (#{:in} colon)
;          (str "Must provide :in option to filter-identity"))
;  `(filter :in ~t :remove ~'(U nil false) 
;           (inst identity ~t) 
;           ; better error message
;           (ann-form ~coll ~`(~'U nil (Seqable ~t)))))
;
;(defmacro 
;  ^{:see-also '[remove-nil remove-false]}
;  remove 
;  "The same as clojure.core/remove, but supports inline annotations
;  to help instantiate positive predicates.
;
;  # Negative predicates
;  
;  Simple negative predicates like `number?` or `symbol?` that have a
;  :else filter of the form `(is x 0) do not require annotation:
;    
;    (filter number? [1 2 3 nil 'a])
;    ; (Seq Number)
;
;  # Negative predicates
;
;  If the predicate's :then filter has the form `(! x 0)`, like 
;  for example `identity`, this macro can help instantiate the expression.
;
;  2 type annotations are needed:
;   :in      the member type of the input
;   :remove  a type the predicate removes from the input collection
;
;    (filter :in (U nil Number), :remove nil
;            identity [1 2 nil])
;    ; Number"
;  [& args]
;  (let [[flat-opt tail] (split-at (- (count args) 2) args)
;        _ (assert (even? (count flat-opt)) (str "Uneven keyword arguments to filter"))
;        _ (assert (#{2} (count tail)) "Wrong arguments to filter")
;        {:as opt} flat-opt
;        extra (seq (set/difference #{:in :remove} (set (keys opt))))
;        _ (assert (not extra) (str "Unsupported options to filter: " (set extra)))
;        has-in (contains? opt :in)
;        has-remove (contains? opt :remove)
;        _ (assert (or (and has-in has-remove)
;                      (and (not has-in)
;                           (not has-remove)))
;                  "Must provide both :in and :remove if supplying one.")]
;    (if (and has-in has-remove)
;      `((inst core/filter ~(:in opt) ~(:remove opt)) ~@tail)
;      `(filter ~@tail))))
;
;(defmacro remove-nil 
;  "Semantically the same as (remove nil? coll)
;
;  eg. (remove-nil :in (U nil Number) [1 2 nil 3])
;      ; (Seq Number)
;      (remove-nil :in (U nil Number false) [1 2 nil 3 false])
;      ; (Seq (U Number false))"
;  [colon t coll]
;  (assert (#{:in} colon)
;          "Must provide :in option to remove-nil")
;  `(remove :in ~t :remove nil nil? ~coll))
;
;(defmacro remove-false 
;  "Semantically the same as (remove false? coll)
;
;  Expands to the equivalent of
; 
;    ((inst remove t false)
;     false?
;     coll)
;
;  eg. (remove-false :- (U false Number) [1 2 false 3])
;      ; (Seq Number)
;      (remove-false :- (U nil Number false) [1 2 nil 3 false])
;      ; (Seq (U Number nil))"
;  [colon t coll]
;  (assert (#{:in} colon)
;          "Must provide :in option to remove-false")
;  `(remove :in ~t :remove false false? ~coll))

#_(defmacro 
  ^{:forms '[(letfn [fn-spec-or-annotation*] expr*)]}
  letfn
  "Like letfn, but each function spec must be annotated.

  eg. (letfn [a :- [Number -> Number]
              (a [b] 2)

              c :- [Symbol -> nil]
              (c [s] nil)]
        ...)"
  [fn-specs-and-annotations & body]
  (let [bindings fn-specs-and-annotations
        ; (Vector (U '[Symbol TypeSyn] LetFnInit))
        normalised-bindings
        (loop [[fbnd :as bindings] bindings
               norm []]
          (cond
            (empty? bindings) norm
            (symbol? fbnd) (do
                             (assert (#{:-} (second bindings))
                                     "letfn annotations require :- separator")
                             (assert (<= 3 (count bindings)))
                             (recur 
                               (drop 3 bindings)
                               (conj norm [(nth bindings 0)
                                           (nth bindings 2)])))
            (list? fbnd) (recur
                           (next bindings)
                           (conj norm fbnd))
            :else (throw (Exception. (str "Unknown syntax to letfn: " fbnd)))))
        {anns false inits true} (group-by list? normalised-bindings)
        ; init-syn unquotes local binding references to be compatible with hygienic expansion
        init-syn (into {}
                   (for [[lb type] anns]
                     [lb `{:full '~type}]))]
    `(core/letfn ~(vec inits)
       ;unquoted to allow bindings to resolve with hygiene
       ~init-syn
       ;preserve letfn empty body
       ~@(or body [nil]))))

(defmacro ^:deprecated defprotocol> [& body]
  "DEPRECATED: use clojure.core.typed/defprotocol

  Like defprotocol, but required for type checking
  its macroexpansion.
  
  eg. (defprotocol> MyProtocol
        (a [this]))"
  `(tc-ignore
     (core/defprotocol ~@body)))

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

(defmacro 
  ^{:forms '[(loop> [binding :- type, init*] exprs*)]}
  ^:deprecated
  loop>
  "DEPRECATED: use clojure.core.typed/loop
  
  Like loop, except loop variables require annotation.

  Suggested idiom: use a comma between the type and the initial
  expression.

  eg. (loop> [a :- Number, 1
              b :- (U nil Number), nil]
        ...)"
  [bndings* & forms]
  (let [normalise-args
        (fn [seq-exprs]
          (loop [flat-result ()
                      seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result
              (and (vector? (first seq-exprs))
                   (#{:-} (-> seq-exprs first second))) (do
                                                          (prn "DEPRECATED WARNING: loop> syntax has changed, use [b :- t i] for clauses"
                                                               "ns: " *ns* " form:" &form)
                                                          (recur (concat flat-result (take 2 seq-exprs))
                                                                 (drop 2 seq-exprs)))
              :else (do (assert (#{:-} (second seq-exprs))
                                "Incorrect syntax in loop>.")
                        (recur (concat flat-result [(vec (take 3 seq-exprs))
                                                    (nth seq-exprs 3)])
                               (drop 4 seq-exprs))))))
        ;group args in flat pairs
        bndings* (normalise-args bndings*)
        bnds (partition 2 bndings*)
        ; [[lhs :- bnd-ann] rhs]
        lhs (map ffirst bnds)
        rhs (map second bnds)
        bnd-anns (map #(-> % first next second) bnds)]
    `(loop>-ann (core/loop ~(vec (mapcat vector lhs rhs))
                  ~@forms)
                '~bnd-anns)))


(defn ^:skip-wiki
  declare-datatypes* 
  "Internal use only. Use declare-datatypes."
  [nms]
  nil)

(defmacro declare-datatypes 
  "Declare datatypes, similar to declare but on the type level."
  [& syms]
  `(declare-datatypes* '~syms))

(defn ^:skip-wiki
  declare-protocols* 
  "Internal use only. Use declare-protocols."
  [syms]
  nil)

(defmacro declare-protocols 
  "Declare protocols, similar to declare but on the type level."
  [& syms]
  `(declare-protocols* '~syms))

(defn ^:skip-wiki
  declare-alias-kind* 
  "Internal use only. Use declare-alias-kind."
  [sym ty]
  nil)

(defmacro declare-alias-kind
  "Declare a kind for an alias, similar to declare but on the kind level."
  [sym ty]
  `(do
     (declare ~sym)
     (declare-alias-kind* '~sym '~ty)))

(defn ^:skip-wiki
  declare-names* 
  "Internal use only. Use declare-names."
  [syms]
  nil)

(defmacro declare-names 
  "Declare names, similar to declare but on the type level."
  [& syms]
  `(declare-names* '~syms))

(defn ^:skip-wiki
  def-alias* 
  "Internal use only. Use def-alias."
  [sym type]
  nil)

(defn ^:skip-wiki add-to-alias-env [&form qsym t]
  (swap! impl/alias-env assoc qsym 
         (impl/with-impl impl/clojure
           (binding [uvars/*current-env* {:ns {:name (ns-name *ns*)}
                                          :file *file*
                                          :line (or (-> &form meta :line)
                                                    @clojure.lang.Compiler/LINE)
                                          :column (or (-> &form meta :column)
                                                      @clojure.lang.Compiler/COLUMN)}]
             (ast/parse-clj t))))
  nil)

(defmacro 
  ^:deprecated
  def-alias 
  "DEPRECATED: use defalias
  
  Define a type alias. Takes an optional doc-string as a second
  argument.

  Updates the corresponding var with documentation.
  
  eg. (def-alias MyAlias
        \"Here is my alias\"
        (U nil String))"
  ([sym doc-str t]
   (assert (string? doc-str) "Doc-string passed to def-alias must be a string")
   `(def-alias ~(vary-meta sym assoc :doc doc-str) ~t))
  ([sym t]
   (assert (symbol? sym) (str "First argument to def-alias must be a symbol: " sym))
   (let [qsym (if (namespace sym)
                sym
                (symbol (-> *ns* ns-name str) (str sym)))
         m (-> (meta sym)
             (update-in [:doc] #(str #_"Type Alias\n\n" % "\n\n" (with-out-str (pprint/pprint t)))))]
     `(do
        (tc-ignore (add-to-alias-env '~&form '~qsym '~t))
        (let [v# (intern '~(symbol (namespace qsym)) '~(symbol (name qsym)))]
          (tc-ignore (alter-meta! v# merge '~m)))
        (def-alias* '~qsym '~t)))))

(defmacro defalias 
  "Define a type alias. Takes an optional doc-string as a second
  argument.

  Updates the corresponding var with documentation.
  
  eg. (defalias MyAlias
        \"Here is my alias\"
        (U nil String))"
  ([sym doc-str t]
   (assert (string? doc-str) "Doc-string passed to defalias must be a string")
   `(defalias ~(vary-meta sym assoc :doc doc-str) ~t))
  ([sym t]
   (assert (symbol? sym) (str "First argument to defalias must be a symbol: " sym))
   (let [qsym (if (namespace sym)
                sym
                (symbol (-> *ns* ns-name str) (str sym)))
         m (-> (meta sym)
             (update-in [:doc] #(str #_"Type Alias\n\n" % "\n\n" (with-out-str (pprint/pprint t)))))]
     `(do
        (tc-ignore (add-to-alias-env '~&form '~qsym '~t))
        (let [v# (intern '~(symbol (namespace qsym)) '~(symbol (name qsym)))]
          (tc-ignore (alter-meta! v# merge '~m)))
        (def-alias* '~qsym '~t)))))

;; `do` is special at the top level
(defmacro tc-ignore 
  "Ignore forms in body during type checking"
  [& body]
  `(do ::special-form
       ::tc-ignore
       ~@(or body [nil])))

(def ^{:doc "Any is the top type that contains all types."
       :forms '[Any]
       ::special-type true}
  Any)

(def ^{:doc "U represents a union of types"
       :forms '[(U type*)]
       ::special-type true}
  U)

(def ^{:doc "Nothing is the bottom type that inhabits no types
            except itself."
       :forms '[Nothing]
       ::special-type true}
  Nothing)

(def ^{:doc "I represents an intersection of types"
       :forms '[(I type*)]
       ::special-type true}
  I)

(def ^{:doc "A singleton type for a constant value."
       :forms '[(Value Constant)
                'Constant]
       ::special-type true}
  Value)

(def ^{:doc "A type representing a range of counts for a collection"
       :forms '[(CountRange Integer)
                (CountRange Integer Integer)]
       ::special-type true}
  CountRange)

(def ^{:doc "A type representing a precise count for a collection"
       :forms '[(ExactCount Integer)]
       ::special-type true}
  ExactCount)

(def ^{:doc "Difference represents a difference of types.
            
            (Difference t s) is the same as type t with type s removed.
            
            eg. (Difference (U Int Long) Int) => Long
                (Difference (U Num nil) nil)  => Num
            "
       :forms '[(Difference type type type*)]
       ::special-type true}
  Difference)

(def ^{:doc "HVec is a type for heterogeneous vectors.
            It extends clojure.core.typed/Vec and is a subtype
            of clojure.core.typed/HSequential."
       :forms '[(HVec [fixed*] :filter-sets [FS*] :objects [obj*])
                (HVec [fixed* type *] :filter-sets [FS*] :objects [obj*])
                (HVec [fixed* type ... bound] :filter-sets [FS*] :objects [obj*])
                '[fixed*]
                '[fixed* type *]
                '[fixed* type ... bound]]
       ::special-type true}
  HVec)

(def ^{:doc "HMap is a type for heterogeneous maps."
       :forms '[(HMap :mandatory {Constant Type*}
                      :optional  {Constant Type*}
                      :absent-keys #{Constant*}
                      :complete? Boolean)
                '{Constant Type*}]
       ::special-type true}
  HMap)

(def ^{:doc "HSequential is a type for heterogeneous sequential collections"
       :forms '[(HSequential [fixed*] :filter-sets [FS*] :objects [obj*])
                (HSequential [fixed* rest *] :filter-sets [FS*] :objects [obj*])
                (HSequential [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
       ::special-type true}
  HSequential)

(def ^{:doc "HSeq is a type for heterogeneous seqs"
       :forms '[(HSeq [fixed*] :filter-sets [FS*] :objects [obj*])
                (HSeq [fixed* rest *] :filter-sets [FS*] :objects [obj*])
                (HSeq [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
       ::special-type true}
  HSeq)

(def ^{:doc "An ordered intersection type of function arities."
       :forms '[(FnCase ArityVec+)
                [fixed* -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
                [fixed* rest * -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
                [fixed* drest ... bound -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]]
       ::special-type true}
  FnCase)

(def ^{:doc "A predicate for the given type."
       :forms '[(Pred type)]
       ::special-type true}
  Pred)

(def ^{:doc "A type representing an assoc operation"
       :forms '[(Assoc type type-pairs*)]
       ::special-type true}
  Assoc)

(def ^{:doc "A type representing a dissoc operation"
       :forms '[(Dissoc type type*)]
       ::special-type true}
  Dissoc)

(def ^{:doc "A type representing a get operation"
       :forms '[(Get type type)
                (Get type type type)]
       ::special-type true}
  Get)

(def ^{:doc "A recursive type"
       :forms '[(Rec binder type)]
       ::special-type true}
  Rec)

(def ^{:doc "A polymorphic binder"
       :forms '[(All binder type)]
       ::special-type true}
  All)

(def ^{:doc "A type function"
       :forms '[(TFn binder type)]
       ::special-type true}
  TFn)

;(def ^{:doc "A polymorphic binder"
;       :forms '[(All binder type)]
;       ::special-type true}
;  Array)

(defmacro init-aliases []
  (core/letfn [(def-alias-many [vinit]
                `(do
                   ~@(for [[k v] (partition 2 vinit)]
                       `(def-alias ~k ~v))))]
    (def-alias-many 
      impl/init-aliases)))

; defines base aliases
(init-aliases)

(defn ^:private rclass-pred [rcls opts]
  (swap! impl/rclass-env assoc (impl/Class->symbol rcls) opts))

(defmacro ^:private rclass-preds [& args]
  `(do
     ~@(for [[k v] (partition 2 args)]
         `(rclass-pred ~k ~v))))

(rclass-preds
;  clojure.lang.Seqable 
;  {:pred (fn [this a?]
;           (cond 
;             (string? this) (every? a? this)
;             (coll? this) (every? a? this)))}
  clojure.lang.IPersistentCollection
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.ISeq
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.IPersistentSet
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.APersistentSet
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.PersistentHashSet
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.PersistentTreeSet
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.Associative
  {:args #{2}
   :pred (fn [this a? b?]
           `(cond
              (vector? ~this) (and (every? ~a? (range (count ~this)))
                                   (every? ~b? ~this))
              (map? ~this) (and (every? ~a? (keys ~this))
                                (every? ~b? (vals ~this)))))}
  clojure.lang.IPersistentStack
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.IPersistentVector
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.APersistentVector
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.PersistentVector
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.IMapEntry
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (~a? (key ~this)) (~b? (val ~this))))}
  clojure.lang.AMapEntry
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (~a? (key ~this)) (~b? (val ~this))))}
  clojure.lang.MapEntry
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (~a? (key ~this)) (~b? (val ~this))))}
  clojure.lang.IPersistentMap
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (every? ~a? (keys ~this))
                 (every? ~b? (vals ~this))))}
  clojure.lang.ASeq
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.APersistentMap
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (every? ~a? (keys ~this))
                 (every? ~b? (vals ~this))))}
  clojure.lang.PersistentHashMap
  {:args #{2}
   :pred (fn [this a? b?] 
           `(and (every? ~a? (keys ~this))
                 (every? ~b? (vals ~this))))}
  clojure.lang.Cons
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.IPersistentList
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.PersistentList
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.LazySeq
  {:args #{1}
   :pred (fn [this a?] 
           `(every? ~a? ~this))}
  clojure.lang.Reduced
  {:args #{1}
   :pred (fn [this a?] 
           `(~a? (deref ~this)))})


;(ann into-array>* [Any Any -> Any])
(defn ^:skip-wiki
  into-array>*
  "Internal use only. Use into-array>."
  ([cljt coll]
   (load-if-needed)
   (let [parse-type @(ns-resolve (find-ns 'clojure.core.typed.parse-unparse)
                                 'parse-type)
         amc @(ns-resolve (find-ns 'clojure.core.typed.array-ops)
                          'Type->array-member-Class)]
     (impl/with-clojure-impl
       (into-array (-> cljt parse-type amc) coll))))
  ([javat cljt coll]
   (load-if-needed)
   (let [parse-type @(ns-resolve (find-ns 'clojure.core.typed.parse-unparse)
                                 'parse-type)
         amc @(ns-resolve (find-ns 'clojure.core.typed.array-ops)
                          'Type->array-member-Class)]
     (impl/with-clojure-impl
       (into-array (-> javat parse-type amc) coll))))
  ;this is the hacky case to prevent full core.typed from loading
  ([into-array-syn javat cljt coll]
   (into-array (resolve into-array-syn) coll)))

;FIXME hacky 4-arity version to prevent full type system from loading
(defmacro into-array> 
  "Make a Java array with Java class javat and Typed Clojure type
  cljt. Resulting array will be of type javat, but elements of coll must be under
  cljt. cljt should be a subtype of javat (the same or more specific).

  *Temporary hack*
  into-array-syn is exactly the syntax to put as the first argument to into-array.
  Calling resolve on this syntax should give the correct class."
  ([cljt coll]
   `(into-array>* '~cljt ~coll))
  ([javat cljt coll]
   `(into-array>* '~javat '~cljt ~coll))
  ([into-array-syn javat cljt coll]
   `(into-array>* '~javat '~cljt ~coll)))


(defn ^:skip-wiki
  non-nil-return* 
  "Internal use only. Use non-nil-return."
  [msym arities]
  nil)

(defmacro non-nil-return 
  "Override the return type of fully qualified method msym to be non-nil.
  Takes a set of relevant arities,
  represented by the number of parameters it takes (rest parameter counts as one),
  or :all which overrides all arities.
  
  eg. ; must use full class name
      (non-nil-return java.lang.Class/getDeclaredMethod :all)"
  [msym arities]
  `(non-nil-return* '~msym '~arities))

(defn ^:skip-wiki
  nilable-param* 
  "Internal use only. Use nilable-param."
  [msym mmap]
  nil)

(defmacro nilable-param 
  "Override which parameters in qualified method msym may accept
  nilable values. If the parameter is a parameterised type or
  an Array, this also declares the parameterised types and the Array type as nilable.

  mmap is a map mapping arity parameter number to a set of parameter
  positions (integers). If the map contains the key :all then this overrides
  other entries. The key can also be :all, which declares all parameters nilable."
  [msym mmap]
  `(nilable-param* '~msym '~mmap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(defn print-env 
  "During type checking, print the type environment to *out*,
  preceeded by literal string debug-str."
  [debug-str]
  nil)

(defn ^:skip-wiki
  ann* 
  "Internal use only. Use ann."
  [varsym typesyn check?]
  nil)

(defmacro ann 
  "Annotate varsym with type. If unqualified, qualify in the current namespace.
  If varsym has metadata {:no-check true}, ignore definitions of varsym 
  while type checking.

  If annotating vars in namespaces other than the current one, a fully
  qualified symbol must be provided. Note that namespace aliases are not
  recognised: the *full* namespace must be given in the first part of the symbol.
  
  eg. ; annotate the var foo in this namespace
      (ann foo [Number -> Number])
  
      ; annotate a var in another namespace
      (ann another.ns/bar [-> nil])
   
      ; don't check this var
      (ann ^:no-check foobar [Integer -> String])"
  [varsym typesyn]
  (let [qsym (if (namespace varsym)
               varsym
               (symbol (-> *ns* ns-name str) (str varsym)))
        _ (when (contains? (meta varsym) :nocheck)
            (println (str "DEPRECATED: :nocheck metadata for " varsym " ann. Use :no-check"))
            (flush))
        opts (meta varsym)
        _ (assert (not (and (contains? opts :nocheck)
                            (contains? opts :no-check)))
                  "Cannot provide both :nocheck and :no-check metadata to ann")
        check? (not (or (:no-check opts)
                        (:nocheck opts)))
        ast (binding [uvars/*current-env* {:ns {:name (ns-name *ns*)}
                                           :file *file*
                                           :line (or (-> &form meta :line)
                                                     @clojure.lang.Compiler/LINE)
                                           :column (or (-> &form meta :column)
                                                       @clojure.lang.Compiler/COLUMN)}]
              (ast/parse-clj typesyn))]
    (swap! impl/var-env assoc qsym ast)
    `(ann* '~qsym '~typesyn '~check?)))

(defmacro ann-many
  "Annotate several vars with type t.

  eg. (ann-many FakeSearch
                web1 web2 image1 image2 video1 video2)"
  [t & vs]
  `(do ~@(map #(list `ann % t) vs)))

(defonce ^:dynamic 
  ^{:deprecated true
    :doc 
    "If a true value, global annotations are collected by the
    type checker when their respective forms are evaluated (eg. ann)."}
  *collect-on-eval* 
  false)

(defn ^:skip-wiki
  ann-datatype*
  "Internal use only. Use ann-datatype."
  [vbnd dname fields opts]
  nil)

(defmacro
  ^{:forms '[(ann-datatype dname [field :- type*] opts*)
             (ann-datatype binder dname [field :- type*] opts*)]}
  ann-datatype
  "Annotate datatype Class name dname with expected fields.
  If unqualified, qualify in the current namespace.
  Takes an optional type variable binder before the name.

  Fields must be specified in the same order as presented 
  in deftype, with exactly the same field names.

  Also annotates datatype factories and constructors.

  Binder is a vector of specs. Each spec is a vector
  with the variable name as the first entry, followed by
  keyword arguments:
  - :variance (mandatory)
    The declared variance of the type variable. Possible
    values are :covariant, :contravariant and :invariant.
  - :< (optional)
    The upper type bound of the type variable. Defaults to
    Any, or the most general type of the same rank as the
    lower bound.
  - :> (optional)
    The lower type bound of the type variable. Defaults to
    Nothing, or the least general type of the same rank as the
    upper bound.

  eg. ; a datatype in the current namespace
      (ann-datatype MyDatatype [a :- Number,
                                b :- Long])

      ; a datatype in another namespace
      (ann-datatype another.ns.TheirDatatype
                    [str :- String,
                     vec :- (Vec Number)])

      ; a datatype, polymorphic in a
      (ann-datatype [[a :variance :covariant]]
                    MyPolyDatatype
                    [str :- String,
                     vec :- (Vec Number)
                     ply :- (Set a)])"
  [& args]
  ;[dname fields & {ancests :unchecked-ancestors rplc :replace :as opts}]
  (let [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [dname fields & {ancests :unchecked-ancestors rplc :replace :as opts}]
        (if bnd-provided?
          (next args)
          args)]
    (assert (not rplc) "Replace NYI")
    (assert (symbol? dname)
            (str "Must provide name symbol: " dname))
    (let [qname (if (some #{\.} (str dname))
                  dname
                  (symbol (str (namespace-munge *ns*) "." dname)))]
      (swap! impl/datatype-env 
             assoc 
             qname
             {:record? false
              :name qname
              :fields fields
              :bnd vbnd}))
    `(ann-datatype* '~vbnd '~dname '~fields '~opts)))

(defn ^:skip-wiki
  ann-pdatatype* 
  "Internal use only. Use ann-pdatatype."
  [dname vbnd fields opt]
  nil)

(defmacro ^:skip-wiki ann-pdatatype 
  "REMOVED OPERATION: ann-pdatatype, use ann-datatype"
  [dname vbnd fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
  (assert nil "REMOVED OPERATION: ann-pdatatype, use ann-datatype")
  (assert (not rplc) "Replace NYI")
  (assert (symbol? dname)
          (str "Must provide local symbol: " dname))
  `(ann-pdatatype* '~dname '~vbnd '~fields '~opt))

(defn ^:skip-wiki
  ann-record* 
  "Internal use only. Use ann-record"
  [vbnd dname fields opt]
  nil)

(defmacro 
  ^{:forms '[(ann-record dname [field :- type*] opts*)
             (ann-record binder dname [field :- type*] opts*)]}
  ann-record 
  "Annotate record Class name dname with expected fields.
  If unqualified, qualify in the current namespace.
  Takes an optional type variable binder before the name.

  Fields must be specified in the same order as presented 
  in defrecord, with exactly the same field names.

  Also annotates record factories and constructors.

  Binder is a vector of specs. Each spec is a vector
  with the variable name as the first entry, followed by
  keyword arguments:
  - :variance (mandatory)
    The declared variance of the type variable. Possible
    values are :covariant, :contravariant and :invariant.
  - :< (optional)
    The upper type bound of the type variable. Defaults to
    Any, or the most general type of the same rank as the
    lower bound.
  - :> (optional)
    The lower type bound of the type variable. Defaults to
    Nothing, or the least general type of the same rank as the
    upper bound.
  
  eg. ; a record in the current namespace
      (ann-record MyRecord [a :- Number,
                            b :- Long])

      ; a record in another namespace
      (ann-record another.ns.TheirRecord
                    [str :- String,
                     vec :- (Vec Number)])

      ; a record, polymorphic in a
      (ann-record [[a :variance :covariant]]
                  MyPolyRecord
                  [str :- String,
                   vec :- (Vec Number)
                   ply :- (Set a)])"
  [& args]
  ;[dname fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
  (let [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [dname fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
        (if bnd-provided?
          (next args)
          args)]
    (let [qname (if (some #{\.} (str dname))
                  dname
                  (symbol (str (namespace-munge *ns*) "." dname)))]
      (swap! impl/datatype-env 
             assoc 
             qname
             {:record? true
              :name qname
              :fields fields
              :bnd vbnd}))
    `(ann-record* '~vbnd '~dname '~fields '~opt)))


(defn ^:skip-wiki
  ann-precord* 
  "Internal use only. Use ann-precord."
  [dname vbnd fields opt]
  nil)

(defmacro ann-precord 
  "Annotate record Class name dname with a polymorphic binder and expected fields.
  If unqualified, qualify in the current namespace."
  [dname vbnd fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
  (println "WARNING: ann-precord is deprecated, use ann-record")
  (flush)
  `(ann-precord* '~dname '~vbnd '~fields '~opt))

(defn ^:skip-wiki
  ann-protocol* 
  "Internal use only. Use ann-protocol."
  [vbnd varsym mth]
  nil)

(defmacro 
  ^{:forms '[(ann-protocol vbnd varsym & methods)
             (ann-protocol varsym & methods)]}
  ann-protocol 
  "Annotate a possibly polymorphic protocol var with method types.
  
  eg. (ann-protocol IFoo
        bar
        (Fn [IFoo -> Any]
            [IFoo Number Symbol -> Any])
        baz
        [IFoo Number -> Number])
      (defprotocol> IFoo
        (bar [this] [this n s])
        (baz [this n]))

      ; polymorphic protocol
      ; x is scoped in the methods
      (ann-protocol [[x :variance :covariant]]
        IFooPoly
        bar
        (Fn [(IFooPoly x) -> Any]
            [(IFooPoly x) Number Symbol -> Any])
        baz
        [(IFooPoly x) Number -> Number])
      (defprotocol> IFooPoly
        (bar [this] [this n s])
        (baz [this n]))"
  [& args]
  (let [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [varsym & mth] (if bnd-provided?
                         (next args)
                         args)
        _ (let [fs (frequencies (map first (partition 2 mth)))]
            (when-let [dups (seq (filter (fn [[_ freq]] (< 1 freq)) fs))]
              (println (str "WARNING: Duplicate method annotations in ann-protocol (" varsym 
                            "): " (str/join ", " (map first dups))))
              (flush)))
        ; duplicates are checked above.
        ; duplicate munged methods are checked in collect-phase
        {:as mth} mth
        qualsym (if (namespace varsym)
                  varsym
                  (symbol (str (ns-name *ns*)) (name varsym)))]
    (swap! impl/protocol-env
           assoc qualsym
           {:name qualsym
            :methods mth
            :bnds vbnd})
    `(ann-protocol* '~vbnd '~varsym '~mth)))

(defn ^:skip-wiki
  ann-interface* 
  "Internal use only. Use ann-interface."
  [vbnd clsym mth]
  nil)

(defmacro 
  ^{:forms '[(ann-interface vbnd varsym & methods)
             (ann-interface varsym & methods)]}
  ann-interface 
  "Annotate a possibly polymorphic interface (created with definterface) with method types.

  Note: Unlike ann-protocol, omit the target ('this') argument in the method signatures.
  
  eg. (ann-interface IFoo
        bar
        (Fn [-> Any]
            [Number Symbol -> Any])
        baz
        [Number -> Number])
      (definterface IFoo
        (bar [] [n s])
        (baz [n]))

      ; polymorphic protocol
      ; x is scoped in the methods
      (ann-protocol [[x :variance :covariant]]
        IFooPoly
        bar
        (Fn [-> Any]
            [Number Symbol -> Any])
        baz
        [Number -> Number])
      (definterface IFooPoly
        (bar [] [n s])
        (baz [n]))"
  [& args]
  (let [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [clsym & mth] (if bnd-provided?
                         (next args)
                         args)
        _ (let [fs (frequencies (map first (partition 2 mth)))]
            (when-let [dups (seq (filter (fn [[_ freq]] (< 1 freq)) fs))]
              (println (str "WARNING: Duplicate method annotations in ann-interface (" clsym 
                            "): " (str/join ", " (map first dups))))
              (flush)))
        ; duplicates are checked above.
        ; duplicate munged methods are checked in collect-phase
        {:as mth} mth
        qualsym (if (namespace clsym)
                  clsym
                  (symbol (munge (str (ns-name *ns*))) (name clsym)))]
    `(ann-interface* '~vbnd '~clsym '~mth)))

(defn ^:skip-wiki
  ann-pprotocol* 
  "Internal use only. Use ann-pprotocol."
  [varsym vbnd mth]
  nil)

(defmacro ^:skip-wiki ann-pprotocol  
  "UNSUPPPORTED OPERATION: ann-pprotocol, use ann-protocol with binder 
  as first argument, ie. before protocol name"
  [varsym vbnd & {:as mth}]
  (prn "UNSUPPPORTED OPERATION: ann-pprotocol, use ann-protocol with binder as first argument, ie. before protocol name")
  `(ann-pprotocol* '~varsym '~vbnd '~mth))

(defn ^:skip-wiki
  override-constructor* 
  "Internal use only. Use override-constructor."
  [ctorsym typesyn]
  nil)

(defmacro override-constructor 
  "Override all constructors for Class ctorsym with type."
  [ctorsym typesyn]
  `(override-constructor* '~ctorsym '~typesyn))

(defn ^:skip-wiki
  override-method* 
  "Internal use only. Use override-method."
  [methodsym typesyn]
  nil)

(defmacro override-method 
  "Override type for qualified method methodsym."
  [methodsym typesyn]
  `(override-method* '~methodsym '~typesyn))

(defn ^:skip-wiki
  typed-deps* 
  "Internal use only. Use typed-deps."
  [args]
  nil)

(defmacro typed-deps 
  "Declare namespaces which should be checked before the current namespace.
  Accepts any number of symbols. Only has effect via check-ns.
  
  eg. (typed-deps clojure.core.typed.holes
                  myns.types)"
  [& args]
  `(typed-deps* '~args))

;(defn unchecked-ns*
;  "Internal use only. Use unchecked-ns."
;  [])
;
;(defmacro unchecked-ns
;  "Declare this namespace to be unchecked. 
;  
;  This disables type collection and checking for the current namespace.
;  Useful when the namespace has a dependency on clojure.core.typed,
;  and therefore a candidate for automatically inferred type dependencies,
;  but should never be type checked.
;  
;  eg. (unchecked-ns)"
;  []
;  `(unchecked-ns*))

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


(defn ^:skip-wiki var>* [sym]
  (impl/the-var sym))

(defmacro var>
  "Like var, but resolves at runtime like ns-resolve and is understood by
  the type checker. sym must be fully qualified (without aliases).
  
  eg. (var> clojure.core/+)"
  [sym]
  `(var>* '~sym))

(defn ^:skip-wiki
  warn-on-unannotated-vars*
  "Internal use only. Use allow-unannotated-vars"
  []
  nil)

(defmacro warn-on-unannotated-vars
  "Allow unannotated vars in the current namespace. 
  
  Emits a warning instead of a type error when checking
  a def without a corresponding expected type.

  Disables automatic inference of `def` expressions.
  
  eg. (warn-on-unannotated-vars)"
  []
  `(warn-on-unannotated-vars*))

(declare check-form-info print-errors! ^:dynamic *currently-checking-clj*)

(defn check-form*
  "Takes a (quoted) form and optional expected type syntax and
  type checks the form. If expected is provided, type-provided?
  must be true."
  ([form] (check-form* form nil nil))
  ([form expected] (check-form* form expected true))
  ([form expected type-provided?]
   (load-if-needed)
   (let [unparse-TCResult-in-ns (impl/v 'clojure.core.typed.parse-unparse/unparse-TCResult-in-ns)
         {:keys [delayed-errors ret]} (check-form-info form 
                                                       :expected expected 
                                                       :type-provided? type-provided?)]
     (if-let [errors (seq delayed-errors)]
       (print-errors! errors)
       (impl/with-clojure-impl
         (binding [*currently-checking-clj* true]
           (unparse-TCResult-in-ns ret *ns*)))))))

; cf can pollute current type environment to allow REPL experimentation, 
; which is ok because check-ns resets it when called.
(defmacro cf
  "Takes a form and an optional expected type and
  returns a human-readable inferred type for that form.
  Throws an exception if type checking fails.

  Do not use cf inside a typed namespace. cf is intended to be
  used at the REPL or within a unit test. Note that testing for
  truthiness is not sufficient to unit test a call to cf, as nil
  and false are valid type syntax.

  cf preserves annotations from previous calls to check-ns or cf,
  and keeps any new ones collected during a cf. This is useful for
  debugging and experimentation. cf may be less strict than check-ns
  with type checker warnings.
  
  eg. (cf 1) 
      ;=> Long

      (cf #(inc %) [Number -> Number])
      ;=> [Number -> Number]"
   ([form] `(check-form* '~form))
   ([form expected] `(check-form* '~form '~expected)))

(declare ^:dynamic *verbose-forms*)

(defn ^:skip-wiki
  print-errors! 
  "Internal use only"
  [errors]
  {:pre [(seq errors)
         (every? #(instance? clojure.lang.ExceptionInfo %) errors)]}
  (binding [*out* *err*]
    (doseq [^Exception e errors]
      (let [{{:keys [file line column] :as env} :env :as data} (ex-data e)]
        (print "Type Error ")
        (print (str "(" (or file (-> env :ns) "NO_SOURCE_FILE")
                    (when line
                      (str ":" line
                           (when column
                             (str ":" column))))
                    ") "))
        (print (.getMessage e))
        (println)
        (flush)
        (let [[_ form :as has-form?] (find data :form)]
          (when has-form?
            (print "in: ")
            (binding [*print-length* (when-not *verbose-forms*
                                       6)
                      *print-level* (when-not *verbose-forms*
                                      4)]
              (println form))
            (println)
            (println)
            (flush)))
        (flush))))
  (throw (ex-info (str "Type Checker: Found " (count errors) " error" (when (< 1 (count errors)) "s"))
                  {:type-error :top-level-error
                   :errors errors})))

(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *already-collected* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *already-checked* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *currently-checking-clj* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *delayed-errors* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *analyze-ns-cache* nil)

(defonce ^:dynamic 
  ^{:doc 
  "If true, print fully qualified types in error messages
  and return values. Bind around a type checking form like 
  cf or check-ns.
  
  eg. 
  (binding [*verbose-types* true] 
    (cf 1 Number))
  ;=> java.lang.Number"}
  *verbose-types* 
  nil)

(defonce ^:dynamic 
  ^{:doc 
  "If true, print complete forms in error messages. Bind
  around a type checking form like cf or check-ns.
  
  eg.
  (binding [*verbose-forms* true]
    (cf ['deep ['deep ['deep ['deep]]]] Number))
  ;=> <full form in error>"}
  *verbose-forms* 
  nil)

(defonce ^:dynamic
  *trace-checker*
  nil)

(defn ^:skip-wiki
  -init-delayed-errors 
  "Internal use only"
  []
  (atom [] :validator #(and (vector? %)
                            (every? (fn [a] 
                                      (instance? clojure.lang.ExceptionInfo a))
                                    %))))

(def ^:skip-wiki ^:private ^:dynamic *currently-loading* false)

(defn load-if-needed 
  "Load and initialize all of core.typed if not already"
  []
  (when-not *currently-loading*
    (binding [*collect-on-eval* false
              *currently-loading* true]
      (when-not (find-ns 'clojure.core.typed.init)
        (require 'clojure.core.typed.init))
      (let [init-ns (find-ns 'clojure.core.typed.init)]
        (assert init-ns)
        (when-not (@(ns-resolve init-ns 'loaded?))
          (println "Initializing core.typed ...")
          (flush)
          (time (@(ns-resolve init-ns 'load-impl)))
          (println "core.typed initialized.")
          (flush))))))


(defn reset-caches 
  "Reset internal type caches."
  []
  (p/p :typed/reset-caches
  (load-if-needed)
  (@(ns-resolve (find-ns 'clojure.core.typed.subtype) 'reset-subtype-cache))
  (@(ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'reset-Un-cache))
  (@(ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'reset-In-cache))
  (@(ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'reset-supers-cache!))
  (@(ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'reset-RClass-of-cache!))
  (@(ns-resolve (find-ns 'clojure.core.typed.cs-gen) 'reset-dotted-var-store!))
  nil))

(defn check-form-info 
  "Alpha - subject to change

  Type checks a (quoted) form and returns a map of results from type checking the
  form.
  
  Options
  - :expected        Type syntax representing the expected type for this form
                     type-provided? option must be true to utilise the type.
  - :type-provided?  If true, use the expected type to check the form
  - :profile         Use Timbre to profile the type checker. Timbre must be
                     added as a dependency."
  [form & {:keys [expected type-provided? profile]}]
  (p/profile-if profile
    (load-if-needed)
    (reset-caches)
    (let [check-expr (impl/v 'clojure.core.typed.check/check-expr)
          expr-type (impl/v 'clojure.core.typed.check/expr-type)
          ast-for-form (impl/v 'clojure.core.typed.analyze-clj/ast-for-form)
          collect-ast (impl/v 'clojure.core.typed.collect-phase/collect-ast)
          ret (impl/v 'clojure.core.typed.type-rep/ret)
          parse-type (impl/v 'clojure.core.typed.parse-unparse/parse-type)]
      (if *currently-checking-clj*
        (throw (Exception. "Found inner call to check-ns or cf"))
        (impl/with-clojure-impl
          (binding [*currently-checking-clj* true
                    *already-collected* (atom #{})
                    *already-checked* (atom #{})
                    *delayed-errors* (-init-delayed-errors)
                    *collect-on-eval* false
                    *analyze-ns-cache* (atom {})]
            (let [expected (when type-provided?
                             (ret (parse-type expected)))
                  ast (ast-for-form form)
                  _ (collect-ast ast)
                  _ (reset-caches)
                  c-ast (check-expr ast expected)
                  res (expr-type c-ast)]
              {:delayed-errors @*delayed-errors*
               :ret res})))))))

(defn check-ns-info
  "Alpha - subject to change

  Same as check-ns, but returns a map of results from type checking the
  namespace."
  ([] (check-ns-info *ns*))
  ([ns-or-syms & {:keys [collect-only trace profile]}]
   (p/profile-if profile
     (let [start (. System (nanoTime))]
       (load-if-needed)
       (reset-caches)
       (let [reset-envs! (impl/v 'clojure.core.typed.reset-env/reset-envs!)
             collect-ns (impl/v 'clojure.core.typed.collect-phase/collect-ns)
             check-ns-and-deps (impl/v 'clojure.core.typed.check/check-ns-and-deps)
             vars-with-unchecked-defs (impl/v 'clojure.core.typed.var-env/vars-with-unchecked-defs)
             uri-for-ns (impl/v 'clojure.jvm.tools.analyzer/uri-for-ns)
             
             nsym-coll (map #(if (symbol? %)
                               ; namespace might not exist yet, so ns-name is not appropriate
                               ; to convert to symbol
                               %
                               (ns-name %))
                            (if ((some-fn symbol? #(instance? clojure.lang.Namespace %))
                                 ns-or-syms)
                              [ns-or-syms]
                              ns-or-syms))]
         (cond
           *currently-checking-clj* (throw (Exception. "Found inner call to check-ns or cf"))

           :else
           (binding [*currently-checking-clj* true
                     *delayed-errors* (-init-delayed-errors)
                     *already-collected* (atom #{})
                     *already-checked* (atom #{})
                     *trace-checker* trace
                     *collect-on-eval* false
                     *analyze-ns-cache* (atom {})]
             (let [terminal-error (atom nil)]
               (letfn [(do-collect []
                         (doseq [nsym nsym-coll]
                           (collect-ns nsym)))
                       (print-collect-summary []
                         (let [ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                               collected @*already-collected*]
                           (println "Collected" (count collected) "namespaces in" ms "msecs")
                           (flush)))
                       (do-check []
                         (when-not collect-only
                           (doseq [nsym nsym-coll]
                             (check-ns-and-deps nsym))))
                       (warn-unchecked-defs []
                         (let [vs (vars-with-unchecked-defs)]
                           (binding [*out* *err*]
                             (doseq [v vs]
                               (println "WARNING: Type Checker: Definition missing:" v 
                                        "\nHint: Use :no-check metadata with ann if this is an unchecked var")
                               (flush)))))
                       (print-check-summary []
                         (let [ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                               checked @*already-checked*
                               nlines (p/p :typed/line-count
                                           (apply + (for [nsym checked]
                                                      (with-open [rdr (io/reader (uri-for-ns nsym))]
                                                        (count (line-seq rdr))))))]
                           (println "Checked" (count checked) "namespaces (approx." nlines "lines) in" ms "msecs")
                           (flush)))
                       (do-check-ns 
                         ; Handles a terminal type error
                         []
                         (impl/with-clojure-impl
                           (reset-envs!)
                           ;(reset-caches)
                           (try
                             ;collect
                             (do-collect)
                             (print-collect-summary)

                             ;check
                             (do-check)
                             (warn-unchecked-defs)
                             (print-check-summary)
                             (catch clojure.lang.ExceptionInfo e
                               (if (-> e ex-data :type-error)
                                 (reset! terminal-error e)
                                 (throw e))))))]
                 (do-check-ns)
                 {:delayed-errors (vec (concat (when-let [es *delayed-errors*]
                                                 @es)
                                               (when-let [e @terminal-error]
                                                 [e])))})))))))))

(defn check-ns
  "Type check a namespace/s (a symbol or Namespace, or collection).
  If not provided default to current namespace.
  Returns a true value if type checking is successful, otherwise
  throws an Exception.

  Do not use check-ns within a checked namespace.
  It is intended to be used at the REPL or within a unit test.
  Suggested idiom for clojure.test: (is (check-ns 'your.ns))

  check-ns resets annotations collected from 
  previous check-ns calls or cf. A successful check-ns call will
  preserve any type annotations collect during that checking run.
  
  Keyword arguments:
  - :collect-only  if true, collect type annotations but don't type check code.
                   Useful for debugging purposes.
  - :trace         if true, print some basic tracing of the type checker
  - :profile       if true, use Timbre to profile type checking. Must include
                   Timbre as a dependency.

  If providing keyword arguments, the namespace to check must be provided
  as the first argument.

  Bind *verbose-types* to true to print fully qualified types.
  Bind *verbose-forms* to print full forms in error messages.
  
  eg. (check-ns 'myns.typed)
      ;=> :ok
     
      ; implicitly check current namespace
      (check-ns)
      ;=> :ok
  
      ; collect but don't check the current namespace
      (check-ns *ns* :collect-only true)"
  ([] (check-ns *ns*))
  ([ns-or-syms & {:keys [collect-only trace profile] :as kw}]
   (let [{:keys [delayed-errors]} (apply check-ns-info ns-or-syms (apply concat kw))]
     (if-let [errors (seq delayed-errors)]
       (print-errors! errors)
       :ok))))

; (ann all-defs-in-ns [Namespace -> (Set Symbol)])
(defn ^:private ^:no-wiki 
  all-defs-in-ns
  [ns]
  {:pre [(instance? clojure.lang.Namespace ns)]}
  (set
    (map #(symbol (str (ns-name ns)) (str %))
         (clojure.set/difference 
           (set (keys (ns-map ns))) 
           (set (keys (ns-refers ns))) 
           (set (keys (ns-imports ns)))))))

;(ann statistics [(Coll Symbol) -> (Map Symbol Stats)])
(defn statistics 
  "Takes a collection of namespace symbols and returns a map mapping the namespace
  symbols to a map of data"
  [nsyms]
  (assert (and (coll? nsyms) (every? symbol? nsyms))
          "Must pass a collection of symbols to statistics")
  (reduce (fn [stats nsym]
            (let [_ (check-ns nsym :collect-only true)
                  ns (find-ns nsym)
                  _ (assert ns (str "Namespace " nsym " not found"))]
              (conj stats
                    [nsym
                     {:vars {:all-vars (all-defs-in-ns ns)
                             :no-checks (let [; deref the atom
                                              all-no-checks @(impl/v 'clojure.core.typed.var-env/CLJ-NOCHECK-VAR?)]
                                          (filter (fn [s] (= (namespace s) nsym)) all-no-checks))
                             :var-annotations (let [; deref the atom
                                                    annots @(impl/v 'clojure.core.typed.var-env/CLJ-VAR-ANNOTATIONS)]
                                                (->> annots
                                                     (filter (fn [[k v]] (= (namespace k) (str nsym))))
                                                     (map (fn [[k v]] [k (binding [*verbose-types* true]
                                                                           ((impl/v 'clojure.core.typed.parse-unparse/unparse-type)
                                                                            v))]))
                                                     (into {})))}}])))
          {} nsyms))

; (ann var-coverage [(Coll Symbol) -> nil])
(defn var-coverage 
  "Summarises annotated var coverage statistics to *out*
  for namespaces nsyms, a collection of symbols or a symbol/namespace.
  Defaults to the current namespace if no argument provided."
  ([] (var-coverage *ns*))
  ([nsyms-or-nsym]
   (assert (or (instance? clojure.lang.Namespace nsyms-or-nsym)
               (symbol? nsyms-or-nsym)
               (and (coll? nsyms-or-nsym) (every? symbol? nsyms-or-nsym)))
           "Must pass a collection of symbols or a symbol/namespace to var-coverage")
   (let [nsyms (if ((some-fn symbol? #(instance? clojure.lang.Namespace %))
                    nsyms-or-nsym)
                 [(ns-name nsyms-or-nsym)]
                 nsyms-or-nsym)
         stats (statistics nsyms)
         nall-vars (->> (vals stats) 
                        (map :vars) 
                        (map :all-vars)
                        (apply set/union)
                        set
                        count)
         nannotated-vars (->> (vals stats) 
                              (map :vars) 
                              (map :var-annotations) 
                              (map count)
                              (apply +))
         perc (if (zero? nall-vars)
                0
                (long (* (/ nannotated-vars nall-vars) 100)))]
     (println (str "Found " nannotated-vars " annotated vars out of " nall-vars " vars"))
     (println (str perc "% var annotation coverage"))
     (flush))))

(defn pred* [tsyn nsym pred]
  pred)

(defmacro pred 
  "Generate a flat (runtime) predicate for type that returns true if the
  argument is a subtype of the type, otherwise false.

  The current type variable and dotted type variable scope is cleared before parsing.
  
  eg. ((pred Number) 1)
      ;=> true"
  [t]
  (require '[clojure.core.typed.type-contract])
  (binding [uvars/*current-env* {:ns {:name (ns-name *ns*)}
                                 :file *file*
                                 :line (or (-> &form meta :line)
                                           @clojure.lang.Compiler/LINE)
                                 :column (or (-> &form meta :column)
                                             @clojure.lang.Compiler/COLUMN)}]
    `(pred* '~t
            '~(ns-name *ns*)
            ~((impl/v 'clojure.core.typed.type-contract/type-syntax->pred) t))))

(comment 
  (check-ns 'clojure.core.typed.test.example)

  ; very slow because of update-composite
  (check-ns 'clojure.core.typed.test.rbt)

  (check-ns 'clojure.core.typed.test.macro)
  (check-ns 'clojure.core.typed.test.conduit)
  (check-ns 'clojure.core.typed.test.person)
  (check-ns 'clojure.core.typed.test.core-logic)
  (check-ns 'clojure.core.typed.test.ckanren)
  )

(ns 
  ^{:doc "This namespace contains typed wrapper macros, type aliases
and functions for type checking Clojure code. check-ns is the interface
for checking namespaces, cf for checking individual forms."}
  clojure.core.typed
  (:refer-clojure :exclude [type defprotocol #_letfn fn loop dotimes let for doseq
                            defn atom ref cast
                            #_def #_filter #_remove])
  (:require [clojure.core :as core]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.load-if-needed :as load]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.profiling :as p]
            ;[clojure.core.typed.parse-ast :as ast]
            [clojure.core.typed.internal :as internal]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.import-macros :as import-m]
            [clojure.core.typed.macros :as macros]
            [clojure.core.typed.contract :as con]
            [clojure.core.typed.load :as typed-load]
            [clojure.java.io :as io])
  (:import (clojure.lang Compiler)))

(import-m/import-macros clojure.core.typed.macros
  [def fn loop let ann-form tc-ignore defprotocol
   when-let-fail defn atom ref])

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

(defn load-if-needed
  "Load and initialize all of core.typed if not already"
  []
  (load/load-if-needed))

(defn reset-caches
  "Reset internal type caches."
  []
  (load-if-needed)
  ((impl/v 'clojure.core.typed.reset-caches/reset-caches))
  nil)

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

(defn install
  "Install the :core.typed :lang. Takes an optional set of features
  to install, defaults to `:all`, which is equivalent to the set of
  all features.

  Features:
    - :load    Installs typed `load` over `clojure.core/load`, which type checks files
               on the presence of a {:lang :core.typed} metadata entry in the `ns` form.
               The metadata must be inserted in the actual `ns` form saved to disk,
               as it is read directly from the file instead of the current Namespace
               metadata.
    - :eval    Installs typed `eval` over `clojure.core/eval`.
               If `(= :core.typed (:lang (meta *ns*)))` is true, the form will be implicitly
               type checked. The syntax save to disk is ignored however.

  eg. (install)            ; installs `load` and `eval`
  eg. (install :all)       ; installs `load` and `eval`
  eg. (install #{:eval})   ; installs `eval`
  eg. (install #{:load})   ; installs `load`"
  ([] (install :all))
  ([features]
   (typed-load/install features)))

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

;FIXME should be a special do-op
(defmacro inst 
  "Instantiate a polymorphic type with a number of types.
  
  eg. (inst foo-fn t1 t2 t3 ...)"
  [inst-of & types]
  `(inst-poly ~inst-of '~types))

;FIXME should be a special do-op
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

(defmacro ^{:deprecated "0.2.45"} dotimes>
  "DEPRECATED: Use clojure.core.typed/dotimes

  Like dotimes.
  
  eg. (dotimes> [_ 100]
        (println \"like normal\"))"
  [bindings & body]
  (err/deprecated-renamed-macro
    &form
    'dotimes>
    'dotimes)
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

      (dotimes [x :- Num, 100.123]
        (println \"like normal\" x))"
  [bindings & body]
  (@#'core/assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [[i t? t n] (if (= :- (second bindings))
                     (let [[i _ t n] bindings]
                       (assert (== (count bindings) 4) "Bad arguments to dotimes")
                       [i true t n])
                     (let [[i n] bindings]
                       (assert (== (count bindings) 2) "Bad arguments to dotimes")
                       [i nil nil n]))]
    `(let [n# (long ~n)]
       (loop [~i :- ~(if t? t `Int) 0]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))

(defmacro ^{:deprecated "0.2.45"} for>
  "DEPRECATED: use clojure.core.typed/for

  Like for but requires annotation for each loop variable: [a [1 2]] becomes [a :- Long [1 2]]
  Also requires annotation for return type.
  
  eg. (for> :- Number
        [a :- (U nil AnyInteger) [1 nil 2 3]
         :when a]
        (inc a))"
  [tk ret-ann seq-exprs body-expr]
  (err/deprecated-macro-syntax
    &form
    (str "clojure.core.typed/for> renamed to clojure.core.typed/for."
         " Note the return type annotation has changed to after the binder: (for [a :- t, i] :- r, i)"))
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
                                                          (err/deprecated-macro-syntax 
                                                            &form
                                                            "for> syntax has changed, use [b :- t i] for clauses")
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
                               (loop> [~gxs :- (Option (Seqable ~bind-ann)) ~gxs]
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
                                 (loop> [~gxs :- (Option (Seqable ~bind-ann)) ~gxs]
                                        (when-let [~gxs (seq ~gxs)]
                                          (if (chunked-seq? ~gxs)
                                            (core/let [c# (chunk-first ~gxs)
                                                       size# (int (count c#))
                                                       ~gb (ann-form (chunk-buffer size#)
                                                                     (~'clojure.lang.ChunkBuffer ~ret-ann))]
                                              (if (loop> [~gi :- AnyInteger, (int 0)]
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
  
  eg. (for [a :- (U nil Int) [1 nil 2 3]
            :when a]
        :- Number
        (inc a))
  
  Metadata using the :clojure.core.typed/ann keyword
  can also be used for annotation.

  eg. (for ^{::ann Number}
        [^{::ann (U nil Int)} a [1 nil 2 3]
         :when a]
        (inc a))
  "
  [seq-exprs & maybe-ann-body-expr]
  (@#'core/assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [orig-seq-exprs seq-exprs
        has-explicit-return-type? (#{:-} (first maybe-ann-body-expr))
        [ret-ann body-expr] (if has-explicit-return-type?
                              (let [_ (assert (#{3} (count maybe-ann-body-expr))
                                              (str "Wrong arguments to for: " maybe-ann-body-expr))
                                    [colon t body] maybe-ann-body-expr]
                                [t body])
                              (let [_ (assert (#{1} (count maybe-ann-body-expr))
                                              (str "Wrong arguments to for: " maybe-ann-body-expr))
                                    [body] maybe-ann-body-expr]
                                [`Any body]))
        ret-ann (if-let [[_ meta-ann] (find (meta seq-exprs) ::ann)]
                  (do (assert (not has-explicit-return-type?)
                              "Cannot mix explicit and metadata return type in for.")
                      meta-ann)
                  ret-ann)
        ;_ (prn "ret-ann" ret-ann)
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
              :else (let [[meta-ann has-meta-ann?]
                          (when-let [[_ meta-ann] (find (meta (first seq-exprs)) ::ann)]
                            [meta-ann true])]
                      (if (#{:-} (second seq-exprs))
                        (let [_ (assert (#{4} (count (take 4 seq-exprs)))
                                        (str "for parameter missing after ':-'"))
                              [b colon t init & rst] seq-exprs]
                          (assert (not meta-ann)
                                  "Cannot mix metadata annotation and explicit annotation in for.")
                          (recur (conj flat-result [b colon t] init)
                                 rst))
                        (let [_ (assert (#{2} (count (take 2 seq-exprs)))
                                        (str "for binding needs initial values"))
                              [b init & rst] seq-exprs
                              ann (if has-meta-ann? meta-ann `Any)]
                          (recur (conj flat-result [b :- ann] init)
                                 rst)))))))

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
                                     :else `(cons (ann-form ~body-expr ~ret-ann) ;; ann-form for better error messages
                                                  (~giter (rest ~gxs)))))]
                      (if next-groups
                        #_"not the inner-most loop"
                        `(fn ~giter 
                           [~gxs :- (Option (Seqable ~bind-ann))]
                           :- (Seq ~ret-ann)
                           (lazy-seq
                             (map (fn [t# :- ~ret-ann] :- ~ret-ann
                                    (let [^{::auto-ann ~(meta orig-seq-exprs)
                                            ::track-kind ::for-return}
                                          t# t#]
                                      ;(prn "tracked t#" t#)
                                      t#))
                                  (loop [~gxs :- (Option (Seqable ~bind-ann)) ~gxs]
                                    (when-let [xs# (seq ~gxs)]
                                      (let [^{::auto-ann ~(meta bind)
                                              ::track-kind ::for-param}
                                            x# (first xs#)
                                            ;_# (prn "for param x#" x#)
                                            ~bind x#]
                                        ~(do-mod mod-pairs)))))))
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
                               (map (fn [t# :- ~ret-ann] :- ~ret-ann
                                      (let [^{::auto-ann ~(meta orig-seq-exprs)
                                              ::track-kind ::for-return}
                                            t# t#]
                                        t#))
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
                                                          ^{::auto-ann ~(meta bind)
                                                            ::track-kind ::for-param}
                                                          x# (nth c# ~gi)
                                                          ~bind x#]
                                                      ~(do-cmod mod-pairs))
                                                    true))
                                              (chunk-cons
                                                (chunk ~gb)
                                                (~giter (chunk-rest ~gxs)))
                                              (chunk-cons (chunk ~gb) nil)))
                                          (let [^{::auto-ann ~(meta bind)
                                                  ::track-kind ::for-param}
                                                x# (first ~gxs)
                                                ;_# (prn "for param x#" x#)
                                                ~bind x#]
                                            ~(do-mod mod-pairs))))))))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
        (iter# ~(second seq-exprs)))))

(defmacro ^{:deprecated "0.2.45"} doseq>
  "DEPRECATED: use clojure.core.typed/doseq

  Like doseq but requires annotation for each loop variable: 
  [a [1 2]] becomes [a :- Long [1 2]]
  
  eg.
  (doseq> [a :- (U nil AnyInteger) [1 nil 2 3]
           :when a]
     (inc a))"
  [seq-exprs & body]
  (err/deprecated-renamed-macro
    &form
    'doseq>
    'doseq)
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
                                                          (err/deprecated-macro-syntax
                                                            &form
                                                            "doseq> binder syntax [[b :- t] i] has changed, use [b :- t i]")
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
                        `(loop> [~seq- :- (U nil (Seq ~k-ann)) (seq ~v), 
                                 ~chunk- :- (U nil (clojure.lang.IChunk ~k-ann)) nil
                                 ~count- :- (U Integer Long) 0,
                                 ~i- :- (U Integer Long) 0]
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
                        (recur (conj flat-result [b :- `Any] init)
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
                        `(loop [~seq- :- (U nil (Seq ~k-ann)) (seq ~v), 
                                ~chunk- :- (U nil (clojure.lang.IChunk ~k-ann)) nil
                                ~count- :- Int 0,
                                ~i- :- Int 0]
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


(defmacro 
  ^{:forms '[(fn> name? :- type? [param :- type* & param :- type * ?] exprs*)
             (fn> name? (:- type? [param :- type* & param :- type * ?] exprs*)+)]}
  ^{:deprecated "0.2.45"}
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
  (err/deprecated-macro-syntax
    &form
    (str "clojure.core.typed/fn> renamed to clojure.core.typed/fn. "
         "Note return type annotation now goes after the binder: (fn [a :- t] :- r, b)"))
  (let [{:keys [fn parsed-methods]} (internal/parse-fn> false forms)]
    `(fn>-ann ~fn '~parsed-methods)))


(defn- defn>-parse-typesig 
  "Helper for parsing type signatures out of defn> forms"
  [forms]
  (if (= :- (first forms))
    (let [ret (second forms)
          args (take-nth 3 (drop 2 (first (drop 2 forms))))]
      `[~@args ~'-> ~ret])
    `(IFn ~@(map defn>-parse-typesig forms))))

(defmacro
  ^{:deprecated "0.2.57"}
  ^{:forms '[(defn> name docstring? :- type [param :- type *] exprs*)
             (defn> name docstring? (:- type [param :- type *] exprs*)+)]}
  defn>
  "DEPRECATED: Use defn
  
  Like defn, but with annotations. Annotations are mandatory for
  parameters and for return type.

  eg. (defn> fname :- Integer [a :- Number, b :- (U Symbol nil)] ...)

  ;annotate return
  (defn> fname :- String [a :- String] ...)

  ;multi-arity
  (defn> fname 
    (:- String [a :- String] ...)
    (:- Long   [a :- String, b :- Number] ...))"
  [name & fdecl]
  (err/deprecated-renamed-macro
    &form
    'defn>
    'defn)
  (let [[docstring fdecl] (internal/take-when string? fdecl)
        signature (defn>-parse-typesig fdecl)]
    `(do (ann ~name ~signature)
         ~(list* 'def name 
                 (concat
                   (when docstring [docstring])
                   [`(fn> ~name ~@fdecl)])))))


(defmacro
  ^{:forms '[(def> name docstring? :- type expr)]}
  ^{:deprecated "0.2.45"}
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
  (err/deprecated-macro-syntax
    &form
    (str "clojure.core.typed/def> renamed to clojure.core.typed/def."
         " Note that it is impossible to :refer to a var called def."))
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

(defmacro ^{:deprecated "0.2.45"} defprotocol>
  "DEPRECATED: use clojure.core.typed/defprotocol

  Like defprotocol, but required for type checking
  its macroexpansion.
  
  eg. (defprotocol> MyProtocol
        (a [this]))"
  [& body]
  (err/deprecated-macro-syntax
    &form
    (str "clojure.core.typed/defprotocol> renamed to clojure.core.typed/defprotocol."
         " Note the new syntax cannot be used with ann-protocol."))
  `(tc-ignore
     (core/defprotocol ~@body)))

;TODO filter/object support

(defmacro 
  ^{:forms '[(loop> [binding :- type, init*] exprs*)]}
  ^{:deprecated "0.2.45"}
  loop>
  "DEPRECATED: use clojure.core.typed/loop
  
  Like loop, except loop variables require annotation.

  Suggested idiom: use a comma between the type and the initial
  expression.

  eg. (loop> [a :- Number, 1
              b :- (U nil Number), nil]
        ...)"
  [bndings* & forms]
  (err/deprecated-renamed-macro
    &form
    'loop>
    'loop)
  (let [normalise-args
        (fn [seq-exprs]
          (loop [flat-result ()
                      seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result
              (and (vector? (first seq-exprs))
                   (#{:-} (-> seq-exprs first second))) (do
                                                          (err/deprecated-macro-syntax
                                                            &form
                                                            (str "loop> syntax has changed, use [b :- t i] for clauses"
                                                                 "ns: " *ns* " form:" &form))
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
  [syms nsym]
  (impl/with-clojure-impl
    (doseq [sym syms]
      (assert (not (or (some #(= \. %) (str sym))
                       (namespace sym)))
              (str "Cannot declare qualified datatype: " sym))
      (let [qsym (symbol (str (munge (name nsym)) \. (name sym)))]
        (impl/declare-datatype* qsym))))
  nil)

(defmacro declare-datatypes 
  "Declare datatypes, similar to declare but on the type level."
  [& syms]
  `(tc-ignore (declare-datatypes* '~syms '~(ns-name *ns*))))

(defn ^:skip-wiki
  declare-protocols* 
  "Internal use only. Use declare-protocols."
  [syms]
  nil)

(defmacro declare-protocols 
  "Declare protocols, similar to declare but on the type level."
  [& syms]
  `(tc-ignore (declare-protocols* '~syms)))

(defn ^:skip-wiki
  declare-alias-kind* 
  "Internal use only. Use declare-alias-kind."
  [sym ty]
  nil)

(defmacro declare-alias-kind
  "Declare a kind for an alias, similar to declare but on the kind level."
  [sym ty]
  `(tc-ignore
     (declare ~sym)
     (declare-alias-kind* '~sym '~ty)))

(defn ^:skip-wiki
  declare-names* 
  "Internal use only. Use declare-names."
  [syms]
  (let [nsym (ns-name *ns*)]
    (doseq [sym syms]
      (impl/declare-name* (symbol (str nsym) (str sym)))))
  nil)

(defmacro declare-names 
  "Declare names, similar to declare but on the type level."
  [& syms]
  (assert (every? (every-pred symbol? (complement namespace)) syms)
          "declare-names only accepts unqualified symbols")
  `(tc-ignore (declare-names* '~syms)))

(declare add-to-rt-alias-env add-tc-type-name)

(defn ^:skip-wiki
  def-alias* 
  "Internal use only. Use def-alias."
  [qsym t form]
  (add-to-rt-alias-env form qsym t)
  (add-tc-type-name form qsym t)
  nil)

(defmacro ^:skip-wiki with-current-location
  [form & body]
  `(let [form# ~form]
     (binding [vs/*current-env* {:ns {:name (ns-name *ns*)}
                                 :file *file*
                                 :line (or (-> form# meta :line)
                                           @Compiler/LINE)
                                 :column (or (-> form# meta :column)
                                             @Compiler/COLUMN)}]
       ~@body)))

(defmacro ^:private delay-rt-parse
  "We can type check c.c.t/parse-ast if we replace all instances
  of parse-ast in clojure.core.typed with delay-parse. Otherwise
  there is a circular dependency."
  [t]
  `(let [t# ~t
         app-outer-context# (bound-fn [f# t#] (f# t#))]
     (delay
       (require '~'clojure.core.typed.parse-ast)
       (let [parse-clj# (impl/v '~'clojure.core.typed.parse-ast/parse-clj)]
         (app-outer-context# parse-clj# t#)))))

(defmacro ^:private delay-tc-parse
  [t]
  `(let [t# ~t
         app-outer-context# (bound-fn [f#] (f#))]
     (delay
       (require '~'clojure.core.typed.parse-unparse)
       (let [parse-clj# (impl/v '~'clojure.core.typed.parse-unparse/parse-clj)
             with-parse-ns*# (impl/v '~'clojure.core.typed.parse-unparse/with-parse-ns*)]
         (app-outer-context#
           (fn []
             (with-parse-ns*#
               (ns-name *ns*)
               #(parse-clj# t#))))))))

(defn ^:skip-wiki add-to-rt-alias-env [form qsym t]
  (impl/with-impl impl/clojure
    (impl/add-alias-env
      qsym
      (with-current-location form
        (delay-rt-parse t))))
  nil)

(defn ^:skip-wiki add-tc-type-name [form qsym t]
  (impl/with-impl impl/clojure
    (let [;; preserve *ns*
          bfn (bound-fn [f] (f))
          t (delay
              (let [t (bfn
                        #(with-current-location form
                           @(delay-tc-parse t)))
                    _ (require 'clojure.core.typed.subtype
                               'clojure.core.typed.declared-kind-env)
                    declared-kind-or-nil (impl/v 'clojure.core.typed.declared-kind-env/declared-kind-or-nil)
                    unparse-type (impl/v 'clojure.core.typed.parse-unparse/unparse-type)
                    subtype? (impl/v 'clojure.core.typed.subtype/subtype?)
                    _ (impl/with-impl impl/clojure
                        (when-let [tfn (declared-kind-or-nil qsym)]
                          (when-not (subtype? t tfn)
                            (err/int-error (str "Declared kind " (unparse-type tfn)
                                                " does not match actual kind " (unparse-type t))))))
                    ]
                t))]
      (impl/add-tc-type-name qsym t)))
  nil)

(defmacro
  ^{:deprecated "0.2.58"}
  atom>
  "DEPRECATED: use clojure.core.typed/atom
  
  Like atom, but creates an Atom1 of type t.
  
  Same as (atom (ann-form init t) args*)
  
  eg. (atom> Number 1)
      (atom> (Vec Any) [])"
  [t init & args]
  (err/deprecated-renamed-macro
    &form
    'atom>
    'atom)
  `(core/atom (ann-form ~init ~t) ~@args))

(defmacro
  ^{:deprecated "0.2.58"}
  ref>
  "DEPRECATED: use clojure.core.typed/ref

  Like ref, but creates a Ref1 of type t.
  
  Same as (ref (ann-form init t) args*)
  
  eg. (ref> Number 1)
      (ref> (Vec Any) [])"
  [t init & args]
  (err/deprecated-renamed-macro
    &form
    'ref>
    'ref)
  `(core/ref (ann-form ~init ~t) ~@args))

(defmacro 
  ^{:deprecated "0.2.45"}
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
   (err/deprecated-renamed-macro
     &form
     'def-alias
     'defalias)
   `(defalias ~sym ~t)))

(defmacro defalias 
  "Define a recursive type alias. Takes an optional doc-string as a second
  argument.

  Updates the corresponding var with documentation.
  
  eg. (defalias MyAlias
        \"Here is my alias\"
        (U nil String))
  
      ;; recursive alias
      (defalias Expr
        (U '{:op ':if :test Expr :then Expr :else Expr}
           '{:op ':const :val Any}))"
  ([sym doc-str t]
   (assert (string? doc-str) "Doc-string passed to defalias must be a string")
   `(defalias ~(vary-meta sym assoc :doc doc-str) ~t))
  ([sym t]
   (assert (symbol? sym) (str "First argument to defalias must be a symbol: " sym))
   (assert (not (namespace sym)) (str "First argument to defalias unqualified: " sym))
   (let [m (vary-meta sym
                      update-in [:doc] #(str #_"Type Alias\n\n" % "\n\n" (with-out-str (pprint/pprint t))))
         qsym (-> (symbol (-> *ns* ns-name str) (str sym))
                  (with-meta (meta m)))]
     `(tc-ignore
        (declare ~sym)
        (def-alias* '~qsym '~t '~&form)))))

(def ^{:doc "Any is the top type that contains all types."
       :forms '[Any]
       ::special-type true}
  Any)

(def ^{:doc "AnyValue contains all Value singleton types"
       :forms '[AnyValue]
       ::special-type true}
  AnyValue)

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
       :forms '[(Val Constant)
                'Constant]
       ::special-type true}
  Val)

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

(def ^{:doc "HSet is a type for heterogeneous sets.
            Takes a set of simple values. By default
            :complete? is true.
            
            eg. (HSet #{:a :b :c} :complete? true)"
       :forms '[(HSet #{fixed*} :complete? Boolean)]
       ::special-type true}
  HSet)

(def ^{:doc "An ordered intersection type of function arities."
       :forms '[(IFn ArityVec+)
                [fixed* -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
                [fixed* rest * -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
                [fixed* drest ... bound -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]]
       ::special-type true}
  IFn)

(def ^{:doc "A predicate for the given type.
            
            eg. Type for integer?: (Pred Int)"
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
  (core/letfn [(defalias-many [vinit]
                `(do
                   ~@(for [[k v] (partition 2 vinit)]
                       `(defalias ~k ~v))))]
    (defalias-many 
      impl/init-aliases)))

; defines base aliases
(init-aliases)

(defn ^:private rclass-pred [rcls opts]
  (impl/with-clojure-impl
    (impl/add-rclass-env (impl/Class->symbol rcls) opts)))

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
  (impl/with-clojure-impl
    (impl/add-nonnilable-method-return msym arities))
  nil)

(defmacro non-nil-return 
  "Override the return type of fully qualified method msym to be non-nil.
  Takes a set of relevant arities,
  represented by the number of parameters it takes (rest parameter counts as one),
  or :all which overrides all arities.
  
  eg. ; must use full class name
      (non-nil-return java.lang.Class/getDeclaredMethod :all)"
  [msym arities]
  `(tc-ignore (non-nil-return* '~msym '~arities)))

(defn ^:skip-wiki
  nilable-param* 
  "Internal use only. Use nilable-param."
  [msym mmap]
  (impl/with-clojure-impl
    (impl/add-method-nilable-param msym mmap))
  nil)

(defmacro nilable-param 
  "Override which parameters in qualified method msym may accept
  nilable values. If the parameter is a parameterised type or
  an Array, this also declares the parameterised types and the Array type as nilable.

  mmap is a map mapping arity parameter number to a set of parameter
  positions (integers). If the map contains the key :all then this overrides
  other entries. The key can also be :all, which declares all parameters nilable."
  [msym mmap]
  `(tc-ignore (nilable-param* '~msym '~mmap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(defn print-env 
  "During type checking, print the type environment to *out*,
  preceeded by literal string debug-str."
  [debug-str]
  nil)

(defn ^:skip-wiki
  untyped-var* 
  "Internal use only. Use untyped-var."
  [varsym typesyn prs-ns form]
  (let [_ (require 'clojure.core.typed.coerce-utils)
        var->symbol (impl/v 'clojure.core.typed.coerce-utils/var->symbol)
        var (resolve varsym)
        _ (assert (var? var) (str varsym " must resolve to a var."))
        qsym (var->symbol var)
        expected-type (with-current-location form
                        (delay-tc-parse typesyn))
        _ (impl/with-clojure-impl
            (impl/add-untyped-var prs-ns qsym expected-type))]
    )
  nil)

(defmacro untyped-var
  "Check a given var has the specified type at runtime."
  [varsym typesyn]
  (let [prs-ns (-> *ns* ns-name)
        qsym (if (namespace varsym)
               varsym
               (symbol (str prs-ns) (str varsym)))]
    `(tc-ignore (untyped-var* '~qsym '~typesyn '~prs-ns '~&form))))

(defn ^:skip-wiki
  ann* 
  "Internal use only. Use ann."
  [qsym typesyn check? form]
  (let [_ (impl/with-impl impl/clojure
            (when (and (contains? (impl/var-env) qsym)
                       (not (impl/check-var? qsym))
                       check?)
              (err/warn (str "Removing :no-check from var " qsym))
              (impl/remove-nocheck-var qsym)))
        _ (impl/with-impl impl/clojure
            (when-not check?
              (impl/add-nocheck-var qsym)))
        ast (with-current-location form
              (delay-rt-parse typesyn))
        tc-type (with-current-location form
                  (delay-tc-parse typesyn))]
    (impl/with-impl impl/clojure
      (impl/add-var-env qsym ast))
    (impl/with-impl impl/clojure
      (impl/add-tc-var-type qsym tc-type)))
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
            (err/deprecated-macro-syntax
              &form 
              (str ":nocheck metadata for ann is renamed :no-check")))
        opts (meta varsym)
        _ (assert (not (and (contains? opts :nocheck)
                            (contains? opts :no-check)))
                  "Cannot provide both :nocheck and :no-check metadata to ann")
        check? (not (or (:no-check opts)
                        (:nocheck opts)))]
    `(tc-ignore (ann* '~qsym '~typesyn '~check? '~&form))))

(defmacro ann-many
  "Annotate several vars with type t.

  eg. (ann-many FakeSearch
                web1 web2 image1 image2 video1 video2)"
  [t & vs]
  `(do ~@(map #(list `ann % t) vs)))

(defn ^:skip-wiki
  ann-datatype*
  "Internal use only. Use ann-datatype."
  [vbnd dname fields opts form]
    (let [qname (if (some #{\.} (str dname))
                  dname
                  (symbol (str (namespace-munge *ns*) "." dname)))]
      (impl/with-clojure-impl
        (impl/add-datatype-env 
          qname
          {:record? false
           :name qname
           :fields fields
           :bnd vbnd})))
    (with-current-location form
      (impl/with-clojure-impl
        (impl/gen-datatype* vs/*current-env* (ns-name *ns*) dname fields vbnd opts false)))
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
    `(tc-ignore (ann-datatype* '~vbnd '~dname '~fields '~opts '~&form))))

(defn ^:skip-wiki
  ann-record* 
  "Internal use only. Use ann-record"
  [vbnd dname fields opt form]
    (let [qname (if (some #{\.} (str dname))
                  dname
                  (symbol (str (namespace-munge *ns*) "." dname)))]
      (impl/with-clojure-impl
        (impl/add-datatype-env 
          qname
          {:record? true
           :name qname
           :fields fields
           :bnd vbnd})))
    (with-current-location form
      (impl/with-clojure-impl
        (impl/gen-datatype* vs/*current-env* (ns-name *ns*) dname fields vbnd opt true)))
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
    `(tc-ignore (ann-record* '~vbnd '~dname '~fields '~opt '~&form))))


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
  `(tc-ignore (ann-precord* '~dname '~vbnd '~fields '~opt)))

(defn ^:skip-wiki
  ann-protocol* 
  "Internal use only. Use ann-protocol."
  [vbnd varsym mth form]
  (let [qualsym (if (namespace varsym)
                  varsym
                  (symbol (str (ns-name *ns*)) (name varsym)))]
    (impl/with-clojure-impl
      (impl/add-protocol-env
        qualsym
        {:name qualsym
         :methods mth
         :bnds vbnd}))
    (impl/with-clojure-impl
      (with-current-location form
        (impl/gen-protocol*
          vs/*current-env*
          (ns-name *ns*)
          varsym
          vbnd
          mth))))
  nil)

(defmacro 
  ^{:forms '[(ann-protocol vbnd varsym & methods)
             (ann-protocol varsym & methods)]}
  ann-protocol 
  "Annotate a possibly polymorphic protocol var with method types.
  
  eg. (ann-protocol IFoo
        bar
        (IFn [IFoo -> Any]
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
        (IFn [(IFooPoly x) -> Any]
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
        {:as mth} mth]
    `(tc-ignore (ann-protocol* '~vbnd '~varsym '~mth '~&form))))

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
    `(tc-ignore (ann-interface* '~vbnd '~clsym '~mth))))

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
  `(tc-ignore (ann-pprotocol* '~varsym '~vbnd '~mth)))

(defn ^:skip-wiki
  override-constructor* 
  "Internal use only. Use override-constructor."
  [ctorsym typesyn form]
  (impl/with-clojure-impl
    (impl/add-constructor-override 
      ctorsym
      (with-current-location form
        (delay-tc-parse typesyn))))
  nil)

(defmacro override-constructor 
  "Override all constructors for Class ctorsym with type."
  [ctorsym typesyn]
  `(tc-ignore (override-constructor* '~ctorsym '~typesyn '~&form)))

(defn ^:skip-wiki
  override-method* 
  "Internal use only. Use override-method."
  [methodsym typesyn form]
  (impl/with-clojure-impl
    (impl/add-method-override 
      methodsym
      (with-current-location form
        (delay-tc-parse typesyn))))
  nil)

(defmacro override-method 
  "Override type for qualified method methodsym.

  methodsym identifies the method to override and should be a
  namespace-qualified symbol in the form <class>/<method-name>.
  The class name needs to be fully qualified.

  typesyn uses the same annotation syntax as functions.

  Use non-nil-return instead of override-method if you want to
  declare that a method can never return nil.

  Example:

    (override-method java.util.Properties/stringPropertyNames
                     [-> (java.util.Set String)])

  This overrides the return type of method stringPropertyNames
  of class java.util.Properties to be (java.util.Set String)."
  [methodsym typesyn]
  (assert ((every-pred symbol? namespace) methodsym) "Method symbol must be a qualified symbol")
  `(tc-ignore (override-method* '~methodsym '~typesyn '~&form)))

(defn ^:skip-wiki
  typed-deps* 
  "Internal use only. Use typed-deps."
  [args form]
  (with-current-location form
    (impl/with-clojure-impl
      (let [_ (require 'clojure.core.typed.coerce-utils)
            ns->URL (impl/v 'clojure.core.typed.coerce-utils/ns->URL)]
        (doseq [dep args]
          (when-not (ns->URL dep)
            (err/int-error (str "Cannot find dependency declared with typed-deps: " dep))))))
    (impl/with-clojure-impl
      (impl/add-ns-deps (ns-name *ns*) (set args))))
  nil)

(defmacro typed-deps 
  "Declare namespaces which should be checked before the current namespace.
  Accepts any number of symbols. Only has effect via check-ns.
  
  eg. (typed-deps clojure.core.typed.holes
                  myns.types)"
  [& args]
  `(tc-ignore (typed-deps* '~args '~&form)))

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
  [nsym]
  (impl/with-clojure-impl
    (impl/register-warn-on-unannotated-vars nsym))
  nil)

(defmacro warn-on-unannotated-vars
  "Allow unannotated vars in the current namespace. 
  
  Emits a warning instead of a type error when checking
  a def without a corresponding expected type.

  Disables automatic inference of `def` expressions.
  
  eg. (warn-on-unannotated-vars)"
  []
  `(tc-ignore (warn-on-unannotated-vars* '~(ns-name *ns*))))

(defn check-form-info 
  "Type checks a (quoted) form and returns a map of results from type checking the
  form.
  
  Options
  - :expected        Type syntax representing the expected type for this form
                     type-provided? option must be true to utilise the type.
  - :type-provided?  If true, use the expected type to check the form.
  - :profile         Use Timbre to profile the type checker. Timbre must be
                     added as a dependency. Must use the \"slim\" JAR.
  - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
                     of (Map '{:line Int :column Int :file Str} Str).
  - :checked-ast     Returns the entire AST for the given form as the :checked-ast entry,
                     annotated with the static types inferred after checking.
                     If a fatal error occurs, mapped to nil.
  - :no-eval         If true, don't evaluate :out-form. Removes :result return value.
                     It is highly recommended to evaluate :out-form manually.
  
  Default return map
  - :ret             TCResult inferred for the current form
  - :out-form        The macroexpanded result of type-checking, if successful. 
  - :result          The evaluated result of :out-form, unless :no-eval is provided.
  - :ex              If an exception was thrown during evaluation, this key will be present
                     with the exception as the value.
  DEPRECATED
  - :delayed-errors  A sequence of delayed errors (ex-info instances)"
  [form & opt]
  (load-if-needed)
  (apply (impl/v 'clojure.core.typed.check-form-clj/check-form-info) form opt))

(defn check-form*
  "Takes a (quoted) form and optional expected type syntax and
  type checks the form. If expected is provided, type-provided?
  must be true."
  ([form] (check-form* form nil nil))
  ([form expected] (check-form* form expected true))
  ([form expected type-provided?]
   (load-if-needed)
   ((impl/v 'clojure.core.typed.check-form-clj/check-form*) form expected type-provided?)))

; cf can pollute current type environment to allow REPL experimentation
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

(defn check-ns-info
  "Same as check-ns, but returns a map of results from type checking the
  namespace.

  Options
  - :collect-only    Don't type check the given namespace/s, but collect the 
                     top level type annotations like ann, ann-record.
  - :type-provided?  If true, use the expected type to check the form
  - :profile         Use Timbre to profile the type checker. Timbre must be
                     added as a dependency. Must use the \"slim\" JAR.
  - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
                     of (Map '{:line Int :column Int :file Str} Str).

  Default return map
  - :delayed-errors  A sequence of delayed errors (ex-info instances)"
  ([] (check-ns-info *ns*))
  ([ns-or-syms & opt]
   (load-if-needed)
   (apply (impl/v 'clojure.core.typed.check-ns-clj/check-ns-info) ns-or-syms opt)))

(defn check-ns
  "Type check a namespace/s (a symbol or Namespace, or collection).
  If not provided default to current namespace.
  Returns a true value if type checking is successful, otherwise
  throws an Exception.

  Do not use check-ns within a checked namespace.
  It is intended to be used at the REPL or within a unit test.
  Suggested idiom for clojure.test: (is (check-ns 'your.ns))
  
  Keyword arguments:
  - :collect-only  if true, collect type annotations but don't type check code.
                   Useful for debugging purposes.
  - :trace         if true, print some basic tracing of the type checker
  - :profile       Use Timbre to profile the type checker. Timbre must be
                   added as a dependency. Must use the \"slim\" JAR.

  If providing keyword arguments, the namespace to check must be provided
  as the first argument.

  Bind clojure.core.typed.util-vars/*verbose-types* to true to print fully qualified types.
  Bind clojure.core.typed.util-vars/*verbose-forms* to print full forms in error messages.
  
  eg. (check-ns 'myns.typed)
      ;=> :ok
     
      ; implicitly check current namespace
      (check-ns)
      ;=> :ok
  
      ; collect but don't check the current namespace
      (check-ns *ns* :collect-only true)"
  ([] (check-ns *ns*))
  ([ns-or-syms & opt]
   (load-if-needed)
   (apply (impl/v 'clojure.core.typed.check-ns-clj/check-ns) ns-or-syms opt)))


;(ann statistics [(Coll Symbol) -> (Map Symbol Stats)])
(defn statistics 
  "Takes a collection of namespace symbols and returns a map mapping the namespace
  symbols to a map of data"
  [nsyms]
  (load-if-needed)
  ((impl/v 'clojure.core.typed.statistics/statistics) nsyms))

; (ann var-coverage [(Coll Symbol) -> nil])
(defn var-coverage 
  "Summarises annotated var coverage statistics to *out*
  for namespaces nsyms, a collection of symbols or a symbol/namespace.
  Defaults to the current namespace if no argument provided."
  ([] (var-coverage *ns*))
  ([nsyms-or-nsym]
   (load-if-needed)
   ((impl/v 'clojure.core.typed.statistics/var-coverage) nsyms-or-nsym)))

(defn envs
  "Returns a map of type environments, according to the current state of the
  type checker.
  
  Output map:
  - :vars      map from var symbols to their verbosely printed types
  - :aliases   map from alias var symbols (made with defalias) to their verbosely printed types
  - :special-types  a set of Vars that are special to the type checker (like Any, U, I)
  "
  []
  (load-if-needed)
  (merge ((impl/v 'clojure.core.typed.all-envs/all-envs-clj))
         {:special-types (set (->> (ns-publics 'clojure.core.typed)
                                vals
                                (filter (fn [v]
                                          (when (var? v)
                                            (-> v meta ::special-type))))))}))

(defn prepare-infer-ns
  "Instruments the current namespace to prepare for runtime type
  or spec inference.

  Optional keys:
    :ns     The namespace to infer types for. (Symbol/Namespace)
            Default: *ns*
    :strategy  Choose which inference preparation strategy to use.
               - :compile      recompile the namespace and wrap at compilation-time.
                               Supports local annotation inference. Source is analyzed
                               via core.typed's custom analyzer.
               - :instrument   wrap top-level vars without recompilation.
                               No support for local annotations, but the default
                               Clojure analyzer is used.
               Default: :compile
    :track-strategy  Choose which track strategy to use.
                     - :lazy    wrap hash maps and possibly other data structures, and
                                lazily track values as they are used.
                     - :eager   eagerly walk over all values, a la clojure.spec checking.
                     Default: :lazy
  "
  [& {:keys [ns strategy] :as config
      :or {strategy :compile
           ns *ns*}}]
  (load-if-needed)
  (case strategy
    :compile
    (impl/with-impl impl/clojure
      (binding [vs/*prepare-infer-ns* true
                vs/*instrument-infer-config* (dissoc config :ns)]
        (typed-load/load-typed-file 
          (subs (@#'clojure.core/root-resource (if (symbol? ns) ns (ns-name ns))) 1))))
    :instrument
    (throw (Exception. ":instrument not yet implemented")))
  :ok)

(defn refresh-runtime-infer 
  "Clean the current state of runtime inference.
  Will forget the results of any tests on instrumented code."
  []
  (load-if-needed)
  (require '[clojure.core.typed.runtime-infer])
  ((impl/v 'clojure.core.typed.runtime-infer/refresh-runtime-infer)))

(defn runtime-infer 
  "Infer and insert annotations for a given namespace.

  There are two ways to instrument your namespace.

  Call `prepare-infer-ns` function on the namespace
  of your choosing.

  Alternatively, use the :runtime-infer
  feature in your namespace metadata. Note: core.typed
  must be installed via `clojure.core.typed/install`.

  eg. (ns my-ns
        {:lang :core.typed
         :core.typed {:features #{:runtime-infer}}}
        (:require [clojure.core.typed :as t]))

  After your namespace is instrumented, run your tests
  and/or exercise the functions in your namespace.

  Then call `runtime-infer` to populate the namespace's
  corresponding file with these generated annotations.

  Optional keys:
    :ns     The namespace to infer types for. (Symbol/Namespace)
            Default: *ns*
    :fuel   Number of iterations to perform in inference algorithm
            (integer)
            Default: nil (don't restrict iterations)
    :debug  Perform print debugging. (:all/:iterations/nil)
            Default: nil
    :track-depth   Maximum nesting depth data will be tracked.
                   Default: nil (don't restrict nestings)
    :track-count   Maximum number of elements of a single collection
                   will be tracked.
                   Default: nil (don't restrict elements)
    :root-results  Maximum number of inference results collected per top-level
                   root form, from the perspective of the tracker (eg. vars, local functions).
                   Default: nil (don't restrict)
    :preserve-unknown  If true, output the symbol `?` where inference was cut off
                       or never reached.
                       Default: nil (convert to unknown to `clojure.core.typed/Any`)
    :out-dir       A classpath-relative directory (string) to which to dump changes to files,
                   instead of modifying the original file.
                   Default: nil (modify original file)
    :no-squash-vertically     If true, disable the `squash-vertically` pass.
                              Default: nil

  eg. (runtime-infer) ; infer for *ns*

      (runtime-infer :ns 'my-ns) ; infer for my-ns

      (runtime-infer :fuel 0) ; iterations in type inference algorithm
                              ; (higher = smaller types + more recursive)

      (runtime-infer :debug :iterations) ; enable iteration debugging
  "
  ([& kws]
   (load-if-needed)
   (require '[clojure.core.typed.runtime-infer])
   (let [m (-> (if (= 1 (count kws))
                 (do
                   (err/deprecated-warn
                     "runtime-infer with 1 arg: use {:ns <ns>}")
                   {:ns (first kws)})
                 (apply hash-map kws))
               (update :ns #(or % *ns*)))]
     ((impl/v 'clojure.core.typed.runtime-infer/runtime-infer)
      m))))

(defn spec-infer 
  "Infer and insert specs for a given namespace.

  There are two ways to instrument your namespace.

  Call `prepare-infer-ns` function on the namespace
  of your choosing.

  Alternatively, use the :runtime-infer
  feature in your namespace metadata. Note: core.typed
  must be installed via `clojure.core.typed/install`.

  eg. (ns my-ns
        {:lang :core.typed
         :core.typed {:features #{:runtime-infer}}}
        (:require [clojure.core.typed :as t]))

  After your namespace is instrumented, run your tests
  and/or exercise the functions in your namespace.

  Then call `spec-infer` to populate the namespace's
  corresponding file with these generated specs.

  Optional keys:
    :ns     The namespace to infer specs for. (Symbol/Namespace)
            Default: *ns*
    :fuel   Number of iterations to perform in inference algorithm
            (integer)
            Default: nil (don't restrict iterations)
    :debug  Perform print debugging. (:all/:iterations/nil)
            Default: nil
    :track-depth   Maximum nesting depth data will be tracked.
                   Default: nil (don't restrict nestings)
    :track-count   Maximum number of elements of a single collection
                   will be tracked.
                   Default: nil (don't restrict elements)
    :root-results  Maximum number of inference results collected per top-level
                   root form, from the perspective of the tracker (eg. vars, local functions).
                   Default: nil (don't restrict)
    :preserve-unknown  If true, output the symbol `?` where inference was cut off
                       or never reached.
                       Default: nil (convert to unknown to `clojure.core/any?`)
    :higher-order-fspec   If true, generate higher-order fspecs.
                          Default: false
    :out-dir       A classpath-relative directory (string) to which to dump changes to files,
                   instead of modifying the original file.
                   Default: nil (modify original file)
    :no-squash-vertically     If true, disable the `squash-vertically` pass.
                              Default: nil
    :spec-macros   If true, output specs for macros.
                   Default: nil (elide macro specs)

  eg. (spec-infer) ; infer for *ns*

      (spec-infer :ns 'my-ns) ; infer for my-ns

      (spec-infer :fuel 0) ; iterations in spec inference algorithm
                           ; (higher = smaller specs + more recursive)

      (spec-infer :debug :iterations) ; enable iteration debugging
  "
  ([& kws]
   (load-if-needed)
   (require '[clojure.core.typed.runtime-infer])
   (let [m (-> (if (= 1 (count kws))
                 (do
                   (err/deprecated-warn
                     "runtime-infer with 1 arg: use {:ns <ns>}")
                   {:ns (first kws)})
                 (apply hash-map kws))
               (update :ns #(or % *ns*)))]
     ((impl/v 'clojure.core.typed.runtime-infer/spec-infer)
      m))))

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
  (with-current-location &form
    `(pred* '~t
            '~(ns-name *ns*)
            ~((impl/v 'clojure.core.typed.type-contract/type-syntax->pred) t))))

(defmacro cast
  "Cast a value to a type. Returns a new value that conforms
  to the given type, otherwise throws an error with blame.

  eg. (cast Int 1)
      ;=> 1

      (cast Int nil)
      ; Fail, <blame positive ...>

      ((cast [Int -> Int] identity)
       1)
      ;=> 1

      ((cast [Int -> Int] identity)
       nil)
      ; Fail, <blame negative ...>

      (cast [Int -> Int] nil)
      ; Fail, <blame positive ...>

  (defalias Options
    (HMap :optional {:positive (U Sym Str),
                     :negative (U Sym Str)
                     :file (U Str nil)
                     :line (U Int nil)
                     :column (U Int nil)}))

  (IFn [Contract Any -> Any]
       [Contract Any Options -> Any]

  Options:
  - :positive   positive blame, (U Sym Str)
  - :negative   negative blame, (U Sym Str)
  - :file       file name where contract is checked, (U Str nil)
  - :line       line number where contract is checked, (U Int nil)
  - :column     column number where contract is checked, (U Int nil)"
  ([t x] `(cast ~t ~x {}))
  ([t x opt]
   (require '[clojure.core.typed.type-contract])
   `(do ~spec/special-form
        ::cast
        {:type '~t}
        ;; type checker expects a contract to be in this form, ie. ((fn [x] ..) x)
        ;; - clojure.core.typed.check.add-cast
        ;; - clojure.core.typed.check.special.cast
        ((fn [x#]
           (con/contract (with-current-location '~&form
                           ;; this compiles code so needs to be in same phase
                           (binding [*ns* ~*ns*]
                             ((impl/v '~'clojure.core.typed.type-contract/type-syntax->contract) '~t)))
                         x#
                         (let [opt# ~opt]
                           (con/make-blame
                             :positive (or (:positive opt#)
                                           "cast")
                             :negative (or (:negative opt#)
                                           "cast")
                             :file (or (:file opt#)
                                       ~*file*)
                             :line (or (:line opt#)
                                       ~(or (-> &form meta :line)
                                            @Compiler/LINE))
                             :column (or (:column opt#)
                                         ~(or (-> &form meta :column)
                                              @Compiler/COLUMN))))))
         ~x))))

(defn infer-unannotated-vars
  "EXPERIMENTAL

  Return a vector of potential var annotations in the given
  namespace, or the current namespace.

  To enable for the current namespace, add the :infer-vars
  :experimental feature to the ns metadata like so:

    (ns infer-me
      {:lang :core.typed
       :core.typed {:experimental #{:infer-vars
                                    :infer-locals}}}
      ...)

  Then run check-ns like usual, and infer-unannotated-vars
  will return the inferred vars without annotations.

  (t/infer-untyped-vars)
  => [(t/ann u/bar t/Int)
      (t/ann u/foo (t/U [t/Any -> t/Any] Int))]
                                "

  ([] (infer-unannotated-vars (ns-name *ns*)))
  ([nsym-or-ns]
   (load-if-needed)
   (impl/with-clojure-impl
     ((impl/v 'clojure.core.typed.infer-vars/infer-unannotated-vars) (ns-name nsym-or-ns)))))

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

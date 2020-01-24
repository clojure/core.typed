;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns 
  ^{:doc "This namespace contains typed wrapper macros, type aliases
and functions for type checking Clojure code. check-ns is the interface
for checking namespaces, cf for checking individual forms."}
  clojure.core.typed
  (:refer-clojure :exclude [type defprotocol #_letfn fn loop dotimes let for doseq
                            defn atom ref cast
                            #_filter #_remove])
  (:require [clojure.core :as core]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.import-macros :as import-m]
            [clojure.core.typed.contract :as contract]
            ; for `pred` and `contract`
            ;clojure.core.typed.type-contract
            ; for `import-macros` below
            clojure.core.typed.macros)
  (:import (clojure.lang Compiler)))

(core/defn- dynaload
  [s]
  (core/let [ns (namespace s)]
    (assert ns)
    (require (symbol ns))
    (core/let [v (resolve s)]
      (if v
        @v
        (throw (RuntimeException. (str "Var " s " is not on the classpath")))))))

(def ^:private with-clojure-impl* (delay (dynaload 'clojure.core.typed.current-impl/with-clojure-impl*)))
(defmacro ^:private with-clojure-impl [& body]
  `(@with-clojure-impl* (core/fn [] (do ~@body))))

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

(core/defn register-ann-ns [nsym]
  (when (:register? (swap! vs/registered-ann-ns
                           (core/fn [{:keys [register?] :as m}]
                             (if (not register?)
                               (update m :namespaces conj nsym)
                               m))))
    (require nsym)))

(core/let [lin (delay (dynaload 'clojure.core.typed.load-if-needed/load-if-needed))]
  (core/defn load-if-needed
    "Load and initialize all of core.typed if not already"
    []
    (@lin)))

(core/let [rset (delay (dynaload 'clojure.core.typed.checker.jvm.reset-caches/reset-caches))]
  (core/defn reset-caches
    "Reset internal type caches."
    []
    (load-if-needed)
    (@rset)
    nil))

;(ann method-type [Symbol -> nil])
(core/let [type-reflect (delay (dynaload 'clojure.reflect/type-reflect))
           Method->Type (delay (dynaload 'clojure.core.typed.checker.jvm.check/Method->Type))
           unparse-type (delay (dynaload 'clojure.core.typed.checker.jvm.parse-unparse/unparse-type))]
  (core/defn method-type
    "Given a method symbol, print the core.typed types assigned to it.
    Intended for use at the REPL."
    [mname]
    (load-if-needed)
    (core/let [ms (->> (@type-reflect (Class/forName (namespace mname)))
                       :members
                       (core/filter #(and (instance? clojure.reflect.Method %)
                                          (= (str (:name %)) (name mname))))
                       set)
               _ (assert (seq ms) (str "Method " mname " not found"))]
      (println "Method name:" mname)
      (flush)
      (core/doseq [m ms]
        (println (@unparse-type
                   (@Method->Type m)))
        (flush)))))

(core/let [tl-install (delay (dynaload 'clojure.core.typed.load/install))]
  (core/defn install
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
     (@tl-install features))))

;=============================================================
; Special functions

(core/defn print-filterset
  "During type checking, print the filter set attached to form, 
  preceeded by literal string debug-string.
  Returns nil.
  
  eg. (let [s (seq (get-a-seqable))]
        (print-filterset \"Here now\" s))"
  [debug-string frm]
  frm)

(core/defn ^:skip-wiki
  inst-poly 
  "Internal use only. Use inst."
  [inst-of types-syn]
  inst-of)

(core/defn ^:skip-wiki
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
  (core/let
       [bindings fn-specs-and-annotations
        ; (Vector (U '[Symbol TypeSyn] LetFnInit))
        normalised-bindings
        (core/loop [[fbnd :as bindings] bindings
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
        {anns false inits true} (group-by list? normalised-bindings)]
    `(core/letfn ~(vec inits)
       '~(mapv second anns)
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
        (core/loop [[fbnd :as bindings] bindings
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
                   (core/for [[lb type] anns]
                     [lb `{:full '~type}]))]
    `(core/letfn ~(vec inits)
       ;unquoted to allow bindings to resolve with hygiene
       ~init-syn
       ;preserve letfn empty body
       ~@(or body [nil]))))

(core/let [declare-datatype* (delay (dynaload 'clojure.core.typed.current-impl/declare-datatype*))]
  (core/defn ^:skip-wiki
    declare-datatypes* 
    "Internal use only. Use declare-datatypes."
    [syms nsym]
    (with-clojure-impl
      (core/doseq [sym syms]
        (assert (not (or (some #(= \. %) (str sym))
                         (namespace sym)))
                (str "Cannot declare qualified datatype: " sym))
        (core/let [qsym (symbol (str (munge (name nsym)) \. (name sym)))]
          (@declare-datatype* qsym))))
    nil))

(defmacro declare-datatypes 
  "Declare datatypes, similar to declare but on the type level."
  [& syms]
  `(tc-ignore (declare-datatypes* '~syms '~(ns-name *ns*))))

(core/defn ^:skip-wiki
  declare-protocols* 
  "Internal use only. Use declare-protocols."
  [syms]
  nil)

(defmacro declare-protocols 
  "Declare protocols, similar to declare but on the type level."
  [& syms]
  `(tc-ignore (declare-protocols* '~syms)))

(core/defn ^:skip-wiki
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

(core/let [declare-name* (delay (dynaload 'clojure.core.typed.current-impl/declare-name*))]
  (core/defn ^:skip-wiki
    declare-names* 
    "Internal use only. Use declare-names."
    [syms]
    (core/let [nsym (ns-name *ns*)]
      (core/doseq [sym syms]
        (@declare-name* (symbol (str nsym) (str sym)))))
    nil))

(defmacro declare-names 
  "Declare names, similar to declare but on the type level."
  [& syms]
  (assert (every? (every-pred symbol? (complement namespace)) syms)
          "declare-names only accepts unqualified symbols")
  `(tc-ignore (declare-names* '~syms)))

(declare add-to-rt-alias-env add-tc-type-name)

(core/defn ^:skip-wiki
  defalias* 
  "Internal use only. Use defalias."
  [qsym t form]
  (add-to-rt-alias-env form qsym t)
  (add-tc-type-name form qsym t)
  nil)

(defmacro ^:skip-wiki with-current-location
  [form & body]
  `(core/let [form# ~form]
     (binding [vs/*current-env* {:ns {:name (ns-name *ns*)}
                                 :file *file*
                                 :line (or (-> form# meta :line)
                                           @Compiler/LINE)
                                 :column (or (-> form# meta :column)
                                             @Compiler/COLUMN)}]
       ~@body)))

(def ^:private parse-clj-rt (delay (dynaload 'clojure.core.typed.parse-ast/parse-clj)))
(def ^:private parse-clj-tc (delay (dynaload 'clojure.core.typed.checker.jvm.parse-unparse/parse-clj)))
(def ^:private with-parse-ns* (delay (dynaload 'clojure.core.typed.checker.jvm.parse-unparse/with-parse-ns*)))

(defmacro ^:private delay-rt-parse
  "We can type check c.c.t/parse-ast if we replace all instances
  of parse-ast in clojure.core.typed with delay-parse. Otherwise
  there is a circular dependency."
  [t]
  `(core/let [t# ~t
              app-outer-context# (bound-fn [f# t#] (f# t#))]
     (delay
       (app-outer-context# @parse-clj-rt t#))))

(defmacro ^:private delay-tc-parse
  [t]
  `(core/let [t# ~t
              app-outer-context# (bound-fn [f#] (f#))]
     (delay
       (app-outer-context#
         (core/fn []
           (@with-parse-ns*
             (ns-name *ns*)
             #(@parse-clj-tc t#)))))))

(core/let [add-alias-env (delay (dynaload 'clojure.core.typed.current-impl/add-alias-env))]
  (core/defn ^:skip-wiki add-to-rt-alias-env [form qsym t]
    (with-clojure-impl
      (@add-alias-env
        qsym
        (with-current-location form
          (delay-rt-parse t))))
    nil))

(core/let [subtype? (delay (dynaload 'clojure.core.typed.checker.jvm.subtype/subtype?))
           declared-kind-or-nil (delay (dynaload 'clojure.core.typed.checker.declared-kind-env/declared-kind-or-nil))
           unparse-type (delay (dynaload 'clojure.core.typed.checker.jvm.parse-unparse/unparse-type))
           int-error (delay (dynaload 'clojure.core.typed.errors/int-error))
           add-tc-type-name* (delay (dynaload 'clojure.core.typed.current-impl/add-tc-type-name))]
  (core/defn ^:skip-wiki add-tc-type-name [form qsym t]
    (with-clojure-impl
      (core/let
           [;; preserve *ns*
            bfn (bound-fn [f] (f))
            t (delay
                (core/let
                     [t (bfn
                          #(with-current-location form
                             @(delay-tc-parse t)))
                      _ (with-clojure-impl
                          (when-let [tfn (@declared-kind-or-nil qsym)]
                            (when-not (@subtype? t tfn)
                              (@int-error (str "Declared kind " (@unparse-type tfn)
                                               " does not match actual kind " (@unparse-type t))))))]
                  t))]
        (@add-tc-type-name* qsym t)))
    nil))

(defmacro defalias 
  "Define a recursive type alias on a qualified symbol. Takes an optional doc-string as a second
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
   (core/let
     [qual (if-let [nsp (some-> sym namespace symbol)]
             (or (some-> (or ((ns-aliases *ns*) nsp)
                             (find-ns nsp))
                         ns-name)
                 (throw (Exception. (str "Could not resolve namespace " nsp " in sym " sym))))
             (ns-name *ns*))
      qsym (-> (symbol (str qual) (name sym))
               (with-meta (meta sym)))]
     `(tc-ignore
        (intern '~qual '~(with-meta (symbol (name sym))
                                    (meta sym)))
        (defalias* '~qsym '~t '~&form)))))

(def ^{:doc "Any is the top type that contains all possible values."
       :forms '[Any]
       ::special-type true}
  Any)

(def ^{:doc "AnyValue contains all Value singleton types"
       :forms '[AnyValue]
       ::special-type true}
  AnyValue)

(def ^{:doc "TCError is the type of a type error in the type checker. Use only after
            a type error has been thrown. Only ever use this type in a custom typing rule."
       :forms '[TCError]
       ::special-type true}
  TCError)

(def ^{:doc "U represents a union of types"
       :forms '[(U type*)]
       ::special-type true}
  U)

(def ^{:doc "(alpha) Resolves to the type of the var (lazily) or local (eagerly) named by sym."
       :forms '[(TypeOf sym)]
       ::special-type true}
  TypeOf)

(def ^{:doc "Nothing is the bottom type that has no values."
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
            
            eg. (Difference (U Num nil) nil)  => Num
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

(def ^{:doc "HSequential is a type for heterogeneous sequential persistent collections.
            It extends IPersistentCollection and Sequential"
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

(def ^{:doc "HList is a type for heterogeneous lists. Is a supertype of HSeq that implements IPersistentList."
       :forms '[(HList [fixed*] :filter-sets [FS*] :objects [obj*])
                (HList [fixed* rest *] :filter-sets [FS*] :objects [obj*])
                (HList [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
       ::special-type true}
  HList)

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

(core/let
     [add-rclass-env (delay (dynaload 'clojure.core.typed.current-impl/add-rclass-env))
      Class->symbol (delay (dynaload 'clojure.core.typed.current-impl/Class->symbol))]
  (core/defn ^:skip-wiki rclass-pred
    "Do not use"
    [rcls opts]
    (with-clojure-impl
      (@add-rclass-env (@Class->symbol rcls) opts))))

(defmacro ^:skip-wiki rclass-preds 
  "Do not use"
  [& args]
  `(do
     ~@(core/for [[k v] (partition 2 args)]
         `(rclass-pred ~k ~v))))

;(ann into-array>* [Any Any -> Any])
(core/let
     [parse-type (delay (dynaload 'clojure.core.typed.checker.jvm.parse-unparse/parse-type))
      Type->array-member-Class (delay (dynaload 'clojure.core.typed.checker.jvm.array-ops/Type->array-member-Class))]
  (core/defn ^:skip-wiki
    into-array>*
    "Internal use only. Use into-array>."
    ([cljt coll]
     (load-if-needed)
     (with-clojure-impl
       (into-array (@Type->array-member-Class (@parse-type cljt)) coll)))
    ([javat cljt coll]
     (load-if-needed)
     (with-clojure-impl
       (into-array (@Type->array-member-Class (@parse-type javat)) coll)))
    ;this is the hacky case to prevent full core.typed from loading
    ([into-array-syn javat cljt coll]
     (into-array (resolve into-array-syn) coll))))

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


(core/let [add-nonnilable-method-return (delay (dynaload 'clojure.core.typed.current-impl/add-nonnilable-method-return))]
  (core/defn ^:skip-wiki
    non-nil-return* 
    "Internal use only. Use non-nil-return."
    [msym arities]
    (with-clojure-impl
      (@add-nonnilable-method-return msym arities))
    nil))

(defmacro non-nil-return 
  "Override the return type of fully qualified method msym to be non-nil.
  Takes a set of relevant arities,
  represented by the number of parameters it takes (rest parameter counts as one),
  or :all which overrides all arities.
  
  eg. ; must use full class name
      (non-nil-return java.lang.Class/getDeclaredMethod :all)"
  [msym arities]
  `(tc-ignore (non-nil-return* '~msym '~arities)))

(core/let [add-method-nilable-param (delay (dynaload 'clojure.core.typed.current-impl/add-method-nilable-param))]
  (core/defn ^:skip-wiki
    nilable-param* 
    "Internal use only. Use nilable-param."
    [msym mmap]
    (with-clojure-impl
      (@add-method-nilable-param msym mmap))
    nil))

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

(core/defn print-env 
  "During type checking, print the type environment to *out*,
  preceeded by literal string debug-str."
  [debug-str]
  nil)

(core/let [var->symbol (delay (dynaload 'clojure.core.typed.coerce-utils/var->symbol))
           add-untyped-var (delay (dynaload 'clojure.core.typed.current-impl/add-untyped-var))]
  (core/defn ^:skip-wiki
    untyped-var* 
    "Internal use only. Use untyped-var."
    [varsym typesyn prs-ns form]
    (core/let
         [var (resolve varsym)
          _ (assert (var? var) (str varsym " must resolve to a var."))
          qsym (@var->symbol var)
          expected-type (with-current-location form
                          (delay-tc-parse typesyn))
          _ (with-clojure-impl
              (@add-untyped-var prs-ns qsym expected-type))]
      )
    nil))

(defmacro untyped-var
  "Check a given var has the specified type at runtime."
  [varsym typesyn]
  (core/let
       [prs-ns (-> *ns* ns-name)
        qsym (if (namespace varsym)
               varsym
               (symbol (str prs-ns) (str varsym)))]
    `(tc-ignore (untyped-var* '~qsym '~typesyn '~prs-ns '~&form))))

(core/let [warn (delay (dynaload 'clojure.core.typed.errors/warn))
           var-env (delay (dynaload 'clojure.core.typed.current-impl/var-env))
           add-var-env (delay (dynaload 'clojure.core.typed.current-impl/add-var-env))
           add-tc-var-type (delay (dynaload 'clojure.core.typed.current-impl/add-tc-var-type))
           check-var? (delay (dynaload 'clojure.core.typed.current-impl/check-var?))
           remove-nocheck-var (delay (dynaload 'clojure.core.typed.current-impl/remove-nocheck-var))
           add-nocheck-var (delay (dynaload 'clojure.core.typed.current-impl/add-nocheck-var))]
  (core/defn ^:skip-wiki
    ann* 
    "Internal use only. Use ann."
    [qsym typesyn check? form]
    (core/let
         [_ (with-clojure-impl
              (when (and (contains? (@var-env) qsym)
                         (not (@check-var? qsym))
                         check?)
                (@warn (str "Removing :no-check from var " qsym))
                (@remove-nocheck-var qsym)))
          _ (with-clojure-impl
              (when-not check?
                (@add-nocheck-var qsym)))
          ast (with-current-location form
                (delay-rt-parse typesyn))
          tc-type (with-current-location form
                    (delay-tc-parse typesyn))]
      (with-clojure-impl
        (@add-var-env qsym ast))
      (with-clojure-impl
        (@add-tc-var-type qsym tc-type)))
    nil))

(defmacro ann 
  "Annotate varsym with type. If unqualified, qualify in the current namespace.
  If varsym has metadata {:no-check true}, ignore definitions of varsym 
  while type checking. Supports namespace aliases and fully qualified namespaces
  to annotate vars in other namespaces.
  
  eg. ; annotate the var foo in this namespace
      (ann foo [Number -> Number])
  
      ; annotate a var in another namespace
      (ann another.ns/bar [-> nil])
   
      ; don't check this var
      (ann ^:no-check foobar [Integer -> String])"
  [varsym typesyn]
  (core/let
       [qsym (if-let [nsym (some-> (namespace varsym) symbol)]
               (symbol (if-let [ns (get (ns-aliases *ns*) nsym)]
                         (-> ns ns-name str)
                         (str nsym))
                       (name varsym))
               (symbol (-> *ns* ns-name str) (str varsym)))
        opts (meta varsym)
        check? (not (:no-check opts))]
    `(tc-ignore (ann* '~qsym '~typesyn '~check? '~&form))))

(defmacro ann-many
  "Annotate several vars with type t.

  eg. (ann-many FakeSearch
                web1 web2 image1 image2 video1 video2)"
  [t & vs]
  `(do ~@(map #(list `ann % t) vs)))

(core/let [add-datatype-env (delay (dynaload 'clojure.core.typed.current-impl/add-datatype-env))
           gen-datatype* (delay (dynaload 'clojure.core.typed.current-impl/gen-datatype*))]
  (core/defn ^:skip-wiki
    ann-datatype*
    "Internal use only. Use ann-datatype."
    [vbnd dname fields opts form]
    (core/let [qname (if (some #{\.} (str dname))
                       dname
                       (symbol (str (namespace-munge *ns*) "." dname)))]
      (with-clojure-impl
        (@add-datatype-env 
          qname
          {:record? false
           :name qname
           :fields fields
           :bnd vbnd})))
    (with-current-location form
      (with-clojure-impl
        (@gen-datatype* vs/*current-env* (ns-name *ns*) dname fields vbnd opts false)))
    nil))

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
  (core/let
       [bnd-provided? (vector? (first args))
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

(core/let [add-datatype-env (delay (dynaload 'clojure.core.typed.current-impl/add-datatype-env))
           gen-datatype* (delay (dynaload 'clojure.core.typed.current-impl/gen-datatype*))]
  (core/defn ^:skip-wiki
    ann-record* 
    "Internal use only. Use ann-record"
    [vbnd dname fields opt form]
    (core/let [qname (if (some #{\.} (str dname))
                       dname
                       (symbol (str (namespace-munge *ns*) "." dname)))]
      (with-clojure-impl
        (@add-datatype-env 
          qname
          {:record? true
           :name qname
           :fields fields
           :bnd vbnd})))
    (with-current-location form
      (with-clojure-impl
        (@gen-datatype* vs/*current-env* (ns-name *ns*) dname fields vbnd opt true)))
    nil))

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
  (core/let
       [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [dname fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
        (if bnd-provided?
          (next args)
          args)]
    `(tc-ignore (ann-record* '~vbnd '~dname '~fields '~opt '~&form))))

(core/let [add-protocol-env (delay (dynaload 'clojure.core.typed.current-impl/add-protocol-env))
           gen-protocol* (delay (dynaload 'clojure.core.typed.current-impl/gen-protocol*))]
  (core/defn ^:skip-wiki
    ann-protocol* 
    "Internal use only. Use ann-protocol."
    [vbnd varsym mth form]
    (core/let [qualsym (if (namespace varsym)
                         varsym
                         (symbol (str (ns-name *ns*)) (name varsym)))]
      (with-clojure-impl
        (@add-protocol-env
          qualsym
          {:name qualsym
           :methods mth
           :bnds vbnd}))
      (with-clojure-impl
        (with-current-location form
          (@gen-protocol*
            vs/*current-env*
            (ns-name *ns*)
            varsym
            vbnd
            mth))))
    nil))

(core/let [str-join (delay (dynaload 'clojure.string/join))]
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
        (t/tc-ignore
          (defprotocol IFoo
            (bar [this] [this n s])
            (baz [this n])))

        ; polymorphic protocol
        ; x is scoped in the methods
        (ann-protocol [[x :variance :covariant]]
          IFooPoly
          bar
          (IFn [(IFooPoly x) -> Any]
               [(IFooPoly x) Number Symbol -> Any])
          baz
          [(IFooPoly x) Number -> Number])
        (t/tc-ignore
          (defprotocol IFooPoly
            (bar [this] [this n s])
            (baz [this n])))"
    [& args]
    (core/let
         [bnd-provided? (vector? (first args))
          vbnd (when bnd-provided?
                 (first args))
          [varsym & mth] (if bnd-provided?
                           (next args)
                           args)
          _ (core/let [fs (frequencies (map first (partition 2 mth)))]
              (when-let [dups (seq (filter (core/fn [[_ freq]] (< 1 freq)) fs))]
                (println (str "WARNING: Duplicate method annotations in ann-protocol (" varsym 
                              "): " (@str-join ", " (map first dups))))
                (flush)))
          ; duplicates are checked above.
          {:as mth} mth]
      `(tc-ignore (ann-protocol* '~vbnd '~varsym '~mth '~&form)))))

(core/defn ^:skip-wiki
  ann-interface* 
  "Internal use only. Use ann-interface."
  [vbnd clsym mth]
  nil)

(core/let [str-join (delay (dynaload 'clojure.string/join))]
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
    (core/let
         [bnd-provided? (vector? (first args))
          vbnd (when bnd-provided?
                 (first args))
          [clsym & mth] (if bnd-provided?
                           (next args)
                           args)
          _ (core/let [fs (frequencies (map first (partition 2 mth)))]
              (when-let [dups (seq (filter (core/fn [[_ freq]] (< 1 freq)) fs))]
                (println (str "WARNING: Duplicate method annotations in ann-interface (" clsym 
                              "): " (@str-join ", " (map first dups))))
                (flush)))
          ; duplicates are checked above.
          {:as mth} mth
          qualsym (if (namespace clsym)
                    clsym
                    (symbol (munge (str (ns-name *ns*))) (name clsym)))]
      `(tc-ignore (ann-interface* '~vbnd '~clsym '~mth)))))

(core/let [add-constructor-override (delay (dynaload 'clojure.core.typed.current-impl/add-constructor-override))]
  (core/defn ^:skip-wiki
    override-constructor* 
    "Internal use only. Use override-constructor."
    [ctorsym typesyn form]
    (with-clojure-impl
      (@add-constructor-override 
        ctorsym
        (with-current-location form
          (delay-tc-parse typesyn))))
    nil))

(defmacro override-constructor 
  "Override all constructors for Class ctorsym with type."
  [ctorsym typesyn]
  `(tc-ignore (override-constructor* '~ctorsym '~typesyn '~&form)))

(core/let [add-method-override (delay (dynaload 'clojure.core.typed.current-impl/add-method-override))]
  (core/defn ^:skip-wiki
    override-method* 
    "Internal use only. Use override-method."
    [methodsym typesyn form]
    (with-clojure-impl
      (@add-method-override 
        methodsym
        (with-current-location form
          (delay-tc-parse typesyn))))
    nil))

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

(core/let [ns->URL (delay (dynaload 'clojure.core.typed.coerce-utils/ns->URL))
           int-error (delay (dynaload 'clojure.core.typed.errors/int-error))
           add-ns-deps (delay (dynaload 'clojure.core.typed.current-impl/add-ns-deps))]
  (core/defn ^:skip-wiki
    typed-deps* 
    "Internal use only. Use typed-deps."
    [args form]
    (with-current-location form
      (with-clojure-impl
        (core/doseq [dep args]
          (when-not (@ns->URL dep)
            (@int-error (str "Cannot find dependency declared with typed-deps: " dep)))))
      (with-clojure-impl
        (@add-ns-deps (ns-name *ns*) (set args))))
    nil))

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


(core/let [the-var (delay (dynaload 'clojure.core.typed.current-impl/the-var))]
  (core/defn ^:skip-wiki var>* [sym]
    (@the-var sym)))

(defmacro var>
  "Like var, but resolves at runtime like ns-resolve and is understood by
  the type checker. sym must be fully qualified (without aliases).
  
  eg. (var> clojure.core/+)"
  [sym]
  `(var>* '~sym))

(core/let [register-warn-on-unannotated-vars (delay (dynaload 'clojure.core.typed.current-impl/register-warn-on-unannotated-vars))]
  (core/defn ^:skip-wiki
    warn-on-unannotated-vars*
    "Internal use only. Use allow-unannotated-vars"
    [nsym]
    (with-clojure-impl
      (@register-warn-on-unannotated-vars nsym))
    nil))

(defmacro warn-on-unannotated-vars
  "Allow unannotated vars in the current namespace. 
  
  Emits a warning instead of a type error when checking
  a def without a corresponding expected type.

  Disables automatic inference of `def` expressions.
  
  eg. (warn-on-unannotated-vars)"
  []
  `(tc-ignore (warn-on-unannotated-vars* '~(ns-name *ns*))))

(core/defn default-check-config []
  {:check-ns-dep :recheck
   :unannotated-def :infer
   :unannotated-var :error
   :unannotated-multi :error
   :type-check-eval :interleave
   #_#_:unannotated-arg :any})

(core/let [chkfi (delay (dynaload 'clojure.core.typed.checker.jvm.check-form-clj/check-form-info))]
  (core/defn check-form-info 
    "Function that type checks a form and returns a map of results from type checking the
    form.
    
    Options
    - :expected        Type syntax representing the expected type for this form
                       type-provided? option must be true to utilise the type.
    - :type-provided?  If true, use the expected type to check the form.
    - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
                       of (Map '{:line Int :column Int :file Str} Str).
    - :checked-ast     Returns the entire AST for the given form as the :checked-ast entry,
                       annotated with the static types inferred after checking.
                       If a fatal error occurs, mapped to nil.
    - :no-eval         If true, don't evaluate :out-form. Removes :result return value.
                       It is highly recommended to evaluate :out-form manually.
    - :beta-limit      A natural integer which denotes the maximum number of beta reductions
                       the type system can perform on a single top-level form (post Gilardi-scenario).
    - :check-config    Configuration map for the type checker. (See corresponding option for `check-ns`)
    
    Default return map
    - :ret             TCResult inferred for the current form
    - :out-form        The macroexpanded result of type-checking, if successful. 
    - :result          The evaluated result of :out-form, unless :no-eval is provided.
    - :ex              If an exception was thrown during evaluation, this key will be present
                       with the exception as the value.
    DEPRECATED
    - :delayed-errors  A sequence of delayed errors (ex-info instances)
    - :profile         Use Timbre to profile the type checker. Timbre must be
                       added as a dependency. Must use the \"slim\" JAR."
    [form & {:as opt}]
    (load-if-needed)
    (core/let [opt (update opt :check-config #(merge (default-check-config) %))]
      (apply @chkfi form (apply concat opt)))))

(core/let [chkf* (delay (dynaload 'clojure.core.typed.checker.jvm.check-form-clj/check-form*))]
  (core/defn check-form*
    "Function that takes a form and optional expected type syntax and
    type checks the form. If expected is provided, type-provided?
    must be true.
    
    Takes same options as check-form-info, except 2nd argument is :expected,
    3rd argument is :type-provided?, and subsequent keys in opt will be merged over
    them."
    ([form] (check-form* form nil nil))
    ([form expected] (check-form* form expected true))
    ([form expected type-provided? & {:as opt}]
     (load-if-needed)
     (core/let [opt (update opt :check-config #(merge (default-check-config) %))]
       (@chkf* form expected type-provided? opt)))))

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

(core/let [chknsi (delay (dynaload 'clojure.core.typed.checker.jvm.check-ns-clj/check-ns-info))]
  (core/defn check-ns-info
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
    - :check-deps      If true, recursively type check namespace dependencies.
    Default: true

    Default return map
    - :delayed-errors  A sequence of delayed errors (ex-info instances)"
    ([] (check-ns-info *ns*))
    ([ns-or-syms & {:as opt}]
     (load-if-needed)
     (core/let [opt (update opt :check-config #(merge (default-check-config) %))]
       (@chknsi ns-or-syms opt)))))

(def ^:private chk-ns-clj (delay (dynaload 'clojure.core.typed.checker.jvm.check-ns-clj/check-ns)))

(core/defn check-ns
  "Type check a namespace/s (a symbol or Namespace, or collection).
  If not provided default to current namespace.
  Returns a true value if type checking is successful, otherwise
  throws an Exception.

  Do not use check-ns within a checked namespace.
  It is intended to be used at the REPL or within a unit test.
  Suggested idiom for clojure.test: (is (check-ns 'your.ns))
  
  Keyword arguments:
  - :collect-only  If true, collect type annotations but don't type check code.
                   Useful for debugging purposes.
                   Default: nil
  - :trace         If true, print some basic tracing of the type checker
                   Default: nil
  - :check-config   Configuration map for the type checker.
    - :check-ns-dep  If `:recheck`, always check dependencies.
                     If `:never`, ns dependencies are ignored.
                     #{:recheck :never}
                     Default: :recheck
    - :unannotated-def   If `:unchecked`, unannotated `def`s are ignored
                         and their type is not recorded.
                         If `:infer`, unannotated `def`s are inferred by their
                         root binding and the type is recorded in the type environment.
                         #{:unchecked :infer}
                         Also applies to `defmethod`s on unannotated `defmulti`s.
                         Default: :infer
    - :unannotated-var   If `:unchecked`, unannotated vars are given an *unsound*
                         annotation that is used to statically infer its type
                         based on usages/definition (see `infer-unannotated-vars`).
                         If `:any`, usages of unannotated vars are given type `Any` (sound).
                         If `:error`, unannotated vars are a type error (sound).
                         #{:unchecked :any :error}
                         Default: :error
    - :unannotated-arg   (Not Yet Implemented)
                         If `:unchecked`, unannotated fn arguments are given an *unsound*
                         annotation that is used to statically infer its argument types
                         based on definition.
                         If `:any`, unannotated fn arguments are give type `Any` (sound).
                         #{:unchecked :any}
                         Default: :any
    (Experimental)
    - :type-check-eval   - If :interleave, type checking and evaluation is interleaved,
                           in a similar spirit to how analysis and evaluation in tools.analyzer's
                           analyze+eval is interleaved to respect Clojure's top-level evaluation order.
                         - If :pre-eval, evaluates each form in advance before type checking,
                           and subsequently does not interleave type checking with evaluation.
                           Furthermore, if checking a file, the first form is assumed to be
                           the `ns` form and is evaluated *after* type checking to gain the
                           desired ns side effect.
                         - If :simulate, two separate macroexpansions will be maintained simultaneously:
                           the 'real' expansion that will be evaluated, and
                           a 'fake' expansion just for type checking, derived via custom expansions
                           for typing rules.
                           For each top-level form in the 'real' expansion, there must be
                           a corresponding top-level form in the 'fake' expansion in the same
                           syntactic position (ie., nested in the same number of `do` expressions,
                           in the same positions).
                           This enables more ambitious transformations on the 
                           'fake' expansions that are tailored to the needs of type checking (eg., symbolic
                           execution, simplified expansions that don't actually 'run' but are
                           easy to type check).
                         Default: :interleave

  Removed:
  - :profile       If true, use Timbre to profile the type checker. Timbre must be
                   added as a dependency. Must use the \"slim\" JAR.
                   Default: nil


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
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   (core/let [opt (update opt :check-config #(merge (default-check-config) %))]
     (@chk-ns-clj ns-or-syms opt))))

(core/defn check-ns2 
  ([] (check-ns2 *ns*))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   (core/let [opt (update opt :check-config
                          #(merge {:check-ns-dep :never
                                   :unannotated-def :unchecked
                                   :unannotated-var :unchecked
                                   :unannotated-arg :unchecked
                                   :type-check-eval :interleave}
                                  %))]
     (@chk-ns-clj ns-or-syms opt))))

;(ann statistics [(Coll Symbol) -> (Map Symbol Stats)])
(core/let [stt (delay (dynaload 'clojure.core.typed.checker.jvm.statistics/statistics))]
  (core/defn statistics 
    "Takes a collection of namespace symbols and returns a map mapping the namespace
    symbols to a map of data"
    [nsyms]
    (load-if-needed)
    (@stt nsyms)))

; (ann var-coverage [(Coll Symbol) -> nil])
(core/let [vrc (delay (dynaload 'clojure.core.typed.checker.jvm.statistics/var-coverage))]
  (core/defn var-coverage 
    "Summarises annotated var coverage statistics to *out*
    for namespaces nsyms, a collection of symbols or a symbol/namespace.
    Defaults to the current namespace if no argument provided."
    ([] (var-coverage *ns*))
    ([nsyms-or-nsym]
     (load-if-needed)
     (@vrc nsyms-or-nsym))))

(core/let [all-envs-clj (delay (dynaload 'clojure.core.typed.all-envs/all-envs-clj))]
  (core/defn envs
    "Returns a map of type environments, according to the current state of the
    type checker.
    
    Output map:
    - :vars      map from var symbols to their verbosely printed types
    - :aliases   map from alias var symbols (made with defalias) to their verbosely printed types
    - :special-types  a set of Vars that are special to the type checker (like Any, U, I)
    "
    []
    (load-if-needed)
    (merge (@all-envs-clj)
           {:special-types (set (->> (ns-publics 'clojure.core.typed)
                                  vals
                                  (filter (core/fn [v]
                                            (when (var? v)
                                              (-> v meta ::special-type))))))})))

(core/let [load-typed-file (delay (dynaload 'clojure.core.typed.load/load-typed-file))]
  (core/defn prepare-infer-ns
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
      (with-clojure-impl
        (binding [vs/*prepare-infer-ns* true
                  vs/*instrument-infer-config* (-> config
                                                   (dissoc :ns))]
          (@load-typed-file 
            (subs (@#'clojure.core/root-resource (if (symbol? ns) ns (ns-name ns))) 1))))
      :instrument
      (throw (Exception. ":instrument not yet implemented")))
    :ok))

(core/let [rfrsh (delay (dynaload 'clojure.core.typed.runtime-infer/refresh-runtime-infer))]
  (core/defn refresh-runtime-infer 
    "Clean the current state of runtime inference.
    Will forget the results of any tests on instrumented code."
    []
    (load-if-needed)
    (require '[clojure.core.typed.runtime-infer])
    (@rfrsh)))

(core/let [rti (delay (dynaload 'clojure.core.typed.runtime-infer/runtime-infer))
      deprecated-warn (delay (dynaload 'clojure.core.typed.errors/deprecated-warn))]
  (core/defn runtime-infer 
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
     (core/let [m (-> (if (= 1 (count kws))
                        (do
                          (@deprecated-warn
                            "runtime-infer with 1 arg: use {:ns <ns>}")
                          {:ns (first kws)})
                        (apply hash-map kws))
                      (update :ns #(or % *ns*)))]
       (@rti m)))))

(core/let [spci (delay (dynaload 'clojure.core.typed.runtime-infer/spec-infer))
           deprecated-warn (delay (dynaload 'clojure.core.typed.errors/deprecated-warn))]
  (core/defn spec-infer 
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
     (core/let [m (-> (if (= 1 (count kws))
                        (do
                          (@deprecated-warn
                            "runtime-infer with 1 arg: use {:ns <ns>}")
                          {:ns (first kws)})
                        (apply hash-map kws))
                      (update :ns #(or % *ns*)))]
       (@spci m)))))

(core/defn pred* [tsyn nsym pred]
  pred)

(core/let [reg! (delay (dynaload 'clojure.core.typed.current-impl/register!))]
  (core/defn register!
    "Internal -- Do not use"
    []
    (@reg!)))

(def ^:private type-syntax->pred (delay (dynaload 'clojure.core.typed.type-contract/type-syntax->pred)))
(def ^:private type-syntax->contract (delay (dynaload 'clojure.core.typed.type-contract/type-syntax->contract)))

(defmacro pred 
  "Generate a flat (runtime) predicate for type that returns true if the
  argument is a subtype of the type, otherwise false.

  The current type variable and dotted type variable scope is cleared before parsing.
  
  eg. ((pred Number) 1)
      ;=> true"
  [t]
  (register!)
  (with-current-location &form
    `(pred* '~t
            '~(ns-name *ns*)
            ~(@type-syntax->pred t))))

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
       [Contract Any Options -> Any])

  Options:
  - :positive   positive blame, (U Sym Str)
  - :negative   negative blame, (U Sym Str)
  - :file       file name where contract is checked, (U Str nil)
  - :line       line number where contract is checked, (U Int nil)
  - :column     column number where contract is checked, (U Int nil)"
  ([t x] `(cast ~t ~x {}))
  ([t x opt]
   (register!) ; for type-syntax->contract
   `(do ~spec/special-form
        ::cast
        {:type '~t}
        ;; type checker expects a contract to be in this form, ie. ((fn [x] ..) x)
        ;; - clojure.core.typed.check.add-cast
        ;; - clojure.core.typed.check.special.cast
        ((core/fn [x#]
           (contract/contract
             ~(with-current-location &form
                (@type-syntax->contract t))
             x#
             (core/let [opt# ~opt]
               (contract/make-blame
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

(core/let [infuv (delay (dynaload 'clojure.core.typed.checker.experimental.infer-vars/infer-unannotated-vars))]
  (core/defn infer-unannotated-vars
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

    (t/infer-unannotated-vars)
    => [(t/ann u/bar t/Int)
        (t/ann u/foo (t/U [t/Any -> t/Any] Int))]
                                  "

    ([] (infer-unannotated-vars (ns-name *ns*)))
    ([nsym-or-ns]
     (load-if-needed)
     (with-clojure-impl
       (@infuv (ns-name nsym-or-ns))))))

;============================================================
; Define clojure.core typed wrappers below here to ensure we don't use them above
; thus dynaload as lazily as possible.
;============================================================

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
  (core/let
       [normalise-args
        ; change [a :- b c] to [[a :- b] c]
        (core/fn [seq-exprs]
          (core/loop [flat-result []
                      seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result

              ;for options (:let, :while etc)
              (keyword? (first seq-exprs)) (core/let
                                                [_ (assert (#{2} (count (take 2 seq-exprs)))
                                                           (str "for option missing " (first seq-exprs)))
                                                 [k v & rst] seq-exprs]
                                             (recur (conj flat-result k v)
                                                    rst))
              :else (if (#{:-} (second seq-exprs))
                      (core/let
                           [_ (assert (#{4} (count (take 4 seq-exprs)))
                                      (str "for parameter missing after ':-'"))
                            [b colon t init & rst] seq-exprs]
                        (recur (conj flat-result [b colon t] init)
                               rst))
                      (core/let
                           [_ (assert (#{2} (count (take 2 seq-exprs)))
                                      (str "for binding needs initial values"))
                            [b init & rst] seq-exprs]
                        (recur (conj flat-result [b :- `Any] init)
                               rst))))))

        ; normalise seq-exprs to be flat pairs
        seq-exprs (normalise-args seq-exprs)
        step (core/fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (core/let
                      [k (first exprs)
                       v (second exprs)]
                   (if (keyword? k)
                     (core/let
                          [steppair (step recform (nnext exprs))
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
                     (core/let
                          [_ (assert (and (vector? k)
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
                             (core/let
                                  [;~k (.nth ~chunk- ~i-)
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
  (core/let [[i t? t n] (if (= :- (second bindings))
                          (core/let [[i _ t n] bindings]
                            (assert (== (count bindings) 4) "Bad arguments to dotimes")
                            [i true t n])
                          (core/let [[i n] bindings]
                            (assert (== (count bindings) 2) "Bad arguments to dotimes")
                            [i nil nil n]))]
    `(core/let [n# (long ~n)]
       (loop [~i :- ~(if t? t `Int) 0]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))

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
  (core/let
       [orig-seq-exprs seq-exprs
        has-explicit-return-type? (#{:-} (first maybe-ann-body-expr))
        [ret-ann body-expr] (if has-explicit-return-type?
                              (core/let 
                                   [_ (assert (#{3} (count maybe-ann-body-expr))
                                              (str "Wrong arguments to for: " maybe-ann-body-expr))
                                    [colon t body] maybe-ann-body-expr]
                                [t body])
                              (core/let 
                                   [_ (assert (#{1} (count maybe-ann-body-expr))
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
        (core/fn [seq-exprs]
          (core/loop [flat-result []
                      seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result

              ;for options (:let, :while etc)
              (keyword? (first seq-exprs)) (core/let 
                                                [_ (assert (#{2} (count (take 2 seq-exprs)))
                                                           (str "for option missing " (first seq-exprs)))
                                                 [k v & rst] seq-exprs]
                                             (recur (conj flat-result k v)
                                                    rst))
              :else (core/let 
                         [[meta-ann has-meta-ann?]
                          (when-let [[_ meta-ann] (find (meta (first seq-exprs)) ::ann)]
                            [meta-ann true])]
                      (if (#{:-} (second seq-exprs))
                        (core/let
                             [_ (assert (#{4} (count (take 4 seq-exprs)))
                                        (str "for parameter missing after ':-'"))
                              [b colon t init & rst] seq-exprs]
                          (assert (not meta-ann)
                                  "Cannot mix metadata annotation and explicit annotation in for.")
                          (recur (conj flat-result [b colon t] init)
                                 rst))
                        (core/let 
                             [_ (assert (#{2} (count (take 2 seq-exprs)))
                                        (str "for binding needs initial values"))
                              [b init & rst] seq-exprs
                              ann (if has-meta-ann? meta-ann `Any)]
                          (recur (conj flat-result [b :- ann] init)
                                 rst)))))))

        ; normalise seq-exprs to be flat pairs
        seq-exprs (normalise-args seq-exprs)

        to-groups (core/fn [seq-exprs]
                    (reduce (core/fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (core/fn [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))
        emit-bind (core/fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (core/let 
                         [_ (assert (and (vector? bind)
                                         (#{3} (count bind))
                                         (#{:-} (second bind))) 
                                    "Binder must be of the form [lhs :- type]")
                          bind-ann (nth bind 2)
                          bind (nth bind 0)
                          giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (core/fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     ;typed let
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                      `(core/let
                                            [iterys# ~(emit-bind next-groups)
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
                                    (core/let
                                         [^{::auto-ann ~(meta orig-seq-exprs)
                                            ::track-kind ::for-return}
                                          t# t#]
                                      ;(prn "tracked t#" t#)
                                      t#))
                                  (loop [~gxs :- (Option (Seqable ~bind-ann)) ~gxs]
                                    (when-let [xs# (seq ~gxs)]
                                      (core/let
                                           [^{::auto-ann ~(meta bind)
                                              ::track-kind ::for-param}
                                            x# (first xs#)
                                            ;_# (prn "for param x#" x#)
                                            ~bind x#]
                                        ~(do-mod mod-pairs)))))))
                        #_"inner-most loop"
                        (core/let
                             [gi (gensym "i__")
                              gb (gensym "b__")
                              do-cmod (core/fn do-cmod [[[k v :as pair] & etc]]
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
                                      (core/let
                                           [^{::auto-ann ~(meta orig-seq-exprs)
                                              ::track-kind ::for-return}
                                            t# t#]
                                        t#))
                                    (loop [~gxs :- (Option (Seqable ~bind-ann)) ~gxs]
                                      (when-let [~gxs (seq ~gxs)]
                                        (if (chunked-seq? ~gxs)
                                          (core/let 
                                               [c# (chunk-first ~gxs)
                                                size# (int (count c#))
                                                ~gb (ann-form (chunk-buffer size#)
                                                              (~'clojure.lang.ChunkBuffer ~ret-ann))]
                                            (if (loop [~gi :- Int, (int 0)]
                                                  (if (< ~gi size#)
                                                    (core/let 
                                                         [;~bind (.nth c# ~gi)]
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
                                          (core/let
                                               [^{::auto-ann ~(meta bind)
                                                  ::track-kind ::for-param}
                                                x# (first ~gxs)
                                                ;_# (prn "for param x#" x#)
                                                ~bind x#]
                                            ~(do-mod mod-pairs))))))))))))]
    `(core/let [iter# ~(emit-bind (to-groups seq-exprs))]
        (iter# ~(second seq-exprs)))))

(import-m/import-macros clojure.core.typed.macros
  [def fn loop let ann-form tc-ignore defprotocol
   when-let-fail defn atom ref])

;====================================================
; Initialize environments
;====================================================

(register-ann-ns 'clojure.core.typed.ann.clojure)

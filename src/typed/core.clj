(ns typed.core
  (:refer-clojure :exclude [defrecord type])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons Seqable IPersistentCollection
                         ISeq ASeq ILookup Var Namespace PersistentVector APersistentVector
                         IFn IPersistentStack Associative IPersistentSet IPersistentMap IMapEntry
                         Keyword Atom PersistentList IMeta PersistentArrayMap Compiler Named
                         IRef AReference ARef IDeref IReference APersistentSet PersistentHashSet Sorted
                         LazySeq APersistentMap))
  (:require [analyze.core :refer [ast] :as analyze]
            [analyze.emit-form :as ana-frm]
            [clojure.set :as set]
            [clojure.reflect :as reflect]
            [clojure.string :as str]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [trammel.core :as contracts]
            [clojure.math.combinatorics :as comb]
            [clojure.java.io :as io]
            [cljs
             [compiler]
             [analyzer :as cljs]]
            [clojure.tools.trace :refer [trace-vars untrace-vars
                                         trace-ns untrace-ns]]))

; constraint shorthands, other handy functions
(load "utils")

;Note: defrecord is now trammel's defconstrainedrecord

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special functions

(defn print-env
  "Print the current type environment, and debug-string"
  [debug-string] nil)

(defn print-filterset
  "Print the filter set attached to form, and debug-string"
  [debug-string frm] 
  frm)

(declare Method->Function unparse-type unparse-filter)

(defn method-type 
  "Given a method symbol, print the Typed Clojure types assigned to it"
  [mname]
  (let [ms (->> (reflect/type-reflect (Class/forName (namespace mname)))
             :members
             (filter #(and (instance? clojure.reflect.Method %)
                           (= (str (:name %)) (name mname))))
             set)
        _ (assert (seq ms) (str "Method " mname " not found"))]
    (prn "Method name:" mname)
    (doseq [m ms]
      (prn (unparse-type (Method->Function m))))))

(defn inst-poly 
  [inst-of types-syn]
  inst-of)

(defn inst-poly-ctor [inst-of types-syn]
  inst-of)

(defmacro inst 
  "Instantiate a polymorphic type with a number of types"
  [inst-of & types]
  `(inst-poly ~inst-of '~types))

(defmacro inst-ctor
  "Instantiate a call to a constructor with a number of types.
  First argument must be an immediate call to a constructor."
  [inst-of & types]
  `(inst-poly-ctor ~inst-of '~types))

(defn fn>-ann [fn-of param-types-syn]
  fn-of)

(defn pfn>-ann [fn-of polys param-types-syn]
  fn-of)

(defn loop>-ann [loop-of bnding-types]
  loop-of)

(defn- parse-fn>
  "(fn> name? :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [is-poly forms]
  (let [name (when (symbol? (first forms))
               (first forms))
        forms (if name (rest forms) forms)
        poly (when is-poly
               (first forms))
        forms (if poly (rest forms) forms)
        methods (if ((some-fn vector? keyword?) (first forms))
                  (list forms)
                  forms)
        ;(fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
        ; (HMap {:dom (Seqable TypeSyntax)
        ;        :rng (U nil TypeSyntax)
        ;        :body Any})
        parsed-methods (doall 
                         (for [method methods]
                           (let [[ret has-ret?] (when (not (vector? (first method)))
                                                  (assert (= :- (first method))
                                                          "Return type for fn> must be prefixed by :-")
                                                  [(second method) true])
                                 method (if ret 
                                          (nnext method)
                                          method)
                                 body (rest method)
                                 arg-anns (first method)
                                 [required-params _ [rest-param]] (split-with #(not= '& %) arg-anns)]
                             (assert (sequential? required-params)
                                     "Must provide a sequence of typed parameters to fn>")
                             (assert (not rest-param) "fn> doesn't support rest parameters yet")
                             {:dom-syntax (doall (map (comp second next) required-params))
                              :dom-lhs (doall (map first required-params))
                              :rng-syntax ret
                              :has-rng? has-ret?
                              :body body})))]
    {:poly poly
     :fn `(fn ~@(concat
                  (when name
                    [name])
                  (for [{:keys [body dom-lhs]} parsed-methods]
                    (apply list (vec dom-lhs) body))))
     :parsed-methods parsed-methods}))

(defmacro pfn> 
  "Define a polymorphic typed anonymous function.
  (pfn> name? [binder+] :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (pfn> name? [binder+] (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [& forms]
  (let [{:keys [poly fn parsed-methods]} (parse-fn> true forms)]
    `(pfn>-ann ~fn '~poly '~parsed-methods)))

(defmacro fn> 
  "Define a typed anonymous function.
  (fn> name? :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [& forms]
  (let [{:keys [fn parsed-methods]} (parse-fn> false forms)]
    `(fn>-ann ~fn '~parsed-methods)))

(defmacro defprotocol> [& body]
  "Define a typed protocol"
  `(tc-ignore
     (defprotocol ~@body)))

(defmacro loop>
  "Define a typed loop"
  [bndings* & forms]
  (let [bnds (partition 2 bndings*)
        ; [[lhs :- bnd-ann] rhs]
        lhs (map ffirst bnds)
        rhs (map second bnds)
        bnd-anns (map #(-> % first next second) bnds)]
    `(loop>-ann (loop ~(vec (mapcat vector lhs rhs))
                  ~@forms)
                '~bnd-anns)))

(defmacro declare-datatypes 
  "Declare datatypes, similar to declare but on the type level."
  [& syms]
  `(tc-ignore
  (doseq [sym# '~syms]
    (assert (not (or (some #(= \. %) (str sym#))
                     (namespace sym#)))
            (str "Cannot declare qualified datatype: " sym#))
    (let [qsym# (symbol (str (munge (name (ns-name *ns*))) \. (name sym#)))]
      (declare-datatype* qsym#)))))

(defmacro declare-protocols 
  "Declare protocols, similar to declare but on the type level."
  [& syms]
  `(tc-ignore
  (doseq [sym# '~syms]
     (let [qsym# (if (namespace sym#)
                   sym#
                   (symbol (str (name (ns-name *ns*))) (name sym#)))]
       (declare-protocol* qsym#)))))

(defmacro declare-alias-kind
  "Declare a kind for an alias, similar to declare but on the kind level."
  [sym ty]
  `(tc-ignore
   (do (ensure-clojure)
     (let [sym# '~sym
           qsym# (if (namespace sym#)
                   sym#
                   (symbol (name (ns-name *ns*)) (name sym#)))
           ty# (parse-type '~ty)]
       (assert (not (namespace sym#)) (str "Cannot declare qualified name " sym#))
       (declare ~sym)
       (declare-names ~sym)
       (declare-alias-kind* qsym# ty#)))))

(defmacro declare-names 
  "Declare names, similar to declare but on the type level."
  [& syms]
  `(tc-ignore
  (doseq [sym# '~syms]
     (let [qsym# (if (namespace sym#)
                   sym#
                   (symbol (name (ns-name *ns*)) (name sym#)))]
       (declare-name* qsym#)))))

(defmacro def-alias 
  "Define a type alias"
  [sym type]
  `(tc-ignore
  (do (ensure-clojure)
    (let [sym# (if (namespace '~sym)
                 '~sym
                 (symbol (name (ns-name *ns*)) (name '~sym)))
          ty# (parse-type '~type)]
      (add-type-name sym# ty#)
      (declare ~sym)
      (when-let [tfn# (@DECLARED-KIND-ENV sym#)]
        (assert (subtype? ty# tfn#) (error-msg "Declared kind " (unparse-type tfn#)
                                               " does not match actual kind " (unparse-type ty#))))
      [sym# (unparse-type ty#)]))))

(declare Type? RClass? PrimitiveArray? RClass->Class parse-type symbol->Class
         requires-resolving? -resolve Nil? Value? Value->Class Union? Intersection?)

;Return a Class that generalises what this Clojure type will look like from Java,
;suitable  for use as a Java primitive array member type.
; 
; (Type->array-member-Class (parse-type 'nil)) => Object
; (Type->array-member-Class (parse-type '(U nil Number))) => Number
; (Type->array-member-Class (parse-type '(Array (U nil Number)))) =~> (Array Number)
(defn Type->array-member-Class 
  ([ty] (Type->array-member-Class ty false))
  ([ty nilok?]
   {:pre [(Type? ty)]}
   (cond
     (requires-resolving? ty) (Type->array-member-Class (-resolve ty) nilok?)
     (Nil? ty) (if nilok?
                 nil
                 Object)
     (Value? ty) (Value->Class ty)
     ;; handles most common case of (U nil Type)
     (Union? ty) (let [clss (map #(Type->array-member-Class % true) (.types ty))
                       prim-and-nil? (and (some nil? clss)
                                          (some #(when % (.isPrimitive %)) clss))
                       nonil-clss (remove nil? clss)]
                   (if (and (= 1 (count nonil-clss))
                            (not prim-and-nil?))
                     (first nonil-clss)
                     Object))
     (Intersection? ty) Object
     (RClass? ty) (RClass->Class ty)
     (PrimitiveArray? ty) (class (make-array (Type->array-member-Class (.jtype ty) false) 0))
     :else Object)))

(defn into-array>* 
  ([cljt coll]
   (into-array (-> cljt parse-type Type->array-member-Class) coll))
  ([javat cljt coll]
   (into-array (-> javat parse-type Type->array-member-Class) coll)))

(defmacro into-array> 
  "Make a Java array with Java class javat and Typed Clojure type
  cljt. Resulting array will be of type javat, but elements of coll must be under
  cljt. cljt should be a subtype of javat (the same or more specific)."
  ([cljt coll]
   `(into-array>* '~cljt ~coll))
  ([javat cljt coll]
   `(into-array>* '~javat '~cljt ~coll)))

(defn ann-form* [form ty]
  form)

(defn ann-form* [form ty]
  form)

(defmacro ann-form [form ty]
  `(ann-form* ~form '~ty))

(defn unsafe-ann-form* [form ty]
  form)

(defmacro unsafe-ann-form [form ty]
  `(unsafe-ann-form* ~form '~ty))

(defn tc-ignore-forms* [r]
  r)

;; `do` is special at the top level
(defmacro tc-ignore 
  "Ignore forms in body during type checking"
  [& body]
  `(do ~@(map (fn [b] `(tc-ignore-forms* ~b)) body)))

(defmacro non-nil-return 
  "Override the return type of method msym to be non-nil.
  Takes a set of relevant arities,
  represented by the number of parameters it takes (rest parameter counts as one),
  or :all which overrides all arities.
  
  eg.  (non-nil-return java.lang.Class/getDeclaredMethod :all)"
  [msym arities]
  `(tc-ignore
  (add-nonnilable-method-return '~msym '~arities)))

(defmacro nilable-param 
  "Overrides which parameters in a method may accept
  nilable values. If the parameter is a parameterised type or
  an Array, this also declares the parameterised types and the Array type as nilable.

  mmap is a map mapping arity parameter number to a set of parameter
  positions (integers). If the map contains the key :all then this overrides
  other entries. The key can also be :all, which declares all parameters nilable."
  [msym mmap]
  `(tc-ignore
  (add-method-nilable-param '~msym '~mmap)))

(declare abstract-many instantiate-many)

(load "type_rep")
(load "type_ops")
(load "filter_rep")
(load "filter_ops")
(load "path_rep")
(load "object_rep")

; must be after type/object/filter definitions
(load "fold")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(declare TCResult?)

(defrecord PropEnv [l props]
  "A lexical environment l, props is a list of known propositions"
  [(every? (every-pred symbol? (complement namespace)) (keys l))
   (every? Type? (vals l))
   (every? Filter? props)])

(declare ^:dynamic *lexical-env*)

(defn print-env 
  ([] (print-env *lexical-env*))
  ([e]
   {:pre [(PropEnv? e)]}
   ;; DO NOT REMOVE
   (prn {:env (into {} (for [[k v] (:l e)]
                         [k (unparse-type v)]))
         :props (map unparse-filter (:props e))})))

(defonce VAR-ANNOTATIONS (atom {}))
(def ^:dynamic *lexical-env* (->PropEnv {} []))

(defmacro with-lexical-env [env & body]
  `(binding [*lexical-env* ~env]
     ~@body))

(set-validator! VAR-ANNOTATIONS #(and (every? (every-pred symbol? namespace) (keys %))
                                      (every? Type? (vals %))))
(set-validator! #'*lexical-env* PropEnv?)

(defmacro ann [varsym typesyn]
  `(tc-ignore
 (do (ensure-clojure)
   (let [t# (parse-type '~typesyn)
         s# (if (namespace '~varsym)
              '~varsym
              (symbol (-> *ns* ns-name str) (str '~varsym)))]
     (do (add-var-type s# t#)
       [s# (unparse-type t#)])))))

(declare parse-type alter-class*)

(defn parse-field [[n _ t]]
  [n (parse-type t)])

(defn gen-datatype* [provided-name fields variances args ancests]
  `(do (ensure-clojure)
  (let [provided-name-str# (str '~provided-name)
         ;_# (prn "provided-name-str" provided-name-str#)
         munged-ns-str# (if (some #(= \. %) provided-name-str#)
                          (apply str (butlast (apply concat (butlast (partition-by #(= \. %) provided-name-str#)))))
                          (str (munge (-> *ns* ns-name))))
         ;_# (prn "munged-ns-str" munged-ns-str#)
         demunged-ns-str# (str (clojure.repl/demunge munged-ns-str#))
         ;_# (prn "demunged-ns-str" demunged-ns-str#)
         local-name# (if (some #(= \. %) provided-name-str#)
                       (symbol (apply str (last (partition-by #(= \. %) (str provided-name-str#)))))
                       provided-name-str#)
         ;_# (prn "local-name" local-name#)
         s# (symbol (str munged-ns-str# \. local-name#))
         fs# (apply array-map (apply concat (with-frees (mapv make-F '~args)
                                              (mapv parse-field (partition 3 '~fields)))))
         as# (set (with-frees (mapv make-F '~args)
                    (mapv parse-type '~ancests)))
         _# (add-datatype-ancestors s# as#)
         pos-ctor-name# (symbol demunged-ns-str# (str "->" local-name#))
         args# '~args
         vs# '~variances
         dt# (if args#
               (Poly* args# (repeat (count args#) no-bounds)
                      (->DataType s# vs# (map make-F args#) fs#)
                      args#)
               (->DataType s# nil nil fs#))
         pos-ctor# (if args#
                     (Poly* args# (repeat (count args#) no-bounds)
                            (make-FnIntersection
                              (make-Function (vec (vals fs#)) (->DataType s# vs# (map make-F args#) fs#)))
                            args#)
                     (make-FnIntersection
                       (make-Function (vec (vals fs#)) dt#)))]
     (do 
       (when vs#
         (let [f# (mapv make-F (repeatedly (count vs#) gensym))]
           (alter-class* s# (RClass* (map :name f#) vs# f# s# {}))))
       (add-datatype s# dt#)
       (add-var-type pos-ctor-name# pos-ctor#)
       [[s# (unparse-type dt#)]
        [pos-ctor-name# (unparse-type pos-ctor#)]]))))

(defmacro ann-datatype [dname fields & {ancests :unchecked-ancestors rplc :replace}]
  (assert (not rplc) "Replace NYI")
  (assert (symbol? dname)
          (str "Must provide name symbol: " dname))
  `(tc-ignore
     ~(gen-datatype* dname fields nil nil ancests)))

(defmacro ann-pdatatype [dname vbnd fields & {ancests :unchecked-ancestors rplc :replace}]
  (assert (not rplc) "Replace NYI")
  (assert (symbol? dname)
          (str "Must provide local symbol: " dname))
  `(tc-ignore
     ~(gen-datatype* dname fields (map second vbnd) (map first vbnd) ancests)))

(defn gen-protocol* [local-varsym variances args mths]
  `(do (ensure-clojure)
  (let [local-vsym# '~local-varsym
         s# (symbol (-> *ns* ns-name str) (str local-vsym#))
         on-class# (symbol (str (munge (namespace s#)) \. local-vsym#))
         ; add a Name so the methods can be parsed
         _# (declare-protocol* s#)
         args# '~args
         fs# (when args# 
               (map make-F args#))
         ms# (into {} (for [[knq# v#] '~mths]
                        (do
                          (assert (not (namespace knq#))
                                  "Protocol method should be unqualified")
                          [knq# (with-frees fs# (parse-type v#))])))
         t# (if fs#
              (Poly* (map :name fs#) (repeat (count fs#) no-bounds) 
                     (->Protocol s# '~variances fs# on-class# ms#)
                     (map :name fs#))
              (->Protocol s# nil nil on-class# ms#))]
     (do
       (add-protocol s# t#)
       (doseq [[kuq# mt#] ms#]
         ;qualify method names when adding methods as vars
         (let [kq# (symbol (-> *ns* ns-name str) (str kuq#))]
           (add-var-type kq# mt#)))
       [s# (unparse-type t#)]))))

(defmacro ann-protocol [local-varsym & {:as mth}]
  (assert (not (or (namespace local-varsym)
                   (some #{\.} (str local-varsym))))
          (str "Must provide local var name for protocol: " local-varsym))
  `(tc-ignore
     ~(gen-protocol* local-varsym nil nil mth)))

(defmacro ann-pprotocol [local-varsym vbnd & {:as mth}]
  (assert (not (or (namespace local-varsym)
                   (some #{\.} (str local-varsym))))
          (str "Must provide local var name for protocol: " local-varsym))
  `(tc-ignore
     ~(gen-protocol* local-varsym (mapv second vbnd) (mapv first vbnd) mth)))

(defmacro override-constructor [ctorsym typesyn]
  `(tc-ignore
   (do (ensure-clojure)
     (let [t# (parse-type '~typesyn)
           s# '~ctorsym]
       (do (add-constructor-override s# t#)
         [s# (unparse-type t#)])))))

(defmacro override-method [methodsym typesyn]
  `(tc-ignore
   (do (ensure-clojure)
     (let [t# (parse-type '~typesyn)
           s# (if (namespace '~methodsym)
                '~methodsym
                (throw (Exception. "Method name must be a qualified symbol")))]
       (do (add-method-override s# t#)
         [s# (unparse-type t#)])))))

(defn add-var-type [sym type]
  (swap! VAR-ANNOTATIONS #(assoc % sym type))
  nil)

(defn lookup-local [sym]
  (-> *lexical-env* :l sym))

(defn var->symbol [var]
  {:pre [(var? var)]
   :post [(symbol? %)
          (namespace %)]}
  (symbol (str (ns-name (.ns ^Var var)))
          (str (.sym ^Var var))))

(defn symbol->Class [sym]
  {:pre [(symbol? sym)]
   :post [(class? %)]}
  (case sym
    byte Byte/TYPE
    short Short/TYPE
    int Integer/TYPE
    long Long/TYPE
    float Float/TYPE
    double Double/TYPE
    boolean Boolean/TYPE
    char Character/TYPE
    (Class/forName (str sym))))

(defn Class->symbol [cls]
  {:pre [(class? cls)]
   :post [(symbol? %)]}
  (symbol (.getName cls)))

(def ^:dynamic *var-annotations*)

(defn lookup-Var [nsym]
  (assert (contains? @*var-annotations* nsym) 
          (str (when *current-env*
                 (str (:line *current-env*) ": "))
            "Untyped var reference: " nsym))
  (@*var-annotations* nsym))

(defn merge-locals [env new]
  (-> env
    (update-in [:l] #(merge % new))))

(defmacro with-locals [locals & body]
  `(binding [*lexical-env* (merge-locals *lexical-env* ~locals)]
     ~@body))

(declare ^:dynamic *current-env*)

(defn type-of [sym]
  {:pre [(symbol? sym)]
   :post [(or (Type? %)
              (TCResult? %))]}
  (cond
    (not (namespace sym)) (if-let [t (lookup-local sym)]
                            t
                            (throw (Exception. (str (when *current-env*
                                                      (str (:line *current-env*) ": "))
                                                    "Reference to untyped binding: " sym))))
    :else (lookup-Var sym)))


(derive ::clojurescript ::default)
(derive ::clojure ::default)

(def TYPED-IMPL (atom ::clojure))
(set-validator! TYPED-IMPL #(isa? % ::default))

(defn ensure-clojure []
  (reset! TYPED-IMPL ::clojure))

(defn ensure-clojurescript []
  (reset! TYPED-IMPL ::clojurescript))

(defn checking-clojure? []
  (= ::clojure @TYPED-IMPL))

(defn checking-clojurescript? []
  (= ::clojurescript @TYPED-IMPL))

(defmacro ann-multi 
  "Annotate a multimethod with a function type."
  [mmsym ftype]
  `(tc-ignore
   (let [mmname# (if (namespace '~mmsym)
                   '~mmsym
                   (symbol (-> *ns* ns-name name) (name '~mmsym)))
         ft# (parse-type '~ftype)
         vt# (In (RClass-of clojure.lang.MultiFn) ft#)]
     (add-multimethod-fn-type mmname# ft#)
     (add-var-type mmname# vt#)
     [mmname# (unparse-type vt#)])))

(load "dvar_env")
(load "datatype_ancestor_env")
(load "datatype_env")
(load "protocol_env")
(load "method_override_env")
(load "ctor_override_env")
(load "method_return_nilables")
(load "method_param_nilables")
(load "declared_kind_env")
(load "name_env")
(load "rclass_env")
(load "mm_env")

(load "parse")
(load "unparse")
(load "frees")
(load "promote_demote")
(load "cs_gen")
(load "subst_dots")
(load "infer")
(load "tvar_rep")
(load "subst")
(load "trans")
(load "inst")
(load "subtype")
(load "alter")
(load "ann")
(load "check")
(load "check_cljs")

;emit something that CLJS can display ie. a quoted unparsed typed
(defmacro cf-cljs
  "Type check a Clojurescript form and return its type"
  ([form]
   (let [t
         (do (ensure-clojurescript)
           (-> (ana-cljs {:locals {} :context :expr :ns {:name cljs/*cljs-ns*}} form) check-cljs expr-type unparse-TCResult))]
     `'~t))
  ([form expected]
   (let [t
         (do (ensure-clojurescript)
           (-> (ana-cljs {:locals {} :context :expr :ns {:name cljs/*cljs-ns*}}
                         (list `ann-form-cljs form expected))
             (#(check-cljs % (ret (parse-type expected)))) expr-type unparse-TCResult))]
     `'~t)))

(defmacro cf 
  "Type check a Clojure form and return its type"
  ([form]
  `(do (ensure-clojure)
     (tc-ignore
       (-> (ast ~form) check expr-type unparse-TCResult))))
  ([form expected]
  `(do (ensure-clojure)
     (tc-ignore
       (-> (ast (ann-form ~form ~expected)) (#(check % (ret (parse-type '~expected)))) expr-type unparse-TCResult)))))

(defn analyze-file-asts
  [^String f]
  (let [res (if (re-find #"^file://" f) (java.net.URL. f) (io/resource f))]
    (assert res (str "Can't find " f " in classpath"))
    (with-altered-specials
      (binding [cljs/*cljs-ns* 'cljs.user
                cljs/*cljs-file* (.getPath ^java.net.URL res)
                *ns* cljs/*reader-ns*]
        (with-open [r (io/reader res)]
          (let [env (cljs/empty-env)
                pbr (clojure.lang.LineNumberingPushbackReader. r)
                eof (Object.)]
            (loop [r (read pbr false eof false)
                   asts []]
              (let [env (assoc env :ns (cljs/get-namespace cljs/*cljs-ns*))]
                (if-not (identical? eof r)
                  (let [ast1 (cljs/analyze env r)]
                    (recur (read pbr false eof false) (conj asts ast1)))
                  asts)))))))))

(defn check-cljs-ns*
  "Type check a CLJS namespace. If not provided default to current namespace"
  ([] (check-cljs-ns* cljs/*cljs-ns*))
  ([nsym]
   (ensure-clojurescript)
   (let [asts (analyze-file-asts (cljs/ns->relpath nsym))]
     (doseq [ast asts]
       (check-cljs ast)))))

(defmacro check-cljs-ns
  ([] (check-cljs-ns*) `'~'success)
  ([nsym] (check-cljs-ns* nsym) `'~'success))

(defn check-ns 
  "Type check a namespace. If not provided default to current namespace"
  ([] (check-ns (ns-name *ns*)))
  ([nsym]
   (ensure-clojure)
   (with-open [pbr (analyze/pb-reader-for-ns nsym)]
     (let [[_ns-decl_ & asts] (analyze/analyze-ns pbr (analyze/uri-for-ns nsym) nsym)]
       (doseq [ast asts]
         (check ast))))))

(defn trepl []
  (clojure.main/repl 
    :eval (fn [f] 
            (let [t (do (ensure-clojure)
                      (-> (analyze/analyze-form f) 
                        check expr-type unparse-TCResult))]
              (prn t) 
              (eval f)))))

(comment 
  (check-ns 'typed.test.example)

  ; very slow because of update-composite
  (check-ns 'typed.test.rbt)

  (check-ns 'typed.test.macro)
  (check-ns 'typed.test.conduit)
  (check-ns 'typed.test.person)
  (check-ns 'typed.test.core-logic)
  (check-ns 'typed.test.ckanren)

  (check-cljs-ns 'typed.test.logic)
  )

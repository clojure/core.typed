(ns typed.core
  (:refer-clojure :exclude [defrecord type])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons Seqable IPersistentCollection
                         ISeq ASeq ILookup Var Namespace PersistentVector APersistentVector
                         IFn IPersistentStack Associative IPersistentSet IPersistentMap IMapEntry
                         Keyword Atom))
  (:require [analyze.core :refer [ast] :as analyze]
            [clojure.set :as set]
            [clojure.repl :refer [pst]]
            [trammel.core :as contracts]
            [clojure.math.combinatorics :as comb]
            [clojure.tools.trace :refer [trace trace-ns]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special functions

(defn inst-poly [inst-of types-syn]
  inst-of)

(defmacro inst [inst-of & types]
  `(inst-poly ~inst-of '~types))

(defn fn>-ann [fn-of param-types-syn]
  fn-of)

(defmacro fn> [& forms]
  (let [methods (if (vector? (first forms))
                  (list forms)
                  forms)
        ;(fn> [[a :- Number] & [n :- Number *]] a) 
        anns (for [[arg-anns] methods]
               (let [[required-params _ [rest-param]] (split-with #(not= '& %) arg-anns)]
                 (assert (not rest-param) "fn> doesn't support rest parameters yet")
                 {:dom (map (comp second next) required-params)
                  :has-rest false}))]
    `(fn>-ann (fn ~@(for [[params & body] methods]
                      (apply list (vec (map first params)) body)))
              '~anns)))

(defn tc-ignore-forms [r]
  r)

(defmacro tc-ignore [& body]
  `(tc-ignore-forms (do
                      ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defmacro defrecord [name slots inv-description invariants & etc]
  ;only define record if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(contracts/defconstrainedrecord ~name ~slots ~inv-description ~invariants ~@etc)))

(declare abstract-many instantiate-many)

;(defn- comp-mm [mm disps]
;  (set/difference disps (set (keys (methods mm)))))
;
;(comp-mm replace-image (disj kinds :scope))
;(comp-mm replace-image (disj kinds :scope))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(def nat? (every-pred (complement neg?) integer?))

(def Type ::Type)

(defn Type? [a]
  (isa? (class a) Type))

(defn declare-type [a]
  (derive a Type))

(defrecord Top []
  "The top type"
  [])

(declare-type Top)

;TODO flatten unions
(defrecord Union [types]
  "An unordered union of types"
  [(every? Type? types)
   (not (some Union? types))])

(declare-type Union)

(defn Un [& types]
  (if (= 1 (count types))
    (first types)
    (->Union (-> types set vec))))

(def empty-union (Un))

(defn Bottom []
  empty-union)

(defn Bottom? [a]
  (= empty-union a))

(declare Fn-Intersection? Function? Poly? PolyDots?)

(defrecord Intersection [types]
  "An ordered intersection of types. Either is an intersection
  of Functions, or contains at most one Function/Poly/PolyDots type"
  [(sequential? types)
   (seq types)
   (or (every? Function? types)
       (<= (count (filter (some-fn Fn-Intersection? Poly? PolyDots?) types))
           1)
       (every? Type? types))])

(defn In [& types]
  (if (empty? types)
    (Bottom)
    (->Intersection types)))

(declare-type Intersection)

(def variances #{:constant :covariant :contravariant :invariant :dotted})

(defn variance? [v]
  (contains? variances v))

(defrecord B [idx upper-bound lower-bound]
  "A bound variable. Should not appear outside this file"
  [(nat? idx)
   (Type? upper-bound)
   (Type? lower-bound)])

(declare-type B)

(defrecord F [name upper-bound lower-bound]
  "A named free variable"
  [(symbol? name)
   (Type? upper-bound)
   (Type? lower-bound)])

(defn make-F
  "Make a free variable with optional bounds"
  ([name] (make-F name (->Top) (Bottom)))
  ([name upper] (make-F name upper (Bottom)))
  ([name upper lower]
   (->F name upper lower)))

(declare-type F)

(declare Scope?)

(defrecord Scope [body]
  "A scope that contains one bound variable, can be nested. Not used directly"
  [((some-fn Type? Scope?) body)])

(defrecord RClass [variances the-class replacements]
  "A restricted class, where ancestors are
  (replace replacements (ancestors the-class))"
  [(or (nil? variances)
       (and (sequential? variances)
            (every? variance?  variances)))
   (class? the-class)
   (map? replacements)
   (every? class? (keys replacements))
   (every? (some-fn Type? Scope?) (vals replacements))])

(defn monomorphic-RClass [class]
  (->RClass nil class {}))

;smart constructor
(defn RClass* [names variances the-class replacements]
  {:pre [(every? symbol? names)
         (every? variance? variances)
         (class? the-class)]}
  (if (seq variances)
    (->RClass variances
              the-class
              (into {} (for [[k v] replacements]
                         [k (abstract-many names v)])))
    (->RClass nil the-class replacements)))

;smart destructor
(defn RClass-replacements* [names rclass]
  (into {} (for [[k v] (:replacements rclass)]
             [k (instantiate-many (map make-F names) v)])))

(declare-type RClass)

(defrecord RInstance [poly? constructor]
  "An instance of a class"
  [(or (nil? poly?)
       (and (sequential? poly?)
            (every? Type? poly?)))
   (RClass? constructor)])

(declare poly-RClass-from)

(defn RInstance-of 
  "Return a RInstance type, optionally parameterised"
  ([class] (->RInstance nil (monomorphic-RClass class)))
  ([class params] (->RInstance params (poly-RClass-from class))))

(declare-type RInstance)

(defrecord Field [name type]
  "A datatype/record field"
  [(symbol? name)
   (Type? type)])

(defrecord Record [the-class fields]
  "A record"
  [(class? the-class)
   (every? Field? fields)])

(declare-type Record)

(defrecord DataType [the-class fields]
  "A Clojure datatype"
  [(class? the-class)
   (every? Field? fields)])

(declare-type DataType)

(defrecord Poly [nbound scope]
  "A polymorphic type containing n bound variables"
  [(nat? nbound)
   (Scope? scope)])

(declare-type Poly)

;smart constructor
(defn Poly* [names body]
  {:pre [(every? symbol names)
         (Type? body)]}
  (if (empty? names)
    body
    (->Poly (count names) (abstract-many names body))))

;smart destructor
(defn Poly-body* [names poly]
  {:pre [(every? symbol? names)
         (Poly? poly)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (instantiate-many (map make-F names) (:scope poly)))

(defrecord PolyDots [nbound scope]
  "A polymorphic type containing n-1 bound variables and 1 ... variable"
  [(nat? nbound)
   (Scope? scope)])

(declare-type PolyDots)

;smart constructor
(defn PolyDots* [names body]
  {:pre [(every? symbol names)
         (Type? body)]}
  (if (empty? names)
    body
    (->PolyDots (count names) (abstract-many names body))))

;smart destructor
(defn PolyDots-body* [names poly]
  {:pre [(every? symbol? names)
         (PolyDots? poly)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (instantiate-many (map make-F names) (:scope poly)))

(defrecord Mu [scope]
  "A recursive type containing one bound variable, itself"
  [(Scope? scope)])

(declare instantiate substitute remove-scopes subtype?)

(defn Mu-body* [name t]
  {:pre [(Mu? t)
         (symbol? name)]}
  (instantiate (make-F name) (:scope t)))

(defn unfold [t]
  {:pre [(Mu? t)]
   :post [(Type? t)]}
  (let [sym (gensym)
        body (Mu-body* sym t)]
    (substitute body t sym)))

(declare-type Mu)

(defrecord Value [val]
  "A Clojure value"
  [(not (nil? val))
   (not (false? val))])

(declare-type Value)

(defrecord HeterogeneousMap [types]
  "A constant map, clojure.lang.IPersistentMap"
  [(map? types)
   (every? #(and (= 2 (count %))
                 (let [[k v] %]
                   (and (Value? k)
                        (Type? v))))
           types)])

(defn make-HMap [mandatory optional]
  (assert (= #{}
             (set/intersection (-> mandatory keys set)
                               (-> optional keys set))))
  (apply Un
         (for [ss (map #(into {} %) (comb/subsets optional))]
           (->HeterogeneousMap (merge mandatory ss)))))

(declare-type HeterogeneousMap)

(defrecord HeterogeneousVector [types]
  "A constant vector, clojure.lang.IPersistentVector"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousVector)

(defrecord HeterogeneousList [types]
  "A constant list, clojure.lang.IPersistentList"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousList)

(defrecord HeterogeneousSeq [types]
  "A constant seq, clojure.lang.ISeq"
  [(sequential? types)
   (every? Type? types)])

(declare-type HeterogeneousSeq)

(declare Result?)

(defrecord DottedPretype [pre-type bound]
  "A dotted pre-type. Not a type"
  [(Type? pre-type)
   ((some-fn F? B?) bound)])

(defrecord KwArgs [mandatory optional]
  "A set of mandatory and optional keywords"
  [(map? mandatory)
   (map? optional)
   (every? Value? (map keys [mandatory optional]))
   (every? Type? (map vals [mandatory optional]))])

(defrecord Function [dom rng rest drest kws]
  "A function arity, must be part of an intersection"
  [(or (empty? dom)
       (sequential? dom))
   (every? Type? dom)
   (Result? rng)
   (<= (count (filter identity [rest drest kws])) 1)
   (or (nil? rest)
       (Type? rest))
   (or (nil? drest)
       (DottedPretype? drest))
   (or (nil? kws)
       (KwArgs? kws))])

(defrecord TopFunction []
  "Supertype to all functions"
  [])

(declare ->NoFilter ->NoObject ->Result)

(defn make-Result
  ([t] (make-Result t nil nil))
  ([t f] (make-Result t f nil))
  ([t f o] (->Result t (or f (->NoFilter)) (or o (->NoObject)))))

(defn make-Function
  "Make a function, wrap range type in a Result.
  Accepts optional :filter and :object parameters that default to NoFilter
  and NoObject"
  ([dom rng] (make-Function dom rng nil nil))
  ([dom rng rest drest & {:keys [filter object] :or {filter (->NoFilter), object (->NoObject)}}]
   (->Function dom (->Result rng filter object) rest drest nil)))

(defn Fn-Intersection [fns]
  {:pre [(every? Function? fns)]}
  (apply In fns))

(defn Fn-Intersection? [fin]
  (and (Intersection? fin)
       (every? Function? (:types fin))))

(declare abstract)

;smart constructor
(defn Mu* [name body]
  (->Mu (abstract name body)))

(defrecord Nil []
  "Type for nil"
  [])

(defrecord True []
  "Type for false"
  [])

(defrecord False []
  "Type for false"
  [])

(declare-type True)
(declare-type False)
(declare-type Nil)

(declare Filter? RObject?)

(defrecord Result [t fl o]
  "A result type with filter f and object o. NOT a type."
  [(Type? t)
   (Filter? fl)
   (RObject? o)])

(defn Result-type* [r]
  {:pre [(Result? r)]}
  (:t r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(def Filter ::filter)

(defn Filter? [a]
  (isa? (class a) Filter))

(defn declare-filter [c]
  (derive c Filter))

(defrecord BotFilter []
  "?"
  [])
(defrecord TopFilter []
  "?"
  [])

(defrecord NoFilter []
  "Represents no info about filters, used for parsing types"
  [])

(declare PathElem?)

(defrecord TypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is of given type"
  [(Type? type)
   (every? PathElem? path)])

(defrecord NotTypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(Type? type)
   (every? PathElem? path)])

(defrecord AndFilter [fs]
  "Logical conjunction of filters"
  [(seq fs)
   (every? Filter? fs)])

(defrecord OrFilter [fs]
  "Logical disjunction of filters"
  [(seq fs)
   (every? Filter? fs)])

(defrecord ImpFilter [a c]
  "a implies c"
  [(Filter? a)
   (Filter? c)])

(defrecord FilterSet [then else]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(Filter? then)
   (Filter? else)])

(declare-filter BotFilter)
(declare-filter TopFilter)
(declare-filter NoFilter)
(declare-filter AndFilter)
(declare-filter OrFilter)
(declare-filter TypeFilter)
(declare-filter NotTypeFilter)
(declare-filter ImpFilter)
(declare-filter FilterSet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(def PathElem ::path-elem)

(defn PathElem? [a]
  (isa? (class a) PathElem))

(defn declare-path-elem [c]
  (derive c PathElem))

(defrecord FirstPE []
  "A path calling clojure.core/first"
  [])
(defrecord NextPE []
  "A path calling clojure.core/next"
  [])

(declare-path-elem FirstPE)
(declare-path-elem NextPE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Objects

(def RObject ::r-object)

(defn RObject? [a]
  (isa? (class a) RObject))

(defn declare-robject [c]
  (derive c RObject))

(defrecord EmptyObject []
  "?"
  [])

(defrecord Path [path id]
  "A path"
  [])

(defrecord NoObject []
  "Represents no info about the object of this expression
  should only be used for parsing type annotations and expected types"
  [])

(declare-robject EmptyObject)
(declare-robject Path)
(declare-robject NoObject)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(defonce VAR-ANNOTATIONS (atom {}))
(def ^:dynamic *local-annotations* {})

(set-validator! VAR-ANNOTATIONS #(and (every? (every-pred symbol? namespace) (keys %))
                                      (every? Type? (vals %))))
(set-validator! #'*local-annotations* #(and (every? (every-pred symbol? (complement namespace)) (keys %))
                                            (every? Type? (vals %))))

(defmacro ann [varsym typesyn]
  `(tc-ignore
     (let [t# (parse-type '~typesyn)
           s# (if (namespace '~varsym)
                '~varsym
                (symbol (-> *ns* ns-name str) (str '~varsym)))]
       (do (add-var-type s# t#)
         [s# (unparse-type t#)]))))

(defn add-var-type [sym type]
  (swap! VAR-ANNOTATIONS #(assoc % sym type)))

(defn lookup-local [sym]
  (*local-annotations* sym))

(defn lookup-var [var]
  (let [nsym (symbol (str (ns-name (.ns ^Var var)))
                     (str (.sym ^Var var)))]
    (assert (contains? @VAR-ANNOTATIONS nsym) (str "Untyped var reference: " var))
    (@VAR-ANNOTATIONS nsym)))

(defmacro with-locals [locals & body]
  `(binding [*local-annotations* (merge *local-annotations* ~locals)]
     ~@body))

(defn type-of [sym-or-var]
  (cond
    (symbol? sym-or-var) (if-let [t (lookup-local sym-or-var)]
                           t
                           (throw (Exception. (str "Cannot resolve type: " sym-or-var))))
    (var? sym-or-var) (lookup-var sym-or-var)
    :else (throw (Exception. (str "Cannot resolve type: " sym-or-var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotted Variable Environment

;symbol -> F
(def ^:dynamic *dotted-scope* {})
(set-validator! #'*dotted-scope* #(and (every? symbol? (keys %))
                                       (every? F? (vals %))))

(defmacro with-dotted [dvar & body]
  `(binding [*dotted-scope* (conj *dotted-scope* [(:name ~dvar) ~dvar])]
     ~@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restricted Class

;Class -> RClass
(defonce RESTRICTED-CLASS (atom {}))

(defn poly-RClass-from [class]
  (let [rclass (@RESTRICTED-CLASS class)]
    (assert rclass (str class " not declared as polymorphic"))
    rclass))

(declare with-frees)

(defn- build-replacement-syntax [m]
  (into {} (for [[k v] m]
             [k `(parse-type '~v)])))

(defn parse-RClass-binder [bnds]
  (for [[nme & {:keys [variance]}] bnds]
    [variance (make-F nme)]))

(defmacro alter-class [the-class frees-syn & opts]
  (let [{replacements-syn :replace} (apply hash-map opts)
        replacements (build-replacement-syntax replacements-syn)]
     `(let [[variances# frees#] (when-let [fs# (seq '~frees-syn)]
                                  (let [b# (parse-RClass-binder fs#)]
                                    [(map first b#) (map second b#)]))]
        (swap! RESTRICTED-CLASS 
               #(assoc % ~the-class (RClass* (map :name frees#) variances#
                                             ~the-class (with-frees frees#
                                                          ~replacements))))
        ~the-class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type syntax

;(Map Symbol F)
(def ^:dynamic *free-scope* {})
(set-validator! #'*free-scope* #(and (every? symbol? (keys %))
                                     (every? F? (vals %))))

(defmacro with-frees [frees & body]
  `(let [m# (zipmap (map :name ~frees) ~frees)]
     (binding [*free-scope* (merge *free-scope* m#)]
       ~@body)))

(defmulti parse-type class)
(defmulti parse-type-list first)

(defn parse-free [f]
  (if (symbol? f)
    (make-F f)
    (let [[n & opts] f
          {upp :<
           low :>} (apply hash-map opts)]
      (->F n 
           (if upp 
             (parse-type upp)
             (->Top)) 
           (if low
             (parse-type low)
             (Bottom))))))

(defn check-forbidden-rec [rec tbody]
  (when (or (= rec tbody) 
            (and (Intersection? tbody)
                 (contains? (set (:types tbody)) rec))
            (and (Union? tbody)
                 (contains? (set (:types tbody)) rec)))
    (throw (Exception. "Recursive type not allowed here"))))

(defn parse-rec-type [[rec [free-symbol :as bnder] type]]
  (let [_ (assert (= 1 (count bnder)) "Only one variable in allowed: Rec")
        f (make-F free-symbol)
        body (with-frees [f]
               (parse-type type))
        
        _ (check-forbidden-rec f body)]
    (Mu* (:name f) body)))

(defmethod parse-type-list 'Rec
  [syn]
  (parse-rec-type syn))

;dispatch on last element of syntax in binder
(defmulti parse-all-type (fn [bnds type] (last bnds)))

;(All [a b ...] type)
(defmethod parse-all-type '...
  [bnds type]
  (let [frees (map parse-free (-> bnds butlast butlast))
        dvar (parse-free (-> bnds butlast last))]
    (-> 
      (PolyDots* (concat (map :name frees) [(:name dvar)])
                 (with-frees frees
                   (with-dotted dvar 
                     (parse-type type))))
      (with-meta {:free-names (map :name frees)
                  :dvar-name (:name dvar)}))))

;(All [a b] type)
(defmethod parse-all-type :default
  [bnds type]
  (let [frees (map parse-free bnds)]
    (-> 
      (Poly* (map :name frees)
             (with-frees frees
               (parse-type type)))
      (with-meta {:free-names (map :name frees)}))))

(defmethod parse-type-list 'All
  [[All bnds syn & more]]
  (assert (not more) "Bad All syntax")
  (parse-all-type bnds syn))

(defn parse-union-type [[u & types]]
  (apply Un (doall (map parse-type types))))

(defmethod parse-type-list 'U
  [syn]
  (parse-union-type syn))

(defn parse-intersection-type [[i & types]]
  (apply In (doall (map parse-type types))))

(defmethod parse-type-list 'I
  [syn]
  (parse-intersection-type syn))

(declare parse-function)

(defn parse-fn-intersection-type [[Fn & types]]
  (Fn-Intersection (doall (map parse-function types))))

(defmethod parse-type-list 'Fn
  [syn]
  (parse-fn-intersection-type syn))

(defmethod parse-type-list 'Vector*
  [syn]
  (->HeterogeneousVector (vec (map parse-type (rest syn)))))

(declare constant-type)

(defmethod parse-type-list 'Map*
  [[_ & {:keys [mandatory optional]}]]
  (letfn [(mapt [m]
            (into {} (for [[k v] m]
                       [(constant-type k)
                        (parse-type v)])))]
    (let [mandatory (mapt mandatory)
          optional (mapt optional)]
      (make-HMap mandatory optional))))

(defn parse-rinstance-type [[cls-sym & params-syn]]
  (let [cls (resolve cls-sym)
        tparams (doall (map parse-type params-syn))]
    (RInstance-of cls tparams)))

(defmethod parse-type-list 'Value
  [[Value syn]]
  (constant-type syn))

(defmethod parse-type-list 'KeywordArgs
  [[_KeywordArgs_ & {:keys [optional mandatory]}]]
  (assert (= #{}
             (set/intersection (set (keys optional))
                               (set (keys mandatory)))))
  (let [optional (into {} (for [[k v] optional]
                            (do (assert (keyword? k))
                              [(->Value k) (parse-type v)])))
        mandatory (into {} (for [[k v] mandatory]
                             (do (assert (keyword? k))
                               [(->Value k) (parse-type v)])))]
    (apply Un (apply concat
                     (for [opts (map #(into {} %) (comb/subsets optional))]
                       (let [m (merge mandatory opts)
                             kss (comb/permutations (keys m))]
                         (for [ks kss]
                           (->HeterogeneousSeq (mapcat #(find m %) ks)))))))))

(defmethod parse-type-list :default [syn] (parse-rinstance-type syn))

(defmethod parse-type Cons [l] (parse-type-list l))
(defmethod parse-type IPersistentList [l] (parse-type-list l))

(defmulti parse-type-symbol identity)
(defmethod parse-type-symbol 'Any [_] (->Top))
(defmethod parse-type-symbol 'Nothing [_] (Bottom))

(defmethod parse-type-symbol :default
  [sym]
  (cond
    (sym *free-scope*) (sym *free-scope*)
    :else (let [res (resolve sym)]
            (cond 
              (class? res) (RInstance-of res)
              :else (throw (Exception. (str "Cannot resolve type: " sym)))))))

(defmethod parse-type Symbol [l] (parse-type-symbol l))
(defmethod parse-type Boolean [v] (if v (->True) (->False))) 
(defmethod parse-type nil [_] (->Nil))

(defn parse-function [f]
  (let [all-dom (take-while #(not= '-> %) f)
        [_ rng & opts :as chk] (drop-while #(not= '-> %) f) ;opts aren't used yet
        _ (assert (= (count chk) 2) "Missing range")

        {ellipsis-pos '...
         asterix-pos '*} 
        (into {} (map vector all-dom (range)))

        _ (assert (not (and asterix-pos ellipsis-pos))
                  "Cannot provide both rest type and dotted rest type")

        fixed-dom (cond 
                    asterix-pos (take (dec asterix-pos) all-dom)
                    ellipsis-pos (take (dec ellipsis-pos) all-dom)
                    :else all-dom)

        rest-type (when asterix-pos
                    (nth all-dom (dec asterix-pos) nil))
        [drest-type _ drest-bnd] (when ellipsis-pos
                                   (drop (dec ellipsis-pos) all-dom))]
    (make-Function (doall (map parse-type fixed-dom))
                   (parse-type rng)
                   (when rest-type
                     (parse-type rest-type))
                   (when drest-type
                     (->DottedPretype
                       (with-frees [(*dotted-scope* drest-bnd)] ;with dotted bound in scope as free
                         (parse-type drest-type))
                       (*dotted-scope* drest-bnd))))))

(defmethod parse-type IPersistentVector
  [f]
  (Fn-Intersection [(parse-function f)]))

(def ^:dynamic *next-nme* 0) ;stupid readable variables

(defmulti unparse-type class)
(defn unp [t] (prn (unparse-type t)))

(defmethod unparse-type Nil [_] nil)
(defmethod unparse-type True [_] true)
(defmethod unparse-type False [_] false)
(defmethod unparse-type Top [_] 'Any)

(defmethod unparse-type Result
  [{:keys [t]}]
  (unparse-type t))

(defmethod unparse-type F
  [{:keys [name]}]
  name)

(defmethod unparse-type Union
  [{types :types}]
  (list* 'U (doall (map unparse-type types))))

(defmethod unparse-type Intersection
  [{types :types}]
  (list* (if (and (seq types)
                  (every? Function? types))
           'Fn
           'I)
         (doall (map unparse-type types))))

(defmethod unparse-type Function
  [{:keys [dom rng rest drest]}]
  (vec (concat (doall (map unparse-type dom))
               (when rest
                 [(unparse-type rest) '*])
               (when drest
                 (let [{:keys [pre-type bound]} drest]
                   [(unparse-type pre-type) '... (unparse-type bound)]))
               (let [{:keys [t fl o]} rng]
                 ['-> (unparse-type t)]))))

(defmethod unparse-type RClass
  [{the-class :the-class}]
  (symbol (.getName the-class)))

(defmethod unparse-type RInstance
  [{poly? :poly? constructor :constructor}]
  (if (empty? poly?)
    (unparse-type constructor)
    (list* (unparse-type constructor)
           (doall (map unparse-type poly?)))))

(defmethod unparse-type Mu
  [m]
  (let [nme (gensym "Mu")
        body (Mu-body* nme m)]
    (list 'Rec [nme] (unparse-type body))))

(defmethod unparse-type PolyDots
  [{:keys [nbound] :as p}]
  (let [{:keys [free-names dvar-name]
         :as given-names?}
        (if ((every-pred :free-names :dvar-name) (meta p))
          (meta p)
          nil)
        end-nme (if given-names?
                  *next-nme*
                  (+ nbound *next-nme*))
        fs (if given-names?
             (vec (concat free-names [dvar-name]))
             (vec 
               (for [x (range *next-nme* end-nme)]
                 (symbol (str "v" x)))))
        body (PolyDots-body* fs p)]
    (binding [*next-nme* end-nme]
      (list 'All (vec (concat (butlast fs) ['... (last fs)])) (unparse-type body)))))

(defmethod unparse-type Poly
  [{:keys [nbound] :as p}]
  (let [free-names (vec (-> p meta :free-names))
        end-nme (if free-names
                  *next-nme*
                  (+ nbound *next-nme*))
        fs (if free-names
             free-names
             (vec
               (for [x (range *next-nme* end-nme)]
                 (symbol (str "v" x)))))
        body (Poly-body* fs p)]
    (binding [*next-nme* end-nme]
      (list 'All fs (unparse-type body)))))

(defmethod unparse-type Value
  [v]
  (list 'Value (:val v)))

(defmethod unparse-type HeterogeneousMap
  [v]
  (list 'Map* (into {} (map (fn [[k v]]
                              (assert (Value? k))
                              (vector (:val k)
                                      (unparse-type v)))
                            (:types v)))))

(defmethod unparse-type HeterogeneousSeq
  [v]
  (list* 'Seq* (doall (map unparse-type (:types v)))))

(defmethod unparse-type HeterogeneousVector
  [v]
  (list* 'Vector* (doall (map unparse-type (:types v)))))

(defmethod unparse-type HeterogeneousList
  [v]
  (list* 'List* (doall (map unparse-type (:types v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting frees

(declare ^:dynamic *frees-mode* frees)

(defn fv-variances 
  "Map of frees to their variances"
  [t]
  (binding [*frees-mode* ::frees]
    (frees t)))

(defn fv 
  "All frees in type"
  [t]
  (keys (fv-variances t)))

(defn idxs 
  "All index variables in type (dotted bounds, etc.)"
  [t]
  (binding [*frees-mode* ::idxs]
    (keys (frees t))))

(def variance-map?
  #(and (every? (some-fn F? B?) (keys %))
        (every? variance? (vals %))))

(defn flip-variances [vs]
  {:pre [(variance-map? vs)]}
  (into {} (for [[k vari] vs]
             [k (case vari
                  :covariant :contravariant
                  :contravariant :covariant
                  vari)])))

(defn combine-frees [& frees]
  {:pre [(every? variance-map? frees)]
   :post [variance-map?]}
  (apply merge-with (fn [old-vari new-vari]
                      (cond 
                        (= old-vari new-vari) old-vari
                        (= old-vari :dotted) new-vari
                        (= new-vari :dotted) old-vari
                        (= old-vari :constant) new-vari
                        (= new-vari :constant) old-vari
                        :else :invariant))
         frees))

(derive ::frees ::any-var)
(derive ::idxs ::any-var)

(def ^:dynamic *frees-mode*)
(set-validator! #'*frees-mode* #(or (= ::frees %)
                                    (= ::idxs %)))

(defmulti frees (fn [t]
                  {:post [variance-map?]}
                  [*frees-mode* (class t)]))

(defmethod frees [::any-var FilterSet]
  [{:keys [then else]}]
  (combine-frees (frees then)
                 (frees else)))

(defmethod frees [::any-var TypeFilter]
  [{:keys [type]}] 
  (frees type))

(defmethod frees [::any-var NotTypeFilter]
  [{:keys [type]}] 
  (frees type))

(defmethod frees [::any-var AndFilter]
  [{:keys [a c]}] 
  (combine-frees (frees a)
                 (frees c)))

(defmethod frees [::any-var F] 
  [t] 
  (combine-frees {t :covariant}
                 (frees (:upper-bound t))
                 (frees (:lower-bound t))))

(defmethod frees [::any-var Nil] [t] {})
(defmethod frees [::any-var True] [t] {})
(defmethod frees [::any-var False] [t] {})
(defmethod frees [::any-var Value] [t] {})
(defmethod frees [::any-var Top] [t] {})

(defmethod frees [::any-var Intersection]
  [{:keys [types]}] 
  (apply combine-frees (mapv frees types)))

(defmethod frees [::any-var Union]
  [{:keys [types]}]
  (apply combine-frees (mapv frees types)))

(defmethod frees [::frees Function]
  [{:keys [dom rng rest drest kws]}]
  (apply combine-frees (concat (mapv (comp flip-variances frees)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               [(vals kws)])))
                               [(frees rng)]
                               (when drest
                                 [(dissoc (-> (:pre-type drest) frees flip-variances)
                                          (:bound drest))]))))

(defmethod frees [::idxs Function]
  [{:keys [dom rng rest drest kws]}]
  (apply combine-frees (concat (mapv (comp flip-variances frees)
                                     (concat dom
                                             (when rest
                                               [rest])
                                             (when kws
                                               (vals kws))))
                               [(frees rng)]
                               (when drest
                                 [{(:bound drest) :contravariant}
                                  (-> (:pre-type drest)
                                    frees flip-variances)]))))

(defmethod frees [::any-var RInstance]
  [t]
  (apply combine-frees (mapv frees (:poly? t))))

(defmethod frees [::any-var Poly]
  [{:keys [nbound scope]}]
  (frees scope))

(defmethod frees [::any-var PolyDots]
  [{:keys [nbound scope]}]
  (frees scope))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Elim

(defmulti promote 
  "Eliminate all variables V in t by promotion"
  (fn [T V] 
    {:pre [(set? V)
           (every? F? V)]
     :post [set?
            (every? F? %)]}
    (class T)))

(defmulti demote 
  "Eliminate all variables V in T by demotion"
  (fn [T V]
    {:pre [(set? V)
           (every? F? V)]
     :post [set?
            (every? F? %)]}
    (class T)))

(defmethod promote F
  [T V]
  (if (V T)
    (:upper-bound T)
    T))

(defmethod demote F
  [T V]
  (if (V T)
    (:lower-bound T)
    T))

(defmethod promote HeterogeneousMap
  [T V]
  (-> T
    (update-in [:types] #(into {}
                               (for [[k v] %]
                                 [k (promote v V)])))))

(defmethod demote HeterogeneousMap
  [T V]
  (-> T
    (update-in [:types] #(into {}
                               (for [[k v] %]
                                 [k (demote v V)])))))

(defmethod promote HeterogeneousVector
  [T V]
  (-> T
    (update-in [:types] #(apply list (map promote % (repeat V))))))

(defmethod demote HeterogeneousVector
  [T V]
  (-> T
    (update-in [:types] #(apply list (map demote % (repeat V))))))

(defmethod promote HeterogeneousList
  [T V]
  (-> T
    (update-in [:types] #(apply list (map promote % (repeat V))))))

(defmethod demote HeterogeneousList
  [T V]
  (-> T
    (update-in [:types] #(apply list (map demote % (repeat V))))))

(defmethod promote False [T V] T)
(defmethod promote Nil [T V] T)
(defmethod promote Value [T V] T)

(defmethod demote False [T V] T)
(defmethod demote Nil [T V] T)
(defmethod demote Value [T V] T)

(defmethod promote Union 
  [T V] 
  (-> T
    (update-in [:types] #(mapv promote % (repeat V)))))

(defmethod demote Union 
  [T V] 
  (-> T
    (update-in [:types] #(mapv demote % (repeat V)))))

(defmethod promote Intersection
  [T V] 
  (-> T
    (update-in [:types] #(mapv promote % (repeat V)))))

(defmethod demote Intersection
  [T V] 
  (-> T
    (update-in [:types] #(mapv demote % (repeat V)))))

(defmethod promote Poly
  [{:keys [nbound] :as T} V]
  (let [names (repeatedly nbound gensym)
        pmt-body (promote (Poly-body* names T) V)]
    (Poly* names pmt-body)))

(defmethod demote Poly
  [{:keys [nbound] :as T} V]
  (let [names (repeatedly nbound gensym)
        dem-body (demote (Poly-body* names T) V)]
    (Poly* names dem-body)))

(defmethod promote Function
  [{:keys [dom rng rest drest kws] :as T} V]
  (let [pmt #(promote % V)
        dmt #(demote % V)
        dmt-kw #(into {} (for [[k v] %]
                           [k (dmt v)]))]
    (cond 
      ;if filter contains V, give up
      (seq (set/intersection V (:fl rng))) (->TopFunction)

      ;if dotted bound is in V, transfer to rest args
      (and drest (V (:bound drest)))
      (-> T
        (update-in [:dom] #(mapv dmt %))
        (update-in [:rng] pmt)
        (assoc :rest (dmt (:pre-type drest)))
        (assoc :drest nil)
        (assoc :kws (when kws
                      (-> kws
                        (update-in [:mandatory] dmt-kw)
                        (update-in [:optional] dmt-kw)))))

      :else
      (-> T
        (update-in [:dom] #(mapv dmt %))
        (update-in [:rng] pmt)
        (update-in [:rest] #(when %
                              (dmt %)))
        (update-in [:drest] #(when %
                               (-> %
                                 (update-in [:pre-type] dmt))))
        (update-in [:kws] #(when %
                             (-> %
                               (update-in [:mandatory] dmt-kw)
                               (update-in [:optional] dmt-kw))))))))

(defmethod demote Function
  [{:keys [dom rng rest drest kws] :as T} V]
  (let [pmt #(promote % V)
        dmt #(demote % V)
        pmt-kw #(into {} (for [[k v] %]
                           [k (pmt v)]))]
    (cond 
      ;if filter contains V, give up
      (seq (set/intersection V (:fl rng))) (->TopFunction)

      ;if dotted bound is in V, transfer to rest args
      (and drest (V (:bound drest)))
      (-> T
        (update-in [:dom] #(mapv pmt %))
        (update-in [:rng] dmt)
        (assoc :rest (pmt (:pre-type drest)))
        (assoc :drest nil)
        (assoc :kws (when kws
                      (-> kws
                        (update-in [:mandatory] pmt-kw)
                        (update-in [:optional] pmt-kw)))))

      :else
      (-> T
        (update-in [:dom] #(mapv pmt %))
        (update-in [:rng] dmt)
        (update-in [:rest] #(when %
                              (pmt %)))
        (update-in [:drest] #(when %
                               (-> %
                                 (update-in [:pre-type] pmt))))
        (update-in [:kws] #(when %
                             (-> %
                               (update-in [:mandatory] pmt-kw)
                               (update-in [:optional] pmt-kw))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Generation

(def Constraint ::constraint)

(defn Constraint? [a]
  (isa? a Constraint))

(defn declare-constraint [a]
  (derive a Constraint))

(defrecord EqConstraint [type]
  "A type constraint that must equal exact type"
  [(Type? type)])
(defrecord SubConstraint [lower upper]
  "A type constraint within an upper and lower bound"
  [(Type? lower)
   (Type? upper)])

(declare-constraint EqConstraint)
(declare-constraint SubConstraint)

(defmulti intersect-cs-fn 
  "Intersects two constraints"
  (fn [a b] [(class a) (class b)]))

(defmethod intersect-cs-fn [EqConstraint EqConstraint]
  [a b]
  (assert (= a b))
  a)

(defmethod intersect-cs-fn [EqConstraint SubConstraint]
  [a b]
  (assert (and (subtype? (:lower b) (:type a))
               (subtype? (:type a) (:upper b))))
  a)

(defmethod intersect-cs-fn [SubConstraint EqConstraint]
  [b a]
  (assert (and (subtype? (:lower b) (:type a))
               (subtype? (:type a) (:upper b))))
  a)

(defmethod intersect-cs-fn [SubConstraint SubConstraint]
  [a b]
  (let [j (Un (:lower a) (:lower b))
        m (In (:upper a) (:upper b))]
    (->SubConstraint j m)))

(defn intersect-cs 
  "Intersect a number of constraints"
  [& cs]
  (merge-with intersect-cs-fn cs))

(defn empty-cs 
  "Returns a constraint set constraining variables xs to
  Bot <: x <: Top"
  [xs]
  (zipmap xs (repeat (->SubConstraint (Bottom) (->Top)))))

(defn singleton-cs 
  "Returns a constraint set constraining variables xs to
  Bot <: x <: Top and constraining variable v to constraint c"
  [xs v c]
  (intersect-cs (empty-cs xs) {v c}))

;current seen subtype relations, for recursive types
;(Set [Type Type])
(def ^:dynamic *cs-current-seen* #{})

;; V : a set of variables not to mention in the constraints
;; X : the set of type variables to be constrained
;; Y : the set of index variables to be constrained
;; S : a type to be the subtype of T
;; T : a type
;; produces a cset which determines a substitution that makes S a subtype of T
;; implements the V |-_X S <: T => C judgment from Pierce+Turner, extended with
;; the index variables from the TOPLAS paper
(defmulti cs-gen*
  (fn [V X Y S T] [(class S) (class T)]))

;cs-gen calls cs-gen*, remembering the current subtype for recursive types
; Add methods to cs-gen*, but always call cs-gen

(defn cs-gen [V X Y S T]
  (cond 
    ;already been around this loop, is a subtype
    (*cs-current-seen* [S T]) 
    (empty-cs X)

    ;already a subtype
    (subtype? S T)
    (empty-cs X)

    (Top? T)
    (empty-cs X)

    ;constrain *each* element of S to be below T, and then combine the constraints
    (Union? S)
    (apply intersect-cs 
           (empty-cs X)
           (mapv #(cs-gen V X Y % T) (:types S)))

    ;; find *an* element of S which can be made to be a supertype of S
    (Union? T)
    (if-let [cs (some #(try (cs-gen V X Y S %)
                         (catch IllegalArgumentException e
                           (throw e))
                         (catch Exception e)) ;TODO specialised data Exceptions
                      (:types T))]
      cs
      (throw (Exception. "")))

    ;; find *an* element of S which can be made to be a subtype of T
    (Intersection? S)
    (if-let [cs (some #(try (cs-gen V X Y % T)
                         (catch IllegalArgumentException e
                           (throw e))
                         (catch Exception e)) ;TODO specialised data Exceptions
                      (:types T))]
      cs
      (throw (Exception. "")))

    ;constrain *each* element of T to be above S, and then combine the constraints
    (Intersection? T)
    (apply intersect-cs 
           (empty-cs X)
           (mapv #(cs-gen V X Y S %) (:types T)))

    :else
    (binding [*cs-current-seen* (conj *cs-current-seen* [S T])]
      (cs-gen* V X Y S T))))

(defmethod cs-gen* :default
  [V X Y S T]
  (assert (subtype? S T))
  (empty-cs X))

(defmethod cs-gen* [Type Top] 
  [V X Y S T] 
  (empty-cs X))

(defmethod cs-gen* [RInstance RInstance] 
  [V X Y S T]
  (assert (= (:constructor S) (:constructor T)))
  (assert (= (count (:poly? S))
             (count (:poly? T))))
  (apply intersect-cs
         (empty-cs X)
         (for [[vari si ti] (map vector
                                 (-> T :constructor :variances)
                                 (:poly? S)
                                 (:poly? T))]
           (case vari
             (:covariant :constant) (cs-gen V X Y si ti)
             :contravariant (cs-gen V X Y ti si)
             :invariant (intersect-cs (cs-gen V X Y si ti)
                                      (cs-gen V X Y ti si))))))

(defmethod cs-gen* [F Type]
  [V X Y S T]
  (assert (X S))
  (assert (empty? (set/intersection X (fv T))))
  (let [dt (demote T)]
    (singleton-cs X S (->SubConstraint (Bottom) dt))))

(defmethod cs-gen* [Type F]
  [V X Y S T]
  (assert (X T))
  (assert (empty? (set/intersection X (fv S))))
  (let [ps (promote S)]
    (singleton-cs X T (->SubConstraint ps (->Top)))))

(defmethod cs-gen* [F F]
  [V X Y S T]
  (assert (= S T))
  (empty-cs X))

(defmethod cs-gen* [Function Function]
  [V X Y S T]
  (assert (= (count (:dom S))
             (count (:dom T))))
  (assert (= (boolean (:rest S))
             (boolean (:rest T))))
  (assert (not (or (:drest S)
                   (:drest T))))
  (apply intersect-cs (concat (map #(cs-gen V X Y %1 %2) (:dom T) (:dom S))
                              [(cs-gen V X Y (:rng S) (:rng T))]
                              (when (:rest S)
                                [(cs-gen V X Y (:rest T) (:rest S))]))))
(comment
;; V : a set of variables not to mention in the constraints
;; X : the set of type variables to be constrained
;; Y : the set of index variables to be constrained
;; S : a list of types to be the subtypes of T
;; T : a list of types
;; expected-cset : a cset representing the expected type, to meet early and
;;  keep the number of constraints in check. (empty by default)
;; produces a cset which determines a substitution that makes the Ss subtypes of the Ts
(defn cs-gen-list [V X Y S T & {:keys [expected-cset] :or {expected-cset (empty-cs #{})}}]
  {:pre [(every? F? [V X Y])
         (every? Type? [S T])]}
  (assert (= (count S) (count T)))
  (apply intersect-cs 
         ;; We meet (intersect) early to prune the csets to a reasonable size.
         ;; This weakens the inference a bit, but sometimes avoids
         ;; constraint explosion.
         (map #(intersect-cs (cgen V X Y %1 %2)) expected-cset)))

;; X : variables to infer
;; Y : indices to infer
;; S : actual argument types
;; T : formal argument types
;; R : result type
;; expected : #f or the expected type
;; returns a substitution
;; if R is #f, we don't care about the substituion
;; just return a boolean result
(defn infer [X Y S T R & [expected]]
  (let [expected-cset (if expected
                        (cs-gen #{} X Y R expected)
                        (empty-cs #{}))
        cs (cs-gen-list #{} X Y S T :expected-cset expected-cset)
        cs* (intersect-cs cs expected-cset)]
    (if R
      (subst-gen cs* Y R)
      true)
))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable rep

(defn add-scopes [n t]
  "Wrap type in n Scopes"
  {:pre [(nat? n)
         (Type? t)]}
  (doall
    (last 
      (take (inc n) (iterate ->Scope t)))))

(defn remove-scopes [n sc]
  "Unwrap n Scopes"
  {:pre [(nat? n)
         (Scope? sc)]
   :post [Type?]}
  (doall
    (last
      (take (inc n) (iterate (fn [t]
                               (assert (Scope? t) "Tried to remove too many Scopes")
                               (:body t))
                             sc)))))

(defmulti name-to
  "Convert a name in the type to de Bruijn index of res,
  where n is the position in the current binder, and outer is the
  number of indices previously bound"
  (fn [ty name res]
    {:pre [((some-fn Type? Function?) ty)
           (symbol? name)
           (nat? res)]}
    (class ty)))

(defmethod name-to B [ty name res] ty)

(defmethod name-to F
  [{name* :name upper :upper-bound lower :lower-bound :as ty} name res]
  (if (= name name*)
    (->B res upper lower)
    ty))

(defmethod name-to Function
  [f name res]
  (assert (NoFilter? (-> f :rng :fl)))
  (assert (NoObject? (-> f :rng :o)))
  (let [ufn #(name-to % name res)]
    (-> f
      (update-in [:dom] #(doall (map ufn %)))
      (update-in [:rng :t] ufn)
      (update-in [:rest] #(when %
                            (ufn %)))
      (update-in [:drest] (fn [drest]
                            (when drest
                              (-> drest
                                (update-in [:pre-type] #(when %
                                                      (ufn %)))
                                (update-in [:bound] #(when %
                                                       (ufn %))))))))))

(defmethod name-to Top [t name res] t)
(defmethod name-to Nil [t name res] t)
(defmethod name-to True [t name res] t)
(defmethod name-to False [t name res] t)
(defmethod name-to Value [t name res] t)

(defmethod name-to HeterogeneousMap
  [t name res]
  (let [up #(name-to % name res)]
    (-> t
      (update-in [:types] #(into {} (for [[k v] %]
                                      [(up k) (up v)]))))))

(defmethod name-to HeterogeneousVector
  [t name res]
  (let [up #(name-to % name res)]
    (-> t
      (update-in [:types] #(mapv up %)))))

(defmethod name-to Intersection
  [{:keys [types]} name res]
  (apply In (doall (map #(name-to % name res) types))))

(defmethod name-to Union
  [{:keys [types]} name res]
  (apply Un (doall (map #(name-to % name res) types))))

;(defmethod name-to RClass
;  [{:keys [variances the-class replacements]} name res]
;  (->RClass variances
;            the-class 
;            (into {} (for [[k v] replacements]
;                       (let [v (remove-scopes (count variances) v)]
;                         [k (name-to v name (+ res (count variances)))])))))

(defmethod name-to RInstance
  [{:keys [poly? constructor]} name res]
  (->RInstance (doall (map #(name-to % name res) poly?))
               constructor)) ;should this call name-to?

(defmethod name-to Poly
  [{n :nbound scope :scope} name res]
  (let [body (remove-scopes n scope)]
    (->Poly n (add-scopes n (name-to body name (+ n res))))))

(defn- rev-indexed 
  "'(a b c) -> '([2 a] [1 b] [0 c])"
  [c]
  (map vector (iterate dec (dec (count c))) c))

(defn abstract-many 
  "Names Type -> Scope^n  where n is (count names)"
  [names ty]
  {:pre [(every? symbol? names)
         (Type? ty)]}
  (let [n (count names)]
    (->> 
      ;convert each given name to a bound de Bruijn index
      (reduce (fn [ty [cnt name]]
                (name-to ty name cnt))
              ty
              (rev-indexed names))
      ;then wrap n scopes
      (add-scopes n))))

(defmulti replace-image
  "Replace all bound variables with index target, with
  the free variable image, keeping bound variable's upper/lower bounds"
  (fn [type image target]
    {:pre [((some-fn Type? Function?) type)
           (F? image)
           (nat? target)]}
    (class type)))

(defmethod replace-image F [ty image target] ty)

(defmethod replace-image B
  [{idx :idx upper :upper-bound lower :lower-bound :as ty} image target]
  (if (= idx target)
    (assoc image
           :upper-bound upper
           :lower-bound lower)
    ty))

(defmethod replace-image Union
  [{types :types} image target]
  (apply Un (doall (map #(replace-image % image target) types))))

(defmethod replace-image Nil [t image target] t)
(defmethod replace-image Top [t image target] t)
(defmethod replace-image False [t image target] t)
(defmethod replace-image True [t image target] t)
(defmethod replace-image Value [t image target] t)

(defmethod replace-image HeterogeneousMap
  [t image target]
  (let [up #(replace-image % image target)]
    (-> t
      (update-in [:types] #(into {} (for [[k v] %]
                                      [(up k) (up v)]))))))


(defmethod replace-image HeterogeneousVector
  [t image target]
  (let [up #(replace-image % image target)]
    (-> t
      (update-in [:types] #(mapv up %)))))

(defmethod replace-image RInstance
  [r image target]
  (let [ufn #(replace-image % image target)]
    (-> r
      (update-in [:poly?] #(when %
                             (doall (map ufn %)))))))

(defn Result-replace-image
  "Result is not a type"
  [r image target]
  (-> r
    (update-in [:t] #(replace-image % image target))))

(defmethod replace-image Function
  [f image target]
  (let [ufn #(replace-image % image target)]
    (-> f
      (update-in [:dom] #(doall (map ufn %)))
      (update-in [:rng] #(Result-replace-image % image target))
      (update-in [:rest] #(when %
                            (ufn %)))
      (update-in [:drest] (fn [drest]
                            (when drest
                              (-> drest
                                (update-in [:pre-type] ufn)
                                (update-in [:bound] ufn))))))))

(defmethod replace-image Intersection
  [{types :types} image target]
  (apply In (doall (map #(replace-image % image target) types))))

(defmethod replace-image Poly
  [{scope :scope n :nbound :as ty} image target]
  (let [body (remove-scopes n scope)]
    (assoc ty :scope (add-scopes n (replace-image body image (+ target n))))))

(defn instantiate-many 
  "instantiate-many : List[Type] Scope^n -> Type
  where n is the length of types
  all of the types MUST be Fs"
  [images sc]
  {:pre [(every? F? images)
         (Scope? sc)]}
  (let [n (count images)]
    (reduce (fn [ty [cnt image]]
              (replace-image ty image cnt))
            (remove-scopes n sc)
            (rev-indexed images))))

(defn abstract [name ty]
  "Make free name bound"
  {:pre [(symbol? name)
         (Type? ty)]}
  (abstract-many [name] ty))

(defn instantiate [f sc]
  "Instantiate bound name to free"
  {:pre [(F? f)
         (Scope? sc)]}
  (instantiate-many [f] sc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable substitution

(declare subtype)

;Substitute in target all free variables called name with image
(defmulti substitute (fn [target image name] 
                       {:pre [((some-fn Type? Function?) target)
                              (Type? image)
                              (symbol? name)]}
                       (class target)))

(defn substitute-many [target images names]
  (reduce (fn [t [im nme]] (substitute t im nme))
          target
          (map vector images names)))

(defmethod substitute F
  [{name* :name :keys [upper-bound lower-bound] :as f} image name]
  (if (= name* name)
    image
    f))

(defmethod substitute Nil [t image name] t)
(defmethod substitute Top [t image name] t)
(defmethod substitute Value [t image name] t)
(defmethod substitute False [t image name] t)
(defmethod substitute True [t image name] t)

(defmethod substitute RInstance
  [rinst image name]
  (let [sub #(substitute % image name)]
    (-> rinst
      (update-in [:poly?] #(when %
                             (doall (map sub %)))))))

(defmethod substitute HeterogeneousMap
  [t image name]
  (let [sub #(substitute % image name)]
    (-> t
      (update-in [:types] #(into {} (for [[k v] %]
                                      [(sub k) (sub v)]))))))

(defmethod substitute HeterogeneousVector
  [t image name]
  (let [sub #(substitute % image name)]
    (-> t
      (update-in [:types] #(mapv sub %)))))

(defmethod substitute Union
  [u image name]
  (let [sub #(substitute % image name)]
    (-> u
      (update-in [:types] #(doall (map sub %))))))

(defmethod substitute Intersection
  [i image name]
  (let [sub #(substitute % image name)]
    (-> i
      (update-in [:types] #(doall (map sub %))))))

(defmethod substitute Function
  [f image name]
  ;what to do otherwise?
  (assert (NoFilter? (-> f :rng :fl)))
  (assert (NoObject? (-> f :rng :o)))
  (let [sub #(substitute % image name)]
    (-> f
      (update-in [:dom] #(doall (map sub %)))
      (update-in [:rng :t] sub)
      (update-in [:rest] #(when %
                            (sub %)))
      (update-in [:drest] #(when %
                             (update-in % [0] sub)))))) ;dont substitute bound

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotted pre-type expansion

;tdr from Practical Variable-Arity Polymorphism paper
; Expand out dotted pretypes to fixed domain, using types bm, if (:name bound) = b
(defmulti trans-dots (fn [t b bm]
                       {:pre [((some-fn Type? Function?) t)
                              (symbol? b)
                              (every? Type? bm)]}
                       (class t)))

(defmethod trans-dots F [t b bm] t)
(defmethod trans-dots RInstance [t b bm] t)

(defmethod trans-dots Intersection
  [t b bm]
  (let [tfn #(trans-dots % b bm)]
    (-> t
      (update-in [:types] #(doall (map tfn %))))))

(defmethod trans-dots Function
  [t b bm]
  ;how to handle filters?
  (assert (NoFilter? (-> t :rng :fl)))
  (assert (NoObject? (-> t :rng :o)))
  (let [tfn #(trans-dots % b bm)]
    (cond
      (:drest t)
      (let [{:keys [pre-type bound]} (:drest t)]
        (assert (F? bound))
        (if (= b (:name bound)) ;identical bounds
          (let [dom (concat 
                        ;keep fixed domain
                        (doall (map tfn (:dom t)))
                        ;expand dotted type to fixed domain
                        (doall (map (fn [bk]
                                      {:post [(Type? %)]}
                                      ;replace free occurences of bound with bk
                                      (-> (substitute pre-type bk b)
                                        tfn))
                                    bm)))]
            (->Function dom
                        (update-in (:rng t) [:t] tfn)
                        nil
                        nil ;dotted pretype now expanded to fixed domain
                        nil))
          (-> t
            (update-in [:dom] #(doall (map tfn %)))
            (update-in [:rng] tfn)
            (update-in [:drest] (fn [drest]
                                  (when drest
                                    (-> drest
                                      (update-in [:pre-type] tfn)))))))) ;translate pre-type
      :else
      (-> t
        (update-in [:dom] #(doall (map tfn %)))
        (update-in [:rng] tfn)
        (update-in [:rest] #(when %
                              (tfn %)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic type instantiation

(defn manual-inst 
  "Poly Type^n -> Type
  Substitute the type parameters of the polymorphic type
  with given types"
  [ptype argtys]
  {:pre [((some-fn Poly? PolyDots?) ptype)
         (every? Type? argtys)]}
  (cond
    (Poly? ptype)
    (let [_ (assert (= (:nbound ptype) (count argtys)) "Wrong number of arguments to instantiate polymorphic type")
          names (repeatedly (:nbound ptype) gensym)
          body (Poly-body* names ptype)]
      (substitute-many body argtys names))

    (PolyDots? ptype)
    (let [nrequired-types (dec (:nbound ptype))
          _ (assert (<= nrequired-types (count argtys)) "Insufficient arguments to instantiate dotted polymorphic type")
          names (repeatedly (:nbound ptype) gensym)
          body (PolyDots-body* names ptype)]
      (-> body
        ; expand dotted pre-types in body
        (trans-dots (last names) ;the bound
                    (drop (dec (:nbound ptype)) argtys)) ;the types to expand pre-type with
        ; substitute normal variables
        (substitute-many (take nrequired-types argtys) (butlast names))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtype

(defn type-error [s t]
  (throw (Exception. (str (with-out-str (pr (unparse-type s)))
                          " is not a subtype of: " 
                          (with-out-str (pr (unparse-type t)))))))

;keeps track of currently seen subtype relations for recursive types.
;(Set [Type Type])
(def ^:dynamic *sub-current-seen* #{})

;subtype and subtype? use *sub-current-seen* for remembering types (for Rec)
;subtypeA* takes an extra argument, called by subtype
;subtype* shouldn't be called directly, is called by subtypeA*
;
; In short, only call subtype (or subtype?)

(defmulti subtype* (fn [s t] [(class s) (class t)]))

(defn subtype? [s t]
  (try 
    (subtype s t)
    true
    (catch IllegalArgumentException e
      (throw e))
    (catch Exception e
      false)))

(declare subtypeA*)

(defn subtypeA*? [A s t]
  (try (subtypeA* A s t)
    true
    (catch IllegalArgumentException e
      (throw e))
    (catch Exception e
      false)))

(defn subtypeA* [A s t]
  (cond
    (or (contains? A [s t])
        (= s t))
    A

    (Union? s)
    (binding [*sub-current-seen* (conj A [s t])]
      (if (every? #(subtype? % t) (:types s))
        *sub-current-seen*
        (type-error s t)))

    (Union? t)
    (binding [*sub-current-seen* (conj A [s t])]
      (if (some #(subtype? s %) (:types t))
        *sub-current-seen*
        (type-error s t)))

    (Intersection? s)
    (binding [*sub-current-seen* (conj A [s t])]
      (if (some #(subtype? % t) (:types s))
        *sub-current-seen*
        (type-error s t)))

    (Intersection? t)
    (binding [*sub-current-seen* (conj A [s t])]
      (if (every? #(subtype? s %) (:types s))
        *sub-current-seen*
        (type-error s t)))

    :else
    (binding [*sub-current-seen* (conj A [s t])]
      (subtype? s t))))

(defn subtype [s t]
  (subtypeA* *sub-current-seen* s t))

(defn- subtype-rclass
  [{variancesl :variances classl :the-class replacementsl :replacements :as s}
   {variancesr :variances classr :the-class replacementsr :replacements :as t}]
  (cond
    ;easy case
    (and (empty? variancesl)
         (empty? variancesr)
         (empty? replacementsl)
         (empty? replacementsr))
    (if (isa? classl classr)
      *sub-current-seen*
      (type-error s t))))

; (Cons Integer) <: (Seqable Integer)
; (ancestors (Seqable Integer)

(defmethod subtype* [Value RInstance]
  [{val :val} t]
  (let [cls (class val)]
    (subtype (->RInstance nil (or (@RESTRICTED-CLASS cls)
                                  (->RClass nil cls {})))
             t)))

(defn- RInstance-supers* 
  "Return a set of Types that are the super-Types
  of this RInstance"
  [{:keys [poly? constructor] :as rinst}]
  {:pre [(RInstance? rinst)]
   :post [(every? Type? %)
          (<= (count (filter (some-fn Fn-Intersection? Poly? PolyDots?) %))
              1)]}
  (let [names (map gensym (range (count poly?)))
        rplce (RClass-replacements* names constructor)
        rplce-subbed (into {} (for [[k v] rplce]
                                [k (substitute-many v poly? names)]))
        ancest (supers (:the-class constructor))
        not-replaced (set/difference ancest (keys rplce-subbed))
        super-types (set/union (set (for [t not-replaced]
                                      (->RInstance nil (or (when-let [r (@RESTRICTED-CLASS t)]
                                                             (assert (empty? (:variances r))
                                                                     (str "RClass " (unparse-type r) " must be instantiated"
                                                                          " in " t))
                                                             r)
                                                           (->RClass nil t {})))))
                               (vals rplce-subbed))]
    super-types))

(defn- subtype-rinstance-common-base 
  [{polyl? :poly? constl :constructor :as s}
   {polyr? :poly? constr :constructor :as t}]
  {:pre [(= constl constr)]}
  (let [{variances :variances} constl]
    (every? identity
            (doall (map #(case %1
                           :covariant (subtype? %2 %3)
                           :contravariant (subtype? %3 %2)
                           (= %2 %3))
                        variances
                        polyl?
                        polyr?)))))

(defmethod subtype* [RInstance RInstance]
  [{polyl? :poly? constl :constructor :as s}
   {polyr? :poly? constr :constructor :as t}]
  (cond 
    (or ;same base class
        (and (= constl constr)
             (subtype-rinstance-common-base s t))

        ;find a supertype of s that is the same base as t, and subtype of it
        (some #(and (= constr (:constructor %))
                    (subtype-rinstance-common-base % t))
              (RInstance-supers* s)))
    *sub-current-seen*

    ;try each ancestor

    :else (type-error s t)))

(prefer-method subtype* 
               [Type Mu]
               [HeterogeneousMap Type])

(defmethod subtype* [HeterogeneousMap Type]
  [s t]
  (let [sk (apply Un (map first (:types s)))
        sv (apply Un (map second (:types s)))]
    (subtype (->RInstance [sk sv] (@RESTRICTED-CLASS IPersistentMap))
             t)))


;every rtype entry must be in ltypes
;eg. {:a 1, :b 2, :c 3} <: {:a 1, :b 2}
(defmethod subtype* [HeterogeneousMap HeterogeneousMap]
  [{ltypes :types :as s}
   {rtypes :types :as t}]
  (last (doall (map (fn [[k v]]
                      (subtype (ltypes k) v))
                    rtypes))))

(defmethod subtype* [HeterogeneousVector HeterogeneousVector]
  [{ltypes :types :as s} 
   {rtypes :types :as t}]
  (last (doall (map #(subtype %1 %2) ltypes rtypes))))

(defmethod subtype* [HeterogeneousVector Type]
  [s t]
  (let [ss (apply Un (:types s))]
    (subtype (->RInstance [ss] (@RESTRICTED-CLASS IPersistentVector))
             t)))

(defmethod subtype* [Function Function]
  [s t]
  (assert (not ((some-fn :rest :drest) s)))
  (assert (not ((some-fn :rest :drest) t)))
  (assert (= (count (:dom s))
             (count (:dom t))))
  (doall (map #(subtype %1 %2) (:dom t) (:dom s)))
  (subtype (:rng s) (:rng t))
  *sub-current-seen*)

(defmethod subtype* [Mu Type]
  [s t]
  (let [s* (unfold s)]
    (subtype s* t)))

(defmethod subtype* [Type Mu]
  [s t]
  (let [t* (unfold t)]
    (subtype s t*)))

(defmethod subtype* :default
  [s t]
  (if (Top? t)
    *sub-current-seen*
    (type-error s t)))

(defmacro sub [s t]
  `(subtype (parse-type '~s)
            (parse-type '~t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Altered Classes

(alter-class Seqable [[a :variance :covariant]])

(alter-class IPersistentCollection [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)})

(alter-class ISeq [[a :variance :covariant]]
             :replace
             {Seqable (Seqable a)
              IPersistentCollection (IPersistentCollection a)})

(alter-class ILookup [[a :variance :invariant]
                      [b :variance :covariant]])

(alter-class IPersistentSet [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)})

(alter-class Associative [[a :variance :invariant]
                          [b :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection Any)
              Seqable (Seqable Any)
              ILookup (ILookup a b)})

(alter-class IMapEntry [[a :variance :covariant]
                        [b :variance :covariant]])

(alter-class IPersistentMap [[a :variance :covariant]
                             [b :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection (IMapEntry a b))
              Seqable (Seqable (IMapEntry a b))
              ILookup (ILookup a b)
              Associative (Associative a b)})

(alter-class ASeq [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              ISeq (ISeq a)})

(alter-class IPersistentStack [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)})

(alter-class IPersistentVector [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentStack (IPersistentStack a)
              ILookup (ILookup Number a)
              Associative (Associative Number a)})

(alter-class APersistentVector [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentVector (IPersistentVector a)
              IFn [Number -> a]
              IPersistentStack (IPersistentStack a)
              ILookup (ILookup Number a)
              Associative (Associative Number a)})

(alter-class PersistentVector [[a :variance :covariant]]
             :replace
             {APersistentVector (APersistentVector a)
              IPersistentCollection (IPersistentCollection a)
              Seqable (Seqable a)
              IPersistentVector (IPersistentVector a)
              IFn [Number -> a]
              IPersistentStack (IPersistentStack a)
              ILookup (ILookup Number a)
              Associative (Associative Number a)})

(alter-class Cons [[a :variance :covariant]]
             :replace
             {IPersistentCollection (IPersistentCollection a)
              ASeq (ASeq a)
              Seqable (Seqable a)
              ISeq (ISeq a)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type annotations

(ann clojure.core/*ns* Namespace)
(ann clojure.core/namespace [(U Symbol String Keyword) -> (U nil String)])
(ann clojure.core/ns-name [Namespace -> Symbol])
(ann clojure.core/in-ns [Symbol -> nil])
(ann clojure.core/import [(IPersistentCollection Symbol) -> nil])

(ann clojure.core/+ [Number * -> Number])

;(ann clojure.core/swap! (All [x b ...] 
;                             [(Atom x) [x b ... b -> x] b ... b -> x]))

(ann clojure.core/symbol
     (Fn [(U Symbol String) -> Symbol]
         [String String -> Symbol]))

(ann clojure.core/seq?  (Fn [Any -> (U false true)]))
(ann clojure.core/number?  (Fn [Any -> (U false true)]))

(ann clojure.core/string?
     (Fn [Any -> (U false true)]))

(ann clojure.core/seq
     (All [x]
          [(Seqable x) -> (U nil (ASeq x))]))

;(ann clojure.core/seq
;     (All [x]
;          (Fn [(Seqable x) -> (U nil (ASeq x))
;               :- [x @ (first 0) | nil @ (first 0)]
;               Empty]
;              [nil -> nil
;               :- [ff | nil @ (first 0)]]
;              [String -> (U nil (ASeq Character))
;               :- [Character @ (first 0) | nil @ (first 0)]
;               Empty]
;              [(U java.util.Map Iterable) -> (U nil (ASeq Any))])))

(ann clojure.core/map
     (All [c a b ...]
          (Fn [[a b ... b -> c] (Seqable a) (Seqable b) ... b -> (Seqable c)])))

(ann clojure.core/reduce
     (All [a c]
          (Fn 
            [(Fn [c a -> c] [-> c]) (Seqable c) -> c]
            [[c a -> c] c (Seqable c) -> c])))

(comment
  (loop> [[x :- (Vector Number) [1 2 3]]]
    (if (seq x)           ; Number :- first(x) @ nil :- NonEmpty(x)
      (do (+ 1 (first x))
        (recur (rest x)))
      'yes))   ;!NonEmpty(x)
  )

(ann clojure.core/first
     (All [x]
          [(Seqable x) -> (U nil x)]))

;(ann clojure.core/first
;     (All [x]
;          (Fn [String -> (U nil Character)]
;              [(U java.util.Map Iterable) -> (U nil Any)]
;              [(U nil (Seqable x)) -> (U nil x)
;               :- [x @ (first 0) | nil @ (first 0)]
;               (first 0)])))

(ann clojure.core/conj
     (All [x y]
          (Fn [(IPersistentVector x) x -> (IPersistentVector x)]
              [(IPersistentMap x y) (U nil (IMapEntry x y) (Vector* x y)) -> (IPersistentMap x y)]
              [(IPersistentSet x) x -> (IPersistentSet x)]
              [(ISeq x) x -> (ASeq x)]
              [(IPersistentCollection Any) Any -> (IPersistentCollection Any)])))

(ann clojure.core/get
     (All [x]
          (Fn [(IPersistentSet x) Any -> (U nil x)]
              [java.util.Map Any -> (U nil Any)]
              [String Any -> (U nil Character)]
              [(U nil (ILookup Any x)) Any -> (U nil x)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker

(def expr-type ::expr-type)

(defmulti check (fn [expr & [expected]]
                  {:pre [((some-fn nil? Type?) expected)]}
                  (:op expr)))

(defn check-top-level [nsym form]
  (let [ast (analyze/analyze-form-in-ns nsym form)]
    (check ast)))

(defmacro tc-t [form]
  `(-> (check-top-level (symbol (ns-name *ns*))
                        '~form)
     expr-type))

(defmacro tc [form]
  `(-> (check-top-level (symbol (ns-name *ns*))
                        '~form)
     expr-type unparse-type))

(defmulti constant-type class)

(defmethod constant-type nil [_] (->Nil))
(defmethod constant-type Symbol [v] (->Value v))
(defmethod constant-type Long [v] (->Value v))
(defmethod constant-type Double [v] (->Value v))
(defmethod constant-type java.math.BigDecimal [v] (->Value v))
(defmethod constant-type clojure.lang.BigInt [v] (->Value v))
(defmethod constant-type String [v] (->Value v))
(defmethod constant-type Character [v] (->Value v))
(defmethod constant-type clojure.lang.Keyword [v] (->Value v))
(defmethod constant-type Boolean [v] (if v (->True) (->False)))

(defmethod constant-type IPersistentList
  [clist]
  (->HeterogeneousList (apply list (map constant-type clist))))

(defmethod constant-type IPersistentVector
  [cvec]
  (->HeterogeneousVector (mapv constant-type cvec)))

(defmethod constant-type IPersistentMap
  [cmap]
  (->HeterogeneousMap (into {} (map #(vector (constant-type (first %))
                                             (constant-type (second %)))
                                    cmap))))

(defn check-value
  [{:keys [val] :as expr} & [expected]]
  (let [actual-type (constant-type val)
        _ (when expected
            (subtype actual-type expected))]
    (assoc expr
           expr-type actual-type)))

(defmethod check :constant [& args] (apply check-value args))
(defmethod check :number [& args] (apply check-value args))
(defmethod check :string [& args] (apply check-value args))
(defmethod check :keyword [& args] (apply check-value args))

(defmethod check :nil 
  [expr & [expected]]
  (assoc expr
         expr-type (->Nil)))

(defmethod check :map
  [{:keys [keyvals] :as expr} & [expected]]
  (let [ckeyvals (mapv check keyvals)]
    (assert (every? Value? (map expr-type (keys (apply hash-map ckeyvals)))))
    (assoc expr
           expr-type (->HeterogeneousMap (apply hash-map (map expr-type ckeyvals))))))

(defmethod check :vector
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (mapv check args)]
    (assoc expr
           expr-type (->HeterogeneousVector (mapv expr-type cargs)))))

(defmethod check :empty-expr 
  [{coll :coll :as expr} & [expected]]
  (assoc expr
         expr-type (constant-type coll)))

; Type Type^n (U nil Type) -> Type
(defn check-app [fexpr-type arg-types expected]
  (assert (not expected) "TODO incorporate expected type")
  (cond
    ((some-fn Intersection? Function?) fexpr-type)
    (let [ftypes (if (Intersection? fexpr-type)
                   (:types fexpr-type)
                   [fexpr-type])
          _ (assert (every? Function? ftypes) "Must be intersection type containing Functions")
          ;find first 
          success-type (some (fn [{:keys [dom rest drest rng] :as current}]
                               (assert (not (or drest rest)) "TODO rest arg checking")
                               (and (= (count dom)
                                       (count arg-types))
                                    (every? true? (map subtype? arg-types dom))
                                    (if expected
                                      (subtype? rng expected)
                                      true)
                                    rng))
                             ftypes)
          _ (when-not success-type
              ;just report first function
              (throw (Exception.
                       (str "Cannot supply arguments " (with-out-str (pr (map unparse-type arg-types)))
                            " to Function " (with-out-str (pr (unparse-type fexpr-type)))
                            (when expected
                              (str " with expected type " (with-out-str (prn (unparse-type expected)))))))))]
      success-type)

    (Poly? fexpr-type)
    (throw (Exception. "Cannot infer arguments to polymorphic functions"))

    :else (throw (Exception. "Give up"))))

(defmethod check :var
  [{:keys [var] :as expr} & [expected]]
  (assoc expr
         expr-type (->Result (lookup-var var)
                             (->FilterSet (->AndFilter [(->NotTypeFilter (->False) nil var)
                                                        (->NotTypeFilter (->Nil) nil var)])
                                          (->OrFilter [(->TypeFilter (->False) nil var)
                                                       (->TypeFilter (->Nil) nil var)]))
                             (->Path nil var))))

(defmulti invoke-special (fn [expr & args] (-> expr :fexpr :var)))
(defmulti invoke-apply (fn [expr & args] (-> expr :args first :var)))
(defmulti static-method-special (fn [{{:keys [declaring-class name]} :method} & args]
                                  (symbol (str declaring-class) (str name))))

;apply
(defmethod invoke-special #'clojure.core/apply
  [& args]
  (apply invoke-apply args))

;manual instantiation
(defmethod invoke-special #'inst-poly
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [[pexpr targs-exprs] args
        ptype (-> (check pexpr) expr-type Result-type*)
        targs (doall (map parse-type (:val targs-exprs)))]
    (assoc expr
           expr-type (manual-inst ptype targs))))

(declare check-anon-fn)

;fn literal
(defmethod invoke-special #'fn>-ann
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [[fexpr methods-syns] args
        method-param-types (doall (map (fn [{dom-syns :dom}]
                                         {:dom (doall (map parse-type dom-syns))})
                                       (:val methods-syns)))
        cfexpr (check-anon-fn fexpr method-param-types)]
    cfexpr))

;don't type check
(defmethod invoke-special #'tc-ignore-forms
  [{:keys [fexpr args] :as expr} & [expected]]
  (first args))

;seq
(defmethod invoke-special #'clojure.core/seq
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [[ccoll] (doall (map check args))]
    (cond
      ((some-fn HeterogeneousVector? 
                HeterogeneousList? 
                HeterogeneousSeq?)
         (expr-type ccoll))
      (assoc expr
             expr-type (if-let [ts (seq (:types (expr-type ccoll)))]
                         (->HeterogeneousSeq ts)
                         (->Nil))))))

;make vector
(defmethod invoke-special #'clojure.core/vector
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (->HeterogeneousVector
                       (mapv expr-type cargs)))))

;make hash-map
(defmethod invoke-special #'clojure.core/hash-map
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (cond
      (every? Value? (keys (apply hash-map (map expr-type cargs))))
      (assoc expr
             expr-type (->HeterogeneousMap
                         (apply hash-map (map expr-type cargs))))
      :else ::not-special)))

;apply hash-map
(defmethod invoke-apply #'clojure.core/hash-map
  [{[_ & args] :args :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (cond
      (and ((some-fn HeterogeneousVector? HeterogeneousList? HeterogeneousSeq?) 
              (expr-type (last cargs)))
           ;; every key must be a Value
           (every? Value? (keys (apply hash-map (concat (map expr-type (butlast cargs))
                                                        (mapcat vector (:types (expr-type (last cargs)))))))))
      (assoc expr
             expr-type (->HeterogeneousMap
                         (apply hash-map (concat (map expr-type (butlast cargs))
                                                 (mapcat vector (:types (expr-type (last cargs))))))))
      :else ::not-special)))

;for map destructuring
(defmethod invoke-special #'clojure.core/seq?
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (cond
      (HeterogeneousMap? (expr-type (first cargs)))
      (assoc expr
             expr-type (->False))

      ((some-fn HeterogeneousList? HeterogeneousSeq?) 
         (expr-type (first cargs)))
      (assoc expr
             expr-type (->True))

      :else ::not-special)))
;nth
(defmethod static-method-special 'clojure.lang.RT/nth
  [{[t & args] :args, :keys [fexpr] :as expr} & [expected]]
  (let [t (expr-type (check t))
        cargs (doall (map check args))]
    (cond
      (and ((some-fn HeterogeneousVector?
                     HeterogeneousList?
                     HeterogeneousSeq?)
              t)
           (Value? (expr-type (first cargs))))
      (assoc expr
             expr-type (let [[k default] (map expr-type cargs)]
                         (apply nth (:types t) (:val k) (when default
                                                          [default]))))
      :else ::not-special)))

;get
(defmethod static-method-special 'clojure.lang.RT/get
  [{[t & args] :args :keys [fexpr] :as expr} & [expected]]
  (let [t (expr-type (check t))
        cargs (doall (map check args))]
    (cond
      (and (HeterogeneousMap? t)
           (Value? (expr-type (first cargs))))
      (assoc expr
             expr-type (let [[k default] (map expr-type cargs)]
                         (get (:types t) k (if default
                                             default
                                             (->Nil)))))
      :else ::not-special)))

;conj
(defmethod invoke-special #'clojure.core/conj
  [{[t & args] :args :keys [fexpr] :as expr} & [expected]]
  (let [t (check t)
        args (doall (map check args))]
    (cond
      ;(conj {...} [a b]) => (merge {...} {a b})
      (and (HeterogeneousMap? (expr-type t))
           (HeterogeneousVector? (expr-type (first args))))
      (let [m (expr-type t)
            arg1 (expr-type (first args))
            _ (assert (= 2 (count (:types arg1)))
                      "Need vector of length 2 to conj to map")
            _ (assert (every? Value? (:types arg1))
                      "Vector must be of Values for now")
            res (->HeterogeneousMap
                  (assoc (:types m)
                         (-> arg1 :types first)
                         (-> arg1 :types second)))]
        (assoc expr
               expr-type res))

      ;(conj {...} nil) => {...}
      (and (HeterogeneousMap? (expr-type t))
           (Nil? (expr-type (first args))))
      (assoc expr
             expr-type (expr-type t))

      ;[...]
      (HeterogeneousVector? (expr-type t))
      (assoc expr
             expr-type (->HeterogeneousVector
                         ;vectors conj onto end
                         (concat (:types (expr-type t)) 
                                 [(expr-type (first args))])))

      :else ::not-special)))

(defmethod invoke-special :default [& args] ::not-special)

;convert apply to normal function application
(defmethod invoke-apply :default 
  [{[fexpr & args] :args :as expr} & [expected]]
  (throw (Exception. "apply not implemented")))


(defn invoke-keyword [{:keys [fexpr args] :as expr} expected]
  (let [cfexpr (check fexpr)
        cargs (doall (map check args))]
    (cond
      (HeterogeneousMap? (expr-type (first cargs)))
      (assoc expr
             expr-type (let [[k default] (map expr-type cargs)]
                         (get (-> cargs first expr-type :types)
                              (expr-type cfexpr)
                              (if default
                                default
                                (->Nil)))))

      :else 
      (assoc expr
             expr-type (->Top)))))

(defmethod check :invoke
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [e (invoke-special expr expected)]
    (cond 
      (not= ::not-special e) e

      (= :keyword (:op fexpr))
      (invoke-keyword expr expected)

      ;must be monomorphic for now
      :else
      (let [cfexpr (check fexpr)
            cargs (doall (map check args))
            ftype (let [ft (expr-type cfexpr)]
                    (or (and (Result? ft)
                             (:t ft))
                        ft))
            actual (check-app ftype (map expr-type cargs) expected)]
        (assoc expr
               :fexpr cfexpr
               :args cargs
               expr-type actual)))))

(defn relevant-Fns
  "Given a set of required-param exprs, rest-param expr, and a Fn-Intersection,
  returns an (ordered) seq of Functions that contains function types
  whos arities match the fixed and rest parameters given"
  [required-params rest-param fin]
  {:pre [(Fn-Intersection? fin)]
   :post [sequential?
          (every? Function? %)]}
  (assert (not (some :drest (:types fin))))
  (let [nreq (count required-params)]
    (letfn [(relevant-rest?
              [{:keys [dom rest drest] :as ftype}]
              "Returns a true value if ftype matches the
              number of required and variable parameters"
              (let [ndom (count dom)]
                (and (= ndom nreq)
                     rest)))
            (relevant-fixed?
              [{:keys [dom rest] :as ftype}]
              "Returns a true value if the ftype matches
              exactly the number of required parameters. 
              ie. has no rest parameters"
              (let [ndom (count dom)]
                (and (= ndom nreq)
                     (not rest))))]
      (let [relevant? (if rest-param
                        relevant-rest?
                        relevant-fixed?)]
        (filter relevant? (:types fin))))))

(declare check-fn-expr check-fn-method)

(defmethod check :fn-expr
  [{:keys [methods] :as expr} & [expected]]
  {:pre [expected]}
  (check-fn-expr expr expected))

(declare check-anon-fn-method)

(defn check-anon-fn
  "Check anonymous function, with annotated methods"
  [{:keys [methods] :as expr} methods-param-types]
  (let [cmethods (doall
                   (map #(check-anon-fn-method %1 %2) methods methods-param-types))]
    (assoc expr
           expr-type (Fn-Intersection (map expr-type cmethods)))))

(defn check-anon-fn-method
  [{:keys [required-params rest-param body] :as expr} method-param-types]
  {:post [(-> % expr-type Function?)]}
  (assert (not rest-param))
  (let [cbody (with-locals (zipmap (map :sym required-params) (:dom method-param-types))
                (check body))
        actual-type 
        (->Function 
          (:dom method-param-types)
          (if (Result? (expr-type cbody))
            (expr-type cbody)
            (make-Result (expr-type cbody)))
          nil nil)]
    (assoc expr
           :body cbody
           expr-type actual-type)))

(defn check-fn-expr [{:keys [methods] :as expr} expected]
  (let [fin (cond
              (Poly? expected) (Poly-body* (repeatedly (:nbound expected) gensym) expected)
              :else expected)
        _ (doseq [{:keys [required-params rest-param] :as method} methods]
            (check-fn-method method (relevant-Fns required-params rest-param fin)))]
    (assoc expr
           expr-type expected)))

(defn check-fn-method
  "Checks type of the method"
  [{:keys [required-params rest-param body] :as expr} expected-fns]
  {:pre [(sequential? expected-fns)
         (seq expected-fns)
         (every? Function? expected-fns)]}
  (doseq [{:keys [dom rng rest drest] :as ftype} expected-fns]
    (assert (not drest))
    (let [param-locals (let [dom-local (zipmap (map :sym required-params) dom)
                             rest-local (when (or rest-param rest)
                                          (assert (and rest rest-param))
                                          [(:sym rest-param) (Un (->Nil)
                                                                 (RInstance-of ASeq [rest]))])]
                         (conj dom-local rest-local))
          res-expr (with-locals param-locals
                     (check body (Result-type* rng)))
          res-type (-> res-expr expr-type Result-type*)]
      (subtype res-type (Result-type* rng)))))

(defmethod check :do
  [{:keys [exprs] :as expr} & [expected]]
  (let [cexprs (doall (map check exprs))]
    (assoc expr
           :exprs cexprs
           expr-type (-> cexprs last expr-type))))

(defmethod check :local-binding-expr
  [{:keys [local-binding] :as expr} & [expected]]
  (assoc expr
         expr-type (type-of (-> local-binding :sym))))

(defmethod check :static-method
  [expr & [expected]]
  (let [spec (static-method-special expr expected)]
    (cond
      (not= ::not-special spec) spec
      :else (throw (Exception. ":static-method not implemented")))))

(defmethod check :let
  [{:keys [binding-inits body is-loop] :as expr} & [expected]]
  (assert (not is-loop))
  (let [locals (reduce (fn [locals {{:keys [sym init]} :local-binding}]
                         (let [cinit (with-locals locals
                                       (check init))]
                           (assoc locals sym (expr-type cinit))))
                       {} binding-inits)
        cbody (with-locals locals
                (check body))]
    (assoc expr
           :body cbody
           expr-type (expr-type cbody))))

(defmethod check :if
  [{:keys [test then else] :as expr} & [expected]]
  (let [ctest (check test)]
    (assoc expr
           expr-type (let [then-reachable? ((every-pred (complement Union?)
                                                        (complement Intersection?)
                                                        (complement Nil?)
                                                        (complement False?))
                                              (expr-type ctest))
                           else-reachable? ((every-pred (complement Union?)
                                                        (complement Intersection?)
                                                        (some-fn
                                                          Nil? False?))
                                              (expr-type ctest))]
                       (apply Un (concat (when then-reachable?
                                           [(-> then check expr-type)])
                                         (when else-reachable?
                                           [(-> else check expr-type)])))))))

(defmethod check :def
  [{:keys [var init init-provided] :as expr} & [expected]]
  (assert (not expected) expected)
  (let [cexpr (cond 
                (not init-provided) expr ;handle (declare ..)
                :else (check init (type-of var)))]
    (assoc cexpr
           expr-type (RInstance-of Var))))

(defmacro cf [form]
  `(check (ast ~form)))

(defn check-ns [nsym]
  (require nsym)
  (let [[_ns-decl_ & asts] (analyze/analyze-path nsym)]
    (doseq [ast asts]
      (check ast))))

(comment 
(check-ns 'typed.test.example)
)

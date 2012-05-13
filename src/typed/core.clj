(ns typed.core
  (:import (clojure.lang Var Symbol IPersistentList IPersistentVector Keyword Cons
                         Ratio Atom IPersistentMap Seqable Counted ILookup ISeq
                         IMeta IObj Associative IFn))
  (:require [trammel.core :refer [defconstrainedrecord defconstrainedvar
                                  constrained-atom]]
            [analyze.core :as a :refer [analyze-path ast]]
            [analyze.util :as util]
            [clojure.set :as set]))
(prn "RELOAD CORE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Annotation

#_(+T qual-sym [ns symbol -> symbol])
(defn qual-sym 
  "If sym is qualified, return it. Otherwise return
  a new symbol with the namespace portion being the namespace a-ns,
  and preserving the name of sym"
  [a-ns sym]
  (if (namespace sym)
    sym
    (symbol (-> a-ns ns-name name) (name sym))))

(defmacro +T 
  "Annotate a top level identifier. Takes
  a symbol and a type. If the symbol is unqualified,
  it is implicitly qualified in the current namespace."
  [nme type-syn]
  `(annotate-def* '~nme '~type-syn))

(defmacro with-type-args [type-syns form]
  `(do (next-form-targs '~type-syns)
       ~form))

#_(+T next-form-targs [Any -> nil])
(defn next-form-targs [types]
  nil)

#_(+T next-form-targs [symbol Any -> nil])
(defn annotate-def* [sym tsyn]
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed require

(defonce namespaces (atom '{clojure.core {:name clojure.core}
                            typed.base {:name typed.base}}))

(declare parse)

(defn add-type-ann [sym typ]
  (assert (namespace sym))
  (let [nsym (symbol (namespace sym))]
    (swap! namespaces assoc-in [nsym :anns sym] (parse typ))))

(defn parse-ns-typed [nsym args]
  (let [requires
        (reduce (fn [rs [op & libs]]
                  (if (= op :require)
                    (into rs (map first libs))
                    rs))
                #{} args)

        aliases
        (reduce (fn [rs [op & libs]]
                  (if (= op :require)
                    (into rs
                          (map #(let [opts (apply hash-map (rest %))
                                      nme (first %)]
                                  [(:as opts) nme])
                               libs))
                    rs))
                {} args)

        refers
        (reduce (fn [rs [op & libs]]
                  (if (= op :require)
                    (into rs
                          (mapcat #(let [opts (apply hash-map (rest %))
                                         nme (first %)
                                         refs (:refer-type opts)]
                                     (map (fn [r] [r (symbol (name nme) (name r))]) refs))
                                  libs))
                    rs))
                {} args)
        
        imports
        (reduce (fn [rs [op & libs]]
                  (if (= op :import)
                    (into rs
                          (mapcat (fn [[pre & sufs]]
                                    (map #(vector % (symbol (str pre "." %))) sufs))
                                  libs))
                    rs))
                {} args)]
    {:name nsym :requires requires :aliases aliases :refers refers :imports imports}))

(defn typed-ns* [nme opts]
  nil)

(defmacro typed-ns [nsym & opts]
  `(typed-ns* '~nsym '~opts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug macros

#_(+T debug-mode (atom boolean))
(def debug-mode (atom false))

#_(+T print-warnings (atom boolean))
(def print-warnings (atom false))

(defmacro warn [& body]
  `(when @print-warnings
     (println ~@body)))

(defmacro debug [& body]
  `(when @debug-mode
     (println ~@body)))

(defmacro log [& body]
  `(when @debug-mode
     (println ~@body)))

(defmacro tc-form [frm]
  `(-> (ast ~frm) tc-expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top levels

(declare parse ^:dynamic *type-db* tc-expr unparse)

#_(+T TYPE-ARGS (Atom (nseqable Any)))
(def TYPE-ARGS (atom nil))

#_(+T set-type-args [Any -> nil])
(defn set-type-args [targs]
  (reset! TYPE-ARGS targs)
  nil)

#_(+T delete-type-args [-> nil])
(defn delete-type-args []
  (reset! TYPE-ARGS nil)
  nil)

(def ^:dynamic *already-reloaded*)

(defn- type-check-ns [nsym]
  (let [asts (analyze-path nsym)

        _ (doseq [a asts]
            (try (tc-expr a)
              (catch Throwable e
                (throw e))))]
    nil))

#_(+T check-namespace [symbol -> nil])
(defn check-namespace [nsym]
  (let [_ (type-check-ns 'typed.class)
        _ (type-check-ns 'typed.base)
        _ (type-check-ns nsym)]
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type hierarchy

#_(+T type-key keyword)
(def type-key ::+T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type variable Scope

(declare Type? TypeVariable?)

#_(+T tvar-scope keyword)
(def tvar-scope :ts)

#_(def-type-alias TvarBinding (SequentialSeq TypeVariable))

#_(+T ScopeBinding (record TvarBinding))
(defconstrainedrecord ScopeBinding [ts]
  "A binding list for type variables"
  [(sequential? ts)
   (every? TypeVariable? ts)])


#_(+T make-tvar-binding (All [(t <! Type)]
                             [t TvarBinding -> (I t (meta ScopeBinding))]))
(defn make-tvar-binding 
  "Make Type t a binding position for variables s"
  [t s]
  (assert (Type? t))
  (assert (every? TypeVariable? s))
  (with-meta t (->ScopeBinding s)))

#_(+T tvar-binding [(I Type 
                       (meta (U nil ScopeBinding)))
                    -> TvarBinding])
(defn tvar-binding
  "If Type t is a binding position, return the variables binded there"
  [t]
  (assert (Type? t) (class t))
  (-> t meta tvar-scope))

#_(+T tvar-binding (All [(m <! (I Type (Meta ScopeBinding)))]
                        [m [TvarBinding -> TvarBinding] -> m]))
(defn update-tvar-binding 
  "Use function f to update the binding position t"
  [t f]
  (vary-meta t #(update-in % [tvar-scope] f)))

#_(+T tvar-binding (All [(t <! Type)]
                        [t -> (I t (meta nil))])) ;TODO minus metadata
(defn remove-tvar-binding
  "Return Type t without a binding position"
  [t]
  (vary-meta t (constantly nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

#_(+T class-satisfies-protocol? [Protocol Class -> boolean])
(defn class-satisfies-protocol?
  "Returns the method that would be dispatched by applying
  an instance of Class c to protocol"
  [protocol c]
  (boolean
    (if (isa? c (:on-interface protocol))
      c
      (let [impl #(get (:impls protocol) %)]
        (or (impl c)
            (and c (or (first (remove nil? (map impl (butlast (@#'clojure.core/super-chain c)))))
                       (when-let [t (@#'clojure.core/reduce1 @#'clojure.core/pref (filter impl (disj (supers c) Object)))]
                         (impl t))
                       (impl Object))))))))

(declare PrimitiveClass-from ClassType-from Type Type? ->ProtocolType
         ->QualifiedKeyword var-or-class->sym)

#_(+T primitives (map symbol Class))
(def ^:private primitive-symbol
  {'pchar Character/TYPE
   'pboolean Boolean/TYPE
   'pbyte Byte/TYPE
   'pshort Short/TYPE
   'pint Integer/TYPE
   'plong Long/TYPE
   'pfloat Float/TYPE
   'pdouble Double/TYPE
   'pvoid Void/TYPE})

(declare ^:dynamic *type-var-scope* TYPES)

#_(+T resolve-symbol [symbol -> Type])
(defn- resolve-symbol [sym]
  (assert (symbol? sym))
  (cond
    (primitive-symbol sym)
    (PrimitiveClass-from sym)

    (*type-var-scope* sym)
    (*type-var-scope* sym)

    (@TYPES sym)
    (@TYPES sym)

    :else
    (let [res (resolve sym)]
      (cond
        (class? res) (if (.isPrimitive res)
                       (PrimitiveClass-from res)
                       (ClassType-from res))

        (var? res) (let [v @res]
                     (cond
                       (Type? v) v 

                       (map? v) (->ProtocolType (var-or-class->sym res))

                       (and (keyword? v)
                            (namespace v))
                       (->QualifiedKeyword v)
                       
                       :else (throw (Exception. (str "Not a type: " sym)))))

        :else (throw (Exception. (str "Could not resolve " sym)))))))

(declare map->Fun map->arity union Nil PrimitiveClass? subtype?)

#_(+T method->Fun [clojure.reflect.Method -> Fun])
(defn- method->Fun [method]
  (try
    (map->Fun
      {:arities #{(map->arity 
                    {:dom (->> 
                            (map resolve-symbol (:parameter-types method))
                            (map #(if (or (PrimitiveClass? %)
                                          (subtype? Nil %))
                                    %                   ; null cannot substutitute for JVM primtiives
                                    (union [Nil %])))) ; Java Objects can be the null reference
                     :rng (let [typ (resolve-symbol (:return-type method))]
                            (if (or (PrimitiveClass? typ)
                                    (subtype? Nil typ))
                              typ                        ; null cannot substutitute for JVM primtiives
                              (union [Nil typ])))})}}) ; Java Objects can be the null reference
    (catch Throwable e
      (throw (ex-info "Could not create Fun from Method" {:method method} e)))))


#_(+T var-or-class->sym [(U var Class) -> symbol])
(defn var-or-class->sym [var-or-class]
  {:pre [(or (var? var-or-class)
             (class? var-or-class))]}
  (cond
    (var? var-or-class) (symbol (str (.name (.ns var-or-class))) (str (.sym var-or-class)))
    :else (symbol (.getName var-or-class))))

(defmacro map-all-true? [& body]
  `(every? true? (map ~@body)))

(declare subtype? unparse-type)

#_(+T unp [Type -> string])
(defn unp
  "Unparse a type and return string representation"
  [t]
  (with-out-str (-> t unparse-type pr)))

#_(+T assert-subtype [Type Type & Any * -> nil])
(defn assert-subtype [actual-type expected-type & msgs]
  (assert (subtype? actual-type expected-type)
          (apply str "Expected " (unp expected-type) ", found " (unp actual-type)
                 msgs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type contexts

(declare Type?)

#_(+T type-db-var-contract [(map Any Any) -> boolean])
(defn type-db-var-contract [m]
  (and (every? namespace (keys @m))
       (every? Type? (vals @m))))

#_(+T type-db-atom-contract [(map Any Any) -> boolean])
(defn type-db-atom-contract [m]
  (and (every? namespace (keys m))
       (every? Type? (vals m))))

#_(+T *type-db* (map Symbol Type))
(def ^:dynamic *type-db* 
  (constrained-atom {}
                    "Map from qualified symbols to types"
                    [type-db-atom-contract]))

#_(+T local-type-db-contract [(map Any Any) -> boolean])
(defn local-type-db-contract [m]
  (and (every? (complement namespace) (keys m))
       (every? Type? (vals m))))

#_(+T *local-type-db* (map symbol Type))
(defconstrainedvar 
  ^:dynamic *local-type-db* {}
  "Map from unqualified names to types"
  [local-type-db-contract])

#_(+T type-var-scope-contract [(map Any Any) -> boolean])
(defn type-var-scope-contract [m]
  (and (every? (every-pred symbol? (complement namespace)) 
               (keys m))
       (every? Type? (vals m))))

#_(+T *type-var-scope* (map symbol TypeVariable))
(defconstrainedvar
  ^:dynamic *type-var-scope* {}
  "Map from unqualified names to types"
  [type-var-scope-contract])

#_(+T reset-type-db [-> nil])
(defn reset-type-db []
  (swap! *type-db* (constantly {}))
  nil)

#_(+T TYPES (atom (map symbol Type)))
(def TYPES (atom {}))

#_(+T add-type [symbol Type -> nil])
(defn add-type [sym t]
  (assert (namespace sym))
  (assert (symbol? sym))
  (assert (Type? t))
  (swap! TYPES assoc sym t)
  nil)

;new-type

(declare ->NewType)

#_(+T add-new-type [symbol -> nil])
(defn add-new-type [sym]
  (assert (symbol? sym) "New type must be a symbol")
  (let [sym (qual-sym *ns* sym)]
    (add-type sym (->NewType sym))
    nil))

(defmacro def-new-type [sym]
  `(add-new-type '~sym))

;type-alias

(declare ->TypeAlias All-literal)

#_(+T type-param-syn [(U symbol (list Any)) Any])
(defn- type-param-syn 
  "Convert type parameter sugar (a b c) to (All [b c] ..)"
  [sym-or-list t]
  (if (list? sym-or-list)
    `(~All-literal ~(vec (rest sym-or-list))
        ~t)
    t))

#_(+T add-type-alias [(U symbol (list Any)) Any -> nil])
(defn add-type-alias [sym-or-list t]
  (let [sym (if (symbol? sym-or-list)
              sym-or-list
              (first sym-or-list))
        sym (qual-sym *ns* sym)

        t (type-param-syn sym-or-list t)

        _ (add-type sym (->TypeAlias sym t))]
    nil))

(defn def-type-alias* [nme t]
  nil)

(defmacro def-type-alias [sym tsyn]
  `(def-type-alias* '~sym '~tsyn))

#_(+T type-of [(U symbol var) -> Type])
(defn type-of [sym-or-var]
  {:pre [(or (symbol? sym-or-var)
             (var? sym-or-var))]
   :post [(Type? %)]}
  (let [sym (if (var? sym-or-var)
              (symbol (str (.name (.ns sym-or-var))) (str (.sym sym-or-var)))
              sym-or-var)]
    (if-let [the-local-type (and (not (namespace sym))
                                 (*local-type-db* sym))]
      the-local-type
      (if-let [the-type (and (namespace sym)
                             (@*type-db* sym))]
        the-type
        (throw (Exception. (str "No type for " sym)))))))

(declare ->ParameterisedType ParameterisedType?)

#_(+T typed-classes-var-contract [(atom Any) -> boolean])
(defn typed-classes-var-contract [a]
  (and (map? @a)
       (every? symbol? (keys @a))
       (every? #(class? (resolve %)) (keys @a))
       (every? ParameterisedType? (vals @a))))

#_(+T typed-classes-atom-contract [Any -> boolean])
(defn typed-classes-atom-contract [m]
  (and (map? m)
       (every? symbol? (keys m))
       (every? #(class? (resolve %)) (keys m))
       (every? ParameterisedType? (vals m))))

(defconstrainedvar 
  ^:dynamic *parameterised-classes*
  (constrained-atom {}
                    "A map of qualified symbols to ParameterisedType's"
                    [typed-classes-atom-contract])
  "Atom containing a map of qualified symbols to ParameterisedType's"
  [typed-classes-var-contract])

(declare parse-tvar-decl)

(defmacro with-type-vars [var-map & body]
  `(binding [*type-var-scope* (merge *type-var-scope* ~var-map)]
     ~@body))

(defmacro with-local-types [type-map & body]
  `(binding [*local-type-db* (merge *local-type-db* ~type-map)]
     ~@body))

#_(+T add-type-ann [symbol Type -> Any])
#_(defn add-type-ann [sym typ]
  (when-let [oldtyp (@*type-db* sym)]
    (when (not= oldtyp typ)
      (warn "Overwriting type for" sym ":" typ "from" (unparse oldtyp))))
  (swap! *type-db* assoc sym typ)
  [sym :- (unparse typ)])

(defmacro with-type-anns [type-map-syn & body]
  `(binding [*type-db* (constrained-atom (into {} 
                                               (doall (map #(vector (or (when-let [var-or-class# (resolve (first %))]
                                                                          (var-or-class->sym var-or-class#))
                                                                        (when (namespace (first %))
                                                                          (first %))
                                                                        (symbol (str (ns-name *ns*)) (name (first %))))
                                                                    (parse-syntax (second %)))
                                                           '~type-map-syn)))
                                         "Map from qualified symbols to types"
                                         [type-db-atom-contract])]
     ~@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed Protocol

(comment
  #_(def-typed-protocol IPro
    (myfn [this (at :- Double)] :- Number
          [this (blah :- Object) & (blahs :- Any *)] :- Long
          "Blah"))

  #_(def-typed-record A
    IPro
    (myfn 
      ([this (at :- Double)] :- Number
       (+ 1 at))
      ([this (blah :- Object) & (blahs :- Any *)] :- Long
       (when (every? number? blahs)
         (apply + blahs)))))
    )


;(defn- convert-protocol-arity [[dom-syn rng-syn]]
;  (let [doms (map last (rest dom-syn))
;        doms ]
;
;(defn- build-protocol-method [[nme & ms]]
;  (let [doc (when (string? (last ms))
;              (last ms))
;        methods (apply hash-map
;                       (remove #(= :- %)
;                               (if (string? (last m))
;                                 (butlast m)
;                                 m)))]
;    (list* nme (concat (map convert-protocol-arity methods)
;                       (when doc
;                         [doc])))))
;
;
;(defmacro def-typed-protocol [name]
;  `(defprotocol ~name


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

#_(+T Type QualifiedKeyword)
(def Type ::type-type)

#_(+T Type? [Any -> boolean])
(defn Type? [t]
  (isa? (class t) Type))

(defprotocol IFreeVars
  (^{+T [IFreeVars -> (seq TypeVariables)]}
   -free-vars [this]))

#_(+T free-vars [Type -> (seq TypeVariables)])
(defn free-vars [t]
  (-free-vars t))

(defmacro def-type [nme & body]
  `(let [a# (defconstrainedrecord ~nme ~@body)]
     (derive a# Type)
     a#))

#_(+T NewType (record symbol))
(def-type NewType [nme]
  "Types created with def-new-type"
  {:pre [(symbol? nme)
         (namespace nme)]})

#_(+T TypeAlias (record symbol Type))
(def-type TypeAlias [nme als-type]
  "Type alias created with def-type-alias"
  {:pre [(symbol? nme)
         (namespace nme)
         (Type? als-type)]})

#_(+T Value (record Any))
(def-type Value [val]
  "A singleton type for values, except nil"
  {:pre [(not (nil? val))]}
  
  IFreeVars
  (-free-vars [this] nil))

(def-type Record [fields]
  "A type for records"
  {:pre [(every? Type? fields)]}
  
  IFreeVars
  (-free-vars [this] (doall (map free-vars fields))))

(declare subtype?)

#_(+T Union (record (set Type)))
(def-type Union [types]
  "A union of types"
  {:pre [(every? Type? types)]}
  
  IFreeVars
  (-free-vars [this] 
    (mapcat free-vars types)))

#_(+T NilType (Record))
(def-type NilType []
  "The nil type"
  []
  
  IFreeVars
  (-free-vars [this] nil))

#_(+T Nil NilType)
(def Nil (->NilType))

#_(+T Nil? [Any -> boolean]) ;predicate, does not test on type...
(def Nil? (partial = Nil))

#_(+T ClassType (record symbol))
(def-type ClassType [the-class]
  "A class"
  {:pre [(symbol? the-class)
         (let [c (resolve the-class)]
           (and (class? c)
                (not (.isPrimitive c))))]}

  IFreeVars
  (-free-vars [this] nil))

#_(+T ClassType-from [Class -> ClassType])
(defn ClassType-from [cls]
  (assert (class? cls))
  (->ClassType (symbol (.getName cls))))

#_(+T ParameterisedType (record symbol (SequentialSeq Type)))
(def-type ParameterisedType [class-sym fields]
  "A type for parameterised classes. Takes a symbol
  representing the class it is parameterising, and a list
  of fields, type variables"
  {:pre [(symbol? class-sym)
         (class? (resolve class-sym))
         (every? Type? fields)]})

#_(+T Any Union)
;TODO primitives?
(def Any (Union. #{Nil (ClassType-from Object)})) ; avoid constrained constructor because of
                                                  ; call to subtype?, which is undefined

#_(+T Any? [Any -> boolean])
(def Any? (partial = Any))

#_(+T Nothing Union)
(def Nothing (Union. #{}))

#_(+T Nothing? [Any -> boolean])
(def Nothing? (partial = Nothing))

#_(+T True Value)
(def True (->Value true))

#_(+T True? [Any -> boolean])
(def True? (partial = True))

#_(+T False Value)
(def False (->Value false))

#_(+T False? [Any -> boolean])
(def False? (partial = False))

#_(+T falsy-values (Set Value))
(def falsy-values #{False Nil})

;keyword

#_(+T QualifiedKeyword (Record Keyword))
(def-type QualifiedKeyword [kwrd]
  "A fully qualified keyword"
  [(keyword? kwrd)
   (namespace kwrd)]

  IFreeVars
  (-free-vars [this] nil))

;; Base types

(declare arity?)

#_(+T Fun (record (set arity)))
(def-type Fun [arities]
  "Function with one or more arities"
  {:pre [(set? arities)
         (seq arities)
         (every? arity? arities)]}

  IFreeVars
  (-free-vars [this] 
    (mapcat free-vars arities)))

#_(+T -fun [(Seq arities) -> Fun])
(defn -fun [arities]
  (->Fun (set arities)))

#_(+T PrimitiveClass (Record Symbol))
(def-type PrimitiveClass [the-class]
  "A primitive class"
  {:pre [(symbol? the-class)
         (let [c (primitive-symbol the-class)]
           (and (class? c)
                (.isPrimitive c)))]}

  IFreeVars
  (-free-vars [this] nil))

#_(+T PrimitiveClass-from [Symbol -> PrimitiveClass])
(defn PrimitiveClass-from 
  "Create a PrimitiveClass from a symbol representing a
  primitive class name (int, long etc.) or a primitive Class
  object"
  [sym]
  (->PrimitiveClass
    (symbol (.getName (or (primitive-symbol sym)
                          (resolve-symbol sym))))))

#_(+T ProtocolType (Record Symbol))
(def-type ProtocolType [the-protocol-var]
  "A protocol"
  {:pre [(symbol? the-protocol-var)
         (namespace the-protocol-var)]}

  IFreeVars
  (-free-vars [this] nil))

#_(+T union [(Seq types) -> Type])
(defn union [ts]
  (let [ts (set ts)]
    (cond
      (empty? ts) Nothing
      (= 1 (count ts)) (first ts)
      :else (->Union ts))))

#_(+T Intersection (Record (Set Type)))
(def-type Intersection [types]
  "An intersection of types"
  {:pre [(set? types)
         (every? Type? types)]}

  IFreeVars
  (-free-vars [this] 
    (mapcat free-vars types)))

(defn -intersection [ts]
  (let [ts (set ts)]
    (cond
      (empty? ts) Nothing
      (= 1 (count ts)) (first ts)
      :else (->Intersection ts))))

;; type variables

#_(+T TypeVariable (Record Symbol Type))
(def-type TypeVariable [nme bnd]
  "A record for bounded type variables, with an unqualified symbol as a name"
  {:pre [(symbol? nme)
         (not (primitive-symbol nme))
         (not (namespace nme))
         (Type? bnd)
         (not (TypeVariable? bnd))]}

  IFreeVars
  (-free-vars [this]
    (concat [this] (free-vars bnd))))

#_(+T -tv (Fun [Symbol -> TypeVariable]
             [Symbol Type -> TypeVariable]))
(defn -tv 
  "Create a type variable with an optional bound. Bound
  defaults to Any"
  ([nme] (-tv nme Any))
  ([nme bnd] (->TypeVariable nme bnd)))

#_(+T type-variables (Set Class))
(def type-variables #{TypeVariable})

#_(+T type-variable? [Any -> boolean])
(defn type-variable? [t]
  (boolean (type-variables (class t))))

;; arities

#_(+T Arity QualifiedKeyword)
(def Arity ::arity-type)

#_(+T Arity? [Any -> boolean])
(defn Arity? [a]
  (isa? (class a) Arity))

(declare FilterSet?)

;; arity is NOT a type
#_(+T arity (Record (SequentialSeq Type)
                  Type
                  (U nil Type)
                  (U nil FilterSet)))
(def-type arity [dom rng rest-type flter]
  "An arity with fixed or variable domain. Supports optional filter, and optional type parameters"
  {:pre [(every? Type? dom)
         (Type? rng)
         (or (nil? rest-type)
             (Type? rest-type))
         (or (nil? flter)
             (FilterSet? flter))]}

  IFreeVars
  (-free-vars [this]
    (mapcat free-vars 
            (concat dom
                    [rng]
                    (when rest-type
                      [rest-type])))))


(declare subtypes?)

#_(+T subtypes?*-varargs [(SequentialSeq Type)
                        (SequentialSeq Type)
                        Type -> boolean])
(defn subtypes?*-varargs [argtys dom rst]
  (loop [dom dom
         argtys argtys]
    (cond
      (and (empty? argtys)
           (empty? dom))
      true

      (empty? argtys)
      false

      (and (empty? dom)
           rst)
      (if (subtype? (first argtys) rst)
        (recur dom (next argtys))
        false)

      (empty? dom)
      false

      (subtype? (first argtys)
                (first dom))
      (recur (next argtys)
             (next dom))

      :else false)))


#_(+T top-arity QualifiedKeyword)
(def top-arity ::top-arity)

(declare subtype?)

#_(+T subtype?*-arity [arity arity -> boolean])
(defn subtype?*-arity [s t]
  (assert (not (:rest-type s)))
  (assert (not (:rest-type t)))
  (and (map-all-true? subtype? 
                      (:dom s)
                      (:dom t))
       (subtype? (:rng s)
                 (:rng t))))

#_(+T similar-arity [arity arity -> (U arity nil)])
(defn similar-arity 
  "Return a2 if a1 looks like it, using number of arguments"
  [a1 a2]
  (when (or (:rest-type a1)
            (:rest-type a2)
            (= (count (:dom a1))
               (count (:dom a2))))
    a2))


#_(+T match-to-fun-arity [arity Fun -> (U nil arity)])
(defn match-to-fun-arity [s fun-type]
  (first 
    (filter #(= (count (:dom s))
                (count (:dom %)))
            (:arities fun-type))))

#_(+T matches-args [arity (Seq Type) -> (U nil arity)])
(defn matches-args [arr args]
  (when (or (and (:rest-type arr)
                 (<= (count (:dom arr))
                     (count args)))
            (= (count (:dom arr))
               (count args)))
    arr))

; data structures

#_(+T Seq (Record Type))
(def-type Seq [type]
  "A seq of type type, subtype of clojure.lang.ISeq"
  {:pre [(Type? type)]}
  
  IFreeVars
  (-free-vars [this]
    (free-vars type)))

#_(+T Vector (Record Type))
(def-type Vector [type]
  "A vector of type type, subtype of clojure.lang.IPersistentVector"
  {:pre [(Type? type)]}

  IFreeVars
  (-free-vars [this]
    (free-vars type)))

#_(+T ConstantVector (Record (SequentialSeq Type)))
(def-type ConstantVector [types]
  "A constant vector type, subtype of clojure.lang.IPersistentVector"
  [(every? Type? types)]

  IFreeVars
  (-free-vars [this]
    (mapcat free-vars types)))

#_(+T Sequential (Record Type))
(def-type Sequential [type]
  "A sequential collection type, subtype of clojure.lang.Sequential"
  [(Type? type)]

  IFreeVars
  (-free-vars [this]
    (free-vars type)))

#_(+T ConstantSequential (Record (SequentialSeq Type)))
(def-type ConstantSequential [types]
  "A constant sequential collection type, subtype of clojure.lang.Sequential"
  [(every? Type? types)]

  IFreeVars
  (-free-vars [this]
    (mapcat free-vars types)))

#_(+T Map (Record Type Type))
(def-type Map [ktype vtype]
  "A sequential collection type, subtype of clojure.lang.IPersistentMap"
  [(Type? ktype)
   (Type? vtype)]

  IFreeVars
  (-free-vars [this]
    (mapcat free-vars [ktype vtype])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(def ^:private Filter ::filter-type)

(defmacro def-filter [nme & body]
  `(let [a# (defconstrainedrecord ~nme ~@body)]
     (derive a# Filter)
     a#))

#_(+T Filter? [Any -> boolean])
(defn Filter? [a]
  (isa? (class a) Filter))

#_(+T TrivialFilter (Record))
(def-filter TrivialFilter []
  "A proposition that is always true"
  [])

#_(+T ImpossibleFilter (Record))
(def-filter ImpossibleFilter []
  "A proposition that is never true"
  [])

#_(+T TypeFilter (Record Symbol Type))
(def-filter TypeFilter [var type]
  "A proposition that says var is of type type"
  {:pre [(symbol? var)
         (Type? type)]})

#_(+T NotTypeFilter (Record Symbol Type))
(def-filter NotTypeFilter [var type]
  "A proposition that says var is not of type type"
  {:pre [(symbol? var)
         (Type? type)]})

#_(+T FilterSet (Record Filter Filter))
(def-filter FilterSet [then else]
  "Contains two propositions, then for the truthy result,
  else for the falsy result"
  {:pre [(Filter? then)
         (Filter? else)]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parse Type syntax

(defprotocol IParseType
  (^{+T [IParseType -> Type]}
   parse-syntax* [this]))

(declare Fun-literal All-literal parse-tvar-decl)

#_(+T parse-syntax [Any -> Type])
(defn parse-syntax
  "Type syntax parser, entry point"
  {:post [Type?]}
  [syn]
  (cond
    ;Parse single arity function syntax
    (vector? syn)
    (parse-syntax (list Fun-literal syn))

    ;Parse (All ..) forms
    (and (list? syn)
         (= All-literal
            (first syn)))
    (let [[_ tvar-syn body-syn] syn
          ;; tvar-syn with optional bound (All [x (y <: Number)] ..)
          tvars (map parse-tvar-decl tvar-syn)
          scope (into {} (map #(vector (:nme %) %) tvars))]
      (with-type-vars scope
        (let [t (parse-syntax body-syn)]
          (update-tvar-binding t #(concat tvars %))))) ; handle nested scopes

    :else
    (parse-syntax* syn)))

(defn parse-tvar-decl [tvarsyn]
  (if (symbol? tvarsyn)
    (-tv tvarsyn)
    (-tv (first tvarsyn)
         (parse-syntax (nth tvarsyn 2)))))

(def parse parse-syntax)

(extend-protocol IParseType
  Symbol
  (parse-syntax* [this]
    (resolve-symbol this))
  
  Boolean
  (parse-syntax* [this]
    (if this
      True
      False))

  String
  (parse-syntax* [this]
    (->Value this))

  Character
  (parse-syntax* [this]
    (->Value this))

  Keyword
  (parse-syntax* [this]
    (if (namespace this)
      (->QualifiedKeyword this)
      (->Value this)))

  Double
  (parse-syntax* [this]
    (->Value this))

  java.math.BigDecimal
  (parse-syntax* [this]
    (->Value this))

  clojure.lang.Ratio
  (parse-syntax* [this]
    (->Value this))
  
  Long
  (parse-syntax* [this]
    (->Value this))
  
  nil
  (parse-syntax* [this]
    Nil))

(defmulti 
  ^{'+T '[(IPersistentList Any) -> Type]}
  parse-list-syntax first)

(def All-literal 'All)
(def Record-literal 'Record)
(def U-literal 'U)
(def I-literal 'I)
(def Fun-literal 'Fun)
(def predicate-literal 'predicate)
(def Vectorof-literal 'Vectorof)
(def Vector*-literal 'Vector*)
(def Sequentialof-literal 'Sequentialof)
(def Sequential*-literal 'Sequential*)
(def Seqof-literal 'Seqof)
(def Mapof-literal 'Map)
(def Map*-literal 'Map*)

(defmethod parse-list-syntax Vector*-literal
  [[_ & syns]]
  (->ConstantVector (doall (map parse syns))))
        
(defmethod parse-list-syntax Vectorof-literal
  [[_ syn]]
  (->Vector (parse syn)))
        
(defmethod parse-list-syntax Sequential*-literal
  [[_ & syns]]
  (->ConstantSequential (doall (map parse syns))))
        
(defmethod parse-list-syntax Sequentialof-literal
  [[_ syn]]
  (->Sequential (parse syn)))

(defmethod parse-list-syntax Seqof-literal
  [[_ syn]]
  (->Seq (parse syn)))
        
#_(defmethod parse-list-syntax Map*-literal
  [[_ & kvsyns]]
  (->ConstantMap (doall (map #(vector (parse (first %))
                                      (parse (second %)))
                             kvsyns))))

(defmethod parse-list-syntax Mapof-literal
  [[_ ksyn vsyn]]
  (->Map (parse ksyn) (parse vsyn)))

(defmethod parse-list-syntax U-literal
  [[_ & syn]]
  (union (doall (map parse-syntax syn))))

(defmethod parse-list-syntax I-literal
  [[_ & syn]]
  (-intersection (doall (map parse-syntax syn))))

(defmethod parse-list-syntax predicate-literal
  [[_ & [typ-syntax :as args]]]
  (assert (= 1 (count args)))
  (let [pred-type (parse-syntax typ-syntax)]
    (-fun [(map->arity
              {:dom [Any]
               :rng (ClassType-from Boolean)
               :pred-type pred-type
               :named-params '(a)
               :flter (map->FilterSet
                        {:then (map->TypeFilter
                                 {:var 'a
                                  :type pred-type})
                         :else (map->NotTypeFilter
                                 {:var 'a
                                  :type pred-type})})})])))

(defmethod parse-list-syntax 'quote
  [[_ & [sym :as args]]]
  (assert (= 1 (count args)))
  (assert (symbol? sym))
  (->Value sym))

(defmethod parse-list-syntax Fun-literal
  [[_ & arities]]
  (map->Fun 
    {:arities (set (doall (map parse-syntax* arities)))})) ; parse-syntax* to avoid implicit arity sugar wrapping

(defmethod parse-list-syntax :default
  [[c & args-syn :as syn]]
  (let [args (doall (map parse args-syn))
        cls-type (resolve-symbol c)
        _ (assert (ClassType? cls-type))
        param-type-template (@*parameterised-classes* (:the-class cls-type))
        _ (assert param-type-template (str "Not a parameterised type: " c))
        _ (assert (= (count args)
                     (count (:fields param-type-template)))
                  (str "Malformed syntax: " syn))]
    (->ParameterisedType (:the-class cls-type)
                         args)))

(extend-protocol IParseType
  IPersistentList
  (parse-syntax* [this]
    (parse-list-syntax this))

  Cons
  (parse-syntax* [this]
    (parse-list-syntax this)))

#_(def-type-alias SequentialSeq
                  (All [a]
                    (I Sequential
                       (Seq a))))

(defrecord AritySyntax [dom rng opts])

#_(+T split-no-check [(Vector Any) -> AritySyntax])
(defn- split-no-check [arity-syntax]
  (let [[dom [_ rng & opts]] (split-with #(not= '-> %) arity-syntax)]
    (->AritySyntax dom rng (apply hash-map opts))))

#_(+T split-no-check [(Vector Any) -> AritySyntax])
(defn- split-arity-syntax
  "Splits arity syntax into [dom rng opts-map]"
  [arity-syntax]
  (assert (some #(= '-> %) arity-syntax) (str "Arity " arity-syntax " missing return type"))
  (split-no-check arity-syntax))

#_(+T parse-filter [Any -> Filter])
(defn- parse-filter [syn]
  (assert (vector? syn))
  (let [[nme-sym keyw type-syn] syn
        type (parse-syntax type-syn)]
    (case keyw
      :-> (map->TypeFilter
            {:var nme-sym
             :type type})
      :!-> (map->NotTypeFilter
             {:var nme-sym
              :type type})
      (throw (Exception. (str "Bad filter syntax: " syn))))))

(defrecord DomTypes [fixed rest-type])

#_(+T parse-dom [Any -> DomTypes])
(defn- parse-dom 
  "Given syntax to the left of an arrow, returns a map with keys
  :dom, :rest-type"
  [dom]
  (let [[fixed-dom [_ & rest-args]]
        (split-with #(not= '& %) dom)

        uniform-rest-syntax (when (seq rest-args)
                              (if (= '* (second rest-args))
                                (first rest-args)
                                (assert false (str "Invalid rest args syntax " dom))))

        fixed-dom-types (doall (map parse-syntax fixed-dom))
        uniform-rest-type (when rest-args
                            (parse-syntax uniform-rest-syntax))]
    (->DomTypes fixed-dom-types uniform-rest-type)))

(extend-protocol IParseType
  IPersistentVector
  (parse-syntax* [this]
    (let [{dom :dom rng :rng opts-map :opts} (split-arity-syntax this)

          {fixed-dom-types :fixed
           rest-type :rest-type}
           (parse-dom dom)

          extras (into {}
                       (for [[nme syn] opts-map]
                         (cond
                           (= :filter nme) [:flter (map->FilterSet
                                                     {:then (parse-filter (:then syn))
                                                      :else (parse-filter (:else syn))})]

                           :else (throw (Exception. (str "Unsupported option " nme))))))

          rng-type (parse-syntax rng)]
      (map->arity
        (merge
          {:dom fixed-dom-types
           :rng rng-type}
          (when rest-type
            {:rest-type rest-type})))))

  nil
  (parse-syntax* [_]
    Nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparse type syntax

(defprotocol IUnparseType
  (^{+T [IUnparseType -> Any]}
   unparse-type* [this]))

#_(+T unparse-tvar-binding [TypeVariable -> Any])
(defn unparse-tvar-binding [tvar]
    (assert (TypeVariable? tvar))
  (if (= Any (:bnd tvar))
    (:nme tvar)
    (list (:nme tvar) '<! (unparse-type (:bnd tvar)))))

#_(+T unparse-type [Type -> Any])
(defn unparse-type
  [type-obj]
  (if-let [scope (tvar-binding type-obj)]
    (list All-literal (doall (mapv unparse-tvar-binding scope))
          (unparse-type (remove-tvar-binding type-obj))) ;remove binding scope
    (unparse-type* type-obj)))

(def unparse unparse-type)

(defmulti 
  ^{'+T '[Filter -> Any]}
  unparse-filter class)

(defmethod unparse-filter FilterSet
  [{:keys [then else]}]
  {:then (unparse-filter then)
   :else (unparse-filter else)})

(defmethod unparse-filter TypeFilter
  [{:keys [var type]}]
  [var :-> (unparse-type type)])

(defmethod unparse-filter NotTypeFilter
  [{:keys [var type]}]
  [var :!-> (unparse-type type)])

(extend-protocol IUnparseType
  TypeAlias
  (unparse-type* [this]
    (:nme this))

  NewType
  (unparse-type* [this]
    (:nme this))

  ClassType
  (unparse-type* [this]
    (:the-class this))

  ParameterisedType
  (unparse-type* [this]
    (list* (:class-sym this) (map unparse (:fields this))))

  PrimitiveClass
  (unparse-type* [this]
    (:the-class this))

  Union
  (unparse-type* [this]
    (list* U-literal (doall (map unparse-type (:types this)))))

  Intersection
  (unparse-type* [this]
    (list* I-literal (doall (map unparse-type (:types this)))))

  Fun
  (unparse-type* [this]
    (if (= 1 (count (:arities this)))
      (-> this :arities first unparse-type) ; single arity syntax
      (list* Fun-literal (doall (map unparse-type (:arities this))))))

  arity
  (unparse-type* [this]
    (let [dom (doall (map unparse-type (:dom this)))
          ;; handle named parameters
          dom (if-let [names (seq (:named-params this))]
                (doall
                  (map #(vector %1 :- %2)
                       names
                       dom))
                dom)
          rng (unparse-type (:rng this))
          flter (when-let [flter (:flter this)]
                  (unparse-filter flter))

          sig (-> (concat dom 
                          (when (:rest-type this)
                            ['& (unparse-type (:rest-type this)) '*])
                          ['-> rng]
                          (when flter
                            [:filter flter]))
                vec)]
      sig))

  Value
  (unparse-type* [{:keys [val]}]
    (if (symbol? val)
      `'~val
      val))

  QualifiedKeyword
  (unparse-type* [{:keys [kwrd]}]
    kwrd)

  ProtocolType
  (unparse-type* [{:keys [the-protocol-var]}]
    the-protocol-var)

  Seq
  (unparse-type* [this]
    (list Seqof-literal (unparse-type (:type this))))

  Vector
  (unparse-type* [this]
    (list Vectorof-literal (unparse-type (:type this))))

  ConstantVector
  (unparse-type* [this]
    (list* Vector*-literal (doall (map unparse-type (:types this)))))

  Sequential
  (unparse-type* [this]
    (list Sequentialof-literal (unparse-type (:type this))))

  ConstantSequential
  (unparse-type* [this]
    (list* Sequential*-literal (doall (map unparse-type (:types this)))))

  Map
  (unparse-type* [{:keys [ktype vtype] :as this}]
    (list Mapof-literal 
          (unparse-type ktype)
          (unparse-type vtype)))

  TypeVariable
  (unparse-type* [{:keys [nme]}]
    nme)
  
  NilType
  (unparse-type* [this]
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subtyping

(declare supertype-of-all subtype-of-all supertype-of-one subtype-of-one)

(defmulti 
  ^{'+T '[Type Type -> boolean]}
  subtype?* (fn [s t]
              [(class s) (class t)]))

;type variables

(defmethod subtype?* [TypeVariable TypeVariable]
  [{s-nme :nme} {t-nme :nme}]
  (= s-nme t-nme))

;unions

(defmethod subtype?* [Union Union]
  [{s-types :types :as s} 
   {t-types :types :as t}]
  (if (empty? t-types) ; ie. t is Nothing
    (empty? s-types)   ; s <: Nothing when s = Nothing
    (or (and (empty? t-types)
             (not (empty? s)))
        (= s t)
        (supertype-of-all t s-types))))

(defmethod subtype?* [Type Union]
  [s t]
  (subtype-of-one s (:types t)))

(defmethod subtype?* [Union Type]
  [s t]
  (supertype-of-all t (:types s)))

;singletons

(defmethod subtype?* [Value Value]
  [s t]
  (= (:val s)
     (:val t)))

(defmethod subtype?* [Value ClassType]
  [s t]
  (subtype? (ClassType-from (-> s :val class))
            t))

(defmethod subtype?* [Value PrimitiveClass]
  [s t]
  (subtype? (ClassType-from (-> s :val class))
            t))

;qualified keywords

(defmethod subtype?* [QualifiedKeyword QualifiedKeyword]
  [{s-kwrd :kwrd :as s} 
   {t-kwrd :kwrd :as t}]
  (isa? s-kwrd t-kwrd))

(defmethod subtype?* [ClassType QualifiedKeyword]
  [{s-class-sym :the-class :as s}
   {t-kwrd :kwrd :as t}]
  (let [s-class (resolve s-class-sym)]
    (isa? s-class t-kwrd)))

; keyword represents a hierarchy, Object or Keyword are not supertypes
(defmethod subtype?* [QualifiedKeyword ClassType]
  [s t]
  false)

;classes

(def ^:private extends-Seqable #{Iterable java.util.Map})

;hardcode Clojure interfaces that should "extend" Seqable
(defmethod subtype?* [ClassType ClassType]
  [{s-class-sym :the-class :as s}
   {t-class-sym :the-class :as t}]
  (let [s-class (resolve s-class-sym)
        t-class (resolve t-class-sym)]
    (or (and (identical? t-class Seqable)
             (boolean
               (some #(isa? s-class %) extends-Seqable)))
        (isa? s-class t-class))))

;protocols

(defmethod subtype?* [ProtocolType ProtocolType]
  [{s-var-sym :the-protocol-var :as s} 
   {t-var-sym :the-protocol-var :as t}]
  (let [s-var (resolve s-var-sym)
        t-var (resolve t-var-sym)]
    (isa? @s-var @t-var)))

(defmethod subtype?* [ClassType ProtocolType]
  [{s-class-sym :the-class :as s}
   {t-var-sym :the-protocol-var :as t}]
  (let [s-class (resolve s-class-sym)
        t-var (resolve t-var-sym)]
    (->> (map isa? (extenders @t-var) (repeat s-class))
      (some true?)
      boolean)))

(defmethod subtype?* [Value ProtocolType]
  [{s-val :val :as s}
   {t-var-sym :the-protocol-var :as t}]
  (let [t-var (resolve t-var-sym)]
    (satisfies? @t-var s-val)))

;nil

(def ^:private extends-nil #{ISeq Counted ILookup IObj IMeta Associative})

;hardcode Clojure interfaces that should "extend" to nil
(defmethod subtype?* [NilType ClassType]
  [s {t-class-sym :the-class :as t}]
  (let [t-class (resolve t-class-sym)]
    (boolean
      (some #(identical? t-class %) extends-nil))))

(defmethod subtype?* [NilType NilType]
  [s t]
  true)

(defmethod subtype?* [NilType ProtocolType]
  [s {t-var-sym :the-protocol-var :as t}]
  (let [t-var (resolve t-var-sym)]
    (class-satisfies-protocol? @t-var nil)))

;void primitive

(defmethod subtype?* [PrimitiveClass NilType]
  [{s-class :the-class :as s} t]
  (= 'void s-class))

(defmethod subtype?* [NilType PrimitiveClass]
  [s {t-class :the-class :as t}]
  (= 'void t-class))

;primitives

(def ^:private primitive-coersions
  {'int #{'long}})

(defmethod subtype?* [PrimitiveClass PrimitiveClass]
  [{s-class :the-class :as s}
   {t-class :the-class :as t}]
  (or (= s-class t-class)
      (let [coerce? (contains? (primitive-coersions s-class)
                               t-class)
            _ (when coerce?
                (debug "coercing primitive" s-class "->" t-class))]
        coerce?)))

(def ^:private coersions
  {'double #{Double}
   'long #{Long}
   'byte #{Byte}
   'char #{Character}
   'int #{Integer}
   'float #{Float}
   'short #{Short}
   'boolean #{Boolean}
   'void #{Void}})

(defmethod subtype?* [PrimitiveClass ClassType]
  [{s-pclass :the-class :as s} t]
  (let [possible-types (coersions s-pclass)]
    (boolean
      (some #(subtype? (ClassType-from %) t) possible-types))))

(defmethod subtype?* [ClassType PrimitiveClass]
  [{s-class-sym :the-class :as s}
   {t-pclass :the-class :as t}]
  (let [s-class (resolve s-class-sym)
        coerce? (contains? (coersions t-pclass) s-class)
        _ (when coerce?
            (debug "coercing" s-class "->" t-pclass))]
    coerce?))

;function

(defmethod subtype?* [Fun ClassType]
  [s t]
  (supertype-of-one t (map ClassType-from (-> #() class ancestors))))

(defmethod subtype?* [Fun Fun]
  [{s-arities :arities} {t-arities :arities}]
  (every? true?
          (map #(supertype-of-one % s-arities)
               t-arities)))

(defmethod subtype?* [arity arity]
  [{s-dom :dom 
    s-rng :rng
    s-rest :rest-type
    :as s}
   {t-dom :dom
    t-rng :rng
    t-rest :rest-type
    :as t}]
  (cond
    ;; simple case
    (and (not s-rest)
         (not t-rest))
    (and (subtypes? t-dom s-dom)
         (subtype? s-rng t-rng))

    (not s-rest)
    false

    (and s-rest
         (not t-rest))
    (and (subtypes?*-varargs t-dom s-dom s-rest)
         (subtype? s-rng t-rng))

    (and s-rest
         t-rest)
    (and (subtypes?*-varargs t-dom s-dom s-rest)
         (subtype? t-rest s-rest)
         (subtype? s-rng t-rng))

    :else false))

;seq

(defmethod subtype?* [Seq Seq]
  [{s-type :type :as s} 
   {t-type :type :as t}]
  (subtype? s-type t-type))

(defmethod subtype?* [Seq ProtocolType]
  [s t]
  (subtype? (ClassType-from ISeq) t))

(defmethod subtype?* [Seq ClassType]
  [s t]
  (subtype? (ClassType-from ISeq) t))

(defmethod subtype?* [Vector Seq]
  [{s-type :type :as s} 
   {t-type :type :as t}]
  (subtype? s-type t-type))

(defmethod subtype?* [Sequential Seq]
  [{s-type :type :as s} 
   {t-type :type :as t}]
  (subtype? s-type t-type))

;vectors

(defmethod subtype?* [Vector Vector]
  [{s-type :type :as s} 
   {t-type :type :as t}]
  (subtype? s-type t-type))

(defmethod subtype?* [Vector ProtocolType]
  [s t]
  (subtype? (ClassType-from IPersistentVector) t))

(defmethod subtype?* [Vector ClassType]
  [s t]
  (subtype? (ClassType-from IPersistentVector) t))

(defmethod subtype?* [ConstantVector ProtocolType]
  [s t]
  (subtype? (ClassType-from IPersistentVector) t))

(defmethod subtype?* [ConstantVector ClassType]
  [s t]
  (subtype? (ClassType-from IPersistentVector) t))

(defmethod subtype?* [ConstantVector ConstantVector]
  [{s-types :types :as s} 
   {t-types :types :as t}]
  (subtypes? s-types t-types))

(defmethod subtype?* [ConstantVector Vector]
  [{s-types :types :as s} 
   {t-type :type :as t}]
  (supertype-of-all t-type s-types))

;sequentials

(defmethod subtype?* [Sequential Sequential]
  [{s-type :type :as s}
   {t-type :type :as t}]
  (subtype? s-type t-type))

(defmethod subtype?* [ConstantSequential ProtocolType]
  [s t]
  (subtype? (ClassType-from clojure.lang.Sequential) t))

(defmethod subtype?* [Sequential ProtocolType]
  [s t]
  (subtype? (ClassType-from clojure.lang.Sequential) t))

(defmethod subtype?* [Sequential ClassType]
  [s t]
  (subtype? (ClassType-from clojure.lang.Sequential) t))

(defmethod subtype?* [ConstantVector Sequential]
  [{s-types :types :as s} 
   {t-type :type :as t}]
  (supertype-of-all t-type s-types))

(defmethod subtype?* [Vector Sequential]
  [{s-type :type :as s} 
   {t-type :type :as t}]
  (subtype? s-type t-type))

(defmethod subtype?* [ConstantVector ConstantSequential]
  [{s-types :types :as s} 
   {t-types :types :as t}]
  (subtypes? s-types t-types))

;maps

(defmethod subtype?* [Map ClassType]
  [s t]
  (subtype? (ClassType-from IPersistentMap) t))

(defmethod subtype?* [Map Map]
  [{s-ktype :ktype s-vtype :vtype :as s} 
   {t-ktype :ktype t-vtype :vtype :as t}]
  (subtypes? [s-ktype s-vtype]
             [t-ktype t-vtype]))

;Object

(prefer-method subtype?*
  [Union Type]
  [Type ClassType])

;; everything except nil is a subtype of java.lang.Object
(defmethod subtype?* [Type ClassType]
  [s t]
  (and (isa? Object (resolve (:the-class t)))
       (not (Nil? s))))
;TODO primitives?

;default

(defmethod subtype?* [Type Type]
  [s t]
  false)

#_(+T subtype-of-one [Type (Seq Type) -> boolean])
(defn subtype-of-one
  "True if s is a subtype to at least one ts"
  [s ts]
  (boolean (some #(subtype? s %) ts)))

#_(+T supertype-of-one [Type (Seq Type) -> boolean])
(defn supertype-of-one
  "True if t is a supertype to at least one ss"
  [t ss]
  (boolean (some #(subtype? % t) ss)))

#_(+T subtype-of-all [Type (Seq Type) -> boolean])
(defn subtype-of-all 
  "True if s is subtype of all ts"
  [s ts]
  (every? true?
          (map #(subtype? s %) ts)))

#_(+T supertype-of-all [Type (Seq Type) -> boolean])
(defn supertype-of-all
  "True if t is a supertype of all ss"
  [t ss]
  (every? true?
          (map #(subtype? % t) ss)))

#_(+T subtypes? [(SequentialSeq Type) (SequentialSeq Type) -> boolean])
(defn subtypes? [ss ts]
  (and (= (count ss)
          (count ts))
       (every? true? 
               (map subtype? ss ts))))

(defn subtype? [s t]
  (subtype?* s t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Renaming

#_(+T unique-variable (Fun [-> TypeVariable]
                         [TypeVariable -> TypeVariable]))
(defn- unique-variable
  ([] (-tv (gensym)))
  ([t] 
   (update-in t [:nme] gensym)))

;recursive protocol example, self type, late binding

(defprotocol IVariableRename
  (^{+T (All [a]
          [a (Map TypeVariable TypeVariable) -> a])}
   -rename [this rmap] 
           "Rename all occurrences of variables in rmap in this.
           Assumes no conflicting variables (no inner scopes introduce variables
           that conflict with rmap)"))

#_(+T rename (All [(x <! Type)]
             [x (Map TypeVariable TypeVariable) -> x]))
(defn rename 
  "Rename all occurrences of variables in rmap in t"
  [t rmap]
  (assert (Type? t) t)
  (assert (map? rmap))
  (assert (every? TypeVariable? (keys rmap)))
  (assert (every? Type? (vals rmap)))
  (let [conflicts (set/intersection (-> t tvar-binding set) (-> rmap keys set))]
    ;scope introduces conflicting variables
    (if (seq conflicts)
      (let [renames (into {}
                          (map #(vector % (unique-variable %))
                               conflicts))
            resolved-conflicts 
            (-> (-rename t renames)
              (update-tvar-binding #(replace renames %)))] ;update binding scope
        (-rename resolved-conflicts rmap))
      (-rename t rmap))))

#_(+T rename-all (All [(x <! (Seq Type))]
                 [x (Seq TypeVariable) -> x]))
(defn rename-all [ts vs]
  (doall (map rename ts (repeat vs))))

(extend-protocol IVariableRename
  Union
  (-rename [this rmap]
    (-> this
     (update-in [:types] #(rename-all % rmap))))

  TypeVariable
  (-rename [this rmap]
    (if-let [r (rmap this)]
      r
      this))
  
  ClassType
  (-rename [this rmap] this)

  Fun
  (-rename [this rmap]
    (-> this
      (update-in [:arities] #(set (rename-all % rmap)))))

  arity
  (-rename [this rmap]
    (-> this
      (update-in [:dom] #(rename-all % rmap))
      (update-in [:rest-type] #(when %
                                 (rename % rmap)))
      (update-in [:rng] #(rename % rmap)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Elimination
;;
;; Local Type Inference, Pierce & Turner
;; Section 5.3

(defprotocol IVariableElim
  (^{+T [IVariableElim (Set TypeVariable) -> Type]}
   -promote [this vs] 
           "Promote type until type variables vs do not occur in it.
           Assumes no conflicting variables are introduced by inner scopes.")
  (^{+T [IVariableElim (Set TypeVariable) -> Type]}
   -demote [this vs] 
          "Demote type until type variables vs do not occur in it.
           Assumes no conflicting variables are introduced by inner scopes."))

#_(+T resolve-conflicts [Type (Set TypeVariable) -> Type])
(defn resolve-conflicts
  "Resolves conflicting type variables introduced by inner scopes
  by uniquely renaming them"
  [t vs]
    (assert (Type? t))
    (assert (set? vs))
  (let [conflicts (set/intersection (set (tvar-binding t)) vs)]
    (if (seq conflicts)
      (rename t (into {}
                      (doall (map #(vector % (unique-variable %))
                                  conflicts))))
      t)))

#_(+T promote [Type (Set TypeVariable) -> Type])
(defn promote 
  "Promote type until type variables vs do not occur in it. 
  Handles conflicting inner scopes"
  [t vs]
    (assert (Type? t) t)
    (assert (set? vs))
    (assert (every? TypeVariable? vs))
  (let [no-conflicts (resolve-conflicts t vs)
        frees (set (free-vars t))
        frees-in-bnds (set (mapcat #(-> % :bnd free-vars) frees))
        vs-in-bnds (set/intersection frees-in-bnds vs)]
    (if (seq vs-in-bnds)
      Any                            ; VU-Fun-2 - Give up if vs occur in bounds of variables inside t
      (-promote no-conflicts vs))))

#_(+T demote [Type (Set TypeVariable) -> Type])
(defn demote 
  "Demotes type until type variables vs do not occur in it. 
  Handles conflicting inner scopes"
  [t vs]
    (assert (Type? t))
    (assert (set? vs))
    (assert (every? TypeVariable? vs))
  (let [no-conflicts (resolve-conflicts t vs)
        frees (set (free-vars t))
        frees-in-bnds (set (mapcat #(-> % :bnd free-vars) frees))
        vs-in-bnds (set/intersection frees-in-bnds vs)]
    (if (seq vs-in-bnds)
      Nothing                        ; VD-Fun-2 - Give up if vs occur in bounds of variables inside t
      (-demote no-conflicts vs))))

#_(+T promote-all [(Seq Type) (Set TypeVariable) -> (Seq Type)])
(defn- promote-all [ts vs]
  (doall (map promote ts (repeat vs))))

#_(+T demote-all [(Seq Type) (Set TypeVariable) -> (Seq Type)])
(defn- demote-all [ts vs]
  (doall (map demote ts (repeat vs))))

(extend-protocol IVariableElim
  Union
  (-promote [this vs]
    (union (promote-all (:types this) vs)))
  (-demote [this vs]
    (union (demote-all (:types this) vs)))

  TypeVariable
  (-promote [this vs]
    (if (vs this)
      (:bnd this)
      this))
  (-demote [this vs]
    (if (vs this)
      Nothing
      this))
  
  ClassType
  (-promote [this vs] this)
  (-demote [this vs] this)

  Value
  (-promote [this vs] this)
  (-demote [this vs] this)
  
  NilType
  (-promote [this vs] this)
  (-demote [this vs] this)

  Fun
  (-promote [this vs]
    (-> this
      (update-in [:arities] #(set (promote-all % vs)))))
  (-demote [this vs]
    (-> this
      (update-in [:arities] #(set (demote-all % vs)))))

  arity
  (-promote [this vs]
    (-> this
      (update-in [:dom] #(demote-all % vs))
      (update-in [:rest-type] #(when %
                                 (demote % vs)))
      (update-in [:rng] #(promote % vs))))
  (-demote [this vs]
    (-> this
      (update-in [:dom] #(promote-all % vs))
      (update-in [:rest-type] #(when %
                                 (promote % vs)))
      (update-in [:rng] #(demote % vs))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Generation
;;
;; Local Type Inference, Pierce & Turner
;; Section 5.4

#_(+T EqConstraint (Record Type))
(defconstrainedrecord EqConstraint [eqtype]
  "Type R satisfies an EqConstraint [S] if (= R S)"
  {:pre [(Type? eqtype)]})

#_(+T SubConstraint (Record Type Type))
(defconstrainedrecord SubConstraint [lower upper]
  "Type R satisfies a SubConstraint [S T] if (subtype? R S) and (subtype? R T)"
  {:pre [(Type? lower)
         (Type? upper)
         (subtype? lower upper)]})

(defn sub-constraint [lower upper]
  (assert (subtype? lower upper))
  (->SubConstraint lower upper))

#_(+T constraints (Set Class))
(def constraints #{EqConstraint SubConstraint})
(def Constraint ::constraint)

(doseq [c constraints]
  (derive c Constraint))

#_(+T constraint? [Any -> boolean])
(defn constraint? [a]
  (isa? (class a) Constraint))

#_(+T ConstraintSet (Record (Map TypeVariable Constraint)))
(defconstrainedrecord ConstraintSet [cs]
  "A map of type variables to constraints"
  {:pre [(map? cs)
         (every? TypeVariable? (keys cs))
         (every? constraint? (vals cs))]})

#_(+T empty-constraint-set [(Set TypeVariable) -> ConstraintSet])
(defn empty-constraint-set
  "The empty xs-without-vs constraint set 
  maps each variable x in xs to the constraint [Bot, Top]"
  [xs]
  {:post [ConstraintSet?]}
    (assert (every? TypeVariable? xs))
    (assert (set? xs))
  (->ConstraintSet
    (into {}
          (map vector
               xs
               (repeat (sub-constraint Nothing Any))))))

(declare intersect-constraint-sets)

#_(+T singleton-constraint-set [TypeVariable Constraint (Set TypeVariable) -> ConstraintSet])
(defn singleton-constraint-set
  "The singleton xs-without-vs constraint set
  map variable v to constraint c, and
  maps each variable x in xs to the constraint [Bot, Top]"
  [v c xs]
  {:post [ConstraintSet?]}
    (assert (TypeVariable? v))
    (assert (constraint? c) c)
    (assert (set? xs))
    (assert (every? TypeVariable? xs))
    (assert (not (contains? xs v)))
  (intersect-constraint-sets (empty-constraint-set xs)
                             (->ConstraintSet {v c})))

(defmulti 
  ^{'+T '[Constraint Constraint -> Constraint]}
  intersect-constraint
  (fn [c1 c2] [(class c1) (class c2)]))

(defmethod intersect-constraint [EqConstraint EqConstraint]
  [{l-type :eqtype :as c1} {r-type :eqtype :as c2}]
  (assert (= l-type r-type) "Intersect of two constraints must be both equal")
  c1)

(defmethod intersect-constraint [EqConstraint SubConstraint]
  [{l-type :eqtype :as c1} 
   {r-lower :lower r-upper :upper :as c2}]
  (assert (and (subtype? r-lower l-type)
               (subtype? l-type r-upper)))
  c1)

(defmethod intersect-constraint [SubConstraint EqConstraint]
  [{l-lower :lower l-upper :upper :as c1}
   {r-type :eqtype :as c2}]
  (assert (and (subtype? l-lower r-type)
               (subtype? r-type l-upper)))
  c2)

(defmethod intersect-constraint [SubConstraint SubConstraint]
  [{l-lower :lower l-upper :upper :as c1}
   {r-lower :lower r-upper :upper :as c2}]
  (let [new-lower (union [l-lower r-lower])
        new-upper (-intersection [l-upper r-upper])]
    (assert (subtype? new-lower new-upper))
    (sub-constraint new-lower new-upper)))

#_(+T intersect-constraint-sets [& ConstraintSet * -> ConstraintSet])
(defn- intersect-constraint-sets [& cs]
  (assert (every? ConstraintSet? cs))
  (->ConstraintSet 
    (apply merge-with intersect-constraint (map :cs cs))))

(declare constraint-gen*)

#_(+T constraint-gen [(Set TypeVariable) (Set TypeVariable) Type Type -> ConstraintSet])
(defn constraint-gen
  "Given a set of type variables vs, a set of unknowns xs, and two
  two types s and t, calculates the minimal xs-without-vs constraint set
  C guaranteeing that (subtype? s t)"
  [vs xs s t]
    (assert (set? vs))
    (assert (set? xs))
    (assert (every? TypeVariable? vs))
    (assert (every? TypeVariable? xs))
    (assert (Type? s))
    (assert (Type? t))
  (let [s (resolve-conflicts s (set/union vs xs))
        t (resolve-conflicts t (set/union vs xs))]
    (constraint-gen* vs xs s t)))

(declare constraint-gen-arity)

#_(+T constraint-gen* [(Set TypeVariable) (Set TypeVariable) Type Type -> ConstraintSet])
(defn- constraint-gen*
  "Assumes types have unique variable names"
  [vs xs s t]
  {:post [ConstraintSet?]}
    (assert (set? vs))
    (assert (set? xs))
    (assert (every? TypeVariable? vs))
    (assert (every? TypeVariable? xs))
    (assert (Type? s))
    (assert (Type? t))
    (assert (or (empty?
                  (set/intersection 
                    (set (free-vars s))
                    xs))
                (empty?
                  (set/intersection 
                    (set (free-vars t))
                    xs)))
            (str "Variables " (with-out-str (pr (map :nme xs))) " occur in both types: "
                 (unp s) " " (unp t)))
  (cond
    (= Any t)
    (empty-constraint-set xs)
    
    (= Nothing s)
    (empty-constraint-set xs)

    (contains? xs s)
    (let [r (demote t vs)
          c (sub-constraint Nothing r)]
      (assert (empty?
                (set/intersection 
                  (set (free-vars t))
                  xs))
      (singleton-constraint-set s c (disj xs s))))

    (contains? xs t)
    (let [r (promote s vs)
          c (sub-constraint r Any)]
      (assert (empty?
                (set/intersection 
                  (set (free-vars s))
                  xs)))
      (singleton-constraint-set t c (disj xs t)))

    (= s t)
    (empty-constraint-set xs)

    (and (Fun? s)
         (Fun? t))
    (apply intersect-constraint-sets
           (for [super-arity (:arities t)]
             (constraint-gen-arity
               xs vs
               (some #(similar-arity super-arity %)
                     (:arities s))
               super-arity)))
    
    :else (throw (Exception. "Cannot generate constraint"))))

#_(+T align-doms [arity arity -> (Pair (Seq Type))])
(defn- align-doms [a-sub a-sup]
  (cond
    (and (:rest-type a-sub)
         (:rest-type a-sup))
    (cond
      (= (count (:dom a-sub))
         (count (:dom a-sup)))
      [(concat (:dom a-sub) [(:rest-type a-sub)])
       (concat (:dom a-sup) [(:rest-type a-sup)])]

      (< (count (:dom a-sub))
         (count (:dom a-sup)))
      (let [l (concat (:dom a-sub) [(:rest-type a-sub)])]
        [l 
         (take (count l)
               (concat (:dom a-sup) (repeat (:rest-type a-sup))))])

      :else
      (let [r (concat (:dom a-sup) [(:rest-type a-sup)])]
        [(take (count r)
               (concat (:dom a-sub) [(:rest-type a-sub)]))
         r]))

    (:rest-type a-sub)
    (let [r (:dom a-sup)]
      [(take r 
             (concat (:dom a-sub) (repeat (:rest-type a-sub))))
       r])

    (:rest-type a-sup)
    (let [l (:dom a-sub)]
      [l
       (take (count l)
             (concat (:dom a-sup) (repeat (:rest-type a-sup))))])

    :else
    [(:dom a-sub)
     (:dom a-sup)]))

#_(+T constraint-gen-arity [(Set TypeVariable) (Set TypeVariable) Type Type -> ConstraintSet])
(defn constraint-gen-arity 
  [vs xs a-sub a-sup]
  {:post [ConstraintSet?]}
    (assert (arity? a-sub))
    (assert (arity? a-sup))
  (let [[sub-dom sup-dom]
        (align-doms a-sub a-sup)
        
        dom-constraint-sets (doall (map constraint-gen sup-dom sub-dom))
        rng-constraint-set (constraint-gen (:rng a-sub) (:rng a-sup))]
    (apply intersect-constraint-sets rng-constraint-set dom-constraint-sets)))

(declare match-constraint-arity)

#_(+T match-constraint [(Set TypeVariable) (Set TypeVariable) Type Type -> (Pair Type ConstraintSet)])
(defn match-constraint
  "Returns a vector [u c]. u is equal to whichever of s or t is concrete, and
  c is a constraint set whose solutions make s and t identical"
  [vs xs s t]
  {:post [(vector? %)
          (Type? (first %))
          (ConstraintSet? (second %))]}
    (assert (set? vs))
    (assert (set? xs))
    (assert (every? TypeVariable? vs))
    (assert (every? TypeVariable? xs))
    (assert (Type? s))
    (assert (Type? t))
    ;variables xs does not occur in one type s or t
    (assert (or (empty?
                  (set/intersection 
                    (set (free-vars t))
                    xs))
                (empty?
                  (set/intersection 
                    (set (free-vars t))
                    xs))))
  (cond
    (and (= Any s)
         (= Any t))
    [Any (empty-constraint-set xs)]

    (and (= Nothing s)
         (= Nothing t))
    [Nothing (empty-constraint-set xs)]

    (and (contains? xs s)
         (empty?
           (set/intersection
             (set (free-vars t))
             (set/union vs xs))))
    [t (singleton-constraint-set s (->EqConstraint t) (disj xs s))]

    (and (contains? xs s)
         (empty?
           (set/intersection
             (set (free-vars s))
             (set/union vs xs))))
    [s (singleton-constraint-set t (->EqConstraint s) (disj xs t))]

    (and (= s t)
         (not (contains? xs s)))
    [s (empty-constraint-set xs)]

    (and (Fun? s)
         (Fun? t))
    (apply intersect-constraint-sets
           (for [super-arity (:arities t)]
             (match-constraint-arity
               xs vs
               (some #(similar-arity super-arity %)
                     (:arities s))
               super-arity)))))

#_(+T match-constraint-arity [(Set TypeVariable) (Set TypeVariable) arity arity -> ConstraintSet])
(defn match-constraint-arity
  [vs xs a-sub a-sup]
    (assert (arity? a-sub))
    (assert (arity? a-sup))
  (let [[sub-dom sup-dom]
        (align-doms a-sub a-sup)
        
        dom-constraint-sets (doall (map match-constraint sup-dom sub-dom))
        rng-constraint-set (match-constraint (:rng a-sub) (:rng a-sup))]
    (apply intersect-constraint-sets rng-constraint-set dom-constraint-sets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculating Variance

(def constant ::constant)
(def covariant ::covariant)
(def contravariant ::contravariant)
(def invariant ::invariant)
(def rigid ::rigid)

(def variances #{covariant contravariant invariant constant rigid})

(def any-variance ::any-variance)
(def but-rigid ::but-rigid)

(doseq [v variances]
  (derive v any-variance))

(doseq [v (disj variances rigid)]
  (derive v but-rigid))

#_(+T *current-variance* any-variance)
(def ^:dynamic *current-variance*)
(set-validator! #'*current-variance* variances)

(defmulti 
  ^{'+T '[any-variance any-variance -> any-variance]}
  update-variance vector)

(defmethod update-variance [constant constant] [& v] constant)

(defmethod update-variance [constant covariant] [& v] covariant)
(defmethod update-variance [covariant constant] [& v] covariant)
(defmethod update-variance [covariant covariant] [& v] covariant)

(defmethod update-variance [constant contravariant] [& v] contravariant)
(defmethod update-variance [contravariant constant] [& v] contravariant)
(defmethod update-variance [contravariant contravariant] [& v] contravariant)

(defmethod update-variance [contravariant covariant] [& v] invariant)
(defmethod update-variance [covariant contravariant] [& v] invariant)

(defmethod update-variance [invariant invariant] [& v] invariant)
(defmethod update-variance [invariant contravariant] [& v] invariant)
(defmethod update-variance [invariant covariant] [& v] invariant)
(defmethod update-variance [covariant invariant] [& v] invariant)
(defmethod update-variance [contravariant invariant] [& v] invariant)
(defmethod update-variance [constant invariant] [& v] invariant)
(defmethod update-variance [invariant constant] [& v] invariant)

(defmethod update-variance [but-rigid rigid] [& v] rigid)
(defmethod update-variance [rigid but-rigid] [& v] rigid)
(defmethod update-variance [rigid rigid] [& v] rigid)

(defmacro with-flipped-variance [& body]
  `(binding [*current-variance* (cond
                                  (= rigid *current-variance*) rigid
                                  (= covariant *current-variance*) contravariant
                                  :else covariant)]
     ~@body))

(defmacro with-contravariance [& body]
  `(binding [*current-variance* contravariant]
     ~@body))

(defmacro with-rigidity [& body]
  `(binding [*current-variance* rigid]
     ~@body))

(defmacro with-covariance [& body]
  `(binding [*current-variance* covariant]
     ~@body))

(defprotocol IVariance
  (^{+T [IVariance (Set TypeVariable) (Map Symbol any-variance) -> (Map Symbol any-variance)]}
   collect-variances [this xs m]))

(defn calculate-variances [t xs]
    (assert (Type? t))
    (assert (set? xs))
    (assert (every? TypeVariable? xs))
    (assert (bound? #'*current-variance*))
  (let [t (resolve-conflicts t xs)]
    (collect-variances t xs (zipmap xs (repeat constant)))))

(extend-protocol IVariance
  NilType
  (collect-variances [this xs m] m)

  ClassType
  (collect-variances [this xs m] m)

  Vector
  (collect-variances [this xs m] 
    (collect-variances (:type this) xs m))

  TypeVariable
  (collect-variances [this xs m]
    (let [bnd-variances (with-rigidity
                          (collect-variances (:bnd this) xs m))]
      (merge-with 
        update-variance
        bnd-variances
        (when (xs this)
          (update-in m [this] #(update-variance % *current-variance*))))))

  Union 
  (collect-variances [this xs m]
    (apply merge-with update-variance
           (doall (map #(collect-variances % xs m) (:types this)))))
  
  Fun
  (collect-variances [this xs m]
    (apply
      merge-with update-variance
      (doall (map #(collect-variances % xs m) (:arities this)))))

  arity
  (collect-variances [this xs m]
    (let [dom-variances (with-flipped-variance
                          (doall 
                            (map #(collect-variances % xs m)
                               (concat (:dom this)
                                       (when (:rest-type this)
                                         [(:rest-type this)])))))
          rng-variances (collect-variances (:rng this) xs m)]
      (apply merge-with update-variance
             (concat dom-variances [rng-variances])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculating Type Arguments
;;
;; Local Type Inference, Pierce & Turner
;; Section 5.7

(defprotocol IRigid
  (rigid-constraint? [this]))

(defprotocol ITight
  (tight-constraint? [this]))

(defprotocol IMaxMinType
  (^{+T [IMaxMinType -> Type]}
   max-type [this])
  (^{+T [IMaxMinType -> Type]}
   min-type [this]))

(extend-protocol IMaxMinType
  EqConstraint
  (max-type [this] (:eqtype this))
  (min-type [this] (:eqtype this))

  SubConstraint
  (max-type [this] (:upper this))
  (min-type [this] (:lower this)))

(extend-protocol IRigid
  EqConstraint
  (rigid-constraint? [this] true)

  SubConstraint
  (rigid-constraint? [this] (= (:upper this)
                               (:lower this))))

(extend-protocol ITight
  EqConstraint
  (tight-constraint? [this] true)

  SubConstraint
  (tight-constraint? [this] (and (subtype? (:upper this) (:lower this))
                                 (subtype? (:lower this) (:upper this)))))


;(def-type-alias Substitution (Map Symbol Type))

#_(+T gen-substitution [Type (Set TypeVariable) ConstraintSet -> Substitution])
(defn gen-substitution
  "Given a type and a satisfiable constraint set, return the substitution map"
  [t xs cs]
  {:post [(map? %)
          (every? TypeVariable? (keys %))
          (every? Type? (vals %))]}
    (assert (Type? t))
    (assert (ConstraintSet? cs))
    (assert (set? xs))
    (assert (every? TypeVariable? xs))
  (let [variance (with-covariance
                   (calculate-variances t xs))
        _(prn variance)]
    (into {}
          (doall 
            (for [x xs]
              [x
               (cond
                 (#{constant covariant} (variance x))
                 (min-type (get-in cs [:cs x]))

                 (#{contravariant} (variance x))
                 (max-type (get-in cs [:cs x]))

                 (and (#{invariant} (variance x))
                      (tight-constraint? (get-in cs [:cs x])))
                 (min-type (get-in cs [:cs x]))

                 (and (#{rigid} (variance x))
                      (rigid-constraint? (get-in cs [:cs x])))
                 (min-type (get-in cs [:cs x]))

                 :else
                 (throw (Exception. (str "Substitution is undefined for variables "
                                         (with-out-str (pr (map :nme xs))) " and constraints "
                                         (with-out-str (pr cs))
                                         " for type " (unp t)))))])))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Inference

;; Bidirectional checking (Local Type Inference (2000) Pierce & Turner, Section 4)

(declare tc-expr)

#_(+T tc-expr-check [(Map Any Any) Type -> (Map Any Any)])
(defn tc-expr-check [expr expected-type]
  (let [expr (tc-expr expr :expected-type expected-type)]
    (assert-subtype (type-key expr) expected-type)
    expr))

(defmulti 
  ^{'+T '[(Map Any Any) & Any * -> (Map Any Any)]}
  tc-expr 
  (fn [expr & opts] (:op expr)))

#_(+T tc-exprs [(Seq (Map Any Any)) -> (Seq (Map Any Any))])
(defn tc-exprs [exprs]
  (doall (map tc-expr exprs)))

;number

(defmethod tc-expr :number
  [{:keys [val] :as expr} & opts]
  (assoc expr
         type-key (->Value val)))

;constant

(defmulti 
  ^{'+T '[Any -> Type]}
  constant-type class)

(defmethod constant-type nil
  [_]
  Nil)

(defmethod constant-type IPersistentMap
  [m]
  (->Map (union (doall (map constant-type (keys m))))
         (union (doall (map constant-type (vals m))))))

(defmethod constant-type IPersistentList
  [l]
  (->ConstantSequential (doall (map constant-type l))))

(defmethod constant-type IPersistentVector
  [v]
  (->ConstantVector (doall (map constant-type v))))

#_(defmethod constant-type IPersistentMap
  [r]
  (->ConstantMap (doall (map constant-type (apply concat r)))))

(defmethod constant-type Ratio
  [r]
  (->Value r))

(defmethod constant-type Keyword
  [kw]
  (if (namespace kw)
    (->QualifiedKeyword kw)
    (->Value kw)))

(defmethod constant-type Long
  [s]
  (->Value s))

(defmethod constant-type Symbol
  [s]
  (->Value s))

(defmethod constant-type Character
  [c]
  (->Value c))

(defmethod tc-expr :constant
  [{:keys [val] :as expr} & opts]
  (assoc expr
         type-key (constant-type val)))

;keyword

(defmethod tc-expr :keyword
  [{:keys [val] :as expr} & opts]
  (assoc expr
         type-key (->Value val)))

;string

(defmethod tc-expr :string
  [{:keys [val] :as expr} & opts]
  (assoc expr
         type-key (->Value val)))

;if

(defmethod tc-expr :if
  [expr & opts]
  (let [{:keys [test then else] :as expr}
        (-> expr
          (update-in [:test] tc-expr)
          (update-in [:then] tc-expr)
          (update-in [:else] tc-expr))]
    (assoc expr
           type-key (union [(type-key then)
                            (type-key else)]))))

;do

(defmethod tc-expr :do
  [expr & opts]
  (let [{cexprs :exprs
         :as expr}
        (-> expr
          (update-in [:exprs] #(map tc-expr %)))]
    (assoc expr
           type-key (-> cexprs last type-key))))

;nil

(defmethod tc-expr :nil
  [expr & opts]
  (assoc expr
         type-key Nil))

;def

(defmethod tc-expr :def
  [{:keys [init-provided var] :as expr} & opts]
  (debug "def:" var)
  (cond 
    (.isMacro var) expr
    :else
    (let [expr (-> expr
                 (update-in [:init] #(if init-provided
                                       (tc-expr-check % (type-of var))
                                       %)))]
      (assoc expr
             type-key (ClassType-from Var)))))

;fn

(defmethod tc-expr :fn-expr
  [{:keys [methods] :as expr} & {:keys [expected-type]}]
  (let [{cmethods :methods
         :as expr}
        (-> expr
          (update-in [:methods] (if expected-type
                                  (fn [m]
                                    (doall (map #(tc-expr % :expected-type expected-type) m)))
                                  tc-exprs)))]
    (assoc expr
           type-key (-fun (map type-key cmethods)))))

#_(+T check-fn-method [(Map Any Any) Fun -> (Map Any Any)])
(defn check-fn-method 
  [{:keys [required-params rest-param] :as expr} expected-fun-type]
  (let [[mtched-arity :as mtched-arities]
        (filter #(or (and (not rest-param)
                          (= (count required-params)
                             (count (:dom %))))
                     (and rest-param
                          (<= (count required-params)
                              (count (:dom %)))))
                (:arities expected-fun-type))

        _ (assert (= 1 (count mtched-arities)))

        dom-syms (map :sym required-params)
        rest-sym (:sym rest-param)

        dom-types (:dom mtched-arity)
        rng-type (:rng mtched-arity)
        rest-type (:rest-type mtched-arity)

        actual-rest-arg-type (when rest-param
                               (->Sequential rest-type))

        expr
        (-> expr
          (update-in [:body] #(with-local-types
                                (into {}
                                      (map vector 
                                           (concat dom-syms (when rest-param
                                                              [rest-sym]))
                                           (concat dom-types (when rest-param
                                                               [actual-rest-arg-type]))))
                                (tc-expr %))))]
    (assoc expr
           type-key mtched-arity)))

(defmacro parse-params [dom-syn]
  `(parse-dom '~dom-syn))

(def annotate-param-syntax :-)
(def annotate-dom-syntax :-params)

#_(+T synthesize-fn-method [(Map Any Any) -> (Map Any Any)])
(defn synthesize-fn-method
  [{:keys [required-params rest-param] :as expr}]
  (letfn [(meta-type-annot [expr]
            (assert (-> expr :sym meta (find annotate-param-syntax))
                    (str "No type for parameter " (-> expr :sym)))
            (-> expr :sym meta (get annotate-param-syntax) parse))]
    (let [dom-syms (map :sym required-params)
          rest-sym (:sym rest-param)

          meta-annots-reqd (doall (map meta-type-annot required-params))
          meta-annot-rst (when rest-param
                           (meta-type-annot rest-param))

          _ (assert (every? Type? (concat meta-annots-reqd (when rest-param
                                                             [meta-annot-rst])))
                    "All function parameters must be annotated in synthesis mode")

          dom-types meta-annots-reqd
          rest-type meta-annot-rst
          actual-rest-arg-type (when rest-param
                                 (->Sequential rest-type))

          {cbody :body
           :as expr} 
          (-> expr
            (update-in [:body] #(with-local-types
                                  (into {}
                                        (map vector 
                                             (concat dom-syms (when rest-param
                                                                [rest-sym]))
                                             (concat dom-types (when rest-param
                                                                 [actual-rest-arg-type]))))
                                  (tc-expr %))))

          rng-type (type-key cbody)]
      (assoc expr
             type-key (map->arity
                        {:dom dom-types
                         :rest-type rest-type
                         :rng rng-type})))))

(defmethod tc-expr :fn-method
  [{:keys [required-params rest-param] :as expr} & {expected-fun-type :expected-type}]
  (cond
    expected-fun-type (check-fn-method expr expected-fun-type)
    :else (synthesize-fn-method expr)))

;local binding expr

(defmethod tc-expr :local-binding-expr
  [{:keys [local-binding] :as expr} & opts]
  (debug "local-binding-expr:" (:sym local-binding) ":-" (type-of (:sym local-binding)))
  (assoc expr
         type-key (type-of (:sym local-binding))))

;var

(defmethod tc-expr :var
  [{:keys [var] :as expr} & opts]
  (assoc expr
         type-key (type-of var)))

;invoke

(defn- check-poly [xs arg-types arity-type]
    (assert (set? xs))
    (assert (every? TypeVariable? xs))
    (assert (sequential? arg-types))
    (assert (every? Type? arg-types))
    (assert (arity? arity-type))
  (let [_ (assert (empty?
                    (set/intersection 
                      (set (mapcat #(free-vars (:bnd %)) xs))
                      xs))
                  "xs cannot occur in bounds")

        ;_ (prn "xs" xs)
        bnd-cons (map #(constraint-gen #{} xs % (:bnd %)) xs)
        ;_ (prn "bnd-cons" bnd-cons)
        ;_ (prn "arg-types" arg-types)
        dom-cons (map #(constraint-gen #{} xs %1 %2)
                      arg-types
                      (concat (:dom arity-type)
                              (repeat (:rest-type arity-type))))
        ;_ (prn "dom-cons" bnd-cons)

        subst (gen-substitution (:rng arity-type) xs (apply intersect-constraint-sets
                                                            (concat bnd-cons dom-cons)))
        ;_ (prn "subst" subst)
        ]
    (rename (:rng arity-type) subst)))

#_(+T subst-type-args [(SequentialSeq Type) Fun -> Fun])
(defn- subst-type-args [type-args fun-type]
    (assert (= (count type-args)
               (tvar-binding fun-type)))
    (assert (= 1 (count (:arities fun-type))))
  (let [subst (zipmap (map :nme tvar-binding) type-args)]
    (-> fun-type
      (update-in [:arities] #(set [(rename (first %) subst)])))))

#_(+T invoke-type [(SequentialSeq Type) Fun -> Type])
(defn- invoke-type [arg-types {:keys [arities] :as fun-type}]
  (let [type-args-syn @TYPE-ARGS
        type-args (seq (map parse type-args-syn))

        _ (delete-type-args)

        [mtched-arity :as mtched-arities]
        (filter #(or (and (not (:rest-type %))
                          (= (count (:dom %))
                             (count arg-types)))
                     (and (:rest-type %)
                          (<= (count (:dom %))
                              (count arg-types))))
                arities)

        _ (assert (= 1 (count mtched-arities))
                  (str "Invoke args " (with-out-str (pr (map unparse arg-types)))
                       " do not match any arity in "
                       (unp fun-type)))

        xs (-> fun-type tvar-binding set)

        rng-poly (if (and (seq xs)
                          (empty? type-args))
                   (check-poly xs arg-types mtched-arity))
        _ (prn "rng-poly" rng-poly)
        
        _ (when-not rng-poly ; no type args
            (doall (map assert-subtype arg-types (concat (:dom mtched-arity)
                                                         (when (:rest-type mtched-arity)
                                                           (repeat (:rest-type mtched-arity)))))))]
    (or rng-poly
        (:rng mtched-arity))))

(def type-args-key ::type-args)

(defmethod tc-expr :invoke
  [expr & opts]
  (debug "invoke:" (or (-> expr :fexpr :var)
                         "??"))
  (cond
    ;handle setting type args for next form
    (and (-> :fexpr :var)
         (= `next-form-targs
            (-> :fexpr :var var-or-class->sym)))
    (do (set-type-args (map parse (-> expr :args first :val)))
      (assoc expr
             type-key (parse nil)))

    :else
    (let [{cfexpr :fexpr
           cargs :args
           :as expr}
          (-> expr
            (update-in [:fexpr] tc-expr)
            (update-in [:args] tc-exprs))

          _ (assert-subtype (type-key cfexpr) (ClassType-from IFn))]
      (assoc expr
             type-key (invoke-type (map type-key cargs)
                                   (type-key cfexpr))))))

;let

(defmethod tc-expr :let
  [{:keys [binding-inits] :as expr} & opts]
  (let [[binding-inits local-env]
        (loop [local-env {}  ;accumulate local environment
               binding-inits binding-inits
               typed-binits []]
          (if (empty? binding-inits)
            [typed-binits local-env]
            (let [tbinit (-> (first binding-inits)
                           (update-in [:init]
                                      #(with-local-types local-env ;with local environment so far
                                                         (tc-expr %))))
                  env-entry {(-> tbinit :local-binding :sym) 
                             (-> tbinit :init type-key)}
                  local-env (merge local-env env-entry)]
              (recur local-env
                     (next binding-inits)
                     (concat typed-binits [tbinit])))))

        expr
        (-> expr
          (update-in [:binding-inits] (constantly binding-inits))
          (update-in [:body] #(with-local-types local-env
                                (tc-expr %))))]
    (assoc expr
           type-key (type-key (:body expr)))))

;static-method

(defn tc-method [{:keys [method] :as expr}]
  (let [method-type (method->Fun method)
        {cargs :args
         :as expr} 
        (-> expr
          (update-in [:args] tc-exprs))]
    (assoc expr
           type-key (invoke-type (map type-key cargs)
                                 method-type))))

(defmethod tc-expr :static-method
  [{:keys [method method-name class] :as expr} & opts]
  (assert method (str "Unresolvable static method " method-name))
  (debug "static-method:" class method-name)
  (tc-method expr))

;instance-method

(defmethod tc-expr :instance-method
  [{:keys [method method-name] :as expr} & opts]
  (assert method (str "Unresolvable instance method " method-name))
  (tc-method expr))

;static-field

#_(+T field->Type [java.lang.reflect.Field -> Type])
(defn field->Type [field]
  (resolve-symbol (:type field)))

(defmethod tc-expr :static-field
  [{:keys [field field-name] :as expr} & opts]
  (assert field (str "Unresolvable static field " field-name))
  (assoc expr
         type-key (field->Type field)))

;instance-field

(defmethod tc-expr :instance-field
  [{:keys [field field-name] :as expr} & opts]
  (assert field (str "Unresolvable instance field " field-name))
  (assoc expr
         type-key (field->Type field)))
;map

(defmethod tc-expr :map
  [expr & opts]
  (let [{ckeyvals :keyvals
         :as expr}
        (-> expr
          (update-in [:keyvals] tc-exprs))
        
        keytype (union (map type-key (-> (apply hash-map ckeyvals) keys)))
        valtype (union (map type-key (-> (apply hash-map ckeyvals) vals)))]
    (assoc expr
           type-key (->Map keytype valtype))))

;vector

(defmethod tc-expr :vector
  [expr & opts]
  (let [{cargs :args
         :as expr}
        (-> expr
          (update-in [:args] tc-exprs))]
    (assoc expr
           type-key (->ConstantVector (map type-key cargs)))))

;emptyexpr

(defmulti 
  ^{'+T '[Any -> Type]}
  empty-types class)

(defmethod empty-types IPersistentMap [m] (->Map Nothing Nothing))
(defmethod empty-types IPersistentVector [m] (->Vector Nothing))

(defmethod tc-expr :empty-expr
  [{:keys [coll] :as expr} & opts]
  (assoc expr
         type-key (empty-types coll)))

;case

(defmethod tc-expr :case*
  [expr & opts]
  (let [{cthens :thens
         cdefault :default
         :as expr}
        (-> expr
          (update-in [:tests] tc-exprs)
          (update-in [:thens] tc-exprs)
          (update-in [:default] tc-expr))]
  (assoc expr
         type-key (union (map type-key (concat (when (not= Nothing (type-key cdefault)) ;; hmm should probably filter 
                                                                                        ;; out Nothings from unions
                                                [cdefault])
                                               cthens))))))

;throw

(defmethod tc-expr :throw
  [expr & opts]
  (let [{cexception :exception
         :as expr}
        (-> expr
          (update-in [:exception] tc-expr))

        _ (assert-subtype (type-key cexception) (ClassType-from Throwable))]
    (assoc expr
           type-key Nothing)))

;try

(defmethod tc-expr :try
  [expr & opts]
  (let [{ctry-expr :try-expr
         ccatch-exprs :catch-exprs
         :as expr}
        (-> expr
          (update-in [:try-expr] tc-expr)
          (update-in [:finally-expr] #(if %
                                        (tc-expr %)
                                        %))
          (update-in [:catch-exprs] tc-exprs))]
    (assoc expr
           type-key (union (map type-key (cons ctry-expr ccatch-exprs))))))

(defmethod tc-expr :catch
  [{:keys [local-binding class] :as expr} & opts]
  (let [lenv {(:sym local-binding) (ClassType-from class)}

        {chandler :handler
         :as expr}
        (-> expr
          (update-in [:handler] #(with-local-types lenv
                                   (tc-expr %))))]
    (assoc expr
           type-key (type-key chandler))))


#_(+T constructor->Fun [clojure.reflect.Constructor -> Fun])
(defn constructor->Fun [{:keys [parameter-types declaring-class] :as ctor}]
  (assert ctor "Unresolved constructor")
  (map->Fun
    {:arities #{(map->arity 
                  {:dom (doall (map parse parameter-types))
                   :rng (parse declaring-class)})}}))

(defmethod tc-expr :new
  [{:keys [ctor] :as expr} & opts]
  (let [ctor-fun (constructor->Fun ctor)

        {cargs :args
         :as expr}
        (-> expr
          (update-in [:args] tc-exprs))]
    (assoc expr
           type-key (invoke-type (map type-key cargs) ctor-fun))))

;import

(defmethod tc-expr :import*
  [expr & opts]
  (assoc expr
         type-key (ClassType-from Class)))

;var

(defmethod tc-expr :the-var
  [expr & opts]
  (assoc expr
         type-key (ClassType-from Var)))

(comment

  (with-type-anns
    {float? (predicate Float)
     integer? (predicate Integer)
     takes-float [Float -> Boolean]
     takes-integer [Integer -> Boolean]
     occur [(U Float Integer) -> Boolean]}
    (synthesize-form 
      (do
        (declare takes-integer takes-float)
        (defn occur [a]
          (cond
            (float? a) (takes-float a)
            (integer? a) (takes-integer a)
            :else false)))))

  (with-type-anns
    {identity (All [a]
                   [a -> a])}
    (synthesize-form
      (do
        (defn identity [b]
          b)
        (identity 1))))

  (with-type-anns
    {both-same (All [a]
                    [a a -> a])}
    (synthesize-form
      (do
        (declare both-same)
        (both-same 1 "a"))))

  (tc-expr (+ 1 1))

  (check-namespace 'typed.example.typed)
  (check-namespace 'typed.core)

)

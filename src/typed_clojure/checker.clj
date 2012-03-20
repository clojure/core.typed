(ns typed-clojure.checker
  (:import (clojure.lang Symbol Var IPersistentMap IPersistentCollection Keyword Namespace Atom
                         IPersistentList IPersistentVector IReference Var IObj))
  (:use [analyze.core :only [analyze-path]])
  (:require [analyze.util :as util]
            [typed-clojure.flag :as flag]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Utils

(def third (comp second rest))

(defn var-or-class->sym [var-or-class]
  (assert (or (var? var-or-class)
              (class? var-or-class)))
  (cond
    (var? var-or-class) (symbol (str (.name (.ns var-or-class))) (str (.sym var-or-class)))
    :else (symbol (.getName var-or-class))))

(defn- class-satisfies-protocol 
  "Returns the method that would be dispatched by applying
  an instance of Class c to protocol"
  [protocol c]
  (if (isa? c (:on-interface protocol))
    c
    (let [impl #(get (:impls protocol) %)]
      (or (impl c)
          (and c (or (first (remove nil? (map impl (butlast (@#'clojure.core/super-chain c)))))
                     (when-let [t (@#'clojure.core/reduce1 @#'clojure.core/pref (filter impl (disj (supers c) Object)))]
                       (impl t))
                     (impl Object)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Protocols

(defprotocol ITypedClojureType
  "A protocol for specifying subtyping rules"
  (subtype* [this sub] "A function to determine if sub is a subtype of this"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type Database

; Namespaced symbol -> type
;(+T type-db Atom)
(def type-db (atom {}))

;(+T *local-type-db* IPersistentMap)
(def ^:dynamic *local-type-db* {})

;(+T *recur-frame* (U nil Arity))
(def ^:dynamic *recur-frame* nil)

(defmacro with-recur-frame [typ & body]
  `(binding [*recur-frame* ~typ]
     (assert (type-map? ~typ))
     ~@body))

(defn reset-type-db []
  (swap! type-db (constantly {})))

(defn type-of [qual-sym]
  (if-let [local (*local-type-db* qual-sym)]
    local
    (do (assert (find @type-db qual-sym) (str "No type for " qual-sym))
      (@type-db qual-sym))))

(defmacro with-local-types [type-map & body]
  `(binding [*local-type-db* (merge *local-type-db* ~type-map)]
     ~@body))

;(+T fully-qualified [Symbol -> (U nil Object))
(defn fully-qualified [sym]
  (or (namespace sym)
      (some #(= \. %) (str sym))))

(declare unparse-type)

(defrecord TypeMap [type])

(defn type-map? [m]
  (instance? TypeMap m))

(defn type-map [t & keyvals]
  (assert (not (type-map? t)) (prn t))
  (merge 
    (->TypeMap t)
    (apply hash-map keyvals)))

;(+T add-type [Symbol ITypedClojureType -> nil])
(defn add-type [sym type]
  (assert (symbol? sym))
  (assert (fully-qualified sym))
  (assert (type-map? type))
  (assert (or (not (find @type-db sym))
              (= (@type-db sym) type))
          (str "Conflicting type annotation for " sym
               " Expected: " (@type-db sym)
               " Found: " type
               " (= (@type-db sym) type): " (= (@type-db sym) type)))
  #_(println "add type for" sym (unparse-type type))
  (swap! type-db #(assoc % sym type))
  nil)

(defn- split-dom-syntax
  "Splits domain syntax into [fixed variable]"
  [dom]
  (let [[fixed [_ variable]] (split-with #(not= '& %) dom)]
    [fixed variable]))

(defn- split-arity-syntax 
  "Splits arity syntax into [dom rng]"
  [arity-syntax]
  (assert (some #(= '-> %) arity-syntax) (str "Arity " arity-syntax " missing return type"))
  (let [[dom [_ rng]] (split-with #(not= '-> %) arity-syntax)]
    [dom rng]))

(declare parse-syntax)

(defmacro +T [nme type]
  (when @flag/type-check-flag
    `(let [sym# (if (fully-qualified '~nme)
                  '~nme
                  (symbol (name (ns-name *ns*)) (name '~nme)))]
       (add-type sym# (parse-syntax '~type)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Types

(deftype TypeVariable [sym]
  Object
  (toString [this]
    (with-out-str (pr sym)))
  (equals [this that]
    (identical? sym that)))

(deftype Union [types]
  Object
  (equals [this that]
    (and (instance? Union that)
         (= types (.types that))))
  (toString [this]
    (with-out-str (pr types))))

(def union? (partial instance? Union))

(defn- simplify-union [type-maps]
  (assert (every? type-map? type-maps) type-maps)
  (set (apply concat (map #(cond
                             (union? (:type %))
                             (let [ts (.types (:type %))]
                               (when (seq ts)
                                 (simplify-union ts)))
                             :else [%])
                          type-maps))))

(defn union [& types]
  (let [simp (simplify-union types)
        _ (assert (every? type-map? simp) simp)]
    (->Union simp)))

(def Any (union (type-map nil) (type-map Object)))

(deftype Nothing [])

(deftype Fun [arities]
  Object
  (equals [this that]
    (and (instance? Fun that)
         (= arities (.arities that))))
  (toString [this]
    (str arities)))

(defprotocol IArity
  (fixed-params [this])
  (variable-param [this]))

(deftype Arity [dom rng]
  Object
  (equals [this that]
    (and (instance? Arity that)
         (= dom (.dom that))
         (= rng (.rng that))))
  (toString [this]
    (str (with-out-str (pr dom)) ", " (with-out-str (pr rng))))
  
  IArity
  (fixed-params [this]
    (take-while #(not= :& %) dom))
  (variable-param [this]
    (when (= :&
             (-> dom butlast last))
      (last dom))))

(declare arity?)

(defn fun [& arities]
  (assert (seq arities) "Must provide at least one arity")
  (assert (and (every? type-map? arities) 
               (every? (comp arity? :type) arities))
          (str "All arguments to fun must be arities: " (with-out-str (pr arities))))
  (->Fun arities))

(defn arity [dom rng]
  (assert (every? type-map? (filter #(not= :& %) dom)) (prn dom))
  (assert (type-map? rng) rng)
  (->Arity dom rng))

(def arity? (partial instance? Arity))
(def fun? (partial instance? Fun))

(defn variable-arity? 
  "Returns the arity if variable, otherwise false"
  [arity]
  (assert (instance? Arity arity) arity)
  (= :&
     (-> (.dom arity) butlast last)))

(def ^{:doc "Returns the arity if fixed, otherwise false"}
  fixed-arity? (complement variable-arity?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Syntax

(defprotocol IParseType
  (parse-type* [this]))

(defprotocol IUnparseType
  (unparse-type* [this]))

(def ^:dynamic *type-vars-scope* {})

(defmacro with-type-vars [vars & body]
  `(binding [*type-vars-scope* (merge *type-vars-scope* ~vars)]
     ~@body))

(defn- lookup-type-var [sym]
  (*type-vars-scope* sym))

(extend-protocol IParseType
  Symbol
  (parse-type* [this]
    (let [type-variable-map (lookup-type-var this)]
      (if type-variable-map
        type-variable-map
        (let [ res (resolve this)
              _ (assert (or (not (nil? res))
                            (nil? this))
                        (str "Unresolvable type " this " in " *ns*))]
          (type-map (cond
                      (var? res) (deref res) ; handle protocols
                      :else res))))))

  IPersistentList ; Union and Fun syntax
  (parse-type* [this]
    (cond 
      (= 'U (first this))
      (type-map (apply union (map parse-syntax (rest this))))

      (= 'exist (first this))
      (let [[_ vars body] this
            _ (assert (every? symbol? vars))
            var-type-map (apply merge (map #(let [type-var-map (type-map (->TypeVariable %))]
                                              {% type-var-map}) 
                                           vars))
            _ (assert (map? var-type-map) (prn var-type-map))]
        (assoc (with-type-vars var-type-map
                 (parse-syntax body))
               :type-var-scope
               var-type-map))

      :else
      (type-map (apply fun (map parse-type* this))))) ; don't use implicit single arity syntax

  IPersistentVector ; Arity syntax
  (parse-type* [this]
    (let [[dom rng] (split-arity-syntax this)
          [fixed-dom variable-dom] (split-dom-syntax dom)
          fixed-dom-types (map parse-syntax fixed-dom)
          variable-dom-type (when variable-dom
                              (parse-syntax variable-dom))
          rng-type (parse-syntax rng)]
      (type-map (arity (vec (concat fixed-dom-types (when (some #(= '& %) this)
                                                      [:& variable-dom-type])))
                    rng-type))))
  
  nil
  (parse-type* [this] 
    (type-map nil)))

(declare unparse-type)

(extend-protocol IUnparseType
  Class
  (unparse-type* [this]
    (assert (not (.isPrimitive this))) 
    (symbol (.getName this)))

  Fun
  (unparse-type* [this]
    (apply list (map unparse-type (.arities this))))

  Arity
  (unparse-type* [this]
    (vec
      (let [dom (->> (.dom this)
                  (split-with #(not= :& %))
                  first
                  (map unparse-type))
            dom (if (variable-arity? this)
                  (concat dom ['& (unparse-type (last (.dom this)))])
                  dom)
            rng (unparse-type (.rng this))]
        (concat dom ['-> rng]))))
  
  Union
  (unparse-type* [this]
    (apply list 'U (map unparse-type (.types this))))

  IPersistentMap ;Protocols
  (unparse-type* [this]
    (let [the-var (:var this)]
      (assert (var? the-var))
      (symbol (-> the-var .ns .name str)
              (-> the-var .sym str))))
  
  nil
  (unparse-type* [this]
    nil)

  TypeVariable
  (unparse-type* [this]
    (.sym this))
  
  Object
  (unparse-type* [this]
    (assert false
            (str "Cannot unparse " (with-out-str (pr this))))))

(defn parse-syntax 
  "Type syntax parser, entry point"
  [syn]
  (parse-type* (if (vector? syn)
                 (list syn) ; handle implicit single arity syntax
                 syn)))

(defn- unparse-exist [type-map]
  `(~'exist ~(vec (keys (:type-var-scope type-map)))
     ~(unparse-type (dissoc type-map :type-var-scope))))

(defn unparse-type
  [type-map]
  (assert (type-map? type-map))
  (assert (not (type-map? (:type type-map)))
          (prn "The type-map" type-map))
  (cond
    (:type-var-scope type-map) (unparse-exist type-map)

    :else (unparse-type* (:type type-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Subtyping

(declare matching-arity subtype?)

(extend-protocol ITypedClojureType
  Union
  (subtype* [this sub]
    (cond 
      (instance? Union sub) 
      (every? identity (map #(subtype? (type-map this) %) (.types sub)))

      :else (some #(subtype? % (type-map sub)) (.types this))))

  Fun
  (subtype* [this sub]
    (cond
      (instance? Fun sub)
      (every? identity
              (for [type-arity-map (.arities this)]
                (when-let [sub-arity-map (some (if (variable-arity? (:type type-arity-map))
                                                 #(and (variable-arity? (:type %))
                                                       %)
                                                 #(and (fixed-arity? (:type %))
                                                       (= (count (.dom (:type %)))
                                                          (count (.dom (:type type-arity-map))))
                                                       %))
                                               (.arities sub))]
                  (subtype? type-arity-map sub-arity-map))))
      
      :else
      (= Nothing sub)))

  Arity
  (subtype* [this sub]
    (cond
      (instance? Arity sub)
      (cond
        (variable-arity? this)
        (every? true? 
                (cond
                  (variable-arity? sub)
                  (map subtype? 
                       (conj (vec (fixed-params this))
                             (variable-param this))
                       (conj (vec (fixed-params sub))
                             (variable-param sub)))

                  :else
                  (map subtype? 
                       (concat (fixed-params this)
                               (repeat (variable-param this)))
                       (fixed-params sub))))


        :else
        (and (instance? Arity sub)
             (every? true? (map subtype? (.dom sub) (.dom this)))
             (subtype? (.rng this) (.rng sub))))

      :else
      (= Nothing sub)))

  Class
  (subtype* [this sub]
    (cond 
      (= Nothing sub)
      true

      (and (identical? this Fun) ; (subtype? Fun (fun ...))
           (instance? Fun sub))
      true

      (and (identical? this Arity) ; (subtype? Arity (arity ...))
           (instance? Arity sub))
      true

      (or (.isPrimitive this) 
          (when (class? sub)
            (.isPrimitive sub)))
      (do (println "WARNING: Assuming value of type" this "coerces to instance of" sub)
        true)

      :else
      (if (instance? Union sub)
        (every? true? (map subtype? (repeat (type-map this)) (.types sub)))
        (isa? sub this))))

  IPersistentMap ;; Protocols
  (subtype* [this sub]
    (cond 
      (= Nothing sub)
      true

      (instance? Union sub)
      (every? true? (map subtype? (repeat (type-map this)) (.types sub)))

      (map? sub)  ; both protocols
      (= this sub)

      :else
      (class-satisfies-protocol this sub)))

  Nothing
  (subtype* [this sub]
    false) ;TODO is Nothing a subtype of itself?

  nil
  (subtype* [this sub]
    (or (= Nothing sub)
        (nil? sub))))

;(+T subtype? [IPersistentMap IPersistentMap -> Boolean])
(defn subtype? [type sub]
  (assert (type-map? type) type)
  (assert (not (type-map? (:type type))))
  (assert (type-map? sub))
  (assert (not (type-map? (:type sub))) (unparse-type sub))
  (assert (not (:type-var-scope type)))
  (assert (not (:type-var-scope sub)))
  (boolean (subtype* (:type type) (:type sub))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Front end macros

(defmacro new-type [& body]
  `(def ~@body))

(defmacro deftypeT
  [name [& typed-fields] & opts+specs]
  (let [fields (map first typed-fields)
        gname name
        [interfaces methods opts] (@#'clojure.core/parse-opts+specs opts+specs)
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." gname))
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        [field-args over] (split-at 20 fields)]
    `(let []
       ~(@#'clojure.core/emit-deftype* name gname (vec hinted-fields) (vec interfaces) methods)
       (import ~classname)

       ; Type for generated ->Type factory
       ~(when @flag/type-check-flag
          `(add-type (symbol (str *ns* "/->" '~name)) (parse-syntax '~(vec (concat (map third typed-fields)
                                                                                   ['-> classname])))))
       
       ~(@#'clojure.core/build-positional-factory gname classname fields)

       ; Type for interop dot constructor
       ~(when @flag/type-check-flag
          `(add-type '~classname (parse-syntax '~(vec (concat (map third typed-fields)
                                                              ['-> classname])))))

       ~classname)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type Annotations for core functions

(+T add-type [Symbol ITypedClojureType -> nil])
(+T var-or-class->sym [(U Class Var) -> Symbol])

(+T fun [& Arity -> Fun])
(+T arity [IPersistentCollection ITypedClojureType -> Arity])

(+T union [& ITypedClojureType -> Union])

(+T fully-qualified [Symbol -> Object])
(+T parse-syntax [IParseType -> ITypedClojureType])

;clojure.core

(+T clojure.core/in-ns [Symbol -> Namespace])
(+T clojure.core/resolve ([Symbol -> (U nil Var Class)]
                          [IPersistentMap Symbol -> (U nil Var Class)]))
(+T clojure.core/symbol ([(U Symbol String) -> Symbol]
                         [String String -> Symbol]))
(+T clojure.core/str [& Object -> String])
(+T clojure.core/*ns* Namespace)
(+T clojure.core/atom [Object & Object -> Atom])
(+T clojure.core/first [(U nil IPersistentCollection) -> (U nil Object)])
(+T clojure.core/rest [(U nil IPersistentCollection) -> IPersistentCollection])
(+T clojure.core/next [(U nil IPersistentCollection) -> (U nil IPersistentCollection)])
(+T clojure.core/namespace [(U String Symbol Keyword) -> (U nil String)])
(+T clojure.core/name [(U String Symbol Keyword) -> String])
(+T clojure.core/ns-name [Namespace -> Symbol])
(+T clojure.core/refer [Symbol & Object -> nil])
(+T clojure.core/use [& Object -> nil])
(+T clojure.core/seq? [Any -> Boolean])
(+T clojure.core/apply [Fun & Any -> Any])
(+T clojure.core/hash-map [& Any -> IPersistentMap])
(+T clojure.core/println [& Any -> nil])
(+T clojure.core/number? [Any -> Boolean])
(+T clojure.core/alter-meta! [IReference Fun & Any -> Any])
(+T clojure.core/assoc [(U IPersistentMap IPersistentVector) Any Any & Any -> (U IPersistentMap IPersistentVector)])
(+T clojure.core/assert-same-protocol [Var IPersistentCollection -> Any])
(+T clojure.core/alter-var-root [Var Fun & Any -> Any])
(+T clojure.core/merge [& (U nil IPersistentCollection) -> (U nil IPersistentCollection)])
(+T clojure.core/intern ([(U Symbol Namespace) Symbol -> Var]
                         [(U Symbol Namespace) Symbol Any -> Var]))
(+T clojure.core/with-meta [IObj (U nil IPersistentCollection) -> IObj])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type Checker

(defn type-error []
  (throw (Exception. "Type error")))

(defmulti type-check :op)

(defmethod type-check :literal
  [{:keys [val]}]
  #_(println "literal" (class val))
  (type-map (class val)))

(defmethod type-check :empty-expr
  [{:keys [coll]}]
  (type-map (class coll)))

(defmethod type-check :map
  [{:keys [keyvals]}]
  (assert (every? identity (map type-check keyvals)))
  (type-map clojure.lang.IPersistentMap))

(defmethod type-check :vector
  [{:keys [args]}]
  (assert (every? identity (map type-check args)))
  (type-map clojure.lang.IPersistentVector))

(defmethod type-check :if
  [{:keys [test then else]}]
  (do
    (type-check test)
    (type-map (union (type-check then)
                     (type-check else)))))

(defmethod type-check :new
  [{:keys [ctor class args] :as expr}]
  (let [_ (assert class (util/print-expr expr :env :children))
        fn-type-map (type-of (symbol (.getName class)))]
    (assert fn-type-map "Constructors only typed if declared with deftypeT")
    (let [matched-arity-map (matching-arity fn-type-map args)
          _ (assert matched-arity-map)
          arg-types-maps (map type-check args)]
      (assert (every? true? (map subtype? (.dom (:type matched-arity-map)) arg-types-maps))
              (str (map unparse-type arg-types-maps) " is not a subtype of " (unparse-type matched-arity-map) " , when trying to invoke constructor " ctor))
      (.rng (:type matched-arity-map)))))

;(+T resolve-class-symbol [Symbol -> Class])
(defn- resolve-class-symbol 
  [sym]
  {:post [(type-map? %)]}
  (let [t (case sym
            char Character/TYPE
            boolean Boolean/TYPE
            byte Byte/TYPE
            short Short/TYPE
            int Integer/TYPE
            long Long/TYPE
            float Float/TYPE
            double Double/TYPE
            void nil
            (do (assert (fully-qualified sym) sym)
              (resolve sym)))]
    (assert (or (nil? sym) (= 'void sym) (class? t)) (str sym " expected to resolve to a class, instead " t))
    (type-map t)))

;(+T method->fun [clojure.reflect.Method -> IPersistentMap])
(defn- method->fun [method]
  (let [the-type-map
        (type-map (fun (type-map (arity (->> 
                                          (map resolve-class-symbol (:parameter-types method))
                                          (map #(type-map (union (type-map nil) %)))) ; Java methods can return null

                                        (type-map (-> (resolve-class-symbol (:return-type method))
                                                    (union (type-map nil))))))))]
    the-type-map))

(defmethod type-check :static-field
  [{:keys [field]}]
  (assert (:type field) "No field resolvable, needs type hint")
  (type-map (union (type-map nil) (type-map (:type field)))))

(defmethod type-check :instance-field
  [{:keys [field]}]
  (assert (:type field) "No field resolvable, needs type hint")
  (type-map (union (type-map nil) (type-map (:type field)))))

(defn- check-method [method args]
  (let [method-type-map (method->fun method)
        arg-types-maps (map type-check args)
        matched-arity-map (matching-arity method-type-map args)
        _ (assert matched-arity-map)]
    (assert (every? true? (map subtype? (.dom (:type matched-arity-map)) arg-types-maps))
            (str (map unparse-type arg-types-maps) " is not a subtype of " (unparse-type matched-arity-map)
                 " when checking method "  (with-out-str (pr method))))
    (.rng ^Arity (:type matched-arity-map))))

(defmethod type-check :static-method
  [{:keys [method args]}]
  (let [type-map (check-method method args)]
    type-map))

(defmethod type-check :instance-method
  [{:keys [method args]}]
  (check-method method args))

(defmethod type-check :try
  [{:keys [try-expr catch-exprs finally-expr]}]
  (assert (type-check finally-expr))
  (let [types-maps (map type-check (cons try-expr catch-exprs))]
    (type-map (apply union types-maps))))

(defmethod type-check :var
  [{:keys [var env tag]}]
  (let [sym (var-or-class->sym var)
        type-map (type-of sym)]
    (when tag 
      #_(println "Unused tag " tag ", for " sym "!"))
    type-map))

(defmethod type-check :the-var
  [{:keys [var]}]
  (type-map Var
            :the-var var))

(defn- matching-arity 
  "Returns the matching arity type corresponding to the args, or nil if none match"
  [fun-map args]
  (assert (type-map? fun-map))
  (let [fun (:type fun-map)]
    (assert (instance? Fun fun) fun)
    (or (some #(and (= (count (.dom (:type %)))
                       (count args))
                    %)
              (filter (comp fixed-arity? :type) (.arities fun))) ; fixed arities
        (some #(and (variable-arity? (:type %))
                    %) (.arities fun))))) ; variable arity

(defn rest-type [arity]
  (assert (variable-arity? arity))
  (last (.dom arity)))

(defmethod type-check :invoke
  [{:keys [fexpr args] :as expr}]
  #_(println "Invoking " (:var fexpr))
  (let [fexpr-type-map (type-check fexpr)
        _ (assert fexpr-type-map)
        fn-type-map (let [t (:type fexpr-type-map)]
                      (cond
                        (fun? t) fexpr-type-map
                        (= Var t) (type-of (var-or-class->sym (:the-var fexpr-type-map)))
                        :else (assert false (str "Invoked type must be function or var" (unparse-type fexpr-type-map)))))
        arg-types-maps (map type-check args)
        matched-arity-map (matching-arity fn-type-map args)
        _ (assert matched-arity-map (str "No matching arity for " (unparse-type fn-type-map)
                                         " when applied to " (with-out-str (prn (map unparse-type arg-types-maps)))))
        matched-arity (:type matched-arity-map)
        _ (assert matched-arity-map (util/print-expr expr :children :env :Expr-obj :ObjMethod-obj))
        _ (assert (or (variable-arity? matched-arity)
                      (= (count (.dom matched-arity))
                         (count arg-types-maps))))

        ; Typecheck args compared to fn signature
        _ (assert (every? true? (doall (map subtype? 
                                            (concat (fixed-params matched-arity)
                                                    (when (variable-arity? matched-arity)
                                                      (repeat (rest-type matched-arity))))
                                            arg-types-maps)))
                  (str "Invoke: type error: " 
                       (with-out-str (pr (map unparse-type arg-types-maps))) " is not a subtype of " 
                       (with-out-str (pr (map unparse-type (.dom matched-arity))))))]
    (.rng matched-arity)))

(defmethod type-check :import*
  [{:keys [class-str]}]
  (type-map nil))

(defmethod type-check :deftype*
  [{:keys []}]
  (type-map nil))

(defn- matching-arity-expr
  "Given a map representing a Method Expr Object and a Fun TypeMap, returns the matching
  Arity in the TypeMap to the Method"
  [{:keys [required-params rest-params] :as expr} fn-type-map]
  (assert (type-map? fn-type-map))
  (if rest-params
    (some #(and (variable-arity? (:type %)) %) (.arities (:type fn-type-map)))
    (some #(and (= (count (.dom ^Arity (:type %)))
                   (count required-params))
                %)
          (filter (comp fixed-arity? :type) (.arities (:type fn-type-map))))))


(defmethod type-check :fn-expr
  [{:keys [methods] :as expr}]
  (let [expected-type-map (::expected-type-map expr)
 ;       _ (assert expected-type (str "No type found for function" (util/print-expr expr :env :children :Expr-obj)))

        actual-type 
        (apply fun (for [method methods]
                     (try
                       (let [type-map (type-check (assoc method ::expected-type-map (when expected-type-map
                                                                                      (matching-arity-expr method expected-type-map))))]
                         (assert type-map)
                         type-map)
                       (catch AssertionError e
                         #_(util/print-expr expr :children :env :Expr-obj)
                         (throw e)))))]
    (type-map actual-type)))

(defmethod type-check :fn-method
  [{:keys [required-params rest-param body] :as expr}]
  (let [expected-type-map (::expected-type-map expr)

        ;[[sym type] ...]
        fixed-types (when (seq required-params)
                      (vec (map vector 
                                (map :sym required-params) 
                                (if expected-type-map
                                  (.dom ^Arity (:type expected-type-map))
                                  (let [ann-doms (map #(let [sym (:sym %)
                                                             [_ syntax :as m] (-> sym meta (find :+T))
                                                             _ (assert m (str "No type annotation for parameter " sym))]
                                                         (parse-syntax syntax))
                                                      required-params)]
                                    (assert (= (count ann-doms) 
                                               (count required-params)))
                                    (assert (every? #(satisfies? ITypedClojureType %) ann-doms) 
                                            (with-out-str (pr ann-doms)))
                                    ann-doms)))))

        ;[sym type]
        rest-type (when rest-param
                    [[(:sym rest-param) (type-map clojure.lang.ISeq)]])
        local-types (apply hash-map (flatten (concat fixed-types rest-type)))
        dom-types-maps (if expected-type-map
                         (.dom (:type expected-type-map))
                         (vec (concat (map second fixed-types) 
                                      (when rest-type
                                        [:& (type-map Object)])))) ; TODO hardwired, recover this info
        rng-type-map (with-local-types local-types
                       (with-recur-frame (type-map (arity dom-types-maps (type-map Nothing)))
                         (type-check body)))]
    (type-map (arity dom-types-maps rng-type-map))))

(defmethod type-check :local-binding-expr
  [{:keys [local-binding]}]
  (let [type-map (type-check local-binding)]
    type-map))

(defmethod type-check :local-binding
  [{:keys [sym init]}]
  (type-of sym))

(defmethod type-check :recur
  [{:keys [loop-locals args]}]
  (let [expected-arity-map *recur-frame*
        _ (assert (arity? (:type expected-arity-map)) "No recur frame")
        arg-types-maps (map type-check args)
        _ (assert (let [op (if (variable-arity? (:type expected-arity-map))
                             <=
                             =)]
                    (op (count (fixed-params (:type expected-arity-map)))
                        (count arg-types-maps)))
                  "Not enough arguments")
        _ (map (fn [ty-map sub-map]
                 (assert (subtype? ty-map sub-map) (str (unparse-type sub-map) " is not a subtype of "
                                                        (unparse-type ty-map))))
               (concat (fixed-params (:type expected-arity-map))
                       (when (variable-arity? (:type expected-arity-map))
                         (repeat (variable-param (:type expected-arity-map)))))
               arg-types-maps)]
    (.rng (:type expected-arity-map))))

(defmethod type-check :let
  [{:keys [env binding-inits body is-loop]}]
  (letfn [(merge-entries [vec-entries]
            (apply merge (map (partial apply hash-map) vec-entries)))]
    (let [local-types-vec ; [[sym type] ...]
          (loop [local-types-vec []
                 binding-inits binding-inits]
            (if (seq binding-inits)
              (let [{sym :sym, init :init} (:local-binding (first binding-inits))
                    local-types (merge-entries local-types-vec)
                    [_ t-syn :as has-annot] (-> sym meta (find :+T))
                    t-annot (parse-syntax t-syn)
                    bnd-type-map (with-local-types local-types
                                   (type-check (if has-annot
                                                 (assoc init ::expected-type-map t-annot)
                                                 init)))
                    sym-type-map (if has-annot
                                   t-annot
                                   bnd-type-map)]
                (recur (vec (conj local-types-vec [sym sym-type-map]))
                       (rest binding-inits)))
              local-types-vec))
          dom-types-maps (map second local-types-vec)
          local-types (merge-entries local-types-vec)
          type-map (with-local-types local-types
                     (if is-loop
                       (with-recur-frame (type-map (arity dom-types-maps (type-map Nothing)))
                         (type-check body))
                       (type-check body)))]
      type-map)))

(defmethod type-check :do
  [{:keys [exprs]}]
  (doseq [expr (butlast exprs)]
    (assert (type-check expr)))
  (let [type-map (type-check (last exprs))]
    type-map))

(defmethod type-check :def
  [{:keys [env init-provided init var]}]
  #_(println "type checking" var)
  (if init-provided
    (let [expected-type-map (type-of (var-or-class->sym var))
          actual-type-map (type-check (assoc init ::expected-type-map expected-type-map))]
      (assert (subtype? expected-type-map actual-type-map) (str "Found " (unparse-type actual-type-map)
                                                            " where expecting " (unparse-type expected-type-map)))
      actual-type-map)
    (do
      (println "No init provided for" (var-or-class->sym var))
      (type-map Nothing))))

(defmethod type-check :default
  [expr]
  (println (:op expr))
  #_(util/print-expr expr :children :Expr-obj :ObjMethod-obj)
  (type-error))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; # Type checker interface

(defn type-check-path [path nsym]
  (swap! flag/type-check-flag (constantly false))
  (require nsym)
  (swap! flag/type-check-flag (constantly true))
  (let [analysis (analyze-path path nsym)]
    (binding [*ns* nsym]
      (doseq [a analysis]
        (assert (type-check a) (util/print-expr a :children :env :Expr-obj)))))
  (swap! flag/type-check-flag (constantly false)))

(defn- type-check-one [form ns]
  (let [a (analyze.core/analyze-one {:ns {:name ns} :context :eval} form)]
    (type-check a)))

(comment
  (util/print-expr (analyze.core/analyze-one {:ns {:name 'user} :context :eval} '(def my-atom (atom {})))
  :children :env :Expr-obj))


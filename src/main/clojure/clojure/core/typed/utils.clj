(ns clojure.core.typed.utils
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.util-vars :refer [*current-env*] :as uvs]
            [clojure.core.contracts.constraints :as contracts]
            [clojure.repl :as repl]
            [clojure.core.contracts]
            [clojure.tools.analyzer :as analyze]
            [clojure.tools.analyzer.hygienic :as hygienic]
            [clojure.set :as set])
  (:import (clojure.lang PersistentArrayMap Var Symbol)))

(t/ann subtype-exn Exception)
(def subtype-exn (Exception. "Subtyping failed."))
(t/ann cs-gen-exn Exception)
(def cs-gen-exn (Exception. "Constraint generation failed."))

(defmacro handle-subtype-failure [& body]
  `(try
     ~@body
     (catch Exception e#
       (if (identical? subtype-exn e#)
         false
         (throw e#)))))

(defmacro handle-cs-gen-failure [& body]
  `(try
     ~@body
     (catch Exception e#
       (if (identical? cs-gen-exn e#)
         false
         (throw e#)))))

(declare emit-form-fn)

(t/ann ^:nocheck nat? (predicate t/AnyInteger))

(t/tc-ignore
(defn every-c? [c]
  #(every? c %))

;[Any * -> String]
(defn ^String error-msg 
  [& msg]
  (apply str (when *current-env*
               (str (:line *current-env*) ":"
                    (:col *current-env*)
                    " "))
         (concat msg)))

;errors from check-ns or cf
(defn top-level-error? [{:keys [type-error] :as exdata}]
  (boolean (#{:top-level-error} type-error)))

(defmacro top-level-error-thrown? [& body]
  `(with-ex-info-handlers
     [top-level-error? (constantly true)]
     ~@body
     false))

(def tc-error-parent ::tc-error-parent)

(defn tc-error? [exdata]
  (assert (not (instance? clojure.lang.ExceptionInfo exdata)))
  (isa? (:type-error exdata) tc-error-parent))

(defn tc-delayed-error [msg & {:keys [return form] :as opt}]
  (when-not (:line *current-env*)
    (try (throw (Exception. ""))
      (catch Exception e
        (prn "core.typed Internal BUG! Delayed error without line number, "
             (when (contains? opt :form) (str "in form " form)))
        (prn "with env:" (pr-str *current-env*))
        #_(binding [*err* *out*] 
          (repl/pst e)))))
  (let [e (ex-info msg (merge {:type-error tc-error-parent}
                              (when (or (contains? opt :form)
                                        (and (bound? #'uvs/*current-expr*)
                                             uvs/*current-expr*))
                                {:form (if (contains? opt :form)
                                         form
                                         (emit-form-fn uvs/*current-expr*))})
                              (when-let [env *current-env*]
                                {:env env})))]
    (cond
      ;can't delay here
      (not (bound? #'clojure.core.typed/*delayed-errors*))
      (throw e)

      :else
      (do
        (swap! clojure.core.typed/*delayed-errors* conj e)
        (or return @(ns-resolve (find-ns 'clojure.core.typed.type-rep) '-nothing))))))

(defn derive-error [kw]
  (derive kw tc-error-parent))

(def int-error-kw ::internal-error)
(def nyi-error-kw ::nyi-error)

(derive-error int-error-kw)
(derive-error nyi-error-kw)

(defn tc-error
  [estr]
  (when-not *current-env*
    (prn "Internal core.typed BUG! No *current-env* with tc-error"))
  (let [env *current-env*]
    (throw (ex-info (str "Type Error "
                         "(" (-> env :ns :name) ":" (:line env) 
                         (when-let [col (:column env)]
                           (str ":"col))
                         ") "
                         estr)
                    {:type-error tc-error-parent}))))

(defn int-error
  [estr]
  (let [env *current-env*]
    (throw (ex-info (str "Internal Error "
                         "(" (-> env :ns :name) ":" (:line env) 
                         (when-let [col (:column env)]
                           (str ":"col))
                         ") "
                         estr)
                    {:type-error int-error-kw}))))

(defn nyi-error
  [estr]
  (let [env *current-env*]
    (throw (ex-info (str "core.typed Not Yet Implemented Error:"
                           "(" (-> env :ns :name) ":" (:line env) 
                           (when-let [col (:column env)]
                             (str ":"col))
                           ") "
                           estr)
                    {:type-error nyi-error-kw}))))

(defmacro with-ex-info-handlers 
  "Handle an ExceptionInfo e thrown in body. The first handler whos left hand
  side returns true, then the right hand side is called passing (ex-info e) and e."
  [handlers & body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo e#
       (let [found?# (atom false)
             result# (reduce (fn [_# [h?# hfn#]]
                               (when (h?# (ex-data e#))
                                 (reset! found?# true)
                                 (reduced (hfn# (ex-data e#) e#))))
                             nil
                             ~(mapv vec (partition 2 handlers)))]
         (if @found?#
           result#
           (throw e#))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

; Temp copy from core.contracts. Faster predicates.
(defmacro defconstrainedrecord
  [name slots inv-description invariants & etc]
  (let [fields (vec slots)
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." name))
        ctor-name (symbol (str name \.))
        positional-factory-name (symbol (str "->" name))
        map-arrow-factory-name (symbol (str "map->" name))
        pred-arg (gensym)
        chk `(clojure.core.contracts/contract
                ~(symbol (str "chk-" name))
                ~inv-description
                [{:keys ~fields :as m#}]
                ~invariants)]
    `(do
       (clojure.core/defrecord ~name ~fields ~@etc)
       (defn ~(symbol (str name \?)) [~pred-arg]
         (instance? ~name ~pred-arg))

       ~(@#'clojure.core.contracts.constraints/build-positional-factory name classname fields invariants chk)

       (clojure.core.contracts.constraints/defconstrainedfn ~map-arrow-factory-name
         ([{:keys ~fields :as m#}]
            ~invariants
            (with-meta
              (merge (new ~name ~@(for [e fields] nil)) m#)
              {:contract ~chk})))
       ~name)))

(comment
  (defconstrainedrecord A [] "" [])
  (-> (clojure.tools.analyzer/ast (defconstrainedrecord A [] "" []))
      :exprs second clojure.pprint/pprint)
  )

(defmacro defrecord [name slots inv-description invariants & etc]
  ;only define record if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(defconstrainedrecord ~name ~slots ~inv-description ~invariants ~@etc)))

(defmacro defprotocol [name & args]
  ;only define record if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(clojure.core/defprotocol ~name ~@args)))

(def third (comp second next))

(defmacro ann-record 
  "Like ann-record, but also adds an unchecked annotation for core.contract's generated
  nme? predicate."
  [nme & args]
  `(do (clojure.core.typed/ann-record ~nme ~@args)
       (clojure.core.typed/ann ~(with-meta (symbol (str nme "?")) {:nocheck true}) ~(list 'predicate nme))))

(defmacro ann-precord 
  "Like ann-precord, but also adds an unchecked annotation for core.contract's generated
  nme? predicate."
  [nme & args]
  `(do (clojure.core.typed/ann-precord ~nme ~@args)
       (clojure.core.typed/ann ~(with-meta (symbol (str nme "?")) {:nocheck true}) ~(list 'predicate nme))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST ops


;AnalysisExpr -> Form
;(ann emit-form-fn [Any -> Any])
(def emit-form-fn hygienic/emit-hy)

(defn constant-expr [expr]
  (case (:op expr)
    (:constant :keyword :number :string :nil :boolean) (:val expr)
    :empty-expr (:coll expr)))

(defn constant-exprs [exprs]
  (map constant-expr exprs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint shorthands

(def nat? (every-pred integer? (complement neg?)))

(def boolean? (some-fn true? false?))

(defn =-c? [& as]
  #(apply = (concat as %&)))

(defn hvector-c? [& ps]
  (apply every-pred vector?
         (map (fn [p i] #(p (nth % i false))) ps (range))))

(defn array-map-c? [ks-c? vs-c?]
  (every-pred #(instance? PersistentArrayMap %)
              #(every? ks-c? (keys %))
              #(every? vs-c? (vals %))))

(defn hmap-c? [& key-vals]
  (every-pred map?
              #(every? identity 
                       (for [[k vc] (partition 2 key-vals)]
                         (and (contains? % k)
                              (vc (get % k)))))))

(defn hash-c? [ks-c? vs-c?]
  (every-pred map?
              #(every? ks-c? (keys %))
              #(every? vs-c? (vals %))))

(defn set-c? [c?]
  (every-pred set?
              #(every? c? %)))

(defn sequential-c? [c?]
  (every-pred sequential?
              (every-c? c?)))

(def set-union (fnil set/union #{}))
(def set-difference (fnil set/difference #{}))

;(defn- comp-mm [mm disps]
;  (set/difference disps (set (keys (methods mm)))))
;
;(comp-mm replace-image (disj kinds :scope))
;(comp-mm replace-image (disj kinds :scope))

) ;end tc-ignore

;TODO to check, needs support for instance field
(t/ann ^:nocheck var->symbol [Var -> Symbol])
(defn var->symbol [^Var var]
  {:pre [(var? var)]
   :post [(symbol? %)
          (namespace %)]}
  (symbol (str (ns-name (.ns var)))
          (str (.sym var))))

(t/ann symbol->Class [Symbol -> Class])
(defn symbol->Class 
  "Returns the Class represented by the symbol. Works for
  primitives (eg. byte, int). Does not further resolve the symbol."
  [sym]
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

(t/ann Class->symbol [Class -> Symbol])
(defn Class->symbol [^Class cls]
  {:pre [(class? cls)]
   :post [(symbol? %)]}
  (symbol (.getName cls)))

(try 
  (require '[taoensso.timbre.profiling])
  (catch Exception e))

(defmacro p [name & body]
  (if (find-ns 'taoensso.timbre.profiling)
    `(taoensso.timbre.profiling/p ~name ~@body)
    `(do ~@body)))

(defmacro profile [& body]
  `(taoensso.timbre.profiling/profile ~@body))

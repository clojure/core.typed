(ns clojure.core.typed.utils
  (:refer-clojure :exclude [defrecord])
  (:import (clojure.lang PersistentArrayMap Var))
  (:require [clojure.core.typed.util-vars :refer [*current-env*] :as uvs]
            [clojure.core.contracts.constraints :as contracts]
            [clojure.repl :as repl]
            [clojure.core.contracts]
            [clojure.tools.analyzer :as analyze]
            [clojure.tools.analyzer.hygienic :as hygienic]))

(declare emit-form-fn)

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
        (prn "core.typed Internal BUG! Delayed error without line number, stacktrace following...")
        (prn "with env:" (pr-str *current-env*))
        (repl/pst e))))
  (swap! clojure.core.typed/*delayed-errors*
         conj (ex-info msg (merge {:type-error tc-error-parent}
                                  (when (or (contains? opt :form)
                                            (and (bound? #'uvs/*current-expr*)
                                                 uvs/*current-expr*))
                                    {:form (if (contains? opt :form)
                                             form
                                             (emit-form-fn uvs/*current-expr*))})
                                  (when-let [env *current-env*]
                                    {:env env}))))
  (or return @(ns-resolve (find-ns 'clojure.core.typed.type-rep) '-nothing)))

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
  (throw (ex-info estr 
                  {:type-error int-error-kw})))

(defn nyi-error
  [estr]
  (throw (ex-info estr 
                  {:type-error nyi-error-kw})))

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

(defmacro defrecord [name slots inv-description invariants & etc]
  ;only define record if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(contracts/defconstrainedrecord ~name ~slots ~inv-description ~invariants ~@etc)))

(def third (comp second next))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST ops


;AnalysisExpr -> Form
;(ann emit-form-fn [Any -> Any])
(def emit-form-fn hygienic/emit-hy)

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

;(defn- comp-mm [mm disps]
;  (set/difference disps (set (keys (methods mm)))))
;
;(comp-mm replace-image (disj kinds :scope))
;(comp-mm replace-image (disj kinds :scope))


(defn var->symbol [var]
  {:pre [(var? var)]
   :post [(symbol? %)
          (namespace %)]}
  (symbol (str (ns-name (.ns ^Var var)))
          (str (.sym ^Var var))))

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

(defn Class->symbol [^Class cls]
  {:pre [(class? cls)]
   :post [(symbol? %)]}
  (symbol (.getName cls)))

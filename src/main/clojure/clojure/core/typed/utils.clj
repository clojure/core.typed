(ns clojure.core.typed.utils
  (:refer-clojure :exclude [defrecord])
  (:import (clojure.lang PersistentArrayMap Var))
  (:require [clojure.core.contracts.constraints :as contracts]
            [clojure.core.contracts]
            [clojure.tools.analyzer :as analyze]
            [clojure.tools.analyzer.hygienic :as hygienic]))

(declare ^:dynamic *current-env*)

;[Any * -> String]
(defn ^String error-msg 
  [& msg]
  (apply str (when *current-env*
               (str (:line *current-env*) ":"
                    (:col *current-env*)
                    " "))
         (concat msg)))

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

(defn every-c? [c]
  #(every? c %))

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

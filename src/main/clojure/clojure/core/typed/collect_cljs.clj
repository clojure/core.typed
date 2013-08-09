(ns clojure.core.typed.collect-cljs
  (:require [cljs.core.typed :as t]
            [clojure.core.typed.analyze-cljs :as ana]
            [clojure.core.typed.type-rep :as r :refer [ret]]
            [clojure.core.typed.check :as chk]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.util-vars :as vs]))

(defn- collected-ns! [nsym]
  (swap! t/*already-collected* conj nsym))

(defn- already-collected? [nsym]
  (boolean (@t/*already-collected* nsym)))

(declare collect)

(defn collect-ns
  "Collect type annotations and dependency information
  for namespace symbol nsym, and recursively check 
  declared typed namespace dependencies."
  ([nsym]
   (if (already-collected? nsym)
     (do #_(println (str "Already collected " nsym ", skipping"))
         #_(flush)
         nil)
     (do (collected-ns! nsym)
         (let [asts (ana/ast-for-ns nsym)]
           (doseq [ast asts]
             (collect ast)))))))

(defmulti collect (fn [expr] (:op expr)))
(defmulti invoke-special-collect (fn [expr]
                                   (when (#{:var} (-> expr :f :op))
                                     (-> expr :f :info :name))))

(defmethod invoke-special-collect 'cljs.core.typed/ann*
  [{[target texpr :as args] :args :keys [env] :as expr}]
  (assert (= (count args) 2) "Wrong arguments to ann")
  (binding [vs/*current-env* env
            vs/*current-expr* expr]
    (let [current-ns (chk/expr-ns expr)
          target-sym (:form target)
          _ (assert (symbol? target-sym) "First argument to ann must be a symbol")
          qsym (if (namespace target-sym)
                 target-sym
                 (symbol (str current-ns) (str target-sym)))
          t (prs/with-parse-ns current-ns
              (prs/parse-type (:form texpr)))]
      (var-env/add-var-type qsym t))))

; only collect invocations in arbitrarily nested `do` blocks
(defmethod collect :do
  [{:keys [statements ret]}]
  (doall (map collect (concat statements [ret]))))

(defmethod collect :invoke
  [expr]
  (invoke-special-collect expr))

(defmethod collect :default
  [expr]
  nil)

(defmethod invoke-special-collect :default
  [expr]
  nil)

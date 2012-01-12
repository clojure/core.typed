(ns typed-clojure.tag
  (:require [analyze.core :as analyze])
  (:use [analyze.util :only [print-expr]]))

(def ^:dynamic *local-types* (atom {}))
(defonce *global-types* (atom {}))

(defrecord Type [class])
(defrecord NotType [class])
(defrecord Union [types])

(def NilType (Type. nil))

(def Any (Union. [(Type. Object) NilType]))
(def Nothing (Union. []))

(defmulti resolve-type :op)

(defmethod resolve-type :literal
  [expr]
  (map->Type {:class (class (:val expr))}))

(defmethod resolve-type :invoke
  [expr]
  (if-let [tag (:tag expr)]
    (map->Type {:class tag})
    (println "No tag for" (-> expr :fexpr :var))))
  
(defmethod resolve-type :default
  [expr]
  nil)

(defn check-binding-inits 
  [bindings]
  {:pre [(every? #(= :binding-init (:op %)) bindings)]}
  (doseq [bnd bindings]
    (when-not (-> bnd :local-binding :tag)
      (let [t (resolve-type (:init bnd))]
        (if t
          (println "Inferred type" t "for" (-> bnd :local-binding :sym))
          (println "No :tag for" (-> bnd :local-binding :sym)))
        (swap! *local-types* assoc (-> bnd :local-binding :sym) t)))))

(defn check-tag [expr]
  (cond
    (:binding-inits expr) (check-binding-inits (:binding-inits expr)))
  (doseq [c (:children expr)]
    (check-tag c)))

(comment

(check-tag 
  (analyze/analyze-one {:ns {:name 'clojure.core} :context :eval} 
                       '(let [a 1])))

(check-tag 
  (analyze/analyze-one {:ns {:name 'clojure.core} :context :eval} 
                       '(let [a (symbol "a")])))

(check-tag 
  (analyze/analyze-one {:ns {:name 'clojure.core} :context :eval} 
                       '(let [a (map + [1 2 3])])))

#_(print-expr 
  (analyze/analyze-one {:ns {:name 'clojure.core} :context :eval} 
                       '(let [a (symbol "a")]))
  :children :Expr-obj :ObjMethod-obj :LocalBinding-obj :env :BindingInit-obj)

#_(print-expr 
  (analyze/analyze-one {:ns {:name 'clojure.core} :context :eval} 
                       '(def a))
  :children :Expr-obj :ObjMethod-obj :LocalBinding-obj :env :BindingInit-obj)
  )

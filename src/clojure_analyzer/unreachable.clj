(ns clojure-analyzer.unreachable
  (:require [clojure-analyzer.compiler :as analyzer]))

(def imports (atom {}))

(defn find-maps-with-entry 
  "Recursively search a seq for maps with the map entry"
  ([ds k v] (find-maps-with-entry ds k v []))
  ([ds k v hits]
   (cond
     (nil? ds) hits
     (not (or (seq? ds)
              (vector? ds)
              (map? ds))) nil
     (or (seq? ds)
         (vector? ds)) (when (seq ds)
                         (concat hits
                                 (find-maps-with-entry (first ds) k v)
                                 (find-maps-with-entry (rest ds) k v)))
     (map? ds) (when (seq ds)
                 (if (= [k v] (find ds k))
                   [ds]
                   (concat hits
                           (find-maps-with-entry (vals (dissoc ds :env)) k v)))))))

(defmulti check-unreachable 
  (fn [e] (assert (map? e))
    (:op e)))

(defmethod check-unreachable :constant
  [exp])

(defmethod check-unreachable :var
  [exp])

(defmethod check-unreachable :local
  [exp])

(defmethod check-unreachable :set
  [exp])

(defmethod check-unreachable :recur
  [exp])

(defmethod check-unreachable :map
  [exp])

(defmethod check-unreachable :vector
  [exp])

(defmethod check-unreachable :def
  [{:keys [init]}]
  (check-unreachable init))

(defmethod check-unreachable :if
  [{:keys [then else test form]}]
  (if (and (= (:op test) :constant)
           (not (:form test))
           (not= nil (:form then)))
    (println "Warning: unreachable then clause")
    (check-unreachable then))

  (if (and (= (:op test) :constant)
           (:form test)
           (not= nil (:form else))) 
    (println "Warning: unreachable else clause" form)
    (check-unreachable else)))

(defmethod check-unreachable :fn
  [{:keys [methods]}]
  (doseq [m methods
          child (:children m)]
    (check-unreachable child)))

(defmethod check-unreachable :invoke
  [{:keys [children]}]
  (doseq [child children]
    (check-unreachable child)))

(defmethod check-unreachable :let
  [{:keys [children bindings]}]
  (doseq [child children]
    (check-unreachable child))
  (doseq [bnd bindings]
    (check-unreachable (:init bnd))))

(defmethod check-unreachable :do
  [{:keys [children]}]
  (doseq [child children]
    (check-unreachable child)))

(defmethod check-unreachable :default
  [{:keys [op] :as exp}]
  (when (not (#{:ns} op))
    (throw (Exception. (str "Fell through :op " op (class exp))))))

(defn find-unreachable-clauses [asts]
  (assert (not (map? asts)))
  (doseq [ast asts]
    (check-unreachable ast)))

(do
  (find-unreachable-clauses (analyzer/analyze-namespace 'clojure.set))
;  (find-unreachable-clauses (analyzer/analyze-namespace 'clojure.repl))
;  (find-unreachable-clauses (analyzer/analyze-namespace 'clojure.java.io))

  (find-unreachable-clauses [(analyzer/analyze {:ns {:name 'myns}} 
                                              '(cond
                                                 :else 2))])

  (find-unreachable-clauses [(analyzer/analyze {:ns {:name 'myns}} 
                                              '(cond 
                                                 (fn-call 1 2 3) 1
                                                 :else 2
                                                 :another-else 3))])
;Warning: unreachable else clause (if :else 2 (clojure.core/cond :another-else 3))

  (find-unreachable-clauses [(analyzer/analyze {:ns {:name 'myns}} 
                                              '(cond 
                                                 :constant 2
                                                 (fn-call 1 2 3) 1
                                                 :else 2
                                                 :another-else 3))]))
;Warning: unreachable else clause (if :constant 2 (clojure.core/cond (fn-call 1 2 3) 1 :else 2 :another-else 3))

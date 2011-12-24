(ns clojure-analyzer.imports
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

(count (find-maps-with-entry (analyzer/analyze-namespace 'clojure.set) :op :var))

;(defn analyze-imports )
;
;(let [ns-ast (analyzer/analyze-namespace 'clojure-analyzer.test)]
;  (swap! imports (constantly {}))
;  (doseq [ast ns-ast]
;

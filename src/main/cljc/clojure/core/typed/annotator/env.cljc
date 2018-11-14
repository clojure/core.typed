(ns clojure.core.typed.annotator.env
  )

(defn initial-results 
  ([] (initial-results nil []))
  ([parent base]
   {:infer-results #{}
    :equivs []
    ;; TODO
    :path-occ {}}))

(defn infer-results? [m]
  (and (map? m)
       (-> m :infer-results set?)
       (-> m :path-occ map?)
       (-> m :equivs vector?)))

; results-atom : (Atom InferResultEnv)
(def results-atom (atom (initial-results) :validator infer-results?))

(defn add-infer-results! [results-atom r]
  (swap! results-atom
         (fn [m]
           (-> m
               (update :root-results
                       (fn [root-results]
                         (reduce (fn [root-results nme]
                                   (if (symbol? nme)
                                     (update root-results nme (fnil inc 1))
                                     root-results))
                                 root-results
                                 (map (comp :name #(nth % 0) :path) r))))
               (update :infer-results #(into (or % #{}) r))))))

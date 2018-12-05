(ns update-dep
  (:require [clojure.pprint :as pp]))

(defn -main [coord version]
  (let [coord (symbol coord)
        dedn (read-string (slurp "deps.edn"))
        _ (assert (map? dedn))]
    (when (and (-> dedn :deps coord :mvn/version)
               (not= version (-> dedn :deps coord :mvn/version)))
      (spit "deps.edn"
            (binding [*print-namespace-maps* false]
              (with-out-str
                (pp/pprint
                  (assoc-in dedn [:deps coord :mvn/version] version))))))))

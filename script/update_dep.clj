(ns update-dep
  (:require [clojure.pprint :as pp]
            [clojure.tools.deps.alpha.gen.pom :as pom]
            [clojure.tools.deps.alpha.util.io :refer [printerrln]]
            [clojure.tools.deps.alpha.script.generate-manifest :as gen]))

(defn- to-dep
  [[lib {:keys [mvn/version classifier exclusions scope] :as coord}]]
  (prn "MEME")
  (if version
    (cond->
      [::pom/dependency
       [::pom/groupId (or (namespace lib) (name lib))]
       [::pom/artifactId (name lib)]
       [::pom/version version]]

      classifier
      (conj [::pom/classifier classifier])

      scope
      (conj [::pom/scope scope])

      (seq exclusions)
      (conj [::pom/exclusions
             (map (fn [excl]
                    [::pom/exclusion
                     [::pom/groupId (or (namespace excl) (name excl))]
                     [::pom/artifactId (name excl)]])
               exclusions)]))
    (printerrln "Skipping coordinate:" coord)))

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
                  (assoc-in dedn [:deps coord :mvn/version] version)))))
      (with-redefs-fn {#'pom/to-dep to-dep}
        (gen/-main "--config-files" "deps.edn"
                   "--gen" "pom")))))

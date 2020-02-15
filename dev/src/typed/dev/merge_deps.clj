(ns typed.dev.merge-deps
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.set :as set])
  (:import java.io.File)
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^String everything-root "../typed")
(def ^String relative-projects-root ".")
(def aliases
  '{:nREPL
    {:extra-deps
     {nrepl/nrepl {:mvn/version "0.6.0"}
      cider/piggieback {:mvn/version "0.3.8"}}
     :main-opts ["-m" "nrepl.cmdline" "--interactive"]}})

(defn -main [& args]
  (let [deps-maps (->> (File. everything-root relative-projects-root)
                       .listFiles 
                       (keep #(let [f (File. (str %) "deps.edn")]
                               (when (.exists f)
                                 [(.getName ^File %)
                                  (-> f
                                      str
                                      slurp
                                      read-string)])))
                       (into {}))
        everything-deps {:deps (apply merge-with
                                      (fn [v1 v2]
                                        (if (= v1 v2)
                                          v2
                                          (throw (ex-info (str "Version conflict: "
                                                               v1 " " v2)
                                                          {:versions [v1 v2]}))))
                                      (keep :deps (vals deps-maps)))
                         :paths (vec (mapcat
                                       (fn [[^String fname {:keys [paths]}]]
                                         (map #(str (-> relative-projects-root
                                                        (File. fname)
                                                        str
                                                        (File. ^String %)))
                                              paths))
                                       deps-maps))
                         :aliases aliases}]
    (spit (str (File. everything-root "deps.edn"))
          (with-out-str
            (binding [*print-length* nil
                      *print-level* nil
                      *print-namespace-maps* nil]
              (pp/pprint everything-deps))))))

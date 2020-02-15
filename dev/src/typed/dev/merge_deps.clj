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
      cider/cider-nrepl {:mvn/version "0.22.4"}
      cider/piggieback {:mvn/version "0.3.8"}}
     :main-opts ["-m" "nrepl.cmdline" "--interactive"
                 #_"
                 Note:
                   introducing other middleware makes vim-fireplace choose
                   fipp for pprint, which doesn't play well with the delicately
                   defined classes in type-rep."
                 "--middleware" "[cider.nrepl/wrap-complete]"
                 ]}})

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
        expand-deps (juxt identity
                          (comp :test :aliases))
        everything-deps {:deps (apply merge-with
                                      (fn [v1 v2]
                                        (if (= v1 v2)
                                          v2
                                          (throw (ex-info (str "Version conflict: "
                                                               v1 " " v2)
                                                          {:versions [v1 v2]}))))
                                      (->> (vals deps-maps)
                                           (mapcat expand-deps)
                                           (mapcat (fn [d]
                                                     {:pre [(map? d)]}
                                                     (concat (some-> d :deps vector)
                                                             (some-> d :extra-deps vector))))))
                         :paths (vec (mapcat
                                       (fn [[^String fname d]]
                                         (let [path->relative #(str (-> relative-projects-root
                                                                        (File. fname)
                                                                        str
                                                                        (File. ^String %)))]
                                           (mapcat (fn [d]
                                                     (let [all-paths (concat (:paths d)
                                                                             (:extra-paths d))]
                                                       (map path->relative all-paths)))
                                                   (expand-deps d))))
                                       deps-maps))
                         :aliases aliases}]
    (spit (str (File. everything-root "deps.edn"))
          (with-out-str
            (binding [*print-length* nil
                      *print-level* nil
                      *print-namespace-maps* nil]
              (pp/pprint everything-deps))))))

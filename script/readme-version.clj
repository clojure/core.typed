#!/usr/bin/env clojure

(set! *warn-on-reflection* true)

(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(def new-version-string (first *command-line-args*))
(assert (string? new-version-string)
        (str "Must pass version string as first argument to program"))

(defn update-readme-file [readme-file]
  (assert (string? readme-file)
          (str "Must pass readme file to program"))
  (let [original-readme-file-lines (doall (-> readme-file io/reader line-seq))
        version-string
        (some (fn [^String line]
                (when (.startsWith line "Latest stable release is")
                  (subs line (count "Latest stable release is ") (dec (count line)))))
              original-readme-file-lines)
        new-readme-file-lines
        (map #(str/replace % version-string new-version-string)
             original-readme-file-lines)]
    (spit readme-file (str (str/join "\n" new-readme-file-lines) "\n"))))

(mapv update-readme-file (rest *command-line-args*))
(apply println (str "Updated to version: " new-version-string ":")
       (rest *command-line-args*))

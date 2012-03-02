(ns leiningen.type-check
  (:use [leiningen.compile :only [eval-in-project]]))

(defn type-check
  ([project nsym]
   (eval-in-project
     project
     `(do (require '~nsym)
        (binding [*ns* (find-ns ~nsym)]


(ns clojure.core.typed.check.seq-ops
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed :as t]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.errors :as err]
            ))

(defn type-to-seq [t]
  {:pre [(r/Type? t)]}
  (cond
    (r/Union? t) (apply c/Un (map type-to-seq (:types t)))
    (r/Intersection? t) (apply c/In (map type-to-seq (:types t)))
    (r/HSequential? t) (if (seq (:types t))
                         t
                         (c/Un r/-nil t))
    ;TODO (sub/subtype? t (prs/parse-type `(t/U nil t/Seqable t/Any))) 
    :else (err/int-error (str "Cannot create seq from " t))))

(defn cons-types [a d]
  {:pre [(r/Type? a)
         (r/Type? d)]}
  (assert nil "TODO")
  )

(defn concat-types [& ts]
  {:pre [(every? r/Type? ts)]}
  (assert nil "TODO")
  )

(ns clojure.core.typed.test.CTYP146
  (:require [clojure.core.typed :refer [ann Seq Option All] :as t]))

(ann distinct-except
     (All [x]
          [[x -> Boolean] (Option (t/I clojure.lang.Sequential (Seq x))) -> (Seq x)]))
(defn distinct-except
  "Same as distinct, but keeps duplicates if they pass exception?"
  [exception? [head & tail :as coll]]
  (lazy-seq
    (when head
      (cons head
            (distinct-except exception?
                             (if (exception? head)
                               (when tail (cast clojure.lang.Sequential tail))
                               (cast clojure.lang.Sequential (remove (partial = head) tail))))))))

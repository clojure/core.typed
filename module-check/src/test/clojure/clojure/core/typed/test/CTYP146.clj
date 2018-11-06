(ns clojure.core.typed.test.CTYP146
  (:require [clojure.core.typed :refer [ann] :as t]))

(ann distinct-except
     (t/All [x]
          [[x -> Boolean] (t/Option (t/I clojure.lang.Sequential (t/Seq x))) -> (t/Seq x)]))
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

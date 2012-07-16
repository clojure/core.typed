(ns typed.test.conduit
  (:import (clojure.lang Seqable IMeta IPersistentMap))
  (:require [typed.core :refer [ann fn> def-alias]]))

(def-alias Parts (Seqable Any))
(def-alias Args (Seqable Any))

(def-alias ConduitMeta
  (U
    (HMap {:created-by (Value :disperse)
           :args Args
           :parts Parts})
    (HMap {:created-by (Value :a-arr)
           :args Args})
    (HMap {:created-by (Value :a-comp)
           :args Args
           :parts Parts})
    (HMap {:created-by (Value :a-nth)
           :args Args
           :parts Parts})))

(ann merge-parts [(IMeta (U (HMap {:parts Any}) nil))
                  -> (IPersistentMap Any Any)])
(defn merge-parts [ps]
  (apply merge-with merge
         (map (fn> [[a :- (IMeta (U (HMap {:parts Any}) nil))]]
                (-> a meta :parts))
              ps)))

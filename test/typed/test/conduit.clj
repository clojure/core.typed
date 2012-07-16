(ns typed.test.conduit
  (:import (clojure.lang Seqable IMeta IPersistentMap))
  (:require [typed.core :refer [ann fn> def-alias]]))

(def-alias Parts (Seqable Any))
(def-alias Args (Seqable Any))

(def-alias ConduitMeta
  (U
    (Map {:created-by (Value :disperse)
          :args Args
          :parts Parts})
    (Map {:created-by (Value :a-arr)
          :args Args})
    (Map {:created-by (Value :a-comp)
          :args Args
          :parts Parts})
    (Map {:created-by (Value :a-nth)
          :args Args
          :parts Parts})))


(ann merge-parts [(IMeta (U (Map* :mandatory {:parts Any}) nil))
                  -> (IPersistentMap Any Any)])
(defn merge-parts [ps]
  (apply merge-with merge
         (map (fn> [[a :- (IMeta (U (Map* :mandatory {:parts Any}) nil))]]
                   (-> a meta :parts))
              ps)))

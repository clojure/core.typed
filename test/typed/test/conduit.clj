(ns typed.test.conduit
  (:import (clojure.lang Seqable IMeta IPersistentMap))
  (:require [typed.core :refer [check-ns ann fn> def-alias tc-ignore]]
            [clojure.repl :refer [pst]]))

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
(tc-ignore
(defn merge-parts [ps]
  (apply merge-with merge
         (map (fn> [[a :- (IMeta (U (HMap {:parts Any}) nil))]]
                (-> a meta :parts))
              ps)))
  )

(ann abort-c [(U nil [(Vector*) -> Any]) -> (U nil Any)])
(defn abort-c [c]
  (when c
    (c [])))

(ann conduit-seq-fn [(Seqable Any)
                     -> [Any -> (Vector* (U nil [Any -> Any])
                                         [Any -> Any])]])

(defn conduit-seq-fn [l]
  (fn curr-fn [x]
    (let [new-f (conduit-seq-fn (rest l))]
      (if (empty? l)
        [nil abort-c]
        [new-f
         (fn [c]
           (c [(first l)]))]))))


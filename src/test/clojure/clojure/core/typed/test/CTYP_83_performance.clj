(ns clojure.core.typed.test.CTYP-83-performance
  (:require
    [clojure.core.typed :refer [ann def-alias loop>
                                NonEmptyVec NilableNonEmptySeq]
     :as t]))

(def-alias VersionVector (NonEmptyVec Number))
(ann version-less [(U nil VersionVector) (U nil VersionVector) -> Any])
(defn version-less
  "Compare two version vectors."
  [v1 v2]
  (loop> [v1 :- (NilableNonEmptySeq Number) (seq v1)
          v2 :- (NilableNonEmptySeq Number) (seq v2)]
    (let [fv1 (first v1)
          fv2 (first v2)]
     (cond
       ;(and (not v1) (not v2)) false
       ;(and v1 (not v2)) false
       (t/print-filterset 
         "(or (and (not v1) v2)
              (and fv1 fv2 (< fv1 fv2))))"
         (or (and (not v1) v2)
             (and fv1 fv2 (< fv1 fv2))))
        true
       ;(and fv1 fv2 (> fv1 fv2)) false
       ;:else (recur (next v1) (next v2))
       ))))

(ns clojure.core.typed.test.CTYP-83-performance
  (:require
    [clojure.core.typed :as t]))

(defmacro pf [frm]
  frm
  #_`(t/print-filterset ~(str frm) ~frm))

(t/defalias VersionVector (t/NonEmptyVec Number))
(t/ann version-less [(t/U nil VersionVector) (t/U nil VersionVector) -> t/Any])
(defn version-less
  "Compare two version vectors."
  [v1 v2]
  (t/loop [v1 :- (t/NilableNonEmptySeq t/Num) (seq v1)
           v2 :- (t/NilableNonEmptySeq t/Num) (seq v2)]
    (let [fv1 (first v1)
          fv2 (first v2)]
     (cond
       (pf (and (pf (not v1)) 
                (pf (not v2)))) false
       (and v1 (not v2)) false
         (or (and (not v1) v2)
             (and fv1 fv2 (< fv1 fv2)))
        true
       (and fv1 fv2 (> fv1 fv2)) false
       :else (recur (next v1) (next v2))
       ))))

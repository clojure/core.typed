;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.open-result
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.object-rep :as obj]
            [clojure.core.typed.checker.filter-rep :as fl]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.subst-obj :as subst-obj]))

;; Used to "instantiate" a Result from a function call.
;; eg. (let [a (ann-form [1] (U nil (Seqable Number)))]
;;       (if (seq a)
;;         ...
;;
;; Here we want to instantiate the result of (seq a).
;; objs is each of the arguments' objects, ie. [obj/-empty]
;; ts is each of the arugments' types, ie. [(U nil (Seqable Number))]
;;
;; The latent result:
; (Option (ISeq x))
; :filters {:then (is (CountRange 1) 0)
;           :else (| (is nil 0)
;                    (is (ExactCount 0) 0))}]))
;; instantiates to 
; (Option (ISeq x))
; :filters {:then (is (CountRange 1) a)
;           :else (| (is nil a)
;                    (is (ExactCount 0) a))}]))
;;
;; Notice the objects are instantiated from 0 -> a
;
;[Result (Seqable RObject) (Option (Seqable Type)) 
;  -> '[Type FilterSet RObject]]
(defn open-Result 
  "Substitute ids for objs in Result t"
  [{t :t fs :fl old-obj :o :keys [flow] :as r} objs & [ts]]
  {:pre [(r/Result? r)
         (every? obj/RObject? objs)
         ((some-fn fl/FilterSet? fl/NoFilter?) fs)
         (obj/RObject? old-obj)
         (r/FlowSet? flow)
         ((some-fn nil? (con/every-c? r/Type?)) ts)]
   :post [((con/hvector-c? r/Type? fl/FilterSet? obj/RObject? r/FlowSet?) %)]}
  ;  (prn "open-result")
  ;  (prn "result type" (prs/unparse-type t))
  ;  (prn "result filterset" (prs/unparse-filter-set fs))
  ;  (prn "result (old) object" old-obj)
  ;  (prn "objs" objs)
  ;  (prn "ts" (mapv prs/unparse-type ts))
  (reduce (fn [[t fs old-obj flow] [[o k] arg-ty]]
            {:pre [(r/Type? t)
                   ((some-fn fl/FilterSet? fl/NoFilter?) fs)
                   (obj/RObject? old-obj)
                   (integer? k)
                   (obj/RObject? o)
                   (r/FlowSet? flow)
                   ((some-fn false? r/Type?) arg-ty)]
             :post [((con/hvector-c? r/Type? fl/FilterSet? obj/RObject? r/FlowSet?) %)]}
            (let [r [(subst-obj/subst-type t k o true)
                     (subst-obj/subst-filter-set fs k o true arg-ty)
                     (subst-obj/subst-object old-obj k o true)
                     (subst-obj/subst-flow-set flow k o true arg-ty)]]
              ;              (prn [(prs/unparse-type t) (prs/unparse-filter-set fs) old-obj])
              ;              (prn "r" r)
              r))
          [t fs old-obj flow]
          ; this is just a sequence of pairs of [not-neg? RObject] and Type?
          ; Represents the object and type of each argument, and its position
          (map vector 
               (map-indexed #(vector %2 %1) ;racket's is opposite..
                            objs)
               (if ts
                 ts
                 (repeat false)))))


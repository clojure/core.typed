(ns clojure.core.typed.cs-rep
  (:refer-clojure :exclude [defrecord])
  (:require [clojure.core.typed
             [utils :as u]
             [type-rep :as r]])) 

(u/defrecord t-subst [type bnds]
  ""
  [(r/Type? type)
   (r/Bounds? bnds)])

(u/defrecord i-subst [types]
  ""
  [(every? r/Type? types)])

(u/defrecord i-subst-starred [types starred]
  ""
  [(every? r/Type? types)
   (r/Type? starred)])

(u/defrecord i-subst-dotted [types dty dbound]
  ""
  [(or (nil? types)
       (every? r/Type? types))
   (r/Type? dty)
   (r/F? dbound)])

(def subst-rhs? (some-fn t-subst? i-subst? i-subst-starred? i-subst-dotted?))

(def substitution-c? (every-pred map? 
                                 #(every? symbol? (keys %)) 
                                 #(every? subst-rhs? (vals %))))

(u/defrecord c [S X T bnds]
  "A type constraint on a variable within an upper and lower bound"
  [(r/Type? S)
   (symbol? X)
   (r/Type? T)
   (r/Bounds? bnds)])

;; fixed : Listof[c]
;; rest : option[c]
;; a constraint on an index variable
;; the index variable must be instantiated with |fixed| arguments, each meeting the appropriate constraint
;; and further instantions of the index variable must respect the rest constraint, if it exists
(u/defrecord dcon [fixed rest]
  ""
  [(every? c? fixed)
   (or (nil? rest)
       (c? rest))])

(u/defrecord dcon-exact [fixed rest]
  ""
  [(every? c? fixed)
   (c? rest)])

(u/defrecord dcon-dotted [fixed dc dbound]
  ""
  [(every? c? fixed)
   (c? dc)
   (r/F? dbound)])

(def dcon-c? (some-fn dcon? dcon-exact? dcon-dotted?))

;; map : hash mapping index variables to dcons
(u/defrecord dmap [map]
  ""
  [((u/hash-c? symbol? dcon-c?) map)])

(u/defrecord cset-entry [fixed dmap projections]
  ""
  [((u/hash-c? symbol? c?) fixed)
   (dmap? dmap)
   ((u/set-c? (u/hvector-c? (some-fn r/Type? r/Projection?)
                            (some-fn r/Type? r/Projection?)))
     projections)])

(defn make-cset-entry
  ([fixed] (make-cset-entry fixed nil nil))
  ([fixed dmap] (make-cset-entry fixed dmap nil))
  ([fixed dmap projections] (->cset-entry fixed 
                                          (or dmap (->dmap {}))
                                          (or projections #{}))))

;; maps is a list of cset-entries, consisting of
;;    - functional maps from vars to c's
;;    - dmaps (see dmap.rkt)
;; we need a bunch of mappings for each cset to handle case-lambda
;; because case-lambda can generate multiple possible solutions, and we
;; don't want to rule them out too early
(u/defrecord cset [maps]
  ""
  [(every? cset-entry? maps)])


;widest constraint possible
(defn no-constraint [v bnds]
  {:pre [(symbol? v)
         (r/Bounds? bnds)]}
  (->c (r/->Union #{}) v (r/->Top) bnds))

;; Create an empty constraint map from a set of type variables X and
;; index variables Y.  For now, we add the widest constraints for
;; variables in X to the cmap and create an empty dmap.
(defn empty-cset [X Y]
  {:pre [(every? (u/hash-c? symbol? r/Bounds?) [X Y])]
   :post [(cset? %)]}
  (->cset [(->cset-entry (into {} (for [[x bnds] X] [x (no-constraint x bnds)]))
                         (->dmap {})
                         #{})]))

(defn empty-cset-projection [X Y proj]
  {:pre [(every? (u/hash-c? symbol? r/Bounds?) [X Y])]
   :post [(cset? %)]}
  (->cset [(->cset-entry (into {} (for [[x bnds] X] [x (no-constraint x bnds)]))
                         (->dmap {})
                         #{proj})]))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.filter-ops
  (:require [clojure.core.typed.checker.type-rep :as r] 
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.filter-rep :as fr]
            [clojure.core.typed.checker.path-rep :as pr]
            [clojure.core.typed.checker.object-rep :as or]
            [clojure.core.typed.checker.indirect-utils :as ind-u]
            [clojure.core.typed.checker.indirect-ops :as ind]
            [clojure.set :as set])
  (:import (clojure.core.typed.checker.filter_rep BotFilter TopFilter NoFilter AndFilter 
                                          OrFilter TypeFilter NotTypeFilter ImpFilter)))

(defn -filter [t i & [p]]
  {:pre [(r/Type? t)
         (fr/name-ref? i)
         ((some-fn nil? #(every? pr/PathElem? %)) p)]
   :post [(fr/Filter? %)]}
  (cond
    (= r/-any t) fr/-top
    (= r/-nothing t) fr/-bot
    :else (fr/TypeFilter-maker t (seq p) i)))

(defn -not-filter [t i & [p]]
  {:pre [(r/Type? t)
         (fr/name-ref? i)
         ((some-fn nil? #(every? pr/PathElem? %)) p)]
   :post [(fr/Filter? %)]}
  (cond
    (= r/-nothing t) fr/-top
    (= r/-any t) fr/-bot
    :else (fr/NotTypeFilter-maker t (seq p) i)))

(defn -filter-at [t o]
  (if (or/Path? o)
    (let [{p :path i :id} o]
      (-filter t i p))
    fr/-top))
(defn -not-filter-at [t o]
  (if (or/Path? o)
    (let [{p :path i :id} o]
      (-not-filter t i p))
    fr/-top))

(defn- subtype?-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.checker.jvm.subtype) 'subtype?)]
    (assert (var? v) "subtype? unbound")
    v))

(defn opposite? [f1 f2]
  {:pre [(fr/Filter? f1)
         (fr/Filter? f2)]
   :post [(boolean? %)]}
  (let [subtype? @(subtype?-var)]
    (cond
      (and (fr/TypeFilter? f1)
           (fr/NotTypeFilter? f2))
      (let [{t1 :type p1 :path i1 :id} f1
            {t2 :type p2 :path i2 :id} f2]
        (and (= p1 p2)
             (= i1 i2)
             (subtype? t1 t2)))

      (and (fr/NotTypeFilter? f1)
           (fr/TypeFilter? f2))
      (let [{t2 :type p2 :path i2 :id} f1
            {t1 :type p1 :path i1 :id} f2]
        (and (= p1 p2)
             (= i1 i2)
             (subtype? t1 t2)))
      :else false)))


;; compact : (Listof prop) bool -> (Listof prop)
;; props : propositions to compress
;; or? : is this an OrFilter (alternative is AndFilter)
(defn compact [props or?]
  {:pre [(every? fr/Filter? props)
         (boolean? or?)]
   :post [(every? fr/Filter? %)]}
;  (prn "compact")
;  (prn "props" (map clojure.core.typed.checker.jvm.parse-unparse/unparse-filter props))
;  (prn "or?" or?)
  (let [tf-map (atom {})
        ntf-map (atom {})]
    ;; props: the propositions we're processing
    ;; others: props that are neither TF or NTF
    (loop [props props
           others nil]
      (if (empty? props)
        (concat others
                (vals @tf-map)
                (vals @ntf-map))
        (cond
          (and or? (fr/TypeFilter? (first props)))
          (let [{t1 :type f1 :path x :id :as p} (first props)]
            (swap! tf-map (fn [m] (update-in m [[f1 x]] #(if %
                                                           (if (fr/TypeFilter? %)
                                                             (let [t2 (:type %)]
                                                               (-filter (c/Un t1 t2) x f1))
                                                             (throw (Exception. (str "got something that isn't a type filter" p))))
                                                           p))))
            (recur (rest props) others))

          (and (not or?) (fr/TypeFilter? (first props)))
          (let [{t1 :type f1 :path x :id} (first props)
                fl (@tf-map [f1 x])]
            (cond
              (and (fr/TypeFilter? fl)
                   (let [t2 (:type fl)]
                     (not (c/overlap t1 (:type fl)))))
              ;; we're in an And, and we got two types for the same path that do not overlap
              [fr/-bot]
              (fr/TypeFilter? fl)
              (let [t2 (:type fl)]
                (swap! tf-map (fn [m] (assoc m [f1 x] (-filter (c/restrict t1 t2) x f1))))
                (recur (next props) others))
              :else
              (do 
                (swap! tf-map (fn [m] (assoc m [f1 x] (-filter t1 x f1))))
                (recur (next props) others))))

          (and (not or?) 
               (fr/NotTypeFilter? (first props)))
          (let [{t1 :type f1 :path x :id :as p} (first props)]
            (swap! ntf-map (fn [m] (update-in m [[f1 x]]
                                              (fn [n]
                                                (if n
                                                  (if (fr/NotTypeFilter? n)
                                                    (let [t2 (:type n)]
                                                      (-not-filter (c/Un t1 t2) x f1))
                                                    (throw (Exception. (str "got something that isn't a nottypefilter" p))))
                                                  p)))))
            (recur (next props) others))
          :else
          (let [p (first props)]
            (recur (next props) (cons p others))))))))


(declare -and)

(defn inverse-atom [a]
  {:pre [((some-fn fr/TypeFilter? fr/NotTypeFilter?) a)]
   :post [((some-fn fr/TypeFilter? fr/NotTypeFilter?) a)]}
  (cond
    (fr/TypeFilter? a) (-not-filter (:type a) (:id a) (:path a))
    (fr/NotTypeFilter? a) (-filter (:type a) (:id a) (:path a))))

(defn simplify-prop
  "Try and use atomic proposition a to simplify composite
  proposition b. a must be correct polarity."
  [a b]
  {:pre [((some-fn fr/TypeFilter? fr/NotTypeFilter?) a)
         ((some-fn fr/AndFilter? fr/OrFilter?) b)]
   :post [(fr/Filter? %)]}
  (cond
    ; assuming a wrapping OrFilter
    (fr/AndFilter? b)
    (let [fs (set (:fs b))
          fs (set
               (for [f fs]
                 (cond
                   ; A ^ (B v A) => A
                   (fr/OrFilter? f) (simplify-prop a f)
                   :else f)))]
      (if (fs a)
        ; A v (notB ^ A) => A v notB
        (apply -and (disj fs a))
        b))

    ; assuming a wrapping AndFilter
    (fr/OrFilter? b)
    (let [fs (set (:fs b))]
      ; A ^ (B v A) => A
      (if (fs a)
        a
        b))))


(comment
  (-or (-not-filter -nil 'a)
       (-and (-filter -nil 'a)
             (-filter -false 'b)))
  (simplify-prop (-filter -nil 'a) (-and (-filter -nil 'a)
                                         (-filter -false 'b)))
  ;=> (-filter -nil 'a)
  '[-or-filter
    [-not-filter (Value :Black) (:tree) 0]
    [-and-filter
     ; or->and, elim -filter (:Black) (:tree 0)
     [-filter (Value :Black) (:tree) 0]
     [-or-filter
      ;and->or,  elim -filter (:Black) (:tree 0)
      [-and-filter
       ;or->and,  elim -not-filter (:Black) (:tree 0)
       [-filter (Value :Black) (:tree) 0]
       [-not-filter (Value :Red) (:left :tree) 0]]

      [-and-filter
       ;or->and,  elim -not-filter (:Black) (:tree 0)
       [-filter (Value :Red) (:left :tree) 0]
       [-filter (Value :Black) (:tree) 0]
       [-or-filter
        [-and-filter
         [-filter (Value :Red) (:left :tree) 0]
         [-filter (Value :Black) (:tree) 0]
         [-not-filter (Value :Red) (:right :tree) 0]]
        [-and-filter
         [-filter (Value :Red) (:left :tree) 0]
         [-filter (Value :Black) (:tree) 0]
         [-filter (Value :Red) (:right :tree) 0]
         [-not-filter (Value :Red) (:right :left :tree) 0]]]]]
     ]
    ]
  )

(declare atomic-filter?)

;remove opposites in and filter
(defn remove-opposite [and-f atom-f]
  {:pre [(fr/Filter? and-f)
         (fr/Filter? atom-f)]
   :post [(fr/Filter? %)]}
  (if (fr/AndFilter? and-f)
    (apply -and (remove #(opposite? % atom-f) (:fs and-f)))
    and-f))

;(defn -or [& args]
;  (loop [new-props (set args)
;         ;atomic propositions
;         atoms #{}
;         last-props #{} ;stop iteration when (= (set/union new-props atoms) last-props)
;         ]
;    (assert ((con/set-c? atomic-filter?) atoms))
;    (assert (every? (con/set-c? fr/Filter?) [new-props last-props]))
;    (cond
;      ;reached fixed point
;      (= (set/union new-props atoms) last-props)
;      (case (count last-props)
;        0 fr/-bot
;        1 (first last-props)
;        (fr/->OrFilter last-props))
;
;      :else
;      (let [;flatten OrFilters
;            original-props (set/union new-props atoms)
;            original-atoms atoms
;            fs (-> (apply concat
;                          (for [a (set/union new-props atoms)]
;                            (if (fr/OrFilter? a)
;                              (:fs a)
;                              [a])))
;                 set (disj fr/-bot))
;            {:keys [atoms] old-props :props} (group-by #(cond
;                                                          ((some-fn fr/TypeFilter? fr/NotTypeFilter?) %) :atoms
;                                                          :else :props)
;                                                       fs)
;            ;simplify AndFilters by removing atomic props directly inside the AndFilter
;            ;if they are opposite of any atomic props we already have
;            next-props (doall
;                         (for [p old-props]
;                           (reduce (fn [p a] (remove-opposite p a))
;                                   p atoms)))
;            {:keys [atoms] new-props :props} (group-by #(cond
;                                                          ((some-fn fr/TypeFilter? fr/NotTypeFilter?) %) :atoms
;                                                          :else :props)
;                                                       (set/union (set next-props) (set atoms)))]
;
;        (assert (<= (count original-atoms) (count atoms)))
;        (recur (set new-props) (set atoms) (set original-props))))))

(declare implied-atomic?)

(defn -or [& args]
  {:pre [(every? fr/Filter? args)]
   :post [(fr/Filter? %)]}
  (letfn [(mk [& fs]
            {:pre [(every? fr/Filter? fs)]
             :post [(fr/Filter? %)]}
            (cond
              (empty? fs) fr/-bot
              (= 1 (count fs)) (first fs)
              :else (fr/OrFilter-maker (set fs))))
          (distribute [args]
            (let [{ands true others false} (group-by fr/AndFilter? args)]
              (if (empty? ands)
                (apply mk others)
                (let [{elems :fs} (first ands)] ;an AndFilter
                  (apply -and (for [a elems]
                                (apply -or a (concat (next ands) others))))))))]
    (loop [fs args
           result nil]
      (assert (every? fr/Filter? fs))
      (assert (every? fr/Filter? result))
      (if (empty? fs)
        (cond
          (empty? result) fr/-bot
          (= 1 (count result)) (first result)
          :else (distribute (compact result true)))
        (cond
          (fr/TopFilter? (first fs)) (first fs)
          (fr/OrFilter? (first fs)) (let [fs* (:fs (first fs))]
                                      (recur (concat fs* (next fs)) result))
          (fr/BotFilter? (first fs)) (recur (next fs) result)
          :else (let [t (first fs)]
                  (assert (fr/Filter? t))
                  (cond 
                    (some (fn [f] (opposite? f t)) (concat (rest fs) result))
                    fr/-top
                    (some (fn [f] (or (= f t)
                                      (implied-atomic? f t)))
                          result)
                    (recur (next fs) result)
                    :else
                    (recur (next fs) (cons t result)))))))))

(ind-u/add-indirection ind/-or -or)

(defn -imp [a c]
  {:pre [(fr/Filter? a)
         (fr/Filter? c)]
   :post [(fr/Filter? %)]}
  (cond
    (fr/BotFilter? a) fr/-top
    (fr/TopFilter? a) c
    ;; P -> tt = tt for any P
    (fr/TopFilter? c) fr/-top
    :else (fr/ImpFilter-maker a c)))



;  A ^ (B v ...) -> (simplify A (B v ...))
;(defn -and [& args]
;             ;flatten direct internal AndFilters
;  (let [flat (apply concat
;                    (for [fl args]
;                      (if (AndFilter? fl)
;                        (:fs fl)
;                        [fl])))
;        fs (set flat)]
;    (cond
;      (empty? fs) -bot
;      (fs -bot) -bot
;      (or (= 1 (count fs))
;          (= 1 (count (disj fs -top)))) (or (first (disj fs -top))
;                                            (first fs))
;      :else (->AndFilter (disj fs -top)))))

(declare implied-atomic?)

(defn -and [& args]
  {:pre [(every? fr/Filter? args)]
   :post [(fr/Filter? %)]}
  (letfn [(mk [& fs]
            {:pre [(every? fr/Filter? fs)]
             :post [(fr/Filter? %)]}
            (cond
              (empty? fs) fr/-top
              (= 1 (count fs)) (first fs)
              :else (apply fr/make-AndFilter fs)))]
    (loop [fs (set args)
           result nil]
      (if (empty? fs)
        (cond
          (empty? result) fr/-top
          (= 1 (count result)) (first result)
          ;; don't think this is useful here
          (= 2 (count result)) (let [;_ (prn "hit special 2 case in -and")
                                     [f1 f2] result]
                                 (if (opposite? f1 f2)
                                   fr/-bot
                                   (if (= f1 f2)
                                     f1
                                     (apply mk (compact [f1 f2] false)))))
          :else
           ;; first, remove anything implied by the atomic propositions
           ;; We commonly see: (And (Or P Q) (Or P R) (Or P S) ... P), which this fixes
          (let [{atomic true not-atomic false} (group-by atomic-filter? result)
                ;_ (prn "not-atomic" (map clojure.core.typed.checker.jvm.parse-unparse/unparse-filter not-atomic))
                not-atomic* (for [p not-atomic
                                  :when (not-any? (fn [a] (implied-atomic? p a)) atomic)]
                              p)]
            ;(prn "not-atomic*" not-atomic*)
             ;; `compact' takes care of implications between atomic props
            (apply mk (compact (concat not-atomic* atomic) false))))
        (let [ffs (first fs)]
          (cond
            (fr/BotFilter? ffs) ffs
            (fr/AndFilter? ffs) (let [fs* (:fs ffs)]
                                  (recur (next fs) (concat fs* result)))
            (fr/TopFilter? ffs) (recur (next fs) result)
            :else (let [t ffs]
                    (cond
                      (some (fn [f] (opposite? f ffs)) (concat (rest fs) result))
                      fr/-bot
                      (some (fn [f] 
                              (or (= f t)
                                  (implied-atomic? t f))) 
                            (concat (rest fs) result))
                      (recur (rest fs) result)
                      :else
                      (recur (rest fs) (cons t result))))))))))

(ind-u/add-indirection ind/-and -and)

(defn -FS [+ -]
  {:pre [(fr/Filter? +)
         (fr/Filter? -)]
   :post [(fr/FilterSet? %)]}
  (fr/FilterSet-maker + -))

(ind-u/add-indirection ind/-FS -FS)

(defn atomic-filter? [a]
  {;TODO :pre [(fr/Filter? a)]
   :post [(boolean? %)]}
  (boolean 
    ((some-fn fr/TypeFilter? fr/NotTypeFilter?
              fr/TopFilter? fr/BotFilter?) 
     a)))

; functions to get around compilation issues
(defn -true-filter [] (-FS fr/-top fr/-bot))
(defn -false-filter [] (-FS fr/-bot fr/-top))
(defn -simple-filter [] (-FS fr/-top fr/-top))
(defn -unreachable-filter [] (-FS fr/-bot fr/-bot))

;; true if f1 is implied by f2
;; (implied-atomic? (is Number 0) (is Integer 0)) ;=> true
;; (implied-atomic? top bot) ;=> true
(defn implied-atomic? [f1 f2]
  {:pre [(fr/Filter? f1)
         (fr/Filter? f2)]
   :post [(boolean? %)]}
  ;(prn "implied-atomic?" f1 f2)
  (let [subtype? @(subtype?-var)]
    (if (= f1 f2)
      true
      (cond
        (fr/BotFilter? f2) true
        (and (fr/TopFilter? f1)
             ((some-fn fr/TypeFilter? fr/NotTypeFilter?) f2)) true

        ; we don't learn anything intesting if everything on the right
        ; appears on the left
        (and (fr/OrFilter? f1)
             (fr/OrFilter? f2))
        (empty? (set/difference (:fs f2) (:fs f1)))

        (fr/OrFilter? f1) (contains? (:fs f1) f2)
        (and (fr/TypeFilter? f1)
             (fr/TypeFilter? f2)) (and (= (:id f1) (:id f2))
                                       (= (:path f1) (:path f2))
                                       (subtype? (:type f2) (:type f1)))
        (and (fr/NotTypeFilter? f1)
             (fr/NotTypeFilter? f2)) (and (= (:id f1) (:id f2))
                                          (= (:path f1) (:path f2))
                                          (subtype? (:type f1) (:type f2)))
        :else false))))

(defmulti opposite-filter class)

(def negate opposite-filter)

(defmethod opposite-filter TypeFilter
  [{:keys [type id path]}]
  (-not-filter type id path))

(defmethod opposite-filter NotTypeFilter
  [{:keys [type id path]}]
  (-filter type id path))

(defmethod opposite-filter AndFilter
  [{:keys [fs]}]
  (apply -or (map opposite-filter fs)))

(defmethod opposite-filter OrFilter
  [{:keys [fs]}]
  (apply -and (map opposite-filter fs)))

(defmethod opposite-filter BotFilter
  [_]
  fr/-top)

(defmethod opposite-filter TopFilter
  [_]
  fr/-bot)

(defmethod opposite-filter ImpFilter
  [f]
  f)

(defmethod opposite-filter NoFilter
  [f]
  f)

(ns typed.test.conduit
  (:import (clojure.lang Seqable IMeta IPersistentMap LazySeq ISeq))
  (:require [typed.core :refer [check-ns ann fn> def-alias tc-ignore ann-form declare-names inst
                                tc-pr-env inst-ctor]]
            [clojure.repl :refer [pst]]
            [arrows.core :refer [defarrow]]))

(def-alias Part (IPersistentMap Any Any))

(def-alias ContRes 
  (All [x]
    (U nil ;stream is closed
       '[] ;abort/skip
       '[x];consume/continue
       )))

(def-alias ContGen
  (All [x]
    [(ContRes x) -> (ContRes y)]))

(def-alias Cont
  (All [x y]
    [(U nil (ContGen x y)) -> (ContRes y)]))

(declare-names ==>)

(def-alias ==>
  (All [in out]
    [in -> '[(U nil (==> in out))
             (Cont out)]]))

(ann merge-parts [(Seqable (IMeta (U '{:parts Part} nil))) -> Part])
(tc-ignore
(defn merge-parts [ps]
  (let [parts (map (fn> [[p :- (IMeta (U '{:parts Part} nil))]] 
                     (-> p meta :parts))
                   ps)]
    (apply (-> merge-with 
             (inst Any Part))
           (-> merge 
             (inst Any Any))
           parts)))
)

(ann abort-c (All [x] (Cont x)))
(defn abort-c [c]
  (when c
    (c [])))

(ann conduit-seq-fn 
     (All [x]
       [(Seqable x) -> (==> Any x)]))
(defn conduit-seq-fn [l]
  (fn curr-fn [_]
    (let [new-f (conduit-seq-fn (rest l))]
      (if (empty? l)
        [nil (-> abort-c 
               (inst x))]
        [new-f
         (-> 
           (fn [c]
             (when c ;`when` added to conform to Cont type - Ambrose
               (c [(first l)])))
           (ann-form (Cont x)))]))))

(ann conduit-seq
     (All [x]
       [(Seqable x) -> (==> Any x)]))
(defn conduit-seq 
  "create a stream processor that emits the contents of a list
  regardless of what is fed to it"
  [l]
  ((inst conduit-seq-fn x) l))

(ann a-run
     (All [x]
       [(==> Any x) -> (Seqable x)]))
(defn a-run 
  "execute a stream processor function"
  [f]
  (let [[new-f c] (f nil)
        y (c identity)]
    (cond
      (nil? new-f) (list)
      (empty? y) (recur new-f)
      :else (lazy-seq
              (cons (first y)
                    (a-run new-f))))))


(tc-ignore
(defn comp-fn [[f & fs]]
  (fn curr-fn [x]
    (let [[new-f first-c] (f x)
          [new-fs new-c] (reduce (fn [[new-fs c] f]
                                   (let [y (c identity)
                                         [new-f new-c] (if (empty? y)
                                                         [f abort-c]
                                                         (f (first y)))]
                                     [(conj new-fs new-f) new-c]))
                                 [[new-f] first-c]
                                 fs)]
      [(when-not (some nil? new-fs)
         (comp-fn new-fs))
       new-c])))
  )

;Type only works for vectors of length 2
(ann nth-fn
     (All [x y z]
       (Fn ['0 (U nil (==> x z)) -> (==> '[x y] '[z y])]
           ['1 (U nil (==> x z)) -> (==> '[x y] '[x z])])))
(defn nth-fn [n f]
  (fn curr-fn [xs]
    (let [abort-c (ann-form abort-c
                            (Fn [(U nil (ContGen '[x y])) -> (ContRes '[z y])]
                                [(U nil (ContGen '[x y])) -> (ContRes '[x z])]))]
      (cond 
        (<= (count xs) n) [curr-fn abort-c]
        ;added - Ambrose
        (nil? f) [nil abort-c]
        :else
        (let [[new-f new-c] (f (nth xs n))
              next-c (->
                       (fn [c]
                         (if (nil? c)
                           (new-c nil)
                           (let [y (new-c identity)]
                             (if (empty? y)
                               (c [])
                               (c [(assoc xs n (first y))])))))
                       (ann-form (Fn [(U nil (ContGen '[x y])) -> (ContRes '[z y])]
                                     [(U nil (ContGen '[x y])) -> (ContRes '[x z])])))]
          [(nth-fn n new-f) next-c])))))


(defn gather-fn [[fs ys] [f y]]
  [(conj fs f) (conj ys y)])

(defn par-fn [fs]
  (fn curr-fn [xs]
      (if (not= (count xs) (count fs))
        [curr-fn abort-c]
        (let [[new-fs cs] (reduce gather-fn
                                  [[] []]
                                  (map #(%1 %2) fs xs))]
          [(par-fn new-fs)
           (fn [c]
               (if (nil? c)
                 (doseq [c cs]
                   (c nil))
                 (let [ys (map #(% identity) cs)]
                   (if (some empty? ys)
                     (c [])
                     (c [(apply concat ys)])))))]))))

(ann select-fn
     (All [x y z]
       [(IPersistentMap x (==> y z)) -> (==> '[x y] z)]))
(defn select-fn [selection-map]
  (fn curr-fn [[v x]]
    (if-let [f (or (get selection-map v)
                   (get selection-map '_))]
      (let [[new-f c] (f x)]
        [(select-fn (assoc selection-map v new-f)) c])
      [curr-fn abort-c])))

(defn loop-fn
  ([f prev-x]
   (fn curr-fn [x]
     (let [[new-f c] (f [prev-x x])
           y (c identity)]
       (if (empty? y)
         [curr-fn abort-c]
         [(loop-fn new-f (first y)) (fn [c]
                                      (when c
                                        (c y)))]))))
  ([f fb-f prev-x]
   (fn curr-fn [x]
     (let [[new-f c] (f [prev-x x])
           y (c identity)]
       (if (empty? y)
         [curr-fn abort-c]
         (let [[new-fb fb-c] (fb-f (first y))
               fb-y (fb-c identity)]
           (if (empty? fb-y)
             [curr-fn abort-c]
             [(loop-fn new-f new-fb (first fb-y))
              (fn [c]
                (when c
                  (c y)))])))))))

(defarrow conduit
  [a-arr (ann-form 
           (fn [f]
             (with-meta
               (fn a-arr [x]
                 (let [y (f x)]
                   [a-arr (fn [c]
                            (when c
                              (c [y])))]))
               {:created-by :a-arr
                :args f}))
           (All [x y]
             [[x -> y] -> (I (==> x y)
                             (IMeta '{:created-by ':a-arr
                                      :args [x -> x]}))]))

   a-comp (ann-form
            (fn [p1 p2]
              (with-meta
                (comp-fn [p1 p2])
                {:parts (merge-parts [p1 p2])
                 :created-by :a-comp
                 :args [p1 p2]}))
            (All [x y z]
              [(==> x y) (==> y z) -> (I (==> x z)
                                         (IMeta '{:created-by ':a-comp
                                                  :parts Any
                                                  :args '[[x -> y] [y -> z]]}))]))

   ;apply p to position n in passed pair
   ;eg. increment second element of each list
   ; (conduit-map (a-nth 1 (a-arr inc)) [[3 5] [3 4]])
   ;([3 6] [3 5])
   a-nth (ann-form
           (fn [n p]
             (with-meta
               (nth-fn n p)
               {:parts (:parts p)
                :created-by :a-nth
                :args [n p]}))
           (All [x y z]
             (Fn ['0 (==> x y) -> (I (==> '[x z] '[y z])
                                     (IMeta '{:created-by ':a-nth
                                              :parts Any
                                              :args '['0 (==> x y)]}))]
                 ['1 (==> x y) -> (I (==> '[z x] '[z y])
                                     (IMeta '{:created-by ':a-nth
                                              :parts Any
                                              :args '['1 (==> x y)]}))])))

   ;like juxt
   ;modified to accept 2 arrows rather than n arrows
   a-par (ann-form 
           (fn [p1 p2]
             (with-meta
               (par-fn [p1 p2])
               {:created-by :a-par
                :args [p1 p2]
                :parts (merge-parts [p1 p2])}))
           (All [x y z]
             [(==> x y) (==> x z) -> (I (==> x '[y z])
                                        (IMeta '{:created-by ':a-par
                                                 :args [(==> x y) (==> x z)]
                                                 :parts Any}))]))

   ;apply functions to lhs and rhs of pairs
   ; modified to accept 2 arrows instead of n arrows
   a-all (ann-form 
           (fn [p1 p2]
             (with-meta
               (a-comp (a-arr (ann-form #(vector % %)
                                        (All [x] 
                                          [x -> '[x x]])))
                       (a-par p1 p2))
               {:created-by :a-all
                :args [p1 p2]
                :parts (merge-parts [p1 p2])}))
           (All [x y]
             [(==> x y) (==> z a) -> (I (==> '[x z] '[y a])
                                        (IMeta '{:created-by ':a-all
                                                 :args '[(==> x y) (==> z a)]
                                                 :parts Any}))]))

   ;select a value
   a-select (ann-form
              (fn [pair-map]
                (with-meta
                  (select-fn pair-map)
                  {:created-by :a-select
                   :args pair-map
                   :parts (merge-parts (vals pair-map))}))
              (All [x y z]
                [(IPersistentMap x (==> y z)) -> (==> x (==> y z))]))

   a-loop (ann-form 
            (fn
              ([p initial-value]
               (with-meta
                 (loop-fn p initial-value)
                 {:created-by :a-loop
                  :args [p initial-value]
                  :parts (:parts p)}))
              ([p initial-value fb-p]
               (with-meta
                 (loop-fn p fb-p initial-value)
                 {:created-by :a-loop
                  :args [p initial-value fb-p]
                  :parts (:parts p)})))
            (All [state in]
                 [(==> '[state in] state) state -> (I (==> in state)
                                                      (IMeta '{:created-by ':a-loop
                                                               :args '[(==> '[state in] state) state]}))]))
   ])

(def a-arr (conduit :a-arr))
(def a-comp (conduit :a-comp))
(def a-nth (conduit :a-nth))
(def a-par (conduit :a-par))
(def a-all (conduit :a-all))
(def a-select (conduit :a-select))
(def a-loop (conduit :a-loop))

(ann conduit-map
     (All [x y]
       [(==> x y) (Seqable x) -> (Seqable y)]))
(defn conduit-map [p l]
  (if (empty? l)
    l
    (a-run (comp-fn [(conduit-seq l) p]))))

(ann pass-through 
     (All [x]
       (==> x x)))
(def pass-through
  (a-arr identity))

(ann a-selectp
     (All [x y z a]
          [[x -> y] (IPersistentMap y (==> z a)) -> (==> '[x z] a)]))
(defn a-selectp [pred pair-map]
  (a-comp
    (a-all (a-arr pred)
           pass-through)
    (a-select pair-map)))

(ann a-if
     (All [x y z]
       (Fn [[x -> y] [y -> z] -> (==> x (U z nil))]
           [[x -> y] [y -> z] [y -> z] -> (==> x z)])))
(defn a-if
  ([a b] (a-if a b nil))
  ([a b c]
   (let [c (or c (a-arr (constantly nil)))]
     (a-comp (a-all (a-arr (comp boolean a))
                    pass-through)
             (a-select
               {true b
                false c})))))

(defn a-catch
  ([p catch-p]
   (a-catch Exception p catch-p))
  ([class p catch-p]
   (letfn [(a-catch [f catch-f]
             (fn [x]
               (try
                 (let [[new-f c] (f x)]
                   [(a-catch f catch-f) c])
                 (catch Throwable e
                   (if (instance? class e)
                     (let [[new-catch c] (catch-f [e x])]
                       [(a-catch f new-catch) c])
                     (throw e))))))]
     (with-meta
       (a-catch p catch-p)
       {:parts (:parts p)
        :created-by :a-catch
        :args [class p catch-p]}))))

(defn a-finally [p final-p]
  (letfn [(a-finally [f final-f]
            (fn [x]
              (try
                (let [[new-f c] (f x)]
                  [(a-finally new-f final-f) c])
                (finally
                  (final-f x)))))]
    (with-meta
      (a-finally p final-p)
      {:parts (:parts p)
       :created-by :a-finally
       :args [p final-p]})))

(defmacro def-arr [name args & body]
  `(def ~name (a-arr (fn ~name ~args ~@body))))

(defn a-filter [f]
  (with-meta
    (fn curr-fn [x]
      (if (f x)
        [curr-fn (fn [c]
                   (when c
                     (c [x])))]
        [curr-fn abort-c]))
    {:created-by :a-filter
     :args f}))

(defn tap [p]
  (fn [x]
    (let [[new-f new-c] (p x)]
      (new-c nil)
      [new-f (fn [c]
               (when c
                 (c [x])))])))

(defn disperse [p]
  (with-meta
    (fn curr-fn [xs]
      (if (empty? xs)
        [curr-fn (fn [c]
                   (when c
                     (c [xs])))]
        (let [[new-f cs] (reduce (fn [[new-f cs] x]
                                   (let [[new-f c] (new-f x)]
                                     [new-f (conj cs c)]))
                                 [p []]
                                 xs)]
          [(disperse new-f) (fn [c]
                              (if (nil? c)
                                (doseq [c cs]
                                  (c nil))
                                (let [ys (map #(% identity) cs)]
                                  (if (some empty? ys)
                                    (c [])
                                    (c [(apply concat ys)])))))])))
    {:created-by :disperse
     :args p
     :parts (:parts p)}))

(defn enqueue [f x]
  ((second (f x)) nil)
  nil)

(defn wait-for-reply [f x]
  ((second (f x)) identity))

(defn test-conduit [p]
  (let [args (:args (meta p))]
    (condp = (:created-by (meta p))
      nil p
      :a-arr (a-arr args)
      :a-comp (apply a-comp (map test-conduit args))
      :a-nth (apply a-nth (map test-conduit args))
      :a-par (apply a-par (map test-conduit args))
      :a-all (apply a-all (map test-conduit args))
      :a-select (apply a-select (mapcat (fn [[k v]]
                                          [k (test-conduit v)])
                                        args))
      :a-loop (let [[bp iv fb] args]
                (if fb
                  (a-loop (test-conduit bp)
                          iv
                          (test-conduit fb))
                  (a-loop (test-conduit bp)
                          iv)))
      :a-catch (apply a-catch (first args)
                      (map test-conduit (rest args)))
      :a-finally (apply a-finally (map test-conduit args))
      :a-filter p
      :disperse (disperse (test-conduit args)))))

(defn test-conduit-fn [p]
  (partial wait-for-reply (test-conduit p)))


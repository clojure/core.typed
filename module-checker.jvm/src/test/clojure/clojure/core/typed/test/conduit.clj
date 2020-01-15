(ns clojure.core.typed.test.conduit
  (:import (clojure.lang Seqable IMeta IPersistentMap LazySeq ISeq))
  (:require [clojure.core.typed :refer [check-ns ann defalias tc-ignore ann-form declare-names inst
                                        print-env inst-ctor cf declare-alias-kind]]
            [clojure.repl :refer [pst]]
            #_[arrows.core :refer [defarrow]]))

(comment
(defalias Result
  (TFn [[x :variance :covariant]]
    (U nil ;stream is closed
       '[] ;abort/skip
       '[x];consume/continue
       )))

(defalias Cont
  (TFn [[in :variance :covariant]
        [out :variance :invariant]]
    [(t/Option [(Result in) -> (Result out)]) -> (Result out)]))

(declare-alias-kind ==> (TFn [[in :variance :contravariant]
                              [out :variance :invariant]] Any))

(defalias ==>
  (TFn [[in :variance :contravariant] 
        [out :variance :invariant]]
    [in -> '[(U nil (==> in out)) (Cont out out)]]))

(ann abort-c (All [x] (Cont x x)))
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
        [nil abort-c]
        [new-f
         (-> 
           (fn [c]
             (when c ;`when` added to conform to Cont type - Ambrose
               (c [(first l)])))
           (ann-form (Cont x x)))]))))

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

(ann comp-fn2
     (All [x y z]
       [(==> x y) (==> y z) -> (==> x z)]))
(defn comp-fn2 [f1 f2]
  (fn curr-fn [x]
    (let [[new-f1 first-c] (f1 x)
          y (first-c identity)
          [new-f2 new-c] (if (empty? y)
                           [f2 abort-c]
                           (f2 (first y)))]
      [(when (and new-f1 new-f2)
         ((inst comp-fn2 x y z) new-f1 new-f2)) 
       new-c])))

;Type only works for vectors of length 2
(ann nth-fn
     (All [x y z]
       (Fn ['0 (U nil (==> x z)) -> (==> '[x y] '[z y])]
           ['1 (U nil (==> y z)) -> (==> '[x y] '[x z])])))
(tc-ignore
(defn nth-fn [n f]
  (fn curr-fn [xs]
    (cond 
      (<= (count xs) n) [curr-fn abort-c]
      (nil? f) [nil abort-c] ;added - Ambrose
      :else
      (let [[new-f new-c] (f (nth xs n))
            _ (print-env "after new-f")
            next-c (ann-form
                     (fn [c]
                       (if (nil? c)
                         (do (new-c nil)
                           nil)
                         (let [y (new-c identity)]
                           (if (empty? y)
                             (c [])
                             (c [(assoc xs n (first y))])))))
                     (Fn [(U nil [(Result '[x y]) -> (Result '[z y])]) -> (Result '[z y])]
                         [(U nil [(Result '[x y]) -> (Result '[x z])]) -> (Result '[x z])]))]
        [((inst nth-fn x y z) n new-f) next-c]))))
  )

(declare-names AParCtor)

(ann par-fn AParCtor)
(defn par-fn [f1 f2]
  (fn curr-fn [[x1 x2 :as xs]]
    (if (= (count xs) 2)
      (let [[new-f1 c1] (f1 x1)
            [new-f2 c2] (f2 x2)]
        (if (and new-f1 new-f2)
          [(par-fn new-f1 new-f2)
           (->
             (fn [c]
               (if (nil? c)
                 (do
                   (c1 nil)
                   (c2 nil))
                 (let [y1 (c1 identity)
                       y2 (c2 identity)]
                   (if (some empty? [y1 y2])
                     (c [])
                     (c [(concat y1 y2)])))))
             (ann-form (Cont '[z a] '[z a])))])
        [curr-fn abort-c])
      [curr-fn abort-c])))

(declare-names ASelectCtor)

(ann select-fn ASelectCtor)
(defn select-fn [selection-map]
  (fn curr-fn [[v x]]
    (if-let [f (ann-form (or (get selection-map v)
                             (get selection-map '_))
                         (U nil (==> y z)))]
      (let [[new-f c] (f x)]
        (if new-f
          [(select-fn (assoc selection-map v new-f)) c]
          [curr-fn abort-c])
      [curr-fn abort-c]))))

(tc-ignore
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
  )

(defalias AArrCtor 
  (All [x y]
    [[x -> y] -> (==> x y)]))

(defalias ACompCtor
  (All [x y z]
    [(==> x y) (==> y z) -> (==> x z)]))

; second arg is an arrow updating the entry named by the first argument
(defalias ANthCtor
  (All [x y z]
    (Fn ['0 (==> x z) -> (==> '[x y] '[z y])]
        ['1 (==> y z) -> (==> '[x y] '[x z])])))

(defalias AParCtor
  (All [x y z a]
    [(==> x z) (==> y a) -> (==> '[x y] '[z a])]))

(defalias AAllCtor
  (All [x y z]
       [(==> x y) (==> x z) -> (==> x '[y z])]))

(defalias ASelectCtor
  (All [x y z]
       [(IPersistentMap x (==> y z)) -> (==> '[x y] (==> y z))]))

(defalias ALoopCtor
  (All [state in]
       [(==> '[state in] state) state -> (==> in state)]))

(ann conduit '{:a-arr AArrCtor :a-comp ACompCtor :a-nth ANthCtor :a-par AParCtor
               :a-all AAllCtor :a-select ASelectCtor 
               ;:a-loop ALoopCtor
               })
(defarrow conduit
  [a-arr (->
           (fn [f]
             (fn a-arr [x]
               (let [y (f x)
                     c (->
                         (fn [c]
                           (when c
                             (c [y])))
                         (ann-form (Cont y y)))]
                 [a-arr c])))
           (ann-form AArrCtor))

   a-comp (->
            (fn [p1 p2]
              ((inst comp-fn2 x y z) p1 p2))
            (ann-form ACompCtor))

   ;apply p to position n in passed pair
   ;eg. increment second element of each list
   ; (conduit-map (a-nth 1 (a-arr inc)) [[3 5] [3 4]])
   ;([3 6] [3 5])
   a-nth (ann-form
           (fn [n p]
             ((inst nth-fn x y z) n p))
           ANthCtor)

   ;like juxt
   ;modified to accept 2 arrows rather than n arrows
   a-par (ann-form par-fn AParCtor)

   ;apply functions to lhs and rhs of pairs
   ; modified to accept 2 arrows instead of n arrows
   a-all (ann-form 
           (fn [p1 p2]
             (ann-form
               ((inst a-comp x '[x x] '[y z])
                  (ann-form
                    (a-arr (ann-form #(vector % %)
                                     [x -> '[x x]]))
                    (==> x '[x x]))
                  (ann-form 
                    ((inst a-par x x y z) p1 p2)
                    (==> '[x x] '[y z])))
               (==> x '[y z])))
           AAllCtor)

   ;select a value
   a-select (ann-form
              (fn [pair-map]
                ((inst select-fn x y z) pair-map))
              ASelectCtor)

;   a-loop (ann-form 
;            (fn
;              ([p initial-value]
;               (loop-fn p initial-value))
;              ([p initial-value fb-p]
;               (loop-fn p fb-p initial-value)))
;            ALoopCtor)
   ])


(ann a-arr AArrCtor)
(def a-arr (:a-arr conduit))
(ann a-comp ACompCtor)
(def a-comp (:a-comp conduit))
(ann a-nth ANthCtor)
(def a-nth (:a-nth conduit))
(ann a-par AParCtor)
(def a-par (:a-par conduit))
(ann a-all AAllCtor)
(def a-all (:a-all conduit))
(ann a-select ASelectCtor)
(def a-select (:a-select conduit))
;(ann a-loop ALoopCtor)
;(def a-loop (conduit :a-loop))

(ann conduit-map
     (All [x y]
       [(==> x y) (t/Option (Seqable x)) -> (t/Option (Seqable y))]))
(defn conduit-map [p l]
  (if (empty? l)
    l
    (a-run ((inst a-comp Any x y)
              (conduit-seq l) p))))

(ann pass-through 
     (All [x]
       (==> x x)))
(def pass-through
  (a-arr (inst identity x)))




; TEST


#_(tc-ignore
(cf ((inst a-run clojure.lang.Keyword) 
   (conduit-seq-fn [:a :b :c]))
  )
  )

(ann pl (==> Number Number))
(def pl ((inst a-arr Number Number) inc))

(ann t2 (==> Number Number))
(def t2 (a-arr (ann-form #(* 2 %) [Number -> Number])))

(ann flt (==> t/AnyInteger t/AnyInteger))
(def flt (fn this-fn [x]
           (if (odd? x)
             [this-fn abort-c]
             [this-fn (ann-form (fn [c] (when c 
                                          (c [x])))
                                (Cont t/AnyInteger t/AnyInteger))])))











;(ann a-selectp
;     (All [x y z a]
;          [[x -> y] (IPersistentMap y (==> z a)) -> (==> '[x z] a)]))
;(defn a-selectp [pred pair-map]
;  (a-comp
;    (ann-form
;      (a-all (ann-form (a-arr pred)
;                       (==> x y))
;             pass-through)
;      (==> x '[y x]))
;    (a-select pair-map)))

;TODO this should type correctly
;(cf (a-arr (constantly nil)))

;(cf (a-arr (ann-form (fn [_] nil)
;                     ['x -> nil])))
;
;(ann t1 (All [x] [[Any -> x] -> [x -> Any]]))
;(declare t1)
;(cf (t1 (ann-form (fn [_] nil)
;                  ['x -> nil])))

;(ann a-if
;     (All [x y]
;       (Fn [[x -> Any] (==> x y) -> (==> x (U y nil))]
;           [[x -> Any] (==> x y) (t/Option (==> x y)) -> (==> x (U y nil))])))
;(defn a-if
;  ([a b] (a-if a b nil))
;  ([a b c]
;   (let [c (ann-form (or c (a-arr (ann-form (fn [_] nil)
;                                            [x -> nil])))
;                     (==> x (U nil y)))]
;     (a-comp (->
;               (a-all (a-arr (comp boolean a))
;                      pass-through)
;               (ann-form (==> x '[boolean x])))
;             (->
;               (a-select
;                 {true b
;                  false c})
;               (ann-form (==> '[boolean x] (U y nil))))))))
;
;(defn a-catch
;  ([p catch-p]
;   (a-catch Exception p catch-p))
;  ([class p catch-p]
;   (letfn [(a-catch [f catch-f]
;             (fn [x]
;               (try
;                 (let [[new-f c] (f x)]
;                   [(a-catch f catch-f) c])
;                 (catch Throwable e
;                   (if (instance? class e)
;                     (let [[new-catch c] (catch-f [e x])]
;                       [(a-catch f new-catch) c])
;                     (throw e))))))]
;     (with-meta
;       (a-catch p catch-p)
;       {:parts (:parts p)
;        :created-by :a-catch
;        :args [class p catch-p]}))))
;
;(defn a-finally [p final-p]
;  (letfn [(a-finally [f final-f]
;            (fn [x]
;              (try
;                (let [[new-f c] (f x)]
;                  [(a-finally new-f final-f) c])
;                (finally
;                  (final-f x)))))]
;    (with-meta
;      (a-finally p final-p)
;      {:parts (:parts p)
;       :created-by :a-finally
;       :args [p final-p]})))
;
;(defmacro def-arr [name args & body]
;  `(def ~name (a-arr (fn ~name ~args ~@body))))
;
;(defn a-filter [f]
;  (with-meta
;    (fn curr-fn [x]
;      (if (f x)
;        [curr-fn (fn [c]
;                   (when c
;                     (c [x])))]
;        [curr-fn abort-c]))
;    {:created-by :a-filter
;     :args f}))
;
;(defn tap [p]
;  (fn [x]
;    (let [[new-f new-c] (p x)]
;      (new-c nil)
;      [new-f (fn [c]
;               (when c
;                 (c [x])))])))
;
;(defn disperse [p]
;  (with-meta
;    (fn curr-fn [xs]
;      (if (empty? xs)
;        [curr-fn (fn [c]
;                   (when c
;                     (c [xs])))]
;        (let [[new-f cs] (reduce (fn [[new-f cs] x]
;                                   (let [[new-f c] (new-f x)]
;                                     [new-f (conj cs c)]))
;                                 [p []]
;                                 xs)]
;          [(disperse new-f) (fn [c]
;                              (if (nil? c)
;                                (doseq [c cs]
;                                  (c nil))
;                                (let [ys (map #(% identity) cs)]
;                                  (if (some empty? ys)
;                                    (c [])
;                                    (c [(apply concat ys)])))))])))
;    {:created-by :disperse
;     :args p
;     :parts (:parts p)}))
;
;(defn enqueue [f x]
;  ((second (f x)) nil)
;  nil)
;
;(defn wait-for-reply [f x]
;  ((second (f x)) identity))
)

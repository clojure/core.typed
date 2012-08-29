(ns typed.test.conduit
  (:import (clojure.lang Seqable IMeta IPersistentMap))
  (:require [typed.core :refer [check-ns ann fn> def-alias tc-ignore]]
            [clojure.repl :refer [pst]]
            [arrows.core :refer [defarrow]]))

(def-alias Parts (IPersistentMap Any Any))
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

(ann merge-parts [(Seqable (IMeta (U (HMap {:parts Parts}) nil)))
                  -> Parts])
(defn merge-parts [ps]
  (apply merge-with merge
         (map (fn> [[a :- (IMeta (U (HMap {:parts Any}) nil))]]
                (-> a meta :parts))
              ps)))

(ann abort-c [(U nil [(Vector*) -> Any]) -> (U nil Any)])
(defn abort-c [c]
  (when c
    (c [])))

(ann conduit-seq-fn [(Seqable Any)
                     -> [Any -> (Vector* (U nil [Any -> Any])
                                         [Any -> Any])]])

(defn conduit-seq-fn [l]
  (fn> curr-fn [[x :- Any]] ;TODO
    (let [new-f (conduit-seq-fn (rest l))]
      (if (empty? l)
        [nil abort-c]
        [new-f
         (fn> [[c :- Any]] ;TODO
           (c [(first l)]))]))))

(defn conduit-seq [l]
  "create a stream processor that emits the contents of a list
  regardless of what is fed to it"
  (conduit-seq-fn l))

(defn a-run [f]
  "execute a stream processor function"
  (let [[new-f c] (f nil)
        y (c identity)]
    (cond
      (nil? new-f) (list)
      (empty? y) (recur new-f)
      :else (lazy-seq
              (cons (first y)
                    (a-run new-f))))))


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

(defn nth-fn [n f]
  (fn curr-fn [xs]
    (if (<= (count xs) n)
      [curr-fn abort-c]
      (let [[new-f new-c] (f (nth xs n))]
        [(nth-fn n new-f)
         (fn [c]
             (if (nil? c)
               (new-c nil)
               (let [y (new-c identity)]
                 (if (empty? y)
                   (c [])
                   (c [(assoc xs n (first y))])))))]))))

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
  [a-arr (fn [f]
           (with-meta
             (fn a-arr [x]
               (let [y (f x)]
                 [a-arr (fn [c]
                          (when c
                            (c [y])))]))
             {:created-by :a-arr
              :args f}))

   a-comp (fn [& ps]
            (with-meta
              (if (< (count ps) 2)
                (first ps)
                (comp-fn ps))
              {:parts (merge-parts ps)
               :created-by :a-comp
               :args ps}))

   a-nth (fn [n p]
           (with-meta
             (nth-fn n p)
             {:parts (:parts p)
              :created-by :a-nth
              :args [n p]}))

   a-par (fn [& ps]
           (with-meta
             (par-fn ps)
             {:created-by :a-par
              :args ps
              :parts (merge-parts ps)}))

   a-all (fn [& ps]
           (with-meta
             (a-comp (a-arr (partial repeat (count ps)))
                          (apply a-par ps))
             {:created-by :a-all
              :args ps
              :parts (merge-parts ps)}))

   a-select (fn [& vp-pairs]
              (let [pair-map (apply hash-map vp-pairs)]
                (with-meta
                  (select-fn pair-map)
                  {:created-by :a-select
                   :args pair-map
                   :parts (merge-parts (vals pair-map))})))

   a-loop (fn
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
   ])

(def a-arr (conduit :a-arr))
(def a-comp (conduit :a-comp))
(def a-nth (conduit :a-nth))
(def a-par (conduit :a-par))
(def a-all (conduit :a-all))
(def a-select (conduit :a-select))
(def a-loop (conduit :a-loop))

(defn conduit-map [p l]
  (if (empty? l)
    l
    (a-run (comp-fn [(conduit-seq l) p]))))

(def pass-through
  (a-arr identity))

(defn a-selectp [pred & vp-pairs]
  (a-comp
   (a-all (a-arr pred)
          pass-through)
   (apply a-select vp-pairs)))

(defn a-if [a b & [c]]
  (let [c (or c (a-arr (constantly nil)))]
    (a-comp (a-all (a-arr (comp boolean a))
                   pass-through)
            (a-select
             true b
             false c))))

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


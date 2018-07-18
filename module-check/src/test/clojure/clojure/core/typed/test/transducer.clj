(ns clojure.core.typed.test.transducer
  (:refer-clojure :exclude [map sequence take drop into])
  (:require [clojure.core.typed :as t]
            [clojure.core :as core]))

;; based on https://hypirion.com/musings/haskell-transducers

;; TODO c.c.t alias for Reduced
(t/defalias Reducer
  "A reducer function with accumulator a and reduces over collections of b"
  (t/TFn [[a :variance :contravariant]
          [b :variance :invariant]]
    (t/IFn 
      ;init
      [:-> b]
      ;complete
      [b :-> b]
      ;step
      [b a :-> (t/U b (clojure.lang.Reduced b))])))

(t/defalias Transducer
  "A transducer function that transforms in to out."
  (t/TFn [[in :variance :contravariant]
          [out :variance :covariant]]
    (t/All [r]
      [(Reducer out r) :-> (Reducer in r)])))

(t/ann ^:no-check map
     (t/All [c a b ...]
          (t/IFn 
            [[a :-> c] :-> (Transducer a c)]
            [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyASeq c)]
            [[a b ... b -> c] (t/U nil (t/Seqable a)) (t/U nil (t/Seqable b)) ... b -> (t/ASeq c)])))
(defn map [& args]
  (apply core/map args))

(t/ann ^:no-check sequence
       (t/All [a b]
         (t/IFn [(t/U nil (t/Seqable a)) -> (t/Seq a)]
                [(Transducer a b) (t/Seqable a) :-> (t/Seqable b)])))
(defn sequence [& args]
  (apply core/sequence args))

(let [map (t/inst map t/Num t/Num)
      m (map (t/inst identity t/Num))
      sequence (t/inst sequence t/Num t/Num)
      _ (t/print-env "debug map")]
  (sequence m [1 2 3]))

(t/ann ^:no-check take
       (t/All [x]
            (t/IFn [t/Int :-> (Transducer x x)]
                   [t/AnyInteger (t/U nil (t/Seqable x)) -> (t/ASeq x)])))
(defn take [& args]
  (apply core/take args))

(let [take (t/inst take t/Num)
      m (take 5)
      sequence (t/inst sequence t/Num t/Num)
      _ (t/print-env "debug take")]
  (sequence m [1 2 3]))

(t/ann ^:no-check drop
     (t/All [x]
         (t/IFn [t/Int :-> (Transducer x x)]
                [t/AnyInteger (t/U nil (t/Seqable x)) -> (t/ASeq x)])))
(defn drop [& args]
  (apply core/drop args))

(let [drop (t/inst drop t/Num)
      m (drop 5)
      sequence (t/inst sequence t/Num t/Num)
      _ (t/print-env "debug drop")]
  (sequence m [1 2 3]))

(t/ann ^:no-check into
       (t/All [x y :named [a]]
              (t/IFn 
                ;non-transducer
                [(t/Map x y) (t/U nil (t/Seqable (t/U nil
                                                      (t/Seqable (clojure.lang.IMapEntry x y))
                                                      (clojure.lang.IMapEntry x y)
                                                      '[x y])))
                 -> (t/Map x y)]
                [(t/Vec x) (t/U nil (t/Seqable x)) -> (t/Vec x)]
                [(t/Set x) (t/U nil (t/Seqable x)) -> (t/Set x)]
                [(t/Coll t/Any) (t/U nil (t/Seqable t/Any)) -> (t/Coll t/Any)]
                ;transducer
                [(t/Map x y) (Transducer a (t/U nil '[x y])) (t/U nil (t/Seqable a))
                 -> (t/Map x y)]
                [(t/Vec x) (Transducer y x) (t/U nil (t/Seqable y)) -> (t/Vec x)]
                [(t/Set x) (Transducer y x) (t/U nil (t/Seqable y)) -> (t/Set x)]
                [(t/Coll t/Any) (Transducer y t/Any) (t/U nil (t/Seqable y)) -> (t/Coll t/Any)])))
(defn into [& args]
  (apply core/into args))

(let [drop (t/inst drop t/Num)
      m (drop 5)
      into (t/inst into t/Num t/Num)
      _ (t/print-env "debug into non-map")]
  (t/ann-form (into [] m [1 2 3]) (t/Vec t/Num))
  (t/ann-form (into #{} m [1 2 3]) (t/Set t/Num))
  (t/ann-form (into '() m [1 2 3]) (t/Coll t/Any))
)

(let [drop (t/inst drop '[t/Num t/Num])
      m (drop 5)
      into (t/inst into t/Num t/Num :named {a '[t/Num t/Num]})
      _ (t/print-env "debug into map")]
  (t/ann-form (into {} m {1 2 3 4})
              (t/Map t/Num t/Num))
)

(let [map (t/inst map
                  '[t/Num t/Bool]
                  '[t/Num t/Num])
      m (map (t/fn [[k v] :- '[t/Num t/Num]]
               [k (boolean v)]))
      into (t/inst into t/Num t/Bool :named {a '[t/Num t/Num]})
      _ (t/print-env "debug into map using core/map")]
  (t/ann-form (into {} m {1 2 3 4})
              (t/Map t/Num t/Bool))
)


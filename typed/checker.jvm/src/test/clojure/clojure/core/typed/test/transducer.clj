(ns clojure.core.typed.test.transducer
  (:refer-clojure :exclude [])
  (:require [clojure.core.typed :as t]
            [clojure.core :as core]))

;; based on https://hypirion.com/musings/haskell-transducers

;; TODO c.c.t alias for Reduced

;; added
#_
(t/ann ^:no-check map
     (t/All [c a b ...]
          (t/IFn 
            [[a :-> c] :-> (t/Transducer a c)]
            [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyASeq c)]
            [[a b ... b -> c] (t/U nil (t/Seqable a)) (t/U nil (t/Seqable b)) ... b -> (t/ASeq c)])))
#_
(defn map [& args]
  (apply core/map args))

#_#_
(t/ann ^:no-check mapcat
     (t/All [c a b ...]
       (t/IFn
          [[a :-> (t/Option (t/Seqable c))] :-> (t/Transducer a c)]
          [[a b ... b -> (t/Option (t/Seqable c))] (t/Option (t/Seqable a)) (t/Option (t/Seqable b)) ... b -> (t/ASeq c)])))
(defn mapcat [& args]
  (apply core/mapcat args))

;; added
#_
(t/ann ^:no-check sequence
       (t/All [a b]
         (t/IFn [(t/U nil (t/Seqable a)) -> (t/Seq a)]
                [(t/Transducer a b) (t/Seqable a) :-> (t/Seqable b)])))
#_
(defn sequence [& args]
  (apply core/sequence args))

(let [map (t/inst map t/Num t/Num)
      m (map (t/inst identity t/Num))
      sequence (t/inst sequence t/Num t/Num)]
  (sequence m [1 2 3]))

(let [mapcat (t/inst mapcat t/Num t/Num)
      m (mapcat (t/inst vector t/Num t/Num))
      sequence (t/inst sequence t/Num t/Num)]
  (sequence m [1 2 3]))

;; added
#_
(t/ann ^:no-check take
       (t/All [x]
            (t/IFn [t/Int :-> (t/Transducer x x)]
                   [t/AnyInteger (t/U nil (t/Seqable x)) -> (t/ASeq x)])))
#_
(defn take [& args]
  (apply core/take args))

(let [take (t/inst take t/Num)
      m (take 5)
      sequence (t/inst sequence t/Num t/Num)]
  (sequence m [1 2 3]))

;; added
#_
(t/ann ^:no-check drop
     (t/All [x]
         (t/IFn [t/Int :-> (t/Transducer x x)]
                [t/AnyInteger (t/U nil (t/Seqable x)) -> (t/ASeq x)])))
#_
(defn drop [& args]
  (apply core/drop args))

(let [drop (t/inst drop t/Num)
      m (drop 5)
      sequence (t/inst sequence t/Num t/Num)]
  (sequence m [1 2 3]))

;; added
#_(t/ann ^:no-check into
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
                [(t/Map x y) (t/Transducer a (t/U nil '[x y])) (t/U nil (t/Seqable a))
                 -> (t/Map x y)]
                [(t/Vec x) (t/Transducer y x) (t/U nil (t/Seqable y)) -> (t/Vec x)]
                [(t/Set x) (t/Transducer y x) (t/U nil (t/Seqable y)) -> (t/Set x)]
                [(t/Coll t/Any) (t/Transducer y t/Any) (t/U nil (t/Seqable y)) -> (t/Coll t/Any)])))
#_
(defn into [& args]
  (apply core/into args))

(let [drop (t/inst drop t/Num)
      m (drop 5)
      into (t/inst into t/Num t/Num)]
  (t/ann-form (into [] m [1 2 3]) (t/Vec t/Num))
  (t/ann-form (into #{} m [1 2 3]) (t/Set t/Num))
  (t/ann-form (into '() m [1 2 3]) (t/Coll t/Any)))

(let [drop (t/inst drop '[t/Num t/Num])
      m (drop 5)
      into (t/inst into t/Num t/Num :named {a '[t/Num t/Num]})]
  (t/ann-form (into {} m {1 2 3 4})
              (t/Map t/Num t/Num)))

(let [map (t/inst map
                  '[t/Num t/Bool]
                  '[t/Num t/Num])
      m (map (t/fn [[k v] :- '[t/Num t/Num]]
               [k (boolean v)]))
      into (t/inst into t/Num t/Bool :named {a '[t/Num t/Num]})]
  (t/ann-form (into {} m {1 2 3 4})
              (t/Map t/Num t/Bool)))


(ns clojure.core.typed.test.set
  (:require [clojure.core.typed :refer [check-ns ann cf tc-ignore print-env ann-form]
             :as t])
  (:import (clojure.lang APersistentSet)))

(ann bubble-max-key (t/All [x]
                      [[x -> Number] (t/I (t/Seqable x) (t/CountRange 2)) -> (t/Seqable x)]))
(tc-ignore
(defn- bubble-max-key [k coll]
  "Move a maximal element of coll according to fn k (which returns a number) 
   to the front of coll."
  (let [max (apply max-key k coll)]
    (cons max (remove #(identical? max %) coll))))
  )

(ann union (t/All [x]
             (t/IFn [ -> (APersistentSet x)]
                 [(APersistentSet x) -> (APersistentSet x)]
                 [(APersistentSet x) (APersistentSet x) -> (APersistentSet x)]
                 [(APersistentSet x) (APersistentSet x) (APersistentSet x) * -> (APersistentSet x)])))
(tc-ignore
(defn union
  "Return a set that is the union of the input sets"
  {:added "1.0"}
  ([] #{})
  ([s1] s1)
  ([s1 s2]
     (if (< (count s1) (count s2))
       (reduce conj s2 s1)
       (reduce conj s1 s2)))
  ([s1 s2 & sets]
   (print-env "top of variable arity")
     (let [bubbled-sets (bubble-max-key count (conj sets s2 s1))]
       (reduce into (first bubbled-sets) (rest bubbled-sets)))))
  )

(tc-ignore
(defn intersection
  "Return a set that is the intersection of the input sets"
  {:added "1.0"}
  ([s1] s1)
  ([s1 s2]
     (if (< (count s2) (count s1))
       (recur s2 s1)
       (reduce (fn [result item]
                 (if (contains? s2 item)
                   result
                   (disj result item)))
               s1 s1)))
  ([s1 s2 & sets] 
     (let [bubbled-sets (bubble-max-key #(- (count %)) (conj sets s2 s1))]
       (reduce intersection (first bubbled-sets) (rest bubbled-sets)))))

(defn difference
  "Return a set that is the first set without elements of the remaining sets"
  {:added "1.0"}
  ([s1] s1)
  ([s1 s2] 
     (if (< (count s1) (count s2))
       (reduce (fn [result item] 
                   (if (contains? s2 item) 
                     (disj result item) 
                     result))
               s1 s1)
       (reduce disj s1 s2)))
  ([s1 s2 & sets] 
     (reduce difference s1 (conj sets s2))))
)

(ann select (t/All [x]
              [[x -> t/Any] (t/Set x) -> (t/Set x)]))
(tc-ignore
(defn select
  "Returns a set of the elements for which pred is true"
  {:added "1.0"}
  [pred xset]
    (reduce (ann-form
              (fn [s k] (if (pred k) s (disj s k)))
              [(t/Set x) x -> (t/Set x)])
            xset xset))

(defn project
  "Returns a rel of the elements of xrel with only the keys in ks"
  {:added "1.0"}
  [xrel ks]
  (with-meta (set (map #(select-keys % ks) xrel)) (meta xrel)))

(defn rename-keys
  "Returns the map with the keys in kmap renamed to the vals in kmap"
  {:added "1.0"}
  [map kmap]
    (reduce 
     (fn [m [old new]]
       (if (contains? map old)
         (assoc m new (get map old))
         m)) 
     (apply dissoc map (keys kmap)) kmap))

(defn rename
  "Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap"
  {:added "1.0"}
  [xrel kmap]
  (with-meta (set (map #(rename-keys % kmap) xrel)) (meta xrel)))

(defn index
  "Returns a map of the distinct values of ks in the xrel mapped to a
  set of the maps in xrel with the corresponding values of ks."
  {:added "1.0"}
  [xrel ks]
    (reduce
     (fn [m x]
       (let [ik (select-keys x ks)]
         (assoc m ik (conj (get m ik #{}) x))))
     {} xrel))
)

(comment
(check-ns)
)

(ann map-invert (t/All [x y]
                  [(t/Map x y) -> (t/Map y x)]))
(defn map-invert
  "Returns the map with the vals mapped to the keys."
  {:added "1.0"}
  [m] 
  (reduce (ann-form
            (fn [m [k v]] 
              (assoc m v k)) 
            [(t/Map y x) '[x y] -> (t/Map y x)])
          {} m))

(defn join
  "When passed 2 rels, returns the rel corresponding to the natural
  join. When passed an additional keymap, joins on the corresponding
  keys."
  {:added "1.0"}
  ([xrel yrel] ;natural join
   (if (and (seq xrel) (seq yrel))
     (let [ks (intersection (set (keys (first xrel))) (set (keys (first yrel))))
           [r s] (if (<= (count xrel) (count yrel))
                   [xrel yrel]
                   [yrel xrel])
           idx (index r ks)]
       (reduce (fn [ret x]
                 (let [found (idx (select-keys x ks))]
                   (if found
                     (reduce #(conj %1 (merge %2 x)) ret found)
                     ret)))
               #{} s))
     #{}))
  ([xrel yrel km] ;arbitrary key mapping
   (let [[r s k] (if (<= (count xrel) (count yrel))
                   [xrel yrel (map-invert km)]
                   [yrel xrel km])
         idx (index r (vals k))]
     (reduce (fn [ret x]
               (let [found (idx (rename-keys (select-keys x (keys k)) k))]
                 (if found
                   (reduce #(conj %1 (merge %2 x)) ret found)
                   ret)))
             #{} s))))

(defn subset? 
  "Is set1 a subset of set2?"
  {:added "1.2",
   :tag Boolean}
  [set1 set2]
  (and (<= (count set1) (count set2))
       (every? #(contains? set2 %) set1)))

(defn superset? 
  "Is set1 a superset of set2?"
  {:added "1.2",
   :tag Boolean}
  [set1 set2]
  (and (>= (count set1) (count set2))
       (every? #(contains? set1 %) set2)))

(comment
(refer 'set)
(def xs #{{:a 11 :b 1 :c 1 :d 4}
         {:a 2 :b 12 :c 2 :d 6}
         {:a 3 :b 3 :c 3 :d 8 :f 42}})

(def ys #{{:a 11 :b 11 :c 11 :e 5}
         {:a 12 :b 11 :c 12 :e 3}
         {:a 3 :b 3 :c 3 :e 7 }})

(join xs ys)
(join xs (rename ys {:b :yb :c :yc}) {:a :a})

(union #{:a :b :c} #{:c :d :e })
(difference #{:a :b :c} #{:c :d :e})
(intersection #{:a :b :c} #{:c :d :e})

(index ys [:b])
)



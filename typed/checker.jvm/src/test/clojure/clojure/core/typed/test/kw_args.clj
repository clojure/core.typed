(ns clojure.core.typed.test.kw-args
  (:require [clojure.core.typed :refer [ann check-ns ann-form cf] :as t]))

(ann empty-kw [& :optional {} -> nil])
(defn empty-kw [& {:as opts}]
  nil)

(ann foo-kw [& :optional {:a Number} -> (t/U nil Number)])
(defn foo-kw [& {:keys [a]}]
  (when a
    (inc a)))

(fn [] (foo-kw :a 1))
;(fn [] (foo-kw :a 'a))

(ann foo-arg-kw [Number & :optional {:a Number} -> (t/U nil Number)])
(defn foo-arg-kw [c & {:keys [a]}]
  (when a
    (inc a)))

;(fn [] (foo-arg-kw 1 2 :a 1))
(fn [] (foo-arg-kw 1 :a 1))

(ann poly-foo-kw (t/All [x]
                      [& :optional {:a x} -> (t/U nil x)]))
(defn poly-foo-kw [& {:keys [a]}]
  a)

(ann-form (poly-foo-kw :a 1)
          (t/U nil Number))
(ann-form (poly-foo-kw :a 'a)
          (t/U nil clojure.lang.Symbol))

(ann poly-foo-mandatory-kw (t/All [x]
                                [& :mandatory {:a x} -> x]))
(defn poly-foo-mandatory-kw [& {:keys [a]}]
  a)

(ann-form (poly-foo-mandatory-kw :a 'a)
          clojure.lang.Symbol)
(ann-form (poly-foo-mandatory-kw :a 1)
          Number)

;(ann poly-kw-variance (t/All [x]
;                      [& {:a x :b [x -> t/Any]} -> t/Any]))
;     What happens if we don't supply a :b parameter? Is `x` detected to be contravariant? It should probably be invariant

(ann test-non-value-kw [& :optional {:a [t/Any -> t/Any]} -> t/Any])
(defn test-non-value-kw [& {:keys [a]}] a)

(test-non-value-kw :a (fn [a] a))

; keyword map is a (union of) complete hash-map(s)

(ann test-complete-hmap [& :optional {:a Number} -> t/Any])
(defn test-complete-hmap [& {:keys [a]}]
  (ann-form a (t/U nil Number)))

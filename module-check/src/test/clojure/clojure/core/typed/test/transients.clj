(ns clojure.core.typed.test.transients
  (:require [clojure.core.typed :refer :all])
  (:import (clojure.lang ITransientMap ITransientVector ITransientSet
                         ITransientAssociative ATransientSet)))

(let [x :- (ITransientVector Number), (transient [1])]
  (conj! x 1)
  (conj! x 2)
  (conj! x "a"))

(let [x :- (ITransientVector Number), (transient [1 2 3])]
  [x x])

(let [x :- (ITransientMap Keyword Number), (transient {:a 1})]
  (assoc! x :b 2)
  (assoc! x :c 3))

(let [x :- (ITransientVector Number), (transient [1 2 3])]
  (let [y x]
    [y y]))

(let [x :- (ITransientVector Number), (transient [1 2 3])]
  (let [y 0]
    (conj! x 1))
  (conj! x 2))

(let [m-atom :- (Atom1 (ITransientMap Keyword Number)), (atom (transient {}))]
  (assoc! @m-atom :a 1)
  (assoc! @m-atom :b 2))

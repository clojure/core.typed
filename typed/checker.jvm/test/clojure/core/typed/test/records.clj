(ns clojure.core.typed.test.records
  (:require [clojure.core.typed :refer [check-ns ann-record ann-form cf]]
            [clojure.repl :refer [pst]]))

(set! *warn-on-reflection* true)

(ann-record MyRecord [a :- Number])

(defrecord MyRecord [a]
  Object
  (toString [this] nil))

(ann-form (:a (->MyRecord 1)) Number)
(ann-form (map->MyRecord {:a 2}) MyRecord)

(ann-form (let [^MyRecord r (MyRecord. 1)]
            (.a r))
          Number)

(let [^MyRecord r (MyRecord. 1)]
  (ann-form (.a r) Number))

(let [r (MyRecord. 1)]
  (ann-form (.a r) Number))

(ann-form (assoc (->MyRecord 1) :a 1)
          MyRecord)

#_(ann-form (assoc (->MyRecord 1) :a 'a) MyRecord)

#_(do
  (defrecord A [a]
    clojure.lang.IFn
    (invoke [this query] (get this query)))
  (ann-record A [a :- Number]))

(ann-record [[a :variance :covariant]]
            MyPolyRecord [a :- Number])

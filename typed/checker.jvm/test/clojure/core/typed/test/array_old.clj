; some old tests that don't type check anymore but look useful
(ns clojure.core.typed.test.array-old
  (:require [clojure.core.typed :refer [ann check-ns into-array> cf print-env ann-form]
             :as t]
            [clojure.repl :refer [pst]]))

(ann my-integer-array [-> (Array Integer)])
(defn my-integer-array [] (into-array> Integer (map int [1 2])))

(ann my-int-array [-> (Array int)])
(defn my-int-array [] (into-array> int (map int [1 2])))

(ann sum [(ReadOnlyArray Number) -> Number])
(defn sum [arr]
  (t/loop [idx :- long 0,
           ret :- Number 0]
    (if (< idx (alength arr))
      (recur 
        (unchecked-inc idx) 
        (+ (aget arr idx) ret))
      ret)))

(fn [] (sum (my-integer-array)))

(ann write-integer-to-zero [(Array2 Integer t/Any) -> nil])
(defn write-integer-to-zero [arr]
  (aset arr 0 (int 12))
  nil)

(ann write-int-to-zero [(Array2 int t/Any) -> nil])
(defn write-int-to-zero [arr]
  (aset arr 0 (int 12))
  nil)

(fn [] (write-integer-to-zero my-integer-array))
(fn [] (write-int-to-zero (my-int-array)))

(ann bad-modify-array [(Array Number) -> nil])
#_(defn bad-modify-array [ar]
  (let [ar2 (ann-form ar (Array2 Number Object))]
    (aset ar2 0 (new java.util.Observable))
    nil))

(ns clojure.core.typed.test.array
  (:require [clojure.core.typed :refer [ann check-ns into-array> cf loop> print-env ann-form]]
            [clojure.repl :refer [pst]]))

(ann my-integer-array (Array Integer))
(def my-integer-array (into-array> Integer (map int [1 2])))

(ann my-int-array (Array int))
(def my-int-array (into-array> int (map int [1 2])))

(ann sum [(ReadOnlyArray Number) -> Number])
(defn sum [arr]
  (loop> [[idx :- long] 0 
          [ret :- Number] 0]
         (if (< idx (alength arr))
           (recur 
             (unchecked-inc idx) 
             (+ (aget arr idx) ret))
           ret)))

(sum my-integer-array)

(ann write-integer-to-zero [(Array2 Integer Any) -> nil])
(defn write-integer-to-zero [arr]
  (aset arr 0 (int 12))
  nil)

(ann write-int-to-zero [(Array2 int Any) -> nil])
(defn write-int-to-zero [arr]
  (aset arr 0 (int 12))
  nil)

(write-integer-to-zero my-integer-array)
(write-int-to-zero my-int-array)

(ann bad-modify-array [(Array Number) -> nil])
#_(defn bad-modify-array [ar]
  (let [ar2 (ann-form ar (Array2 Number Object))]
    (aset ar2 0 (new java.util.Observable))
    nil))

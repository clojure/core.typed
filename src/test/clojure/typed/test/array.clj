(ns typed.test.array
  #_(:import (clojure.lang ))
  (:require [typed.core :refer [ann check-ns into-array> cf loop> print-env]]
            [clojure.repl :refer [pst]]))

(set! *warn-on-reflection* true)

(ann my-integer-array (Array Integer))
(def my-integer-array (into-array> Integer (map int [1 2])))

(ann my-int-array (Array int))
(def my-int-array (into-array> int (map int [1 2])))

(ann sum [(ReadOnlyArray Number) -> Number])
(defn sum [^"[Ljava.lang.Object;" arr]
  (loop> [[idx :- long] 0 
          [ret :- Number] 0]
         (if (< idx (alength arr))
           (recur 
             (unchecked-inc idx) 
             (+ (aget arr idx) ret))
           ret)))

(sum my-integer-array)

(ann write-integer-to-zero [(Array2 Integer Any) -> nil])
(defn write-integer-to-zero [^"[Ljava.lang.Object;" arr]
  (aset arr 0 (int 12))
  nil)

(ann write-int-to-zero [(Array2 int Any) -> nil])
(defn write-int-to-zero [^ints arr]
  (aset arr 0 (int 12))
  nil)

(write-integer-to-zero my-integer-array)
(write-int-to-zero my-int-array)

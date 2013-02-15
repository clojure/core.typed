(ns typed.test.array
  #_(:import (clojure.lang ))
  (:require [typed.core :refer [ann check-ns into-array> cf loop> print-env]]
            [clojure.repl :refer [pst]]))

(set! *warn-on-reflection* true)

(ann my-integer-array (Array Integer))
(def my-integer-array (into-array> Integer (map int [1 2])))

(ann sum [(ReadOnlyArray Number) -> Number])
#_(defn sum [arr]
  (let [^"[Ljava.lang.Number;" arr arr]
    (loop> [[idx :- Number] 0 
            [ret :- Number] 0]
           (print-env "ret")
           (if (< idx (alength arr))
             (recur 
               (unchecked-inc idx) 
               (+ (aget arr idx) ret))
             ret))))

(sum my-integer-array)


(ann write-integer-to-zero 
     [(Array2 Integer Any) -> nil])
(defn write-integer-to-zero [arr]
  (aset arr 0 (int 12))
  nil)

(write-integer-to-zero my-integer-array)

(ns clojure.core.typed.test.async-alts
  (:require [clojure.core.typed :refer [ann def-alias check-ns cf doseq> loop>
                                        AnyInteger dotimes> Seqable ann-form]
             :as t]
            [clojure.core.typed.async :refer [Chan TimeoutChan chan> Port]]
            [clojure.core.async :as async :refer [<! >! <!! >!! timeout chan alt! alts! alts!!]]))

(ann fan-in [(Seqable (Port Any)) -> (Chan Any)])
(defn fan-in [ins]
  (let [c (chan> Any)]
    (future (while true
              (let [[x] (alts!! ins)]
                (>!! c x))))
    c))

(ann fan-out [(Port Any) (U t/Int (Extends [(Seqable (Chan Any))] :without [Number])) -> (Seqable (Chan Any))])
(defn fan-out [in cs-or-n]
  (let [cs (if (number? cs-or-n)
             (repeatedly cs-or-n chan)
             cs-or-n)]
    (future (while true
              (let [x (<!! in)
                    outs (map (ann-form #(vector % x) [(Chan Any) -> '[(Chan Any) Any]])
                              (ann-form cs (Seqable (Chan Any))))]
                (alts!! outs))))
    cs))

(fn []
(let [cout (chan> Any)
      cin (fan-in (fan-out cout (repeatedly 3 chan)))]
  (dotimes> [n 10]
    (>!! cout n)
    (prn (<!! cin))))
  )



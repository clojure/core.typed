(ns clojure.core.typed.test.async-go
  (:require [clojure.core.typed :refer [ann def-alias check-ns cf doseq> loop>
                                        AnyInteger]
             :as t]
            [clojure.core.typed.async :refer [Chan TimeoutChan go> chan>]]
            [clojure.core.async :as async :refer [<! >! <!! timeout alt! ]]))

(def-alias Kind
  "Id for search"
  Any)

(def-alias Query
  "A google query"
  Any)

(def-alias FakeSearch [(Chan Any) Query -> (Chan Any)])

(defmacro ann-many [t & vs]
  `(do ~@(map #(list `ann % t) vs)))

(ann fake-search [Kind -> FakeSearch])
(defn fake-search [kind]
  (fn [c query]
    (go>
     (<! (timeout (rand-int 100)))
     (>! c [kind query]))))

(ann-many FakeSearch
          web1 web2 image1 image2 video1 video2)

(def web1 (fake-search :web1))
(def web2 (fake-search :web2))
(def image1 (fake-search :image1))
(def image2 (fake-search :image2))
(def video1 (fake-search :video1))
(def video2 (fake-search :video2))

(ann fastest [Query FakeSearch * -> (Chan Query)])
(defn fastest [query & replicas]
  (let [c (chan> Query)]
    (doseq> [replica :- FakeSearch, replicas]
      (replica c query))
    c))

(ann google [Query -> (Chan Any)])
(defn google [query]
  (let [c (chan> Query)
        t (timeout 80)]
    (go> (>! c (<! (fastest query web1 web2))))
    (go> (>! c (<! (fastest query image1 image2))))
    (go> (>! c (<! (fastest query video1 video2))))
    (go> (loop> [i :- AnyInteger, 0 
                ret :- (Vec '[(Chan Query) TimeoutChan]), []]
          (if (= i 3)
            ret
            (recur (inc i) (conj ret (alt! [c t] ([v] v)))))))))

(<!! (google "clojure"))

; from David Nolen's blog
(ns cljs.core.typed.test.csp
  (:refer-clojure :exclude [map])
  (:require [cljs.core.async :as async
              :refer [<! >! chan put! timeout]]
            [clojure.string :as string]
            [cljs.core.typed.test.dnolen.utils.dom :refer [by-id set-html! offset]]
            [cljs.core.typed.test.dnolen.utils.reactive :refer [listen map]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(def c (chan))

(defn render [q]
  (apply str
    (for [p (reverse q)]
      (str "<div class='proc-" p "'>Process " p "</div>"))))

#_(go (while true (<! (timeout 250)) (>! c 1)))
#_(go (while true (<! (timeout 1000)) (>! c 2)))
#_(go (while true (<! (timeout 1500)) (>! c 3)))

(defn peekn
  "Returns vector of (up to) n items from the end of vector v"
  [v n]
  (if (> (count v) n)
    (subvec v (- (count v) n))
    v))

(let [el  (by-id "ex0")
      out (by-id "ex0-out")]
  (go (loop [q []]
        (set-html! out (render q))
        (recur (-> (conj q (<! c)) (peekn 10))))))

(let [el  (by-id "ex1")
      out (by-id "ex1-mouse")
      c   (listen el :mousemove)]
  (go (while true
        (let [e (<! c)]
          (set-html! out (str (.-offsetX e) ", " (.-offsetY e)))))))

(defn location [el]
  (let [[left top] (cljs.core/map int (offset el))]
    (fn [e]
      {:x (+ (.-offsetX e) left)
       :y (+ (.-offsetY e) top)})))


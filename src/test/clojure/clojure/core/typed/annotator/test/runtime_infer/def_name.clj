(ns clojure.core.typed.annotator.test.runtime-infer.def-name
  {:lang :core.typed
   :core.typed {:features #{:runtime-infer}}
   }
  (:require [clojure.core.typed :as t]
            [clojure.pprint :refer [pprint]]))

(defn game-over-success [game-state]
  (reset! game-state {:sector [1 2]
                      :quadrant [3 2]
                      :energy 33
                      :is_docked false
                      :shields 343})
  (reset! game-state {:extra 1
                      :sector [1 2]
                      :quadrant [3 2]
                      :energy 33
                      :is_docked false
                      :shields 343})
  (swap! game-state assoc :is_docked true)
  nil)

(game-over-success (atom {}))

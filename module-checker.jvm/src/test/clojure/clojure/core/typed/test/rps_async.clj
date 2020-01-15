; Adapted from: http://tech.puredanger.com/2013/07/10/rps-core-async/
(ns clojure.core.typed.test.rps-async
  (:require [clojure.core.typed :as t]
            [clojure.core.async :as a]
            [clojure.core.typed.async :as ta]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/defalias Move
  "A legal move in rock-paper-scissors"
  (t/U ':rock ':paper ':scissors))

(t/defalias PlayerName
  "A player's name in rock-paper-scissors"
  t/Str)

(t/defalias PlayerMove
  "A move in rock-paper-scissors. A Tuple of player name and move"
  '[PlayerName Move])

(t/defalias RPSResult
  "The result of a rock-paper-scissors match.
  A 3 place vector of the two player moves, and the winner"
  '[PlayerMove PlayerMove PlayerName])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(t/ann MOVES (t/Vec Move))
(def MOVES [:rock :paper :scissors])

(t/ann BEATS (t/Map Move Move))
(def BEATS {:rock :scissors, :paper :rock, :scissors :paper})

(t/ann rand-player [PlayerName -> (ta/Chan PlayerMove)])
(defn rand-player
  "Create a named player and return a channel to report moves."
  [name]
  (let [out (ta/chan :- PlayerMove)]
    (ta/go (while true (a/>! out [name (rand-nth MOVES)])))
    out))

(t/ann winner [PlayerMove PlayerMove -> PlayerName])
(defn winner
  "Based on two moves, return the name of the winner."
  [[name1 move1] [name2 move2]]
  (cond
    (= move1 move2) "no one"
    (= move2 (BEATS move1)) name1
    :else name2))

(t/ann judge [(ta/Chan PlayerMove) (ta/Chan PlayerMove) -> (ta/Chan RPSResult)])
(defn judge
  "Given two channels on which players report moves, create and return an
  output channel to report the results of each match as [move1 move2 winner]."
  [p1 p2]
  (let [out (ta/chan :- RPSResult)]
    (ta/go
      (while true
        (let [m1 (a/<! p1)
              m2 (a/<! p2)]
          (assert m1)
          (assert m2)
          (a/>! out (t/ann-form [m1 m2 (winner m1 m2)]
                                RPSResult)))))
    out))

(t/ann init (t/IFn [PlayerName PlayerName -> (ta/Chan RPSResult)]
                   [-> (ta/Chan RPSResult)]))
(defn init
  "Create 2 players (by default Alice and Bob) and return an output channel of match results."
  ([] (init "Alice" "Bob"))
  ([n1 n2] (judge (rand-player n1) (rand-player n2))))

(t/ann report [PlayerMove PlayerMove PlayerName -> nil])
(defn report
  "Report results of a match to the console."
  [[name1 move1] [name2 move2] winner]
  (println)
  (println name1 "throws" move1)
  (println name2 "throws" move2)
  (println winner "wins!"))

(t/ann play [(ta/Chan RPSResult) -> nil])
(defn play
  "Play by taking a match reporting channel and reporting the results of the latest match."
  [out-chan]
  (let [[move1 move2 winner :as c] (a/<!! out-chan)]
    (assert c)
    (report move1 move2 winner)))

(t/ann play-many [(ta/Chan RPSResult) t/Int -> (t/Map t/Any t/Any)])
(defn play-many
  "Play n matches from out-chan and report a summary of the results."
  [out-chan n]
  (t/loop [remaining :- t/Int, n
           results :- (t/Map PlayerName t/Int), {}]
    (if (zero? remaining)
      results
      (let [[m1 m2 winner :as c] (a/<!! out-chan)]
        (assert c)
        (recur (dec remaining)
               (merge-with (t/fn [i :- t/Num, j :- t/Num]
                             (int (+ i j)))
                           results {winner 1}))))))


(fn []
  (t/ann-form (a/<!! (init))
              (t/U nil RPSResult)))


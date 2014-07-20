(ns clojure.core.typed.test.async
  (:require 
    [clojure.core.typed.test.test-utils :refer :all]
    [clojure.test :refer :all]
    [clojure.core.typed :as t]
    [clojure.core.async :as a]))

; wrap all these tests in thunks to prevent side effects

(deftest async-test
  (is-tc-e 
    #(let [c (chan :- t/Str)]
       (go (a/>! c "hello"))
       (prn (a/<!! (go :- Str (a/<! c))))
       (a/close! c))
    :requires [[clojure.core.async :as a]
               [clojure.core.typed.async :refer [go chan]]])
  (is-tc-e 
    #(let [c1 (chan :- t/Str)
           c2 (chan :- t/Str)]
       (a/thread (while true
                   (let [[v ch] (a/alts!! [c1 c2])]
                     (println "Read" v "from" ch))))
       (a/>!! c1 "hi")
       (a/>!! c2 "there"))
    :requires [[clojure.core.async :as a]
               [clojure.core.typed.async :as ta :refer [go chan]]])
  )

;(let [c1 (chan)
;      c2 (chan :- t/Str)]
;  (go (while true
;        (let [[v ch] (a/alts! [c1 c2])]
;          (println "Read" v "from" ch))))
;  (go (a/>! c1 "hi"))
;  (go (a/>! c2 "there")))

(comment
  (require '[clojure.core.typed.current-impl :as impl]
           '[clojure.core.typed.analyze-clj :as ana]
           '[clojure.core.typed.util-vars :as vs])
  (-> (binding [vs/*checking* true]
        (impl/with-clojure-impl
          (ana/ast-for-ns *ns*)))
      (nth 1)
      :body
      :statements
      first
      :body
      :statements
      first
      :form
      meta
      )
  )

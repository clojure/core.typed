(ns clojure.core.typed.test.async
  (:require [clojure.core.typed.async :refer :all]
            [clojure.core.typed :as t]
            [clojure.core.async :as a]))

#_(let [c (chan :- t/Str)]
;this call randomly fails to be inferred. Try and make In/Un sorted.
  (go (a/>! c "hello"))
  (prn (a/<!! (go :- Str (a/<! c))))
  (a/close! c))

;(let [c1 (chan :- t/Str)
;      c2 (chan :- t/Str)]
;  (a/thread (while true
;              (let [[v ch] (a/alts!! [c1 c2])]
;                (println "Read" v "from" ch))))
;  (a/>!! c1 "hi")
;  (a/>!! c2 "there"))
;
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

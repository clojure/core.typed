(ns clojure.core.typed.test.runtime-infer.polymorphic
  {:lang :core.typed
   :core.typed {:features #{:runtime-infer}}
   }
  (:refer-clojure :exclude [identity memoize])
  (:require [clojure.core.typed :as t]
            [clojure.core :as core]
            [clojure.pprint :refer [pprint]]))

(defn identity [x]
  x)

(defn memoize [f]
  (fn [a]
    (f a)))

;(identity 1)
;(identity 'a)
;(identity :a)

(memoize identity)
;((memoize identity) 1)
;
;((memoize identity) 'a)
;((nth (iterate memoize identity) 100) :a)

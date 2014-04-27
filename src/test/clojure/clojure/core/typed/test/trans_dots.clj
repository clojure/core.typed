(ns clojure.core.typed.test.trans-dots
  (:refer-clojure :exclude [defprotocol])
  (:require [clojure.core.typed :as t :refer [defprotocol inst]]))

(defprotocol [x] Foo)

(t/ann-datatype [x] Bar [])
(deftype Bar [])

(t/ann foo (All [x b ...]
             [(U x (Foo x) (Bar x)) ... b -> Any]))
(defn foo [& args])

;(defn [x b ...] foo 
;  [& args :- (U x Foo) ... b] :- Any)

(inst foo Any)
(inst foo Any Any)

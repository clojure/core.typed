(ns clojure.core.typed.test.trans-dots
  (:refer-clojure :exclude [defprotocol])
  (:require [clojure.core.typed :as t :refer [defprotocol inst]]))

(defprotocol [x] Foo)

(t/ann-datatype [x] Bar [])
(deftype Bar [])

(t/ann foo (t/All [x b ...]
             [(t/U x (Foo x) (Bar x)) ... b -> t/Any]))
(defn foo [& args])

;(defn [x b ...] foo 
;  [& args :- (U x Foo) ... b] :- t/Any)

(inst foo t/Any)
(inst foo t/Any t/Any)

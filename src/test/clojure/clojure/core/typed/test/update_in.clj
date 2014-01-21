(ns clojure.core.typed.test.update-in
  (:require [clojure.core.typed :as t]))

(t/ann-form (get-in {} [:a]) nil)
(t/ann-form (get-in {:a 1} [:a]) (Value 1))
(t/ann-form (get-in {:a {:b 1}} [:a :b]) (Value 1))

(t/ann-form (get-in {:a {:b 1}} [:a 'a]) Any)

(t/ann-form (get-in {:a {:b 1}} [:a 'a] 1) Any)
(t/ann-form (get-in {:a {:b 1}} [:a] 1) Any)

(assoc-in {} [:a] (constantly 1))

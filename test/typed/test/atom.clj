(ns typed.test.atom
  (:require [typed.core :refer [ann check-ns]])
  (:import (clojure.lang Atom)))

(ann my-atom (Atom Number Number))
(def my-atom (atom 2))

(reset! my-atom 1)

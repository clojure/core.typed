(ns typed.test.atom
  (:require [typed.core :refer [ann ann-form check-ns cf]]
            [clojure.repl :refer [pst]])
  (:import (clojure.lang Atom)))

(ann my-atom (Atom Number Number))
(def my-atom (atom 2))

(reset! my-atom 1)
(swap! my-atom + 1 2)

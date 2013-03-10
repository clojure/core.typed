(ns clojure.core.typed.test.interop
  (:import (java.io File))
  (:require [clojure.core.typed :refer [ann non-nil-return check-ns cf]]))

(check-ns)

(cf 1 Number)

(ann f nil)
(def f (File. "a"))

(ann prt (U nil String))
(def prt (.getParent ^File f))

(non-nil-return java.io.File/getName :all)
(ann nme String)
(def nme (.getName ^File f))

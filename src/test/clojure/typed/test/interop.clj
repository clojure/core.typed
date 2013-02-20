(ns typed.test.interop
  (:import (java.io File))
  (:require [typed.core :refer [ann non-nil-return check-ns]]))

(ann f nil)
(def f (File. "a"))

(ann prt (U nil String))
(def prt (.getParent ^File f))

(non-nil-return java.io.File/getName :all)
(ann nme String)
(def nme (.getName ^File f))

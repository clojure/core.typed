(ns clojure.core.typed.test.interop
  (:import (java.io File))
  (:require [clojure.core.typed :as t]))

(t/ann f File)
(def f (File. "a"))

(t/ann prt (t/U nil String))
(def prt (.getParent ^File f))

(t/non-nil-return java.io.File/getName :all)
(t/ann nme String)
(def nme (.getName ^File f))

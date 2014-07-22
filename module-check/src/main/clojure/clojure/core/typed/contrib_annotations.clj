(ns clojure.core.typed.contrib-annotations
  (:require [clojure.core.typed :as t :refer [ann-protocol ann]]))

(ann-protocol clojure.java.io/IOFactory 
              make-reader
              [clojure.java.io/IOFactory '{:append t/Any, :encoding (t/U nil String)} -> java.io.BufferedReader]

              make-writer 
              [clojure.java.io/IOFactory '{:append t/Any, :encoding (t/U nil String)} -> java.io.BufferedWriter]

              make-input-stream 
              [clojure.java.io/IOFactory '{:append t/Any, :encoding (t/U nil String)} -> java.io.BufferedInputStream]

              make-output-stream
              [clojure.java.io/IOFactory '{:append t/Any, :encoding (t/U nil String)} -> java.io.BufferedOutputStream])

(ann ^:no-check clojure.java.io/reader
     [clojure.java.io/IOFactory -> java.io.BufferedReader])
(ann ^:no-check clojure.java.io/writer
     [clojure.java.io/IOFactory -> java.io.BufferedWriter])

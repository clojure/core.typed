;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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

;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.deps.clojure.tools.reader.impl.ExceptionInfo
  (:gen-class :extends java.lang.RuntimeException
              :init init
              :constructors {[String clojure.lang.IPersistentMap] [String]
                             [String clojure.lang.IPersistentMap Throwable] [String Throwable]}
              :state data
              :methods [[getData [] clojure.lang.IPersistentMap]]))

(defn -init
  ([s data]
     [[s] data])
  ([s data throwable]
     [[s throwable] data]))

(defn -getData [^clojure.core.typed.deps.clojure.tools.reader.impl.ExceptionInfo this]
  (.data this))

(defn -toString [^clojure.core.typed.deps.clojure.tools.reader.impl.ExceptionInfo this]
  (str "clojure.tools.reader.ExceptionInfo: " (.getMessage this) " " (str (.data this))))

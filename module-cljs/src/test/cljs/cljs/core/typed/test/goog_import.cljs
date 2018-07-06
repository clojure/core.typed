(ns cljs.core.typed.test.goog-import
  (:require [cljs.core.typed :as t])
  (:import [goog.events EventType]))

(t/ann et [EventType -> EventType])
(defn et [e]
  e)

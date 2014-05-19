(ns clojure.core.typed.nilsafe-utils
  (:require [clojure.set :as set]))

(def set-union (fnil set/union #{}))
(def set-difference (fnil set/difference #{}))

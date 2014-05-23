(ns clojure.core.typed.reflect-utils
  (:require [clojure.reflect :as reflect]
            [clojure.string :as str])
  (:import (clojure.lang RT)))

(defn reflect
  [obj & options]
  (apply reflect/type-reflect (if (class? obj) obj (class obj))
         :reflector (reflect/->JavaReflector (RT/baseLoader))
         options))

(defn reflect-friendly-sym [cls]
  (-> (reflect/typename cls)
      (str/replace "[]" "<>")
      symbol))

(defn pprint-reflection-sym [cls]
  (-> (reflect/typename cls)
      (str/replace "<>" "[]")
      symbol))

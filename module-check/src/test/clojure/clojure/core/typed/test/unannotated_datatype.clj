(ns clojure.core.typed.test.unannotated-datatype
  (:require [clojure.core.typed :refer [ann tc-ignore check-ns]]))

(tc-ignore (deftype NoAnnotate [a]))

(ann test-fn [-> NoAnnotate])
(defn test-fn []
  (NoAnnotate. 1))

(.a (NoAnnotate. 1))

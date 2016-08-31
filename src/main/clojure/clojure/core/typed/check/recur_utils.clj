(ns ^:skip-wiki clojure.core.typed.check.recur-utils
  (:require [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-rep :as r]))

(alter-meta! *ns* assoc :skip-wiki true)

(u/def-type RecurTarget [dom rest drest kws]
  "A target for recur"
  [(every? r/Type? dom)
   ((some-fn nil? r/Type?) rest)
   ((some-fn nil? r/DottedPretype?) drest)
   (nil? kws)]) ;TODO

(defmacro ^:private set-validator-doc! [var val-fn]
  `(set-validator! ~var (fn [a#] (assert (~val-fn a#)
                                         (str "Invalid reference state: " ~var
                                              " with value: "
                                              (pr-str a#)))
                          true)))

(defonce ^:dynamic *recur-target* nil)
(set-validator-doc! #'*recur-target* (some-fn nil? RecurTarget?))

(defmacro with-recur-target [tgt & body]
  `(binding [*recur-target* ~tgt]
     ~@body))

(defonce ^:dynamic *loop-bnd-anns* nil)
(set-validator! #'*loop-bnd-anns* #(or (nil? %)
                                       (every? r/Type? %)))

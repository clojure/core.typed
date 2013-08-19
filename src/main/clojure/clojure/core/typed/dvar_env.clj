(ns clojure.core.typed.dvar-env
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed :as t])
  (:import [clojure.lang Symbol]
           [clojure.core.typed.type_rep F]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotted Variable Environment

;symbol -> F
(t/ann *dotted-scope* (t/Map Symbol F))
(def ^:dynamic *dotted-scope* {})
(set-validator! #'*dotted-scope* (u/hash-c? symbol? r/F?))

(t/ann bound-index? [Any -> Any])
(defn bound-index? [n]
  (contains? *dotted-scope* n))

(defmacro with-dotted [dvars & body]
  `(with-dotted-mappings (into {} (for [v# ~dvars]
                                    [(:name v#) v#]))
     ~@body))

(defmacro with-dotted-mappings [dvar-map & body]
  `(binding [*dotted-scope* (merge *dotted-scope* ~dvar-map)]
     ~@body))

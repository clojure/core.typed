(ns clojure.core.typed.util-vars)

(defonce ^:dynamic *current-env* nil)
(defonce ^:dynamic *current-expr* nil)

(defonce ^:dynamic 
  ^{:doc 
  "If true, print fully qualified types in error messages
  and return values. Bind around a type checking form like 
  cf or check-ns.
  
  eg. 
  (binding [*verbose-types* true] 
    (cf 1 Number))
  ;=> java.lang.Number"}
  *verbose-types* 
  nil)

(defonce ^:dynamic 
  ^{:doc 
  "If true, print complete forms in error messages. Bind
  around a type checking form like cf or check-ns.
  
  eg.
  (binding [*verbose-forms* true]
    (cf ['deep ['deep ['deep ['deep]]]] Number))
  ;=> <full form in error>"}
  *verbose-forms* 
  nil)

(defonce ^:dynamic
  *trace-checker*
  nil)

(def ^:skip-wiki ^:dynamic *currently-loading* false)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *currently-checking-clj* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *already-collected* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *already-checked* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *delayed-errors* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *analyze-ns-cache* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *checked-asts* nil)

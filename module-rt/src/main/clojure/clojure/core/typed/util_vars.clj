(ns clojure.core.typed.util-vars)

(defonce ^:skip-wiki ^:dynamic *current-env* nil)
(defonce ^:skip-wiki ^:dynamic *current-expr* nil)
(defonce ^:skip-wiki ^:dynamic *in-check-form* nil)

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
  ^{:doc "If true, print tracing information during type checking."}
  *trace-checker*
  nil)

(def ^:skip-wiki ^:dynamic *currently-loading* false)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *already-collected* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *already-checked* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *delayed-errors* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *analyze-ns-cache* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *checked-asts* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *lexical-env* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *can-rewrite* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *in-typed-load* nil)
;; keep track of state throughout a `load`
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *typed-load-atom* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *prepare-infer-ns* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *instrument-infer-config* nil)

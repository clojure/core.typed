(ns clojure.core.typed.compiler
  (:refer-clojure :exclude [*warn-on-reflection*])
  )

;; don't use the actual WARN_ON_REFLECTION because core.typed might be able
;; to resolve the reflection.
(def ^:dynamic *warn-on-reflection* false)
;; symbol->localbinding
(def ^:dynamic *local-env* nil)
;; vector<localbinding>
(def ^:dynamic *loop-locals*)
;; Label
(def ^:dynamic *loop-label*)
;; vector<object>
(def ^:dynamic *constants*)
;; IdentityHashMap
(def ^:dynamic *constant-ids*)
;; vector<keyword>
(def ^:dynamic *keyword-callsites*)
;; vector<var>
(def ^:dynamic *protocol-callsites*)
;; set<var>
(def ^:dynamic *var-callsites*)
;; keyword->constid
(def ^:dynamic *keywords*)
;; var->constid
(def ^:dynamic *vars*)
;; FnFrame
(def ^:dynamic *method* nil)
;; null or not
(def ^:dynamic *in-catch-finally* nil)
(def ^:dynamic *no-recur* nil)
;; DynamicClassLoader
(def ^:dynamic *loader*)

;; Integer
(def ^:dynamic *line* 0)
(def ^:dynamic *column* 0)
(def ^:dynamic *line-before* 0)
(def ^:dynamic *column-before* 0)
(def ^:dynamic *line-after* 0)
(def ^:dynamic *column-after* 0)

;; Integer
(def ^:dynamic *next-local-num* 0)
;; Integer
(def ^:dynamic *ret-local-num*)

(def ^:dynamic *compile-stub-sym* nil)
(def ^:dynamic *compile-stub-class* nil)

;; PathNode chain
(def ^:dynamic *clear-path* nil)
;; tail of PathNode chain
(def ^:dynamic *clear-root* nil)
;; LocalBinding -> Set<LocalBindingExpr>
(def ^:dynamic *clear-sites* nil)

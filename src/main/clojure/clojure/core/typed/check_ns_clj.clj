(ns clojure.core.typed.check-ns-clj
  (:require [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.check-ns-common :as chk-ns]
            [clojure.core.typed.collect-phase :as collect-clj]
            [clojure.core.typed.check :as chk-clj]))

; cache of namespaces that should be skipped in check-ns
(def already-checked (atom #{}))

(defn check-ns-info
  "Same as check-ns, but returns a map of results from type checking the
  namespace.

  Options
  - :collect-only    Don't type check the given namespace/s, but collect the 
                     top level type annotations like ann, ann-record.
  - :type-provided?  If true, use the expected type to check the form
  - :profile         Use Timbre to profile the type checker. Timbre must be
                     added as a dependency.
  - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
                     of (Map '{:line Int :column Int :file Str} Str).
  - :already-checked an atom of a set of namespace symbols that should be skipped
                     on the next check-ns. By default, is a global cache that remembers
                     what we have already checked the last run.
  - :clean           if true, resets the type environment and recheck all namespace
                     dependencies
  "
  [ns-or-syms & opt] 
  (apply chk-ns/check-ns-info impl/clojure ns-or-syms 
         :already-checked already-checked
         :collect-ns collect-clj/collect-ns
         :check-ns chk-clj/check-ns-and-deps
         opt))

(defn check-ns
  [ns-or-syms & opt]
  (apply chk-ns/check-ns impl/clojure ns-or-syms 
         :already-checked already-checked
         :collect-ns collect-clj/collect-ns
         :check-ns chk-clj/check-ns-and-deps
         opt))

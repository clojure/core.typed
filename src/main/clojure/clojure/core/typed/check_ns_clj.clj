(ns clojure.core.typed.check-ns-clj
  (:require [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.check-ns-common :as chk-ns]))

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
                     of (Map '{:line Int :column Int :file Str} Str)."
  [ns-or-syms & opt] 
  (apply chk-ns/check-ns-info impl/clojure ns-or-syms opt))

(defn check-ns
  [ns-or-syms & opt]
  (apply chk-ns/check-ns impl/clojure ns-or-syms opt))

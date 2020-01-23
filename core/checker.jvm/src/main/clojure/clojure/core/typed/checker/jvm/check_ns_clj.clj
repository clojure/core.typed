;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.check-ns-clj
  (:require [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.check-ns-common :as chk-ns]))

(defn check-ns-info
  "Same as check-ns, but returns a map of results from type checking the
  namespace.

  Options
  - :collect-only    Don't type check the given namespace/s, but collect the 
                     top level type annotations like ann, ann-record.
  - :type-provided?  If true, use the expected type to check the form
  - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
                     of (Map '{:line Int :column Int :file Str} Str).
  
  Deprecated:
  - :profile         Use Timbre to profile the type checker. Timbre must be
                     added as a dependency."
  [ns-or-syms opt] 
  (chk-ns/check-ns-info impl/clojure ns-or-syms opt))

(defn check-ns
  [ns-or-syms opt]
  (chk-ns/check-ns impl/clojure ns-or-syms opt))

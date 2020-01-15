;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.runtime-check
  "Adds runtime checks where annotations are instead of type checking"
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.check.do :as do]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.check.special.ann-form :as ann-form]
            [clojure.core.typed.checker.check.def :as def]
            [clojure.core.typed.ast-utils :as ast]))

(defn check
  "Add runtime checks to the output AST, propagating just enough types
  for immediate ann-form expressions to propagate to fn expected types.

  Static checking is disabled, outside ill-formed types.
  
  Unsafe contracts can be generated, and contract generation cannot fail.
  
  Assumes collect-expr is already called on this AST."
  ([expr] (check expr nil))
  ([expr expected]
   (case (:op expr)
     (:def) (if (def/defmacro-or-declare? expr)
              ;; ignore defmacro and declare
              expr
              (def/add-checks-normal-def check expr expected))
     (:do) (letfn [(default-do [expr expected]
                     (assoc expr
                            :statements (mapv check (:statements expr))
                            :ret (check (:ret expr) expected)))]

             (if (do/internal-form? expr)
               (case (u/internal-dispatch-val expr)
                 (::t/ann-form) 
                 (ann-form/add-checks-ann-form check expr expected)

                 (::t/tc-ignore) 
                 expr

                 ;; could be an error or another special form, 
                 ;; but we'll let it slide in runtime checking mode.
                 expr)
               (default-do expr expected)))
     (ast/walk-children check expr))))

(def runtime-check-expr check)

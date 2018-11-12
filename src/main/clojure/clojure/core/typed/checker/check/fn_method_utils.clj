;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.check.fn-method-utils
  (:require [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.abo :as abo]))

(alter-meta! *ns* assoc :skip-wiki true)

(defonce ^:dynamic *check-fn-method1-checkfn* nil)
; [(U nil Type) (U nil DottedPretype) -> Type]
; takes the current rest or drest argument (only one is non-nil) and returns
; the type to assign the rest parameter
(defonce ^:dynamic *check-fn-method1-rest-type* nil)

;lam-result in TR
(u/def-type FnResult [args kws rest drest prest pdot body]
  "Results of checking a fn method"
  [(every? symbol? (map first args))
   (every? r/Type? (map second args))
   ((some-fn nil? (con/hvector-c? symbol? r/KwArgs?)) kws)
   ((some-fn nil? (con/hvector-c? symbol? r/Type?)) rest)
   ((some-fn nil? (con/hvector-c? symbol? r/Type?)) prest)
   ((some-fn nil? (con/hvector-c? symbol? r/DottedPretype?)) drest)
   ((some-fn nil? (con/hvector-c? symbol? r/DottedPretype?)) pdot)
   (r/TCResult? body)])

;[FnResult -> Function]
(defn FnResult->Function [{:keys [args kws rest drest prest pdot body] :as fres}]
  {:pre [(FnResult? fres)]
   :post [(r/Function? %)]}
  (let [; names of formal parameters to abstract from result type
        rest-param-name (or (first rest)
                            (first drest)
                            (first prest)
                            (first kws)
                            (first pdot))
        arg-names (concat (map first args)
                          (when rest-param-name
                            [rest-param-name]))]
    (r/Function-maker
      (map second args)
      (abo/abstract-result body arg-names)
      (when rest
        (second rest))
      (when drest
        (second drest))
      (when kws
        (second kws))
      (when prest
        (second prest))
      (when pdot
        (second pdot)))))

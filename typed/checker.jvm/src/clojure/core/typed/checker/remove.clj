;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.remove
  (:require [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.type-rep :as r]))

;; also not yet correct
;; produces old without the contents of rem
;[Type Type -> Type]
(defn remove* [old rem]
  (let [old (c/fully-resolve-type old)
        rem (c/fully-resolve-type rem)
        initial (if (sub/subtype? old rem)
                  (c/Un) ;the empty type
                  (cond
                    ;FIXME TR also tests for App? here. ie (or (r/Name? old) (App? old))
                    (r/Name? old) ;; must be different, since they're not subtypes 
                    ;; and n must refer to a distinct struct type
                    old
                    (r/Union? old) (let [l (:types old)]
                                   (apply c/Un (map (fn [e] (remove* e rem)) l)))
                    (r/Mu? old) (remove* (c/unfold old) rem)
                    (r/Poly? old) (let [vs (c/Poly-fresh-symbols* old)
                                        b (c/Poly-body* vs old)]
                                    (c/Poly* vs 
                                             (c/Poly-bbnds* vs old)
                                             (remove* b rem)))
                    :else old))]
    (if (sub/subtype? old initial) old initial)))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.seq-ops
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed :as t]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.errors :as err]
            ))

(defn type-to-seq [t]
  {:pre [(r/Type? t)]}
  (cond
    (r/Union? t) (apply c/Un (map type-to-seq (:types t)))
    (r/Intersection? t) (apply c/In (map type-to-seq (:types t)))
    (r/HSequential? t) (if (seq (:types t))
                         t
                         (c/Un r/-nil t))
    ;TODO (sub/subtype? t (prs/parse-type `(t/U nil t/Seqable t/Any))) 
    :else (err/int-error (str "Cannot create seq from " t))))

(defn cons-types [a d]
  {:pre [(r/Type? a)
         (r/Type? d)]}
  (assert nil "TODO")
  )

(defn concat-types [& ts]
  {:pre [(every? r/Type? ts)]}
  (assert nil "TODO")
  )

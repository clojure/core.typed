;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.analyzer.jvm.utils
  (:require [clojure.tools.analyzer.jvm.utils :as ju]
            [clojure.tools.analyzer.utils :as u]
            [clojure.core.typed.analyzer :as ana2]
            [clojure.tools.analyzer.env :as env1]))

;; ensure `taj-utils/maybe-class-literal` does not use ta-env/*env*,
;; instead falls back to jvm-specific implementation.
;; probably not portable to cljs?
(defn maybe-class-literal [x]
  (binding [env1/*env* nil]
    (ju/maybe-class-literal x)))

; copied from clojure.tools.analyzer.jvm.utils
;- use resolve-sym
(defn macro? [sym env]
  (when-let [v (ana2/resolve-sym sym env)]
    (and (not (-> env :locals (get sym)))
         (u/macro? v)
         v)))

; copied from clojure.tools.analyzer.jvm.utils
(defn inline? [sym args env]
  (when-let [v (ana2/resolve-sym sym env)]
    (let [inline-arities-f (:inline-arities (meta v))]
      (and (not (-> env :locals (get sym)))
           (or (not inline-arities-f)
               (inline-arities-f (count args)))
           (:inline (meta v))))))

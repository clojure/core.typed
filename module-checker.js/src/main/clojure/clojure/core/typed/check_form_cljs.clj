;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.check-form-cljs
  (:require [clojure.core.typed.checker.check-form-common :as chk-form] ;;TODO use check-form-common2
            [clojure.core.typed.analyze-cljs :as ana-cljs]
            [clojure.core.typed.check-cljs :as chk-cljs]
            [clojure.core.typed.util-cljs :as ucljs]
            [cljs.env :as env]
            [cljs.compiler :as comp]
            [clojure.core.typed.current-impl :as impl]))

(defn config-map []
  {:impl impl/clojurescript 
   :unparse-ns (ucljs/cljs-ns)
   :ast-for-form ana-cljs/ast-for-form
   :check-expr chk-cljs/check-expr})

(defn check-form-info
  [form & opt]
  (let [config (config-map)]
    (impl/with-full-impl (:impl config)
      (apply chk-form/check-form-info config
             form opt))))

(defn check-form-cljs
  "Check a single form with an optional expected type.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see cf."
  [form expected expected-provided?]
  (ucljs/with-cljs-typed-env
    (comp/with-core-cljs
      nil
      #(let [config (config-map)]
         (impl/with-full-impl (:impl config)
           (chk-form/check-form* config
              form expected expected-provided?))))))

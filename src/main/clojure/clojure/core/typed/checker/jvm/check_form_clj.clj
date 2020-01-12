;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.jvm.check-form-clj
  (:require [clojure.core.typed.checker.check-form-common :as chk-form]
            [clojure.core.typed.checker.check-form-common2 :as chk-form2]
            [clojure.core.typed.checker.jvm.analyze-clj :as ana-clj]
            [clojure.core.typed.checker.jvm.check :as chk-clj]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.checker.runtime-check :as rt-chk]
            [clojure.core.typed.current-impl :as impl]))

(def ^:private runtime-infer-expr
  (delay (impl/dynaload 'clojure.core.typed.runtime-infer/runtime-infer-expr)))

(def version 2)

(defn config-map []
  {:impl impl/clojure
   :ast-for-form ana-clj/ast-for-form
   :unparse-ns (ns-name *ns*)
   :check-expr chk-clj/check-expr
   :runtime-check-expr rt-chk/runtime-check-expr
   :runtime-infer-expr (fn [& args]
                         (apply @runtime-infer-expr args))
   :eval-out-ast (fn eval-out-ast
                   ([ast] (eval-out-ast ast {}))
                   ([ast opts] (ana-clj/eval-ast ast opts)))
   :custom-expansions? (-> *ns*
                           meta
                           :core.typed
                           :experimental
                           (contains? :custom-expansions))
   :emit-form ast-u/emit-form-fn
   :analyze-bindings-fn ana-clj/thread-bindings
   :check-form-info chk-form/check-form-info
   :check-form* chk-form/check-form*
   })

(defn config-map2 []
  {:impl impl/clojure
   :check-top-level chk-clj/check-top-level
   :unparse-ns (ns-name *ns*)
   :runtime-check-expr rt-chk/runtime-check-expr
   :runtime-infer-expr (fn [& args]
                         (apply @runtime-infer-expr args))
   :eval-out-ast (fn eval-out-ast
                   ([ast] (eval-out-ast ast {}))
                   ([ast opts] (ana-clj/eval-ast ast opts)))
   :custom-expansions? (-> *ns*
                           meta
                           :core.typed
                           :experimental
                           (contains? :custom-expansions))
   :emit-form ast-u/emit-form-fn
   :check-form-info chk-form2/check-form-info
   :check-form* chk-form2/check-form*
   })

(defn check-form-info
  [form & opt]
  (let [config (case (int version)
                 1 (config-map)
                 2 (config-map2))]
    (impl/with-full-impl (:impl config)
      (apply (:check-form-info config) config
             form opt))))

(defn check-form-info-with-config
  [form config opt]
  {:pre [(map? config)
         (not (map? opt))]}
  (impl/with-full-impl (:impl config)
    (apply (:check-form-info config) config
           form opt)))

(defn check-form*
  [form expected type-provided? opt]
  {:pre [(map? opt)]}
  (let [config (case (int version)
                 1 (config-map)
                 2 (config-map2))]
    (impl/with-full-impl (:impl config)
      ((:check-form* config) config
        form expected type-provided? opt))))

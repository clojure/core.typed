(ns ^:skip-wiki clojure.core.typed.check-form-clj
  (:require [clojure.core.typed.check-form-common :as chk-form]
            [clojure.core.typed.analyze-clj :as ana-clj]
            [clojure.core.typed.check :as chk-clj]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.runtime-check :as rt-chk]
            [clojure.core.typed.current-impl :as impl]))

(defn config-map []
  {:impl impl/clojure
   :ast-for-form ana-clj/ast-for-form
   :unparse-ns (ns-name *ns*)
   :collect-expr (fn [_] nil)
   :check-expr chk-clj/check-expr
   :runtime-check-expr rt-chk/runtime-check-expr
   :runtime-infer-expr (fn [& args]
                         (require 'clojure.core.typed.runtime-infer)
                         (apply (impl/v 'clojure.core.typed.runtime-infer/runtime-infer-expr) args))
   :eval-out-ast (fn eval-out-ast
                   ([ast] (eval-out-ast ast {}))
                   ([ast opts] (ana-clj/eval-ast ast opts)))
   :custom-expansions? ana-clj/custom-expansions?
   :emit-form ast-u/emit-form-fn})

(defn check-form-info
  [form & opt]
  (let [config (config-map)]
    (impl/with-full-impl (:impl config)
      (apply chk-form/check-form-info config
             form opt))))

(defn check-form*
  [form expected type-provided?]
  (let [config (config-map)]
    (impl/with-full-impl (:impl config)
      (chk-form/check-form* config
        form expected type-provided?))))

(ns ^:skip-wiki clojure.core.typed.check-form-clj
  (:require [clojure.core.typed.check-form-common :as chk-form]
            [clojure.core.typed.analyze-clj :as ana-clj]
            [clojure.core.typed.check :as chk-clj]
            [clojure.core.typed.collect-phase :as collect-clj]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.core.typed.runtime-check :as rt-chk]
            [clojure.core.typed.runtime-infer :as rt-infer]
            [clojure.core.typed.current-impl :as impl]))

(defn config-map []
  {:impl impl/clojure
   :ast-for-form ana-clj/ast-for-form
   :unparse-ns *ns*
   :collect-expr (fn [_] nil) #_collect-clj/collect-ast
   :check-expr chk-clj/check-expr
   :runtime-check-expr rt-chk/runtime-check-expr
   :runtime-infer-expr rt-infer/runtime-infer-expr
   :eval-out-ast (partial ana-clj/eval-ast {})
   :emit-form emit-form/emit-form})

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

(ns ^:skip-wiki clojure.core.typed.check-form-clj
  (:require [clojure.core.typed.check-form-common :as chk-form]
            [clojure.core.typed.check-form-common2 :as chk-form2]
            [clojure.core.typed.analyze-clj :as ana-clj]
            [clojure.core.typed.check :as chk-clj]
            [clojure.core.typed.check2 :as chk-clj2]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.runtime-check :as rt-chk]
            [clojure.core.typed.current-impl :as impl]))

(def version 2)

(defn config-map []
  {:impl impl/clojure
   :ast-for-form ana-clj/ast-for-form
   :unparse-ns (ns-name *ns*)
   :check-expr chk-clj/check-expr
   :runtime-check-expr rt-chk/runtime-check-expr
   :runtime-infer-expr (fn [& args]
                         (require 'clojure.core.typed.runtime-infer)
                         (apply (impl/v 'clojure.core.typed.runtime-infer/runtime-infer-expr) args))
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
                         (require 'clojure.core.typed.runtime-infer)
                         (apply (impl/v 'clojure.core.typed.runtime-infer/runtime-infer-expr) args))
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
  {:pre [(map? config)]}
  (impl/with-full-impl (:impl config)
    (apply (:check-form-info config) config
           form opt)))

(defn check-form*
  [form expected type-provided?]
  (let [config (case (int version)
                 1 (config-map)
                 2 (config-map2))]
    (impl/with-full-impl (:impl config)
      ((:check-form* config) config
        form expected type-provided?))))

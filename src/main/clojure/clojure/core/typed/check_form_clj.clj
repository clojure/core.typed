(ns clojure.core.typed.check-form-clj
  (:require [clojure.core.typed.check-form-common :as chk-form]
            [clojure.core.typed.analyze-clj :as ana-clj]
            [clojure.core.typed.check :as chk-clj]
            [clojure.core.typed.collect-phase :as collect-clj]
            [clojure.core.typed.current-impl :as impl])) 

(def config 
  {:impl impl/clojure
   :ast-for-form ana-clj/ast-for-form
   :unparse-ns *ns*
   :collect-expr collect-clj/collect-ast
   :check-expr chk-clj/check-expr})

(defn check-form-info
  [form & opt]
  (apply chk-form/check-form-info config
         form opt))

(defn check-form*
  [form expected type-provided?]
  (chk-form/check-form* config
    form expected type-provided?))

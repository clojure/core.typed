(ns clojure.core.typed.check-form-cljs
  (:require [clojure.core.typed.check-form-common :as chk-form]
            [clojure.core.typed.analyze-cljs :as ana-cljs]
            [clojure.core.typed.collect-cljs :as collect-cljs]
            [clojure.core.typed.check-cljs :as chk-cljs]
            [clojure.core.typed.util-cljs :as ucljs]
            [cljs.env :as env]
            [cljs.compiler :as comp]
            [clojure.core.typed.current-impl :as impl]))

(defn check-form-info
  [form & opt]
  (apply chk-form/check-form-info 
         {:impl impl/clojurescript 
          :unparse-ns (ucljs/cljs-ns)
          :ast-for-form ana-cljs/ast-for-form
          :collect-expr collect-cljs/collect
          :check-expr chk-cljs/check}
         form opt))

(defn check-form-cljs
  "Check a single form with an optional expected type.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see cf."
  [form expected expected-provided?]
  (env/ensure
    (comp/with-core-cljs
      (chk-form/check-form*
         {:impl impl/clojurescript 
          :unparse-ns (ucljs/cljs-ns)
          :ast-for-form ana-cljs/ast-for-form
          :collect-expr collect-cljs/collect
          :check-expr chk-cljs/check}
         form expected expected-provided?))))

(ns ^:skip-wiki clojure.core.typed.check.utils
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.path-rep :as pe]
            [clojure.core.typed.util-vars :as vs]))

;(t/ann expr-ns [Any -> t/Sym])
(defn expr-ns [expr]
  {:post [(symbol? %)]}
  (impl/impl-case
    :clojure (let [nsym (get-in expr [:env :ns])
                   _ (assert (symbol? nsym) (str "Bug! " (:op expr) " expr has no associated namespace"
                                                 nsym))]
               (ns-name nsym))
    :cljs (or (-> expr :env :ns :name)
              (do (prn "WARNING: No associated ns for ClojureScript expr, defaulting to cljs.user")
                  'cljs.user))))

(defn KeyPE->Type [k]
  {:pre [(pe/KeyPE? k)]
   :post [(r/Type? %)]}
  (r/-val (:val k)))

(defn fn-self-name [{:keys [op] :as fexpr}]
  (impl/impl-case
    :clojure (do (assert (#{:fn} op))
                 (-> fexpr :local :name))
    :cljs (do (assert (#{:fn} op))
              (-> fexpr :name :name))))



(ns ^:skip-wiki clojure.core.typed.coerce-utils
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.typed.current-impl :as impl])
  (:import (clojure.lang RT Var)))

;(t/ann symbol->Class [Symbol -> Class])
(defn symbol->Class 
  "Returns the Class represented by the symbol. Works for
  primitives (eg. byte, int). Does not further resolve the symbol."
  [sym]
  {:pre [(symbol? sym)]
   :post [(class? %)]}
  (case sym
    byte Byte/TYPE
    short Short/TYPE
    int Integer/TYPE
    long Long/TYPE
    float Float/TYPE
    double Double/TYPE
    boolean Boolean/TYPE
    char Character/TYPE
    (RT/classForName (str sym))))

;(t/ann Class->symbol [Class -> Symbol])
(defn Class->symbol [^Class cls]
  {:pre [(class? cls)]
   :post [(symbol? %)]}
  (symbol (.getName cls)))

;(t/ann ^:no-check var->symbol [(Var Nothing Any) -> Symbol])
(defn var->symbol [^Var var]
  {:pre [(var? var)]
   :post [(symbol? %)
          (namespace %)]}
  (let [ns (.ns var)
        _ (assert ns)]
    (symbol (str (ns-name ns))
            (str (.sym var)))))

(defn ns->file 
  ([nsym] (ns->file nsym true))
  ([nsym suffix?]
   {:pre [(symbol? nsym)]
    :post [(string? %)]}
   ;copied basic approach from tools.emitter.jvm
   (let [res (munge nsym)
         f (str/replace res #"\." "/")
         ex (when suffix?
              (impl/impl-case
                :clojure ".clj"
                :cljs ".cljs"))
         p (str f ex)
         p (if (or (io/resource p)
                   (not suffix?))
             p
             (str f ".cljc"))
         p (if (.startsWith p "/") (subs p 1) p)]
     p)))

(defn ns->URL [nsym]
  {:pre [(symbol? nsym)]
   :post [((some-fn #(instance? java.net.URL %)
                    nil?) 
           %)]}
  (let [p (ns->file nsym)]
    (io/resource p)))

(defn sym->kw [sym]
  {:pre [(symbol? sym)]
   :post [(keyword? %)]}
  (keyword (namespace sym)
           (name sym)))

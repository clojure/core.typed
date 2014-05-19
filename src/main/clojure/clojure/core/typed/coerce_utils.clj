(ns clojure.core.typed.coerce-utils
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

(defn ctor-Class->symbol 
  "Returns a symbol representing this constructor's Class, removing any compiler stubs."
  [cls]
  (Class->symbol cls))

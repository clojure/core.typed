(ns clojure.core.typed.ns-deps-utils
  (:require [clojure.tools.namespace.parse :as ns-parse]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.profiling :as p]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.java.io :as io]))

(defn ns-form-for-file
  "Returns the namespace declaration for the file, or
  nil if not found"
  [file]
  (p/p :ns-deps-utils/ns-form-for-file
   (ns-file/read-file-ns-decl file)))

(defn ns-form-for-ns
  "Returns the namespace declaration for the namespace, or
  nil if not found. Throws an int-error if file cannot be
  found for namespace."
  [nsym]
  {:pre [(symbol? nsym)]}
  (let [f (-> nsym coerce/ns->file)
        res (io/resource f)
        _ (when-not res
            (err/int-error (str "File for " nsym " not found on classpath: " f)))]
    (ns-form-for-file res)))

(defn ns-form-deps
  "Given a ns-form, returns a set of dependencies"
  [ns-form]
  {:pre [ns-form]
   :post [((con/set-c? symbol?) %)]}
  (p/p :ns-deps-utils/ns-form-deps
    (ns-parse/deps-from-ns-decl ns-form)))

(defn deps-for-ns
  "Returns the dependencies for a namespace"
  [nsym]
  {:pre [(symbol? nsym)]}
  (let [ns-form (ns-form-for-ns nsym)
        _ (when-not ns-form
            (err/int-error (str "No ns form for " nsym)))]
    (ns-form-deps ns-form)))

(defn requires-tc? 
  "Returns true if the ns-form refers to clojure.core.typed"
  [ns-form]
  {:pre [ns-form]
   :post [(con/boolean? %)]}
  (let [deps (ns-parse/deps-from-ns-decl ns-form)]
    (contains? deps 'clojure.core.typed)))

(defn ns-form-name
  "Returns the symbol naming this namespace, with any
  metadata attached."
  [ns-form]
  {:post [(symbol? %)]}
  (let [nsym (second ns-form)
        _ (when-not (symbol? nsym)
            (err/int-error "Malformed ns form"))
        metamap (nth ns-form 2 nil)]
    (if (map? metamap)
      (vary-meta nsym merge metamap)
      nsym)))

(defn collect-only-ns?
  "Returns true if the ns-form has collect-only metadata."
  [ns-form]
  {:pre [ns-form]
   :post [(con/boolean? %)]}
  (let [nsym (ns-form-name ns-form)]
    (boolean (-> (meta nsym) :core.typed :collect-only))))

(defn should-check-ns? 
  "Returns true if the given namespace should be type checked"
  [nsym]
  {:pre [(symbol? nsym)]
   :post [(con/boolean? %)]}
  (p/p :ns-deps-utils/should-check-ns?
    (let [ns-form (ns-form-for-ns nsym)]
      (and ns-form
           (requires-tc? ns-form)
           (not (collect-only-ns? ns-form))))))

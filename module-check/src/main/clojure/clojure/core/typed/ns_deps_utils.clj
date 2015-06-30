(ns ^:skip-wiki clojure.core.typed.ns-deps-utils
  (:require [clojure.core.typed.deps.clojure.tools.namespace.parse :as ns-parse]
            [clojure.core.typed.ns-deps :as ns-deps]
            [clojure.core.typed.deps.clojure.tools.namespace.file :as ns-file]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.profiling :as p]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.java.io :as io]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn ns-form-for-file
  "Returns the namespace declaration for the file, or
  nil if not found"
  [file]
  (p/p :ns-deps-utils/ns-form-for-file
   (some-> (io/resource file)
           ns-file/read-file-ns-decl)))

(defn ns-form-for-ns
  "Returns the namespace declaration for the namespace, or
  nil if not found."
  [nsym]
  {:pre [(symbol? nsym)]}
  (let [f (-> nsym coerce/ns->file)
        ns (ns-form-for-file f)]
    (or ns
        (err/warn (str "File for " nsym " not found on classpath: " f)))))

(defn ns-form-deps
  "Given a ns-form, returns a set of dependencies"
  [ns-form]
  {:pre [ns-form]
   :post [((con/set-c? symbol?) %)]}
  (p/p :ns-deps-utils/ns-form-deps
   (let [ndeps (ns-parse/deps-from-ns-decl ns-form)]
     ;; tools.namespace can return nil here
     (set ndeps))))

(defn deps-for-ns
  "Returns the dependencies for a namespace"
  [nsym]
  {:pre [(symbol? nsym)]
   :post [(set? %)]}
  (if-let [ns-form (ns-form-for-ns nsym)]
    (ns-form-deps ns-form)
    #{}))

(defn requires-tc?
  "Returns true if the ns-form refers to clojure.core.typed"
  [ns-form]
  {:pre [ns-form]
   :post [(con/boolean? %)]}
  (if-let [deps (ns-parse/deps-from-ns-decl ns-form)]
    (contains? deps (impl/impl-case
                      :clojure 'clojure.core.typed
                      :cljs 'cljs.core.typed))
    false))

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

(defn should-check-ns-form?
  [ns-form]
  {:post [(con/boolean? %)]}
  (and ns-form
       (requires-tc? ns-form)
       (not (collect-only-ns? ns-form))))

(defn should-check-ns?
  "Returns true if the given namespace should be type checked"
  [nsym]
  {:pre [(symbol? nsym)]
   :post [(con/boolean? %)]}
  (p/p :ns-deps-utils/should-check-ns?
       (if-let [ns-form (ns-form-for-ns nsym)]
         (and ns-form
              (requires-tc? ns-form)
              (not (collect-only-ns? ns-form)))
         false)))

(defn ns-has-core-typed-metadata?
  "Returns true if the given ns form has :core.typed metadata."
  [rcode]
  {:post [(con/boolean? %)]}
  (-> (second rcode)
      meta :core.typed boolean))

(defn file-has-core-typed-metadata?
  "Returns true if the given file has :core.typed metadata."
  [res]
  {:pre [(string? res)]
   :post [(con/boolean? %)]}
  (if-let [ns-form (ns-form-for-file res)]
    (some-> ns-form ns-has-core-typed-metadata?)
    false))

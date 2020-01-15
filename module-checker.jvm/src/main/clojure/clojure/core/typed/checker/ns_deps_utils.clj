;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.ns-deps-utils
  (:require [clojure.tools.namespace.parse :as ns-parse]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.internal :as internal]
            [clojure.java.io :as io]))

(defn ns-form-for-file
  "Returns the namespace declaration for the file, or
  nil if not found"
  [file]
  (some-> (io/resource file)
          (ns-file/read-file-ns-decl
            (impl/impl-case
              :clojure ns-parse/clj-read-opts
              :cljs ns-parse/cljs-read-opts))))

(defn ns-form-for-ns
  "Returns the namespace declaration for the namespace, or
  nil if not found."
  [nsym]
  {:pre [(symbol? nsym)]}
  (let [f (-> nsym coerce/ns->file)
        ns (ns-form-for-file f)]
    (or ns
        (err/int-error (str "File for " nsym " not found on classpath: " f)))))

(defn ns-form-deps
  "Given a ns-form, returns a set of dependencies"
  [ns-form]
  {:pre [ns-form]
   :post [((con/set-c? symbol?) %)]}
  (let [ndeps (ns-parse/deps-from-ns-decl ns-form)]
    ;; tools.namespace can return nil here
    (set ndeps)))

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
   :post [(boolean? %)]}
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
  (let [ns-form (next ns-form)
        [nsym ns-form] (internal/take-when symbol? ns-form)
        _ (when-not (symbol? nsym)
            (err/int-error "Malformed ns form"))
        [docstr ns-form]  (internal/take-when string? ns-form)
        [metamap ns-form] (internal/take-when map? ns-form)]
    (if (map? metamap)
      (vary-meta nsym merge metamap)
      nsym)))

(defn ns-meta
  "Returns the metadata map for this namespace"
  [ns-form]
  (meta (ns-form-name ns-form)))

(defn collect-only-ns?
  "Returns true if the ns-form has collect-only metadata."
  [ns-form]
  {:pre [ns-form]
   :post [(boolean? %)]}
  (boolean (-> (ns-meta ns-form) :core.typed :collect-only)))

(defn should-check-ns-form?
  [ns-form]
  {:post [(boolean? %)]}
  (and (boolean ns-form)
       (requires-tc? ns-form)
       (not (collect-only-ns? ns-form))))

(defn should-custom-expand-ns-form?
  [ns-form]
  {:post [(boolean? %)]}
  (let [m (ns-meta ns-form)]
    (-> m :core.typed :experimental
        (contains? :custom-expansions))))

(defn should-check-ns?
  "Returns true if the given namespace should be type checked"
  [nsym]
  {:pre [(symbol? nsym)]
   :post [(boolean? %)]}
  (should-check-ns-form? (ns-form-for-ns nsym)))

(defn ns-has-core-typed-metadata?
  "Returns true if the given ns form has :core.typed metadata."
  [rcode]
  {:post [(boolean? %)]}
  (boolean (-> (ns-meta rcode) :core.typed)))

(defn should-use-typed-load?
  "Returns true if typed load should be triggered for this namespace."
  [ns-form]
  {:post [(boolean? %)]}
  (let [m (ns-meta ns-form)]
    (and (= :core.typed (:lang m))
         (not (-> m :core.typed :no-typed-load)))))

(defn file-has-core-typed-metadata?
  "Returns true if the given file has :core.typed metadata."
  [res]
  {:pre [(string? res)]
   :post [(boolean? %)]}
  (if-let [ns-form (ns-form-for-file res)]
    (boolean (some-> ns-form ns-has-core-typed-metadata?))
    false))

(defn file-should-use-typed-load?
  "Returns true if the given file should be loaded with typed load."
  [res]
  {:pre [(string? res)]
   :post [(boolean? %)]}
  (if-let [ns-form (ns-form-for-file res)]
    (boolean (some-> ns-form should-use-typed-load?))
    false))

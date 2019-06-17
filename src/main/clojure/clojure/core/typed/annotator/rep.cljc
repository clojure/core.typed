;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.annotator.rep
  "Intermediate representation for types"
  ; Note: clojure.core.typed.annotator.util depends on this ns
  )

;;========================
;;Type Predicates
;;========================

(defn type? [t]
  (and (map? t)
       (keyword? (:op t))))

(defn alias? [t]
  (= :alias (:op t)))

(defn HMap? [t]
  (= :HMap (:op t)))

(defn HVec? [t]
  (= :HVec (:op t)))

(defn union? [t]
  (= :union (:op t)))

(defn Any? [m]
  {:pre [(map? m)]
   :post [(boolean? %)]}
  (= :Top (:op m)))

(defn unknown? [m]
  (= :unknown
     (:op m)))

(defn nothing? [t]
  (boolean
    (when (union? t)
      (empty? (:types t)))))

(def val? (comp boolean #{:val} :op))

;;========================
;;Type Constructors
;;========================

(defn -class? [m]
  (boolean (#{:class} (:op m))))

(defn -alias [name]
  {:pre [(symbol? name)]}
  {:op :alias
   :name name})

(def -any {:op :Top})

(def -nothing {:op :union :types #{}})

(defn -val [v]
  {:op :val
   :val v})

(defn -class [cls args]
  {:pre [(vector? args)
         (every? type? args)]}
  (assert ((some-fn keyword? string?) cls) cls)
  {:op :class
   :clojure.core.typed.annotator.rep/class-instance cls
   :args args})

(defn make-HMap [req opt]
  {:op :HMap
   :clojure.core.typed.annotator.rep/HMap-req req
   :clojure.core.typed.annotator.rep/HMap-opt opt})

;;========================
;;   Inference results
;;========================

(defn infer-result [path type]
  {:op :path-type
   :type type
   :path path})

(defn infer-results [paths type]
  (map #(infer-result % type) paths))

;; ========================
;;     Path elements
;; ========================

(defn key-path 
  ([keys key] (key-path {} keys key))
  ([kw-entries keys key]
   {:pre [(keyword? key)]}
   {:op :key
    ;; (Map Kw (ValType Kw)) for constant keyword entries
    :kw-entries kw-entries
    :keys keys
    :key key}))

(defn map-keys-path []
  {:op :map-keys})

(defn map-vals-path []
  {:op :map-vals})

;; for zero arity, use (fn-dom-path 0 -1)
(defn fn-dom-path [arity pos]
  (assert (< pos arity)
          (str "Arity: " arity
               "Position:" pos))
  {:op :fn-domain
   :arity arity :position pos})

(defn fn-rng-path [arity]
  {:op :fn-range
   :arity arity})

(defn seq-entry []
  {:op :seq-entry})

(defn transient-vector-entry []
  {:op :transient-vector-entry})

(defn index-path [count nth]
  {:op :index
   :count count
   :nth nth})

(defn vec-entry-path []
  {:op :vec-entry})

(defn set-entry []
  {:op :set-entry})

(defn atom-contents []
  {:op :atom-contents})

(defn var-path 
  ([name] (var-path nil name))
  ([ns name]
   {:pre [((some-fn symbol? nil?) ns)
          (symbol? name)]}
   {:op :var
    :ns ns
    :name name}))

(ns clojure.core.typed.annotator.util
  (:require [clojure.set :as set]
            #?@(:clj [[clojure.core.typed.coerce-utils :as coerce]])
            ))

#?(:clj
(defn ^:private try-resolve-nsyms [nsyms]
  (reduce (fn [_ s]
            (try
              (require [s])
              (reduced s)
              (catch #?(:clj Throwable :cljs :default) e
                nil)))
          nil
          nsyms)))
(def spec-ns'
  #?(:clj (try-resolve-nsyms '[clojure.spec clojure.spec.alpha])
     :cljs 'clojure.spec.alpha))
(def core-specs-ns'
  #?(:clj (try-resolve-nsyms '[clojure.core.specs clojure.core.specs.alpha])
     :cljs 'clojure.core.specs.alpha))

(def spec-ns (or spec-ns' 'clojure.spec.alpha))
(def core-specs-ns (or core-specs-ns' 'clojure.core.specs.alpha))

(def ^:dynamic unparse-type nil)

(def ^:dynamic *ann-for-ns* 
  (fn [] #?(:clj *ns*
            :cljs (throw (ex-info "No annotation namespace bound" {})))))

(defn current-ns []
  #?(:clj (ns-name (*ann-for-ns*))
     :cljs (*ann-for-ns*)))

#?(:clj
(defn namespace-alias-in [ns maybe-aliased-ns]
  {:pre [((some-fn nil? #(instance? clojure.lang.Namespace %)) maybe-aliased-ns)]
   :post [((some-fn nil? symbol) %)]}
  (get (set/map-invert (ns-aliases ns)) maybe-aliased-ns)))

(def ^:dynamic *verbose-specs* nil)

#?(:clj 
(defn qualify-symbol-in [nsym s]
  {:pre [(symbol? nsym)
         (symbol? s)
         (not (namespace s))]
   :post [(symbol? %)]}
  (let [ns (find-ns nsym)
        talias (namespace-alias-in (the-ns (current-ns)) ns)
        already-referred? (let [actual (let [v (ns-resolve (the-ns (current-ns)) s)]
                                         (when (var? v)
                                           (coerce/var->symbol v)))
                                desired (symbol (name nsym)
                                                (name s))]
                            ;(prn actual desired)
                            (= actual desired))]
    (symbol (if *verbose-specs*
              (str nsym)
              (when-not already-referred?
                (or (when talias
                      (str talias))
                    (when ns
                      (str (ns-name ns)))
                    (name nsym))))
            (str s))))
:cljs
(defn qualify-symbol-in [nsym s]
  {:pre [(symbol? nsym)
         (symbol? s)
         (not (namespace s))]
   :post [(symbol? %)]}
  ;TODO
  (symbol (get {'clojure.core.typed "t"
                'clojure.spec.alpha "s"
                'clojure.core nil}
               nsym
               (str nsym))
          (str s))))

(defn qualify-spec-symbol [s]
  {:pre [(symbol? s)]
   :post [(symbol? %)]}
  (qualify-symbol-in spec-ns s))

(defn qualify-typed-symbol [s]
  {:pre [(symbol? s)]
   :post [(symbol? %)]}
  (qualify-symbol-in 'clojure.core.typed s))

(defn qualify-core-symbol [s]
  {:pre [(symbol? s)]
   :post [(symbol? %)]}
  (qualify-symbol-in 'clojure.core s))

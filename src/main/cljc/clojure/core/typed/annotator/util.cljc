(ns clojure.core.typed.annotator.util
  (:require [clojure.set :as set]
            [clojure.string :as str]
            #?@(:clj [[clojure.core.typed.coerce-utils :as coerce]])
            ))

(def ^:dynamic *debug* nil)
(def ^:dynamic *debug-depth* 0)

#?(:clj
(defn current-time [] (. System (nanoTime))))
#?(:cljs
(defn current-time [] (.getTime (js/Date.))))


;; https://github.com/r0man/inflections-clj/blob/master/src/inflections/core.cljc
(defn str-name
  "Same as `clojure.core/name`, but keeps the namespace for keywords
  and symbols."
  [x]
  (cond
    (nil? x)
    x
    (string? x)
    x
    (or (keyword? x)
        (symbol? x))
    (if-let [ns (namespace x)]
      (str ns "/" (name x))
      (name x))))
(defn coerce
  "Coerce the string `s` to the type of `obj`."
  [obj s]
  (cond
    (keyword? obj)
    (keyword s)
    (symbol? obj)
    (symbol s)
    :else s))
(defn camel-case
  "Convert `word` to camel case. By default, camel-case converts to
  UpperCamelCase. If the argument to camel-case is set to :lower then
  camel-case produces lowerCamelCase. The camel-case fn will also
  convert \"/\" to \"::\" which is useful for converting paths to
  namespaces.
  Examples:
    (camel-case \"active_record\")
    ;=> \"ActiveRecord\"
    (camel-case \"active_record\" :lower)
    ;=> \"activeRecord\"
    (camel-case \"active_record/errors\")
    ;=> \"ActiveRecord::Errors\"
    (camel-case \"active_record/errors\" :lower)
    ;=> \"activeRecord::Errors\""
  [word & [mode]]
  (when word
    (->> (let [word (str-name word)]
           (cond
             (= mode :lower) (camel-case word str/lower-case)
             (= mode :upper) (camel-case word str/upper-case)
             (fn? mode) (str (mode (str (first word)))
                             (apply str (rest (camel-case word nil))))
             :else (-> (str/replace word #"/(.?)" #(str "::" (str/upper-case (nth % 1))))
                       (str/replace #"(^|_|-)(.)"
                                #(str (if (#{\_ \-} (nth % 1))
                                        (nth % 1))
                                      (str/upper-case (nth % 2)))))))
         (coerce word))))

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

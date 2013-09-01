(ns clojure.core.typed.current-impl
  (:require [clojure.core.typed.profiling :as p]))

(defn v [vsym]
  {:pre [(symbol? vsym)
         (namespace vsym)]}
  (let [ns (find-ns (symbol (namespace vsym)))
        _ (assert ns (str "Cannot find namespace: " (namespace vsym)))
        var (ns-resolve ns (symbol (name vsym)))]
    (assert (var? var) (str "Cannot find var: " vsym))
    @var))

(defn the-var [vsym]
  {:pre [(symbol? vsym)
         (namespace vsym)]
   :post [(var? %)]}
  (let [ns (find-ns (symbol (namespace vsym)))
        _ (assert ns (str "Cannot find namespace: " (namespace vsym)))
        var (ns-resolve ns (symbol (name vsym)))]
    (assert (var? var) (str "Cannot find var: " vsym))
    var))

(def clojure ::clojure)
(def clojurescript ::clojurescript)

(def any-impl ::any-impl)

(derive clojure any-impl)
(derive clojurescript any-impl)

(defonce ^:dynamic *current-impl* nil)
(set-validator! #'*current-impl* (some-fn nil? keyword?))

(defmacro with-impl [impl & body]
  `(do (assert ((some-fn #{~impl} nil?) *current-impl*) 
               (str "Cannot overlay different core.typed implementations: " (pr-str *current-impl*)
                    ", expected " (pr-str ~impl)))
     (binding [*current-impl* ~impl]
       ~@body)))

(defmacro with-clojure-impl [& body]
  `(with-impl clojure
     (clojure.core.typed.profiling/p :current-impl/push-thread-bindings
     (push-thread-bindings {(the-var '~'clojure.core.typed.name-env/*current-name-env*)
                             (v '~'clojure.core.typed.name-env/CLJ-TYPE-NAME-ENV)
                            (the-var '~'clojure.core.typed.protocol-env/*current-protocol-env*)
                             (v '~'clojure.core.typed.protocol-env/CLJ-PROTOCOL-ENV)
                            (the-var '~'clojure.core.typed.ns-deps/*current-deps*)
                             (v '~'clojure.core.typed.ns-deps/CLJ-TYPED-DEPS)
                            ; var env
                            (the-var '~'clojure.core.typed.var-env/*current-var-annotations*)
                             (v '~'clojure.core.typed.var-env/CLJ-VAR-ANNOTATIONS)
                            (the-var '~'clojure.core.typed.var-env/*current-nocheck-var?*)
                             (v '~'clojure.core.typed.var-env/CLJ-NOCHECK-VAR?)
                            (the-var '~'clojure.core.typed.var-env/*current-used-vars*)
                             (v '~'clojure.core.typed.var-env/CLJ-USED-VARS)
                            (the-var '~'clojure.core.typed.var-env/*current-checked-var-defs*)
                             (v '~'clojure.core.typed.var-env/CLJ-CHECKED-VAR-DEFS) 

                            (the-var '~'clojure.core.typed.declared-kind-env/*current-declared-kinds*)
                             (v '~'clojure.core.typed.declared-kind-env/CLJ-DECLARED-KIND-ENV) 
                            (the-var '~'clojure.core.typed.datatype-env/*current-datatype-env*)
                             (v '~'clojure.core.typed.datatype-env/CLJ-DATATYPE-ENV) 
                            (the-var '~'clojure.core.typed.datatype-ancestor-env/*current-dt-ancestors*)
                             (v '~'clojure.core.typed.datatype-ancestor-env/CLJ-DT-ANCESTOR-ENV) 
                            }))
     (try 
       ~@body
       (finally (clojure.core.typed.profiling/p :current-impl/pop-thread-bindings 
                     (pop-thread-bindings))))))

(defmacro with-cljs-impl [& body]
  `(with-impl clojurescript
     (push-thread-bindings {(the-var '~'clojure.core.typed.name-env/*current-name-env*)
                             (v '~'clojure.core.typed.name-env/CLJS-TYPE-NAME-ENV)
                            (the-var '~'clojure.core.typed.protocol-env/*current-protocol-env*)
                             (v '~'clojure.core.typed.protocol-env/CLJS-PROTOCOL-ENV)
                            (the-var '~'clojure.core.typed.ns-deps/*current-deps*)
                             (v '~'clojure.core.typed.ns-deps/CLJS-TYPED-DEPS)
                            ; var env
                            (the-var '~'clojure.core.typed.var-env/*current-var-annotations*)
                             (v '~'clojure.core.typed.var-env/CLJS-VAR-ANNOTATIONS)
                            (the-var '~'clojure.core.typed.var-env/*current-nocheck-var?*)
                             (v '~'clojure.core.typed.var-env/CLJS-NOCHECK-VAR?)
                            (the-var '~'clojure.core.typed.var-env/*current-used-vars*)
                             (v '~'clojure.core.typed.var-env/CLJS-USED-VARS)
                            (the-var '~'clojure.core.typed.var-env/*current-checked-var-defs*)
                             (v '~'clojure.core.typed.var-env/CLJS-CHECKED-VAR-DEFS) 

                            (the-var '~'clojure.core.typed.declared-kind-env/*current-declared-kinds*)
                             (v '~'clojure.core.typed.declared-kind-env/CLJS-DECLARED-KIND-ENV) 
                            (the-var '~'clojure.core.typed.datatype-env/*current-datatype-env*)
                             (v '~'clojure.core.typed.datatype-env/CLJS-DATATYPE-ENV) 
                            })
     (try 
       ~@body
       (finally (pop-thread-bindings)))))

(defn implementation-specified? []
  (boolean *current-impl*))

(defn ensure-impl-specified []
  (assert (implementation-specified?) "No implementation specified"))

(defn current-impl []
  (ensure-impl-specified)
  *current-impl*)

(defn checking-clojure? []
  (ensure-impl-specified)
  (= clojure *current-impl*))

(defn checking-clojurescript? []
  (ensure-impl-specified)
  (= clojurescript *current-impl*))

(defn assert-clojure 
  ([] (assert-clojure nil))
  ([msg] (assert (= clojure *current-impl*) (str "Clojure implementation only"
                                                 (when (seq msg)
                                                   (str ": " msg))))))

(defn assert-cljs []
  (assert (= clojurescript *current-impl*) "Clojurescript implementation only"))

(defmacro impl-case [& {:keys [clojure cljs] :as opts}]
  (assert (= #{:clojure :cljs} (set (keys opts)))
          "Incorrect cases to impl-case")
  `(condp = (current-impl)
     clojure ~clojure
     clojurescript ~cljs
     (assert nil "No case matched for impl-case")))

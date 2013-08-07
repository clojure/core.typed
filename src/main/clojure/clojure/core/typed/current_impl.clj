(ns clojure.core.typed.current-impl)

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

(def ^:dynamic *current-impl* nil)
(set-validator! #'*current-impl* (some-fn nil? keyword?))

(defmacro with-impl [impl & body]
  `(do (assert ((some-fn #{~impl} nil?) *current-impl*) "Cannot overlay different core.typed implementations")
     (binding [*current-impl* ~impl]
       ~@body)))

(defmacro with-clojure-impl [& body]
  `(with-impl clojure
     (push-thread-bindings {(the-var '~'clojure.core.typed.name-env/*current-name-env*)
                             (v '~'clojure.core.typed.name-env/CLJ-TYPE-NAME-ENV)
                            (the-var '~'clojure.core.typed.protocol-env/*current-protocol-env*)
                             (v '~'clojure.core.typed.protocol-env/CLJ-PROTOCOL-ENV)})
     (try 
       ~@body
       (finally (pop-thread-bindings)))))

(defmacro with-cljs-impl [& body]
  `(with-impl clojurescript
     (clojure.core.typed.name-env/with-cljs-name-env
       ~@body)))

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

(defn assert-clojure []
  (assert (= clojure *current-impl*) "Clojure implementation only"))

(defn assert-cljs []
  (assert (= clojurescript *current-impl*) "Clojurescript implementation only"))

(defmacro impl-case [& {:keys [clojure cljs] :as opts}]
  (assert (= #{:clojure :cljs} (set (keys opts)))
          "Incorrect cases to impl-case")
  `(condp = (current-impl)
     clojure ~clojure
     clojurescript ~cljs
     (assert nil "No case matched for impl-case")))

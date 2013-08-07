(ns clojure.core.typed.current-impl)
; FIXME these should be dynamic vars

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
     ~@body))

(defmacro with-cljs-impl [& body]
  `(with-impl clojurescript
     ~@body))

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

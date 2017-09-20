;; copied from https://github.com/ztellman/riddley/blob/master/src/riddley/compiler.clj
(ns clojure.core.typed.dep.riddley.compiler
  (:import
    [clojure.lang
     Var
     Compiler
     Compiler$ObjMethod
     Compiler$ObjExpr]
    [clojure.core.typed.dep.riddley
     Util]))

(defn- stub-method []
  (proxy [Compiler$ObjMethod] [(Compiler$ObjExpr. nil) nil]))

(defn tag-of
  "Returns a symbol representing the tagged class of the symbol, or `nil` if none exists."
  [x]
  (when-let [tag (-> x meta :tag)]
    (let [sym (symbol
                (if (instance? Class tag)
                  (.getName ^Class tag)
                  (name tag)))]
      (when-not (= 'java.lang.Object sym)
        sym))))

(let [n (atom 0)]
  (defn- local-id []
    (swap! n inc)))

(defn locals
  "Returns the local binding map, equivalent to the value of `&env`."
  []
  (when (.isBound Compiler/LOCAL_ENV)
    @Compiler/LOCAL_ENV))

(defmacro with-base-env [& body]
  `(binding [*warn-on-reflection* false]
     (with-bindings (if (locals)
                      {}
                      {Compiler/LOCAL_ENV {}})
       ~@body)))

(defmacro with-lexical-scoping
  "Defines a lexical scope where new locals may be registered."
  [& body]
  `(with-bindings {Compiler/LOCAL_ENV (locals)}
     ~@body))

(defmacro with-stub-vars [& body]
  `(with-bindings {Compiler/CLEAR_SITES nil
                   Compiler/METHOD (stub-method)}
     ~@body))

;; if we don't do this in Java, the checkcasts emitted by Clojure cause an
;; IllegalAccessError on Compiler$Expr.  Whee.
(defn register-local
  "Registers a locally bound variable `v`, which is being set to form `x`."
  [v x]
  (with-stub-vars
    (.set ^Var Compiler/LOCAL_ENV

      ;; we want to allow metadata on the symbols to persist, so remove old symbols first
      (-> (locals)
        (dissoc v)
        (assoc v (try
                   (Util/localBinding (local-id) v (tag-of v) x)
                   (catch Exception _
                     ::analyze-failure)))))))

(defn register-arg
  "Registers a function argument `x`."
  [x]
  (with-stub-vars
    (.set ^Var Compiler/LOCAL_ENV
      (-> (locals)
        (dissoc x)
        (assoc x (Util/localArgument (local-id) x (tag-of x)))))))




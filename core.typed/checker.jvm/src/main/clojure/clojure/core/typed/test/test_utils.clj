(ns clojure.core.typed.test.test-utils
  {:core.typed {:collect-only true}}
  (:require [clojure.core.typed :as t]
            [clojure.set :as set]
            [clojure.test :as test :refer [is]]
            [clojure.core.typed.lang.jvm :as lang]
            [clojure.core.typed.load :as load]
            [clojure.core.typed.test.common-utils :as common-test]))

(when-not *compile-files*
  (t/load-if-needed))

(require '[clojure.core.typed.checker.utils :as u]
         '[clojure.core.typed.errors :as err]
         '[clojure.core.typed.checker.jvm.parse-unparse :refer [parse-type]]
         '[clojure.core.typed.current-impl :as impl]
         '[clojure.core.typed.checker.type-ctors :as c]
         '[clojure.core.typed.checker.type-rep :as r]
         '[clojure.core.typed.checker.jvm.check :as chk]
         '[clojure.core.typed.checker.jvm.subtype :as sub])


(defn check-opt [opt]
  #_(assert (empty? (set/difference (set (keys opt))
                                  #{:expected :ret}))))

(defn tc-common* [frm {{:keys [syn provided?]} :expected-syntax :keys [expected-ret requires ns-meta check-config] :as opt}]
  (check-opt opt)
  (let [ns-form 
        `(ns ~(gensym 'clojure.core.typed.test.temp)
           ~@(when ns-meta [ns-meta])
           (:refer-clojure :exclude 
                           ~'[type defprotocol #_letfn fn loop dotimes let for doseq
                              #_def #_filter #_remove defn atom ref cast])
           (:require ~@'[[clojure.core.typed :refer :all :as t]
                         [clojure.core.typed.unsafe :as unsafe]
                         [clojure.core :as core]]
                     ~@requires))]
    `(let [expected-ret# ~expected-ret
           check-config# ~check-config]
       (binding [*ns* *ns*
                 *file* *file*]
         (let [{ex# :ex} (t/check-form-info '~ns-form
                                            :check-config check-config#)]
           (assert (nil? ex#) ex#))
         (t/check-form-info 
           '~frm
           :expected-ret expected-ret#
           :expected '~syn
           :type-provided? ~provided?
           :check-config check-config#)))))

(defmacro tc-e 
  "Type check an an expression in namespace that :refer's
  all of clojure.core.typed (aliased to t) and aliases clojure.core
  to core.

  Takes one form and then options, and returns true if the form checks
  with the expected input/output types according to the provided options.
  
  The first form in the options can be a static type syntax scoped
  in the new namespace. This is disambiguated with a call to keyword?
  (literal keywords aren't valid type syntax).
  
  eg. (tc-e (+ 1 1) Num)
      ;=> Num

  Keyword Options:

    :expected-ret An expected ret, evaluated in the current namespace (not the new
                  one that refers c.c.t). Cannot be provided in combination with the implicit
                  first option as a type, as above.
    :ret          Check the return TCResult of this expression against this ret. Evaluated
                  in the current namespace.
    :ns-meta      Map to use as ns metadata.
    :check-config Map specifying check-config."
  [frm & opts]
  (apply common-test/tc-e tc-common* frm opts))

(comment
  (tc-e 1)
  (tc-e 1 Num)
  (tc-e 1 Num :ret )
  (tc-err (fn [] (inc 'a)))
  (tc-err (fn [] (inc 1)))
  (tc-err (fn [] (inc 1)) Num)
  )

(defmacro tc-err [frm & opts]
  (apply common-test/tc-err tc-common* frm opts))

(defmacro is-tc-e [& body]
  `(test/is (do (tc-e ~@body)
                true)))

(defmacro is-tc-err [& body]
  `(test/is (tc-err ~@body)))

(defmacro throws-tc-error? [& body]
  `(err/with-ex-info-handlers
     [err/tc-error? (constantly true)]
     ~@body
     false))

(defmacro sub? [s t]
  `(impl/with-clojure-impl
     (subtype? (parse-type '~s)
               (parse-type '~t))))

(defmacro sub?-q [s t]
  `(impl/with-clojure-impl
     (subtype? (parse-type ~s)
               (parse-type ~t))))

(defn subtype? [& rs]
  (impl/with-clojure-impl
    (apply sub/subtype? rs)))

(defn both-subtype? [s t]
  (and (not= r/-error s)
       (not= r/-error t)
       (subtype? s t)
       (subtype? t s)))

(defmacro both-sub? [s t]
  `(both-subtype? (parse-type '~s)
                  (parse-type '~t)))

(defn check [& as]
  (impl/with-clojure-impl
    (apply chk/check-expr as)))

(defmacro is-cf [& args]
  `(is (do
         (t/cf ~@args)
         true)))

(defmacro is-clj [& args]
  `(is (clj ~@args)))

(defmacro cljs [& body]
  `(impl/with-cljs-impl ~@body))

(defmacro clj [& body]
  `(impl/with-clojure-impl ~@body))

;return ret for an expression f
(defmacro eret [f]
  `(let [ret# (-> ~(tc-common* f {}) :ret)]
     (assert (r/TCResult? ret#))
     ret#))

;return type for an expression f
(defmacro ety [f]
  `(-> (eret ~f) r/ret-t))

(defmacro caught-top-level-errors [nfn & body]
  `(err/with-ex-info-handlers
     [err/top-level-error? (fn [data# _#]
                           (~nfn (count (:errors data#))))]
     ~@body
     false))

(defmacro equal-types-noparse [l r]
  `(clj (is (let [l# (ety ~l)
                  r# ~r]
              (or (both-subtype? l# r#)
                  (do (println "Actual" l#)
                      (println "Expected" r#)
                      (println "In" (quote ~l))
                      nil))))))

(defmacro equal-types [l r]
  `(equal-types-noparse ~l (binding [*ns* (find-ns '~'clojure.core.typed)] (parse-type (quote ~r)))))

(defmacro tc-t [form]
  `(let [{ex# :ex ret# :ret}
         ~(tc-common* form {})]
     (if ex#
       (throw ex#)
       ret#)))

(defmacro tc [form]
  `(impl/with-clojure-impl
     (t/check-form* '~form)))

;; from clojure.test-helper
(defmacro with-err-string-writer
  "Evaluate with err pointing to a temporary StringWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)]
     (binding [*err* s#]
       ~@body
       (str s#))))

;; from clojure.test-helper
(defmacro with-err-print-writer
  "Evaluate with err pointing to a temporary PrintWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)
         p# (java.io.PrintWriter. s#)]
     (binding [*err* p#]
       ~@body
       (str s#))))

;; from clojure.test-helper
(defmacro should-not-reflect
  "Turn on all warning flags, and test that reflection does not occur
   (as identified by messages to *err*)."
  [form]
  `(binding [*warn-on-reflection* true]
     (is (nil? (re-find #"^Reflection warning" (with-err-string-writer ~form))))
     (is (nil? (re-find #"^Reflection warning" (with-err-print-writer ~form))))))

(defmacro with-typed-load [& body]
  `(do
     (load/install-typed-load)
     (with-redefs [load #'lang/extensible-load]
     ~@body)))

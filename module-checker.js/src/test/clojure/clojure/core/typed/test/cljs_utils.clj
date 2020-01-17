(ns clojure.core.typed.test.cljs-utils
  (:require [clojure.core.typed :as clj-t]
            [cljs.core.typed :as cljs-t]
            [cljs.repl.rhino :as rhino]
            [cljs.repl :as repl]
            [cljs.analyzer :as ana]
            [clojure.core.typed.analyze-cljs :as ana-cljs]
            [clojure.core.typed.errors :as err]
            [clojure.set :as set]))

(cljs-t/load-if-needed)

(require '[clojure.test :refer :all :as test]
         '[cljs.core.typed :as t]
         '[clojure.core.typed.type-ctors :as c]
         '[clojure.core.typed.type-rep :as r]
         '[clojure.core.typed.current-impl :as impl]
         '[clojure.core.typed.parse-unparse :as prs]
         '[clojure.core.typed.subtype :as sub]
         '[clojure.core.typed.util-cljs :as ucljs]
         '[clojure.core.typed.coerce-utils :as coerce]
         '[clojure.core.typed.test.common-utils :as common-test]
         '[clojure.core.typed.check-form-cljs :as chk-frm-cljs]
         '[cljs.env :as env])

(defmacro cljs [& body]
  `(impl/with-cljs-impl
     (ucljs/with-cljs-typed-env
       ~@body)))

(defmacro is-cljs [& body]
  `(is (cljs ~@body)))

(defmacro is-cf [& body]
  `(is-cljs (t/cf ~@body) true))

(defn check-opt [opt]
  #_(assert (empty? (set/difference (set (keys opt))
                                  #{:expected :ret}))))

(defn tc-common* [frm {{:keys [syn provided?]} :expected-syntax :keys [expected-ret] :as opt}]
  (let [nsym (gensym 'clojure.core.typed.test.temp)]
    (check-opt opt)
    `(binding [ana/*cljs-ns* ana/*cljs-ns*]
       (ucljs/with-cljs-typed-env
         (let [expected-ret# ~expected-ret
               ; first element of this list must be the symbol ns
               ns-form# '(~'ns ~nsym
                           ;~'(:refer-clojure :exclude [fn])
                           ~'(:require [cljs.core.typed :as t :include-macros true]
                                       [cljs.core :as core]))
               _# (ana/analyze (ana/empty-env) ns-form#)]
           (t/check-form-info 
             '~frm
             :expected-ret expected-ret#
             :expected '~syn
             :type-provided? ~provided?))))))

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
                  in the current namespace."
  [frm & opts]
  (apply common-test/tc-e tc-common* frm opts))

(defmacro is-tc-e [& body]
  `(test/is (do (tc-e ~@body)
                true)))

(defmacro is-tc-err [& body]
  `(test/is (tc-err ~@body)))

(defmacro tc-err [frm & opts]
  (apply common-test/tc-err tc-common* frm opts))

(defn subtype? [& rs]
  (impl/with-cljs-impl
    (sub/reset-subtype-cache)
    (apply sub/subtype? rs)))

(defmacro sub? [s t]
  `(impl/with-cljs-impl
     (sub/reset-subtype-cache)
     (subtype? (prs/parse-type '~s)
               (prs/parse-type '~t))))

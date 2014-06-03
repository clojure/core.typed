(ns clojure.core.typed.test.test-utils
  (:require [clojure.core.typed :as t]
            [clojure.set :as set]
            [clojure.test :as test :refer [is]]
            ))

(t/load-if-needed)

(require '[clojure.core.typed.utils :as u]
         '[clojure.core.typed.errors :as err]
         '[clojure.core.typed.parse-unparse :refer [parse-type]]
         '[clojure.core.typed.current-impl :as impl]
         '[clojure.core.typed.type-ctors :as c]
         '[clojure.core.typed.type-rep :as r]
         '[clojure.core.typed.check :as chk]
         '[clojure.core.typed.subtype :as sub])


(defn check-opt [opt]
  #_(assert (empty? (set/difference (set (keys opt))
                                  #{:expected :ret}))))

(defn tc-common* [frm {{:keys [syn provided?]} :expected-syntax :keys [expected-ret] :as opt}]
  (check-opt opt)
  `(let [expected-ret# ~expected-ret]
     (binding [*ns* *ns*
               *file* *file*]
       (ns ~(gensym 'clojure.core.typed.test.temp)
         ~'(:refer-clojure :exclude [type defprotocol #_letfn fn loop dotimes let for doseq
                                     #_def filter remove])
         ~'(:require [clojure.core.typed :refer :all :as t]
                     [clojure.core :as core]))
       (t/check-form-info 
         '~frm
         :expected-ret expected-ret#
         :expected '~syn
         :type-provided? ~provided?))))

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
                  one that refers c.c.t. Cannot be provided in combination with the implicit
                  first option as a type, as above.
    :ret          Check the return TCResult of this expression against this ret. Evaluated
                  in the current namespace."
  [frm & opts]
  (let [[opts t has-t?] (if (and opts (not (keyword? (first opts))))
                          [(rest opts) (first opts) true]
                          [opts])
        _ (assert (even? (count opts))
                  "Uneven arguments to tc-e")
        {:as opts} opts
        _ (assert (not (and has-t? (contains? opts :expected)))
                  "Can't provide both implicit expected type and :expected kw to tc-e")
        has-t? (or has-t? (contains? opts :expected))
        t (or t (:expected opts))
        has-ret? (contains? opts :ret)
        _ (assert (not (and has-t? (contains? opts :expected-ret)))
                  "Can't provide both expected type and expected ret")
        actual-ret (gensym 'ret)]
    `(let [{~actual-ret :ret delayed-errors# :delayed-errors} ~(tc-common* frm (assoc opts
                                                                               :expected-syntax {:provided? has-t?
                                                                                                 :syn t}))]
       (or (when (empty? delayed-errors#)
             ~(when has-ret?
                `(assert (= ~actual-ret ~(:ret opts))))
             ~actual-ret)
           (err/print-errors! delayed-errors#)))))

(comment
  (tc-e 1)
  (tc-e 1 Num)
  (tc-e 1 Num :ret )
  (tc-err (fn [] (inc 'a)))
  (tc-err (fn [] (inc 1)))
  (tc-err (fn [] (inc 1)) Num)
  )

(defmacro tc-err [frm & opts]
  (let [[opts t has-t?] (if (and opts (not (keyword? (first opts))))
                          [(rest opts) (first opts) true]
                          [opts])
        _ (assert (even? (count opts))
                  "Uneven arguments to tc-e")
        {:as opts} opts
        _ (assert (not (and has-t? (contains? opts :expected)))
                  "Can't provide both implicit expected type and :expected kw to tc-err")
        has-t? (or has-t? (contains? opts :expected))
        t (or t (:expected opts))
        has-ret? (contains? opts :ret)
        _ (assert (not (and has-t? (contains? opts :expected-ret)))
                  "Can't provide both expected type and expected ret")
        actual-ret (gensym 'ret)]
    `(err/with-ex-info-handlers
       [err/tc-error? (constantly true)]
       (let [{~actual-ret :ret delayed-errors# :delayed-errors} 
             ~(tc-common* frm 
                          (assoc 
                            opts
                            :expected-syntax {:provided? has-t?
                                              :syn t}))]
         ~(when has-ret?
            `(assert (= ~actual-ret ~(:ret opts))))
         (boolean
           (seq delayed-errors#))))))

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
  (and (subtype? s t)
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

(defmacro clj [& body]
  `(impl/with-clojure-impl ~@body))

;return ret for an expression f
(defmacro eret [f]
  `(let [ret# (-> (t/check-form-info '~f) :ret)]
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
  `(equal-types-noparse ~l (parse-type (quote ~r))))

(defmacro overlap-prs [s1 s2]
  `(clj
     (c/overlap (parse-type '~s1) (parse-type '~s2))))

(defmacro tc-t [form]
  `(let [{delayed-errors# :delayed-errors ret# :ret}
         (impl/with-clojure-impl
           (t/check-form-info '~form))]
     (if-let [errors# (seq delayed-errors#)]
       (err/print-errors! errors#)
       ret#)))

(defmacro tc [form]
  `(impl/with-clojure-impl
     (t/check-form* '~form)))


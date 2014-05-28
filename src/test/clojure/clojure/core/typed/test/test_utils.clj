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
  (assert (empty? (set/difference (set (keys opt))
                                  #{:expected :ret}))))

(defn tc-common* [frm flat-opt]
  (let [_ (assert (even? (count flat-opt))
                  "Uneven arguments to tc-e/tc-err")
        {:keys [expected] :as opt} flat-opt]
    (check-opt opt)
  `(binding [*ns* *ns*
             *file* *file*]
     (ns ~(gensym 'clojure.core.typed.test.temp)
       ~'(:refer-clojure :exclude [type defprotocol #_letfn fn loop dotimes let for doseq
                                   #_def filter remove])
       ~'(:require [clojure.core.typed :refer :all :as t]
                   [clojure.core :as core]))
     (t/check-form-info 
       '~frm
       ~@(when (contains? opt :expected)
           [:expected `'~expected
            :type-provided? true])))))

(defmacro tc-e [frm & opts]
  `(let [{ret# :ret delayed-errors# :delayed-errors} ~(tc-common* frm opts)]
     (or (when (empty? delayed-errors#)
           ret#)
         (err/print-errors! delayed-errors#))))

(defmacro tc-err [frm & opts]
  `(err/with-ex-info-handlers
     [err/tc-error? (constantly true)]
     (let [{delayed-errors# :delayed-errors} ~(tc-common* frm opts)]
       (boolean
         (seq delayed-errors#)))))

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


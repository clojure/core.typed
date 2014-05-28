(ns clojure.core.typed.test.cljs-utils
  (:require [clojure.core.typed :as clj-t]
            [clojure.set :as set]))

(clj-t/load-if-needed)

(require '[clojure.test :refer :all]
         '[cljs.core.typed :as t]
         '[clojure.core.typed.type-ctors :as c]
         '[clojure.core.typed.type-rep :as r]
         '[clojure.core.typed.current-impl :as impl]
         '[clojure.core.typed.parse-unparse :as prs]
         '[clojure.core.typed.subtype :as sub]
         '[clojure.core.typed.util-cljs :as ucljs]
         '[cljs.env :as env])

(def cljs-env (env/default-compiler-env))

(defmacro cljs [& body]
  `(impl/with-cljs-impl
     (env/with-compiler-env (or env/*compiler*
                                cljs-env)
       ~@body)))

(defmacro is-cljs [& body]
  `(is (cljs ~@body)))

(defmacro is-cf [& body]
  `(is-cljs (t/cf ~@body) true))

(defn check-opt [opt]
  (assert (empty? (set/difference (set (keys opt))
                                  #{:expected :ret}))))

#_(defn tc-common* [frm flat-opt]
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

#_(defmacro tc-e [frm & opts]
  `(let [{ret# :ret delayed-errors# :delayed-errors} ~(tc-common* frm opts)]
     (or (when (empty? delayed-errors#)
           ret#)
         (err/print-errors! delayed-errors#))))


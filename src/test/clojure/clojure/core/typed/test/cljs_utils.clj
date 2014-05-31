(ns clojure.core.typed.test.cljs-utils
  (:require [clojure.core.typed :as clj-t]
            [cljs.repl.rhino :as rhino]
            [cljs.repl :as repl]
            [cljs.analyzer :as ana]
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
        {:keys [expected] :as opt} flat-opt
        nsym (gensym 'clojure.core.typed.test.temp)]
    (check-opt opt)
  `(let [repl-env# (rhino/repl-env)
         _ (rhino/rhino-setup repl-env#)
         ns-form# '(ns ~nsym
                     ~'(:refer-clojure :exclude [fn])
                     ~'(:require [cljs.core.typed :refer :all :as t]
                                 [cljs.core :as core]))
         _ (repl/evaluate repl-env# (ana/empty-env) ns-form#)]
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


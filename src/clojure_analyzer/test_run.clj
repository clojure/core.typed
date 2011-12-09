(ns clojure-analyzer.test-run
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT))
  (:require [clojure.pprint :as pprint]
            [clojure-analyzer.compiler :as a]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defmacro with-core-clj
  "Ensure that clojure.core has been loaded."
  [& body]
  `(do (when-not (:defs (get @a/namespaces 'clojure.core))
         (doseq [[defsym# _#] (ns-publics 'clojure.core)]
           (swap! a/namespaces assoc-in ['clojure.core :defs defsym#] (symbol "clojure.core" (name defsym#)))))
       ~@body))

(defn analyze-namespace [nssym]
  (require nssym)
  (with-core-clj
    (binding [a/*cljs-ns* 'clojure.user]
      (let [ns (-> (ns-publics nssym) first second meta :file)
            strm (.getResourceAsStream (RT/baseLoader) ns)]
        (with-open [rdr (PushbackReader. (InputStreamReader. strm))]
          (loop [forms (a/forms-seq nil rdr)
                 ns-name nil
                 deps nil]
            (if (seq forms)
              (let [env {:ns (@a/namespaces a/*cljs-ns*) :context :statement :locals {}}
                    ast (a/analyze env (first forms))]
                (do (pprint/pprint ast)
                  (if (= (:op ast) :ns)
                    (recur (rest forms) (:name ast) (merge (:uses ast) (:requires ast)))
                    (recur (rest forms) ns-name deps))))
              {:ns (or ns-name 'clojure.user)
               :provides [ns-name]
               :requires (if (= ns-name 'clojure.core) (set (vals deps)) (conj (set (vals deps)) 'clojure.core))})))))))

(analyze-namespace 'clojure-analyzer.test)
;(analyze-namespace "src/clojure_analyzer/test2.clj" "")

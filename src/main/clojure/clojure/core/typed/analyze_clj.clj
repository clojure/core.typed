(ns ^:skip-wiki clojure.core.typed.analyze-clj
  (:require [clojure.tools.analyzer :as ta]
            [clojure.tools.analyzer.jvm :as taj]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.tools.reader :as tr]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed :as t]))

(alter-meta! *ns* assoc :skip-wiki true)

(defn analyze1 [form env]
  (taj/analyze+eval form env))

(defn ast-for-form-in-ns
  "Returns an AST node for the form 
  analyzed in the given namespace"
  [nsym form]
  (binding [*ns* (or (find-ns nsym)
                     *ns*)]
    (analyze1 form (taj/empty-env))))

(defn ast-for-form
  "Returns an AST node for the form"
  [form]
  (analyze1 form (taj/empty-env)))

(defn ast-for-ns 
  "Returns a vector of AST nodes contained
  in the given namespace symbol nsym"
  [nsym]
  {:pre [((some-fn symbol? #(instance? clojure.lang.Namespace %)) 
          nsym)]}
  (u/p :analyze/ast-for-ns
   (let [nsym (or (when (instance? clojure.lang.Namespace nsym)
                    (ns-name nsym))
                  ; don't call ns-name on symbols in case the namespace
                  ; doesn't exist yet
                  nsym)
         _ (assert (symbol? nsym))
         cache (when-let [cache t/*analyze-ns-cache*]
                 @cache)]
     (if (and cache (contains? cache nsym))
       (cache nsym)
       ;copied basic approach from tools.emitter.jvm
       (let [res (munge nsym)
             p    (str (str/replace res #"\." "/") ".clj")
             eof  (reify)
             p (if (.startsWith p "/") (subs p 1) p)
             pres (io/resource p)
             _ (assert pres (str "Cannot find file for " nsym ": " p))
             file (-> pres io/reader slurp)
             reader (readers/indexing-push-back-reader file)
             asts (binding [*ns* (or (find-ns nsym)
                                     *ns*)
                            *file* p]
                    (loop [asts []]
                      (let [form (tr/read reader false eof)]
                        (if (not= eof form)
                          (let [a (analyze1 form (taj/empty-env))]
                            (recur (conj asts a)))
                          asts))))]
         (when-let [cache t/*analyze-ns-cache*]
           (swap! cache assoc nsym asts))
         asts)))))

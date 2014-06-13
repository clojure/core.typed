(ns ^:skip-wiki clojure.core.typed.analyze-clj
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.tools.analyzer :as ta]
            [clojure.tools.analyzer.env :as ta-env]
            [clojure.tools.analyzer.jvm :as taj]
            [clojure.tools.analyzer.utils :as taj-utils]
            [clojure.tools.analyzer.passes.source-info :as source-info]
            [clojure.tools.analyzer.passes.cleanup :as cleanup]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.tools.reader :as tr]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.java.io :as io]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed :as T]
            [clojure.core.cache :as cache]
            [clojure.core :as core]))

(alter-meta! *ns* assoc :skip-wiki true)

(def typed-macros
  {#'clojure.core/ns 
   (fn [&form &env name & references]
     (let [process-reference
           (fn [[kname & args]]
             `(~(symbol "clojure.core" (clojure.core/name kname))
                        ~@(map #(list 'quote %) args)))
           docstring  (when (string? (first references)) (first references))
           references (if docstring (next references) references)
           name (if docstring
                  (vary-meta name assoc :doc docstring)
                  name)
           metadata   (when (map? (first references)) (first references))
           references (if metadata (next references) references)
           name (if metadata
                  (vary-meta name merge metadata)
                  name)
           gen-class-clause (first (filter #(= :gen-class (first %)) references))
           gen-class-call
           (when gen-class-clause
             (list* `gen-class :name (.replace (str name) \- \_) :impl-ns name :main true (next gen-class-clause)))
           references (remove #(= :gen-class (first %)) references)
           ;ns-effect (clojure.core/in-ns name)
           ]
       `(do
          ::T/special-collect
          ::core/ns
          {:form '~&form}
          (clojure.core/in-ns '~name)
          (with-loading-context
            ~@(when gen-class-call (list gen-class-call))
            ~@(when (and (not= name 'clojure.core) (not-any? #(= :refer-clojure (first %)) references))
                `((clojure.core/refer '~'clojure.core)))
            ~@(map process-reference references))
          (if (.equals '~name 'clojure.core) 
            nil
            (do (dosync (commute @#'clojure.core/*loaded-libs* (T/inst conj T/Symbol ~'Any) '~name)) nil)))))
   })

(defn macroexpand-1
  "If form represents a macro form or an inlineable function,
   returns its expansion, else returns form."
  [form env]
  ;(prn "macroexpand-1" form (meta form))
    (ta-env/ensure (taj/global-env)
    (if (seq? form)
      (let [[op & args] form]
        (if (taj/specials op)
          form
          (let [v (taj-utils/resolve-var op env)
                m (meta v)
                ;_ (prn "op" (meta op)  m)
                local? (-> env :locals (get op))
                macro? (and (not local?) (:macro m)) ;; locals shadow macros
                inline-arities-f (:inline-arities m)
                inline? (and (not local?)
                             (or (not inline-arities-f)
                                 (inline-arities-f (count args)))
                             (:inline m))
                t (:tag m)]
            (cond

             macro?
             (let [res (apply (typed-macros v v) form (:locals env) (rest form))] ; (m &form &env & args)
               (taj/update-ns-map!)
               (if (taj-utils/obj? res)
                 (vary-meta res merge (meta form))
                 res))

             inline?
             (let [res (apply inline? args)]
               (taj/update-ns-map!)
               (if (taj-utils/obj? res)
                 (vary-meta res merge
                            (and t {:tag t})
                            ; we want the top-most inlining op
                            {::inline-op op
                             ::inline-var v}
                            (meta form))
                 res))

             :else
             (taj/desugar-host-expr form env)))))
      (taj/desugar-host-expr form env))))

(defn analyze1 [form env]
  (taj/analyze+eval form env {:bindings {#'ta/macroexpand-1 macroexpand-1}}))

(defn ast-for-form-in-ns
  "Returns an AST node for the form 
  analyzed in the given namespace"
  [nsym form]
  (binding [*ns* (or (find-ns nsym)
                     *ns*)]
    (analyze1 form (taj/empty-env))))

(def reread-with-tr (comp tr/read readers/indexing-push-back-reader print-str))

(defn ast-for-str
  "Returns an AST node for the string, using tools.reader."
  [form-str]
  (analyze1 (-> form-str readers/indexing-push-back-reader tr/read) (taj/empty-env)))

(defn ast-for-form
  "Returns an AST node for the form"
  [form]
  (analyze1 form (taj/empty-env)))


(defn ast-for-file
  "Returns a vector of AST nodes contained
  in the given file"
  [p]
  {:pre [(string? p)]}
  (let [pres (io/resource p)
        _ (assert (instance? java.net.URL pres) (str "Cannot find file: " p))
        file (-> pres io/reader slurp)
        reader (readers/indexing-push-back-reader file 1 p)
        eof  (reify)
        asts (binding [*ns* *ns*
                       *file* p]
               (loop [asts []]
                 (let [form (tr/read reader false eof)]
                   (if (not= eof form)
                     (let [a (analyze1 form (taj/empty-env))]
                       (recur (conj asts a)))
                     asts))))]
    asts))

(defn ast-for-ns 
  "Returns a vector of AST nodes contained
  in the given namespace symbol nsym"
  [nsym]
  {:pre [((some-fn symbol? #(instance? clojure.lang.Namespace %)) 
          nsym)]
   :post [(vector? %)]}
  (u/p :analyze/ast-for-ns
   (let [nsym (or (when (instance? clojure.lang.Namespace nsym)
                    (ns-name nsym))
                  ; don't call ns-name on symbols in case the namespace
                  ; doesn't exist yet
                  nsym)
         _ (assert (symbol? nsym))
         cache vs/*analyze-ns-cache*]
     (if (and cache (cache/has? cache nsym))
       (-> cache
           (cache/hit nsym)
           (cache/lookup nsym))
       ;copied basic approach from tools.emitter.jvm
       (let [p (coerce/ns->file nsym)
             asts (ast-for-file p)]
         (when cache
           (cache/miss cache nsym asts))
         asts)))))

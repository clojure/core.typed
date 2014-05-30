(ns clojure.core.typed.check-ns
  (:require [clojure.core.typed.profiling :as p]
            [clojure.core.typed.reset-env :as reset-env]
            [clojure.core.typed.reset-caches :as reset-caches]
            [clojure.core.typed.collect-phase :as collect]
            [clojure.core.typed.check :as chk]
            [clojure.core.typed.file-mapping :as file-map]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.java.io :as io]
            [clojure.jvm.tools.analyzer :as jta]))

(defn check-ns-info
  "Same as check-ns, but returns a map of results from type checking the
  namespace.

  Options
  - :collect-only    Don't type check the given namespace/s, but collect the 
                     top level type annotations like ann, ann-record.
  - :type-provided?  If true, use the expected type to check the form
  - :profile         Use Timbre to profile the type checker. Timbre must be
                     added as a dependency.
  - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
                     of (Map '{:line Int :column Int :file Str} Str)."
  ([] (check-ns-info *ns*))
  ([ns-or-syms & {:keys [collect-only trace profile file-mapping]}]
   (p/profile-if profile
     (let [start (. System (nanoTime))]
       (reset-caches/reset-caches)
       (let [nsym-coll (map #(if (symbol? %)
                               ; namespace might not exist yet, so ns-name is not appropriate
                               ; to convert to symbol
                               %
                               (ns-name %))
                            (if ((some-fn symbol? #(instance? clojure.lang.Namespace %))
                                 ns-or-syms)
                              [ns-or-syms]
                              ns-or-syms))]
         (cond
           vs/*currently-checking-clj* (throw (Exception. "Found inner call to check-ns or cf"))

           :else
           (binding [vs/*currently-checking-clj* true
                     vs/*delayed-errors* (err/-init-delayed-errors)
                     vs/*already-collected* (atom #{})
                     vs/*already-checked* (atom #{})
                     vs/*trace-checker* trace
                     vs/*analyze-ns-cache* (atom {})
                     ; we only use this if we have exactly one namespace passed
                     vs/*checked-asts* (when (== 1 (count nsym-coll))
                                         (atom {}))]
             (let [terminal-error (atom nil)
                   typed-asts (atom {})]
               (impl/with-clojure-impl
                 (reset-env/reset-envs!)
                 ;(reset-caches)
                 ;; handle terminal type error
                 (try
                   ;-------------------------
                   ; Collect phase
                   ;-------------------------
                   (doseq [nsym nsym-coll]
                     (collect/collect-ns nsym))
                   (let [ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                         collected @vs/*already-collected*]
                     (println "Collected" (count collected) "namespaces in" ms "msecs")
                     (flush))

                   ;-------------------------
                   ; Check phase
                   ;-------------------------
                   (when-not collect-only
                     (doseq [nsym nsym-coll]
                       (chk/check-ns-and-deps nsym))
                     (let [vs (var-env/vars-with-unchecked-defs)]
                       (binding [*out* *err*]
                         (doseq [v vs]
                           (println "WARNING: Type Checker: Definition missing:" v 
                                    "\nHint: Use :no-check metadata with ann if this is an unchecked var")
                           (flush))))
                     (let [ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                           checked @vs/*already-checked*
                           nlines (p/p :typed/line-count
                                       (apply + (for [nsym checked]
                                                  (with-open [rdr (io/reader (jta/uri-for-ns nsym))]
                                                    (count (line-seq rdr))))))]
                       (println "Checked" (count checked) "namespaces (approx." nlines "lines) in" ms "msecs")
                       (flush)))
                   (catch clojure.lang.ExceptionInfo e
                     (if (-> e ex-data :type-error)
                       (reset! terminal-error e)
                       (throw e)))))
               (merge
                 {:delayed-errors (vec (concat (when-let [es vs/*delayed-errors*]
                                                 @es)
                                               (when-let [e @terminal-error]
                                                 [e])))}
                 (when (and file-mapping
                            (== 1 (count nsym-coll)))
                   {:file-mapping (apply merge
                                         (map #(impl/with-clojure-impl
                                                 (file-map/ast->file-mapping %))
                                              (get @vs/*checked-asts* (first nsym-coll))))}))))))))))

(defn check-ns
  ([ns-or-syms & opt]
   (let [{:keys [delayed-errors]} (apply check-ns-info ns-or-syms opt)]
     (if-let [errors (seq delayed-errors)]
       (err/print-errors! errors)
       :ok))))

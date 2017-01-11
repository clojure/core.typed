(ns clojure.core.typed.check-ns-common
  (:require [clojure.core.typed.profiling :as p]
            [clojure.core.typed.reset-env :as reset-env]
            [clojure.core.typed.reset-caches :as reset-caches]
            [clojure.core.typed.collect-phase :as collect-clj]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.check :as chk-clj]
            [clojure.core.typed.file-mapping :as file-map]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.lex-env :as lex-env]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.ns-deps-utils :as ns-deps-u]
            [clojure.java.io :as io]
            [clojure.core.cache :as cache]
            [clojure.tools.analyzer.jvm.utils :as jvm-u])
  (:import (clojure.lang ExceptionInfo)))

(defn cljs-reader [nsym]
  (let [f ((impl/v 'cljs.util/ns->relpath) nsym)
        res (if (re-find #"^file://" f) (java.net.URL. f) (io/resource f))]
    (assert res (str "Can't find " f " in classpath"))
    (io/reader res)))

;; returns a map with keys
;; - :delayed errors    a vector of ExceptionInfo instances representing type errors
;;
;; Optional
;; - :file-mapping      a map from namespace symbols to vectors of AST nodes
;;                      Added if true :file-mapping keyword is passed as an option
(defn check-ns-info
  [impl ns-or-syms & {:keys [collect-only trace profile file-mapping]}]
  (p/profile-if profile
    (let [start (. System (nanoTime))]
      (reset-caches/reset-caches)
      (let [nsym-coll (map #(if (symbol? %)
                              ; namespace might not exist yet, so ns-name is not appropriate
                              ; to convert to symbol
                              %
                              (ns-name %))
                           (if ((some-fn symbol? con/namespace?)
                                ns-or-syms)
                             [ns-or-syms]
                             ns-or-syms))]
        (impl/with-full-impl impl
          (binding [vs/*delayed-errors* (err/-init-delayed-errors)
                    vs/*already-checked* (atom #{})
                    vs/*trace-checker* trace
                    vs/*analyze-ns-cache* (cache/soft-cache-factory {})
                    ; we only use this if we have exactly one namespace passed
                    vs/*checked-asts* (when (#{impl/clojure} impl)
                                        (when (== 1 (count nsym-coll))
                                          (atom {})))
                    vs/*already-collected* (atom #{})
                    vs/*lexical-env* (lex-env/init-lexical-env)
                    ;; nested check-ns inside check-form switches off check-form
                    vs/*in-check-form* false]
            (let [terminal-error (atom nil)]
              ;(reset-env/reset-envs!)
              ;(reset-caches)
              ;; handle terminal type error
              (try
                ;-------------------------
                ; Collect phase
                ;-------------------------
                (impl/impl-case
                  :clojure nil
                  :cljs (let [collect-ns (impl/v 'clojure.core.typed.collect-cljs/collect-ns)
                              _ (doseq [nsym nsym-coll]
                                  (collect-ns nsym))
                              ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                              collected (if-let [c vs/*already-collected*]
                                          @c
                                          (err/int-error "*already-collected* unbound"))]
                          (println "Collected" (count collected) "namespaces in" ms "msecs")
                          (flush)))
                ;-------------------------
                ; Check phase
                ;-------------------------
                (when-not collect-only
                  (let [check-ns (impl/impl-case
                                   :clojure chk-clj/check-ns-and-deps
                                   :cljs    (impl/v 'clojure.core.typed.check-cljs/check-ns))]
                    (doseq [nsym nsym-coll]
                      (check-ns nsym)))
                  #_
                  (let [vs (var-env/vars-with-unchecked-defs)]
                    (binding [*out* *err*]
                      (doseq [v vs]
                        (println "WARNING: Type Checker: Definition missing:" v 
                                 "\nHint: Use :no-check metadata with ann if this is an unchecked var")
                        (flush))))
                  #_
                  (let [ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                        checked (some-> vs/*already-checked* deref)
                        _ (when (#{impl/clojure} impl)
                            (u/trace 
                              (binding [*print-length* nil]
                                (let [checked (set (filter ns-deps-u/should-check-ns? checked))]
                                  (str "Checked namespaces: " checked
                                       ", " (apply + (for [nsym checked]
                                                       (with-open [rdr (io/reader 
                                                                         (impl/impl-case
                                                                           :clojure (jvm-u/ns-url nsym)
                                                                           :cljs    (cljs-reader nsym)))]
                                                         (count (line-seq rdr)))))
                                       " lines")))))]
                    (println "Checked" (count checked) "namespaces "
                             "in" ms "msecs")
                    (flush)))
                (catch ExceptionInfo e
                  (if (-> e ex-data :type-error)
                    (reset! terminal-error e)
                    (throw e))))
              (merge
                {:delayed-errors (vec (concat (some-> vs/*delayed-errors* deref)
                                              (when-let [e @terminal-error]
                                                [e])))}
                (when (#{impl/clojure} impl)
                  (when (and file-mapping
                             (== 1 (count nsym-coll)))
                    {:file-mapping (apply merge
                                          (map #(impl/with-full-impl impl
                                                  (file-map/ast->file-mapping %))
                                               (get (some-> vs/*checked-asts* deref) (first nsym-coll))))}))))))))))

(defn check-ns
  ([impl ns-or-syms & opt]
   (let [{:keys [delayed-errors]} (apply check-ns-info impl ns-or-syms opt)]
     (impl/with-full-impl impl
       (if-let [errors (seq delayed-errors)]
         (err/print-errors! errors)
         :ok)))))

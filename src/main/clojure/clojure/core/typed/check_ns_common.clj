(ns clojure.core.typed.check-ns-common
  (:require [clojure.core.typed.profiling :as p]
            [clojure.core.typed.reset-env :as reset-env]
            [clojure.core.typed.reset-caches :as reset-caches]
            [clojure.core.typed.collect-phase :as collect-clj]
            [clojure.core.typed.collect-cljs :as collect-cljs]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.check :as chk-clj]
            [clojure.core.typed.check-cljs :as chk-cljs]
            [clojure.core.typed.file-mapping :as file-map]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.java.io :as io]
            [clojure.jvm.tools.analyzer :as jta])
  (:import (clojure.lang ExceptionInfo)))

(defn cljs-reader [nsym]
  (let [f ((impl/v 'cljs.analyzer/ns->relpath) nsym)
        res (if (re-find #"^file://" f) (java.net.URL. f) (io/resource f))]
    (assert res (str "Can't find " f " in classpath"))
    (io/reader res)))

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
        (cond
          vs/*checking* (err/int-error "Found inner call to check-ns or cf")

          :else
          (impl/with-full-impl impl
            (binding [vs/*checking* true
                      vs/*delayed-errors* (err/-init-delayed-errors)
                      vs/*already-checked* (atom #{})
                      vs/*trace-checker* trace
                      vs/*analyze-ns-cache* (atom {})
                      ; we only use this if we have exactly one namespace passed
                      vs/*checked-asts* (when (#{impl/clojure} impl)
                                          (when (== 1 (count nsym-coll))
                                            (atom {})))]
              (with-bindings {(impl/impl-case
                                :clojure #'vs/*already-collected*
                                :cljs (impl/the-var 'cljs.core.typed/*already-collected*))
                              (atom #{})}
                (let [terminal-error (atom nil)]
                  (reset-env/reset-envs!)
                  ;(reset-caches)
                  ;; handle terminal type error
                  (try
                    ;-------------------------
                    ; Collect phase
                    ;-------------------------
                    (doseq [nsym nsym-coll]
                      (impl/impl-case
                        :clojure (collect-clj/collect-ns  nsym)
                        :cljs    (collect-cljs/collect-ns nsym)))
                    (let [ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                          collected @(impl/impl-case
                                       :clojure vs/*already-collected*
                                       :cljs (impl/v 'cljs.core.typed/*already-collected*))]
                      (println "Collected" (count collected) "namespaces in" ms "msecs")
                      (flush))

                    ;-------------------------
                    ; Check phase
                    ;-------------------------
                    (when-not collect-only
                      (doseq [nsym nsym-coll]
                        (impl/impl-case
                          :clojure (chk-clj/check-ns-and-deps nsym)
                          :cljs    (chk-cljs/check-ns nsym)))
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
                                                   (with-open [rdr (io/reader 
                                                                     (impl/impl-case
                                                                       :clojure (jta/uri-for-ns nsym)
                                                                       :cljs    (cljs-reader nsym)))]
                                                     (count (line-seq rdr))))))]
                        (println "Checked" (count checked) "namespaces (approx." nlines "lines) in" ms "msecs")
                        (flush)))
                    (catch ExceptionInfo e
                      (if (-> e ex-data :type-error)
                        (reset! terminal-error e)
                        (throw e))))
                  (merge
                    {:delayed-errors (vec (concat (when-let [es vs/*delayed-errors*]
                                                    @es)
                                                  (when-let [e @terminal-error]
                                                    [e])))}
                    (when (#{impl/clojure} impl)
                      (when (and file-mapping
                                 (== 1 (count nsym-coll)))
                        {:file-mapping (apply merge
                                              (map #(impl/with-full-impl impl
                                                      (file-map/ast->file-mapping %))
                                                   (get @vs/*checked-asts* (first nsym-coll))))}))))))))))))

(defn check-ns
  ([impl ns-or-syms & opt]
   (let [{:keys [delayed-errors]} (apply check-ns-info impl ns-or-syms opt)]
     (if-let [errors (seq delayed-errors)]
       (err/print-errors! errors)
       :ok))))

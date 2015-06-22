(ns clojure.core.typed.check-ns-common
  (:require [clojure.core.typed.profiling :as p]
            [clojure.core.typed.reset-env :as reset-env]
            [clojure.core.typed.reset-caches :as reset-caches]
            [clojure.core.typed.collect-phase :as collect-clj]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.check :as chk-clj]
            [clojure.core.typed.file-mapping :as file-map]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.java.io :as io]
            [clojure.core.cache :as cache]
            [clojure.jvm.tools.analyzer :as jta])
  (:import (clojure.lang ExceptionInfo)))

;; Take keywords: 
;;  Mandatory
;; - :already-checked   an atom of a set of namespace symbols that should be skipped
;;                      on the next check-ns
;; - :collect-ns        a function that takes a namespace symbol and collects annotations
;;                      from the corresponding file
;; - :check-ns          a function that takes a namespace symbol and type checks the corresponding
;;                      file
;;
;;  Optional
;; - :clean             if true, resets the type environment and recheck all namespace
;;                      dependencies
;; Returns a map with keys:
;; - :delayed errors    a vector of ExceptionInfo instances representing type errors
;;
;; Optional
;; - :file-mapping      a map from namespace symbols to vectors of AST nodes
;;                      Added if true :file-mapping keyword is passed as an option
(defn check-ns-info
  [impl ns-or-syms & {:keys [collect-only trace profile file-mapping already-checked clean
                             collect-ns check-ns]}]
  {:pre [(con/atom? already-checked)
         (ifn? collect-ns)
         (ifn? check-ns)]}
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
          (when clean
            (reset! already-checked #{})
            (reset-env/reset-envs!))
          (binding [vs/*delayed-errors* (err/-init-delayed-errors)
                    vs/*already-collected* (atom @already-checked) ;; collect seems wasteful now
                    vs/*already-checked*   already-checked
                    vs/*trace-checker* trace
                    vs/*analyze-ns-cache* (cache/soft-cache-factory {})
                    ; we only use this if we have exactly one namespace passed
                    vs/*checked-asts* (when (#{impl/clojure} impl)
                                        (when (== 1 (count nsym-coll))
                                          (atom {})))]
            (let [terminal-error (atom nil)]
              ;; handle terminal type error
              (try
                ;-------------------------
                ; Collect phase
                ;-------------------------
                (doseq [nsym nsym-coll]
                  (collect-ns nsym))
                (let [ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                      collected (if-let [c vs/*already-collected*]
                                  @c
                                  (err/int-error "*already-collected* unbound"))]
                  (println "Collected" (count collected) "namespaces in" ms "msecs")
                  (flush))

                ;-------------------------
                ; Check phase
                ;-------------------------
                (when-not collect-only
                  (doseq [nsym nsym-coll]
                    (check-ns nsym))
                  (let [vs (var-env/vars-with-unchecked-defs)]
                    (binding [*out* *err*]
                      (let [printed-hint? (atom false)]
                        (doseq [v vs]
                          (println "WARNING: Type Checker: Definition missing:" v 
                                   (when-not @printed-hint?
                                     "\nHint: Use :no-check metadata with ann if this is an unchecked var"))
                          (flush)
                          (reset! printed-hint? true)))))
                  (let [ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                        checked (some-> vs/*already-checked* deref)]
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

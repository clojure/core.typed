;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.load1
  "Implementation of clojure.core.typed.load."
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.analyzer.common.env :as env]
            [clojure.core.typed.analyzer.jvm :as jana2]
            [clojure.core.typed.checker.check-form-common2 :as chk-frm]
            [clojure.core.typed.checker.jvm.check-form-clj :as chk-frm-clj]
            [clojure.core.typed.checker.ns-deps-utils :as ns-utils]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.lang.jvm :as lang]
            [clojure.core.typed.util-vars :as vs]
            [clojure.java.io :as io]
            [clojure.tools.reader :as reader]
            [clojure.tools.reader.reader-types :as readers])
  (:import java.net.URL))

;; based on clojure.tools.analyzer.jvm/analyze-ns
;; (IFn [String -> nil]
;;      [String ToolsAnalyzerEnv -> nil]
;;      [String ToolsAnalyzerEnv ToolsReaderOpts -> nil])
(defn load-typed-file
  "Loads a whole typed namespace, returns nil. Assumes the file is typed."
  ([filename] (load-typed-file filename (jana2/empty-env) {}))
  ([filename env] (load-typed-file filename env {}))
  ([filename env opts]
   {:pre [(string? filename)]
    :post [(nil? %)]}
   ;(prn "load-typed-file" filename)
    (t/load-if-needed)
    (env/ensure (jana2/global-env)
     (let [should-runtime-infer? vs/*prepare-infer-ns*
           instrument-infer-config vs/*instrument-infer-config*
           _ (when should-runtime-infer?
               (println "Refreshing runtime inference")
               (t/refresh-runtime-infer))
           orig-filename filename
           [file-url filename]
           (or (let [f (str filename ".clj")]
                 (when-let [r (io/resource f)]
                   [r f]))
               (let [f (str filename ".cljc")]
                 (when-let [r (io/resource f)]
                   [r f])))]
       (assert file-url (str "Cannot find file " orig-filename))
       (binding [*ns*   *ns*
                 *file* filename
                 vs/*in-typed-load* true
                 vs/*typed-load-atom* (atom {})
                 vs/*prepare-infer-ns* nil
                 vs/*instrument-infer-config* nil]
         (with-open [rdr (io/reader file-url)]
           (let [pbr (readers/indexing-push-back-reader
                       (java.io.PushbackReader. rdr) 1 filename)
                 eof (Object.)
                 opts {:eof eof :features #{:clj :t.a.jvm}}
                 opts (if (.endsWith ^String filename "cljc")
                        (assoc opts :read-cond :allow)
                        opts)
                 config (assoc (chk-frm-clj/config-map2)
                               :env env
                               :should-runtime-infer? should-runtime-infer?
                               :instrument-infer-config instrument-infer-config)]
             (impl/with-full-impl (:impl config)
               (loop []
                 (let [form (reader/read opts pbr)]
                   (when-not (identical? form eof)
                     (let [{:keys [ex]} (chk-frm/check-form-info config form
                                                                 :check-config (t/default-check-config))]
                       (when ex
                         (throw ex)))
                     (recur))))))))))))

(defn typed-load1
  "For each path, checks if the given file is typed, and loads it with core.typed if so,
  otherwise with clojure.core/load"
  [& base-resource-paths]
  {:pre [(every? string? base-resource-paths)]
   :post [(nil? %)]}
  ;(prn "typed load" base-resource-paths)
  (doseq [base-resource-path base-resource-paths]
    (cond
      (impl/with-clojure-impl
        (or (ns-utils/file-should-use-typed-load? (str base-resource-path ".clj"))
            (ns-utils/file-should-use-typed-load? (str base-resource-path ".cljc"))))
      (do
        (when @#'clojure.core/*loading-verbosely*
          (printf "Loading typed file\n" base-resource-path))
        (load-typed-file base-resource-path))

      :else (clojure.lang.RT/load base-resource-path))))

(defn typed-eval [form]
  (let [{:keys [ex result]} (t/check-form-info form)]
    (if ex
      (throw ex)
      result)))

(defn install-typed-load
  "Extend the :lang dispatch table with the :core.typed language"
  []
  {:post [(nil? %)]}
  (alter-var-root #'lang/lang-dispatch
                  (fn [m]
                    (-> m 
                        (assoc-in [:core.typed :load] #'typed-load1)
                        (assoc-in [:core.typed :eval] #'typed-eval))))
  nil)

(defn monkey-patch-typed-load
  "Install the :core.typed :lang, and monkey patch `load`"
  []
  {:post [(nil? %)]}
  (install-typed-load)
  (lang/monkey-patch-extensible-load)
  nil)

(defn monkey-patch-typed-eval
  "Install the :core.typed :lang, and monkey patch `eval`"
  []
  {:post [(nil? %)]}
  (install-typed-load)
  (lang/monkey-patch-extensible-eval)
  nil)

(defn install 
  "Install the :core.typed :lang. Takes an optional set of features
  to install, defaults to #{:load :eval}.

  Features:
    - :load    Installs typed `load` over `clojure.core/load`
    - :eval    Installs typed `eval` over `clojure.core/eval`

  eg. (install)            ; installs `load` and `eval`
  eg. (install #{:eval})   ; installs `eval`
  eg. (install #{:load})   ; installs `load`"
  ([] (install :all))
  ([features]
   {:pre [((some-fn set? #{:all}) features)]
    :post [(nil? %)]}
   (lang/install features)
   (when (or (= features :all)
             (:load features))
     (monkey-patch-typed-load))
   (when (or (= features :all)
             (:eval features))
     (monkey-patch-typed-eval))
   nil))

(comment (find-resource "clojure/core/typed/test/load_file.clj")
         (typed-load "/clojure/core/typed/test/load_file.clj")
         (load "/clojure/core/typed/test/load_file")
         (require 'clojure.core.typed.test.load-file :reload :verbose)
         )

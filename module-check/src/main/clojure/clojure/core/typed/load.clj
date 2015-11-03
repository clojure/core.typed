(ns ^:skip-wiki clojure.core.typed.load
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.ns-deps-utils :as ns-utils]
            [clojure.core.typed.analyze-clj :as ana-clj]
            [clojure.tools.analyzer.env :as ta-env]
            [clojure.core.typed.current-impl :as impl]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.tools.reader :as reader]
            [clojure.java.io :as io]
            [clojure.core.typed.profiling :as p]
            [clojure.core.typed.check-form-clj :as chk-frm-clj]
            [clojure.core.typed.check-form-common :as chk-frm]
            [clojure.core.typed.lang :as lang]
            [clojure.tools.analyzer.jvm :as taj])
  (:import java.net.URL))

;; copied from cljx
#_(defn- find-resource
  [name]
  (if-let [cl (clojure.lang.RT/baseLoader)]
    (.getResource cl name)
    (ClassLoader/getSystemResourceAsStream name)))

;; based on clojure.tools.analyzer.jvm/analyze-ns
;; (IFn [String -> nil]
;;      [String ToolsAnalyzerEnv -> nil]
;;      [String ToolsAnalyzerEnv ToolsReaderOpts -> nil])
(defn load-typed-file
  "Loads a whole typed namespace, returns nil."
  ([filename] (load-typed-file filename (taj/empty-env) {}))
  ([filename env] (load-typed-file filename env {}))
  ([filename env opts]
   {:pre [(string? filename)]
    :post [(nil? %)]}
    (t/load-if-needed)
    (ta-env/ensure (p/p :typed-load/global-env
                        (taj/global-env))
     (let [[file-url filename]
           (or (let [f (str filename ".clj")]
                 (when-let [r (io/resource f)]
                   [r f]))
               (let [f (str filename ".cljc")]
                 (when-let [r (io/resource f)]
                   [r f])))]
       (assert file-url (str "Cannot find file " filename))
       (binding [*ns*   *ns*
                 *file* filename]
         (with-open [rdr (io/reader file-url)]
           (let [pbr (readers/indexing-push-back-reader
                       (java.io.PushbackReader. rdr) 1 filename)
                 eof (Object.)
                 opts {:eof eof :features #{:clj :t.a.jvm}}
                 opts (if (.endsWith ^String filename "cljc")
                        (assoc opts :read-cond :allow)
                        opts)
                 config (assoc (chk-frm-clj/config-map)
                               :env env)]
             (impl/with-full-impl (:impl config)
               (loop []
                 (let [form (p/p :typed-load/read (reader/read opts pbr))]
                   (when-not (identical? form eof)
                     (let [{:keys [ex]} (p/p :typed-load/check-form
                                             (chk-frm/check-form-info config form))]
                       (when ex
                         (throw ex)))
                     ;(ana-clj/analyze+eval form (assoc env :ns (ns-name *ns*)) opts)
                     (recur))))))))))))

(defn typed-load1 [base-resource-path]
  {:pre [(string? base-resource-path)]
   :post [(nil? %)]}
  ;(prn "typed load" base-resource-path)
  (cond
    (or (ns-utils/file-should-use-typed-load? (str base-resource-path ".clj"))
        (ns-utils/file-should-use-typed-load? (str base-resource-path ".cljc")))
    (do
      (when @#'clojure.core/*loading-verbosely*
        (printf "Loading typed file\n" base-resource-path))
      (load-typed-file base-resource-path))

    :else (clojure.lang.RT/load base-resource-path)))

(defn install-typed-load
  []
  {:post [(nil? %)]}
  (alter-var-root #'lang/lang-dispatch
                  assoc :core.typed #'typed-load1)
  nil)

(comment (find-resource "clojure/core/typed/test/load_file.clj")
         (typed-load "/clojure/core/typed/test/load_file.clj")
         (load "/clojure/core/typed/test/load_file")
         (require 'clojure.core.typed.test.load-file :reload :verbose)
         )

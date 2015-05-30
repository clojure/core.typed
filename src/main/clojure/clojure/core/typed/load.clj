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
            [clojure.core.typed.check-form-clj :as chk-frm-clj]
            [clojure.core.typed.check-form-common :as chk-frm]
            [clojure.tools.analyzer.jvm :as taj])
  (:import java.net.URL))

;; copied from cljx
(defn- find-resource
  [name]
  (if-let [cl (clojure.lang.RT/baseLoader)]
    (.getResource cl name)
    (ClassLoader/getSystemResourceAsStream name)))

;; based on clojure.tools.analyzer.jvm/analyze-ns
(defn load-typed-file
  "Loads a whole typed namespace, returns nil."
  ([filename] (load-typed-file filename (taj/empty-env) {}))
  ([filename env] (load-typed-file filename env {}))
  ([^String filename env opts]
    (t/load-if-needed)
    (ta-env/ensure (taj/global-env)
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
                 opts (if (.endsWith filename "cljc")
                        (assoc opts :read-cond :allow)
                        opts)
                 config (assoc (chk-frm-clj/config-map)
                               :env env)]
             (impl/with-full-impl (:impl config)
               (loop []
                 (let [form (reader/read opts pbr)]
                   (when-not (identical? form eof)
                     (let [{:keys [ex]} (chk-frm/check-form-info config form)]
                       (when ex
                         (throw ex)))
                     ;(ana-clj/analyze+eval form (assoc env :ns (ns-name *ns*)) opts)
                     (recur))))))))))))

;; modified from cljx
(defn typed-load
  "Loads Clojure code from resources in classpath. A path is interpreted as
classpath-relative if it begins with a slash or relative to the root
directory for the current namespace otherwise."
  {:added "1.0"}
  [& paths]
  (doseq [^String path paths]
    (let [^String path (if (.startsWith path "/")
                          path
                          (str (#'clojure.core/root-directory (ns-name *ns*)) \/ path))]
      (when @#'clojure.core/*loading-verbosely*
        (printf "(clojure.core/load \"%s\")\n" path)
        (flush))
      (#'clojure.core/check-cyclic-dependency path)
      (when-not (= path (first @#'clojure.core/*pending-paths*))
        (with-bindings {#'clojure.core/*pending-paths* (conj @#'clojure.core/*pending-paths* path)}
          (let [base-resource-path (.substring path 1)
                cljx-path (str base-resource-path ".cljx")]
            (if-let [cljx (find-resource cljx-path)]
              (assert nil (str "TODO: cljx compatibility"))
              ;(do
              ;  (when @#'clojure.core/*loading-verbosely*
              ;    (printf "Transforming cljx => clj from %s.cljx\n" base-resource-path))
              ;  (-> (slurp cljx)
              ;      (cljx/transform (:clj @cljx-load-rules))
              ;      java.io.StringReader.
              ;      (clojure.lang.Compiler/load base-resource-path
              ;                                  (last (re-find #"([^/]+$)" cljx-path)))))
              (cond
                (or (ns-utils/file-has-core-typed-metadata? (str base-resource-path ".clj"))
                    (ns-utils/file-has-core-typed-metadata? (str base-resource-path ".cljc")))
                (do
                  (when @#'clojure.core/*loading-verbosely*
                    (printf "Loading typed file\n" base-resource-path))
                  (load-typed-file base-resource-path))

                :else (clojure.lang.RT/load base-resource-path)))))))))

(comment (find-resource "clojure/core/typed/test/load_file.clj")
         (typed-load "/clojure/core/typed/test/load_file.clj")
         (load "/clojure/core/typed/test/load_file")
         (require 'clojure.core.typed.test.load-file :reload :verbose)
         )

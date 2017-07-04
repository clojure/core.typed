(ns clojure.core.typed.load
  "Front end for actual implementation in clojure.core.typed.load1.

  Indirection is necessary to delay loading core.typed as long as possible."
  (:require [clojure.core.typed.load-if-needed :refer [load-if-needed]]
            [clojure.core.typed.current-impl :as impl]))

;; based on clojure.tools.analyzer.jvm/analyze-ns
;; (IFn [String -> nil]
;;      [String ToolsAnalyzerEnv -> nil]
;;      [String ToolsAnalyzerEnv ToolsReaderOpts -> nil])
(defn load-typed-file
  "Loads a whole typed namespace, returns nil. Assumes the file is typed."
  ([filename]
   (load-if-needed)
   ((impl/v 'clojure.core.typed.load1/load-typed-file)
    filename))
  ([filename env]
   (load-if-needed)
   ((impl/v 'clojure.core.typed.load1/load-typed-file)
    filename
    env))
  ([filename env opts]
   {:pre [(string? filename)]
    :post [(nil? %)]}
   (load-if-needed)
   ((impl/v 'clojure.core.typed.load1/load-typed-file)
    filename
    env
    opts)))

(defn typed-load1
  "For each path, checks if the given file is typed, and loads it with core.typed if so,
  otherwise with clojure.core/load"
  [& base-resource-paths]
  {:pre [(every? string? base-resource-paths)]
   :post [(nil? %)]}
  (load-if-needed)
  (apply (impl/v 'clojure.core.typed.load1/typed-load1)
   base-resource-paths))

(defn typed-eval [form]
  (load-if-needed)
  ((impl/v 'clojure.core.typed.load1/typed-eval)
   form))

(defn install-typed-load
  "Extend the :lang dispatch table with the :core.typed language"
  []
  {:post [(nil? %)]}
  (load-if-needed)
  ((impl/v 'clojure.core.typed.load1/install-typed-load)))

(defn monkey-patch-typed-load
  "Install the :core.typed :lang, and monkey patch `load`"
  []
  {:post [(nil? %)]}
  (load-if-needed)
  ((impl/v 'clojure.core.typed.load1/monkey-patch-typed-load)))

(defn monkey-patch-typed-eval
  "Install the :core.typed :lang, and monkey patch `eval`"
  []
  {:post [(nil? %)]}
  (load-if-needed)
  ((impl/v 'clojure.core.typed.load1/monkey-patch-typed-eval)))

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
   (load-if-needed)
   ((impl/v 'clojure.core.typed.load1/install)
    features)))

(comment (find-resource "clojure/core/typed/test/load_file.clj")
         (typed-load "/clojure/core/typed/test/load_file.clj")
         (load "/clojure/core/typed/test/load_file")
         (require 'clojure.core.typed.test.load-file :reload :verbose)
         )

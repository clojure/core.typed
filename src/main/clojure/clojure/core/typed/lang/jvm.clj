;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.lang.jvm
  "Extensible languages in Clojure, a la Racket's #lang.
  
  This is a simple library that monkey patches clojure.core/load
  to be extensible to different backends.

  `monkey-patch-extensible-load` does the actual monkey-patching and
  must be called explicitly.
  
  `lang-dispatch` is a map from keywords to alternative `load` functions
  (of type [String -> nil]). The corresponding function will be used to
  load a file according its :lang metadata entry in the `ns` form.

  To add a new implementation, use
    (alter-var-root lang-dispatch assoc :new-impl my-load)

  eg. A file with a `ns` form
        (ns fancy-ns-form
          {:lang :new-impl})
      will use `my-load` to load the file.
  "
  (:require [clojure.tools.namespace.file :as ns-file]
            [clojure.tools.namespace.parse :as ns-parse]
            [clojure.java.io :as io]))

; (Map Kw (HMap :optional {:eval [Any -> Any], 
;                          :load [Str -> nil]}))
(def lang-dispatch 
  "A map from :lang entries to their corresponding `load` and `eval` alternatives."
  {})

; [String -> nil]
(defn default-load1 
  "Roughly equivalent to clojure.core/load."
  [^String base-resource-path]
  (clojure.lang.RT/load base-resource-path))

; [Any -> Any]
(defn default-eval 
  "Roughly equivalent to clojure.core/eval."
  [form]
  (. clojure.lang.Compiler (eval form)))

(defn- ns-form-for-file
  "Returns the namespace declaration for the file, or
  nil if not found"
  [file]
  (some-> (io/resource file)
          (ns-file/read-file-ns-decl ns-parse/clj-read-opts)))

(defn- take-when
  "When pred is true of the head of seq, return [head tail]. Otherwise
  [nil seq]. Used as a helper for parsing optinal typed elements out
  of sequences. Say docstrings out of argument seqs."
  [pred seq]
  (if (pred (first seq))
    ((juxt first rest) seq)
    [nil seq]))

(defn- ns-form-name
  "Returns the symbol naming this namespace, with any
  metadata attached."
  [ns-form]
  {:post [(symbol? %)]}
  (let [ns-form (next ns-form)
        [nsym ns-form] (take-when symbol? ns-form)
        _ (when-not (symbol? nsym)
            (throw (ex-info "Malformed ns form" {})))
        [docstr ns-form]  (take-when string? ns-form)
        [metamap ns-form] (take-when map? ns-form)]
    (if (map? metamap)
      (vary-meta nsym merge metamap)
      nsym)))

(defn- ns-meta
  "Returns the metadata map for this namespace"
  [ns-form]
  (meta (ns-form-name ns-form)))

; [Str -> Any]
(defn file-lang
  "Returns the :lang entry in ns form in the given namespace."
  [res]
  (some-> res 
          ns-form-for-file 
          ns-meta 
          :lang))

; [Namespace -> Any]
(defn ns-lang
  "Returns the :lang value in the give Namespace's metadata."
  [ns]
  (-> (meta ns) :lang))

; [Str * -> nil]
(defn extensible-load
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
                lang (or (file-lang (str base-resource-path ".clj"))
                         (file-lang (str base-resource-path ".cljc")))
                disp (get-in lang-dispatch [lang :load] default-load1)]
            (disp base-resource-path)))))))

(defn extensible-eval
  "Evaluates the form data structure (not text!) and returns the result."
  [form]
  (let [lang (ns-lang *ns*)
        disp (get-in lang-dispatch [lang :eval] default-eval)]
    (disp form)))


; [-> nil]
(def monkey-patch-extensible-load
  "A no-argument function that installs the extensible `load` function
  over clojure.core/load."
  (let [l (delay (alter-var-root #'load (constantly #'extensible-load)))]
    (fn []
      @l
      nil)))

; [-> nil]
(def monkey-patch-extensible-eval
  "A no-argument function that installs the extensible `eval` function
  over clojure.core/eval."
  (let [l (delay (alter-var-root #'eval (constantly #'extensible-eval)))]
    (fn []
      @l
      nil)))

(defn install 
  "A no-argument function that installs extensible `eval` and `load`
  alternatives that respect :lang ns metadata"
  ([] (install :all))
  ([features]
   (when (or (= features :all)
             (:load features))
     (monkey-patch-extensible-load))
   (when (or (= features :all)
             (:eval features))
     (monkey-patch-extensible-eval))
   nil))

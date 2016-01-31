(ns clojure.core.typed.lang
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
  (:require [clojure.core.typed.ns-deps-utils :as ns-utils]))

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

; [Str -> Any]
(defn file-lang
  "Returns the :lang entry in ns form in the given namespace."
  [res]
  (some-> res 
          ns-utils/ns-form-for-file 
          ns-utils/ns-meta 
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

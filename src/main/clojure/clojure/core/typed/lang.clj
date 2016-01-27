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
  (:refer-clojure :exclude [require use])
  (:require [clojure.core.typed.ns-deps-utils :as ns-utils]
            [clojure.core :as core]))

; (Map Kw [Str -> nil])
(def lang-dispatch 
  "A map from :lang entries to their corresponding `load` alternatives."
  {})

; [String -> nil]
(defn default-load1 
  "Roughly equivalent to clojure.core/load."
  [^String base-resource-path]
  (clojure.lang.RT/load base-resource-path))

; [Str -> Any]
(defn file-lang
  "Returns the :lang entry in ns form in the given namespace."
  [res]
  (some-> res 
          ns-utils/ns-form-for-file 
          ns-utils/ns-meta 
          :lang))

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
                disp (get lang-dispatch lang default-load1)]
            (disp base-resource-path)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From here, functions from clojure.core are copied.
;; The goal is to redefine `load` to our extensible load. However, from
;; Clojure 1.8.0, clojure.core is compiled to be directly linked, so calls
;; to `require` and `use` no longer see the updated `load`.
;;
;; The solution is to manually redefine all clojure.core functions that use
;; `load` downstream.
;;  - require
;;  - use

(defn- load-one
  "Loads a lib given its name. If need-ns, ensures that the associated
  namespace exists after loading. If require, records the load so any
  duplicate loads can be skipped."
  [lib need-ns require]
  (load (#'core/root-resource lib))
  (@#'core/throw-if (and need-ns (not (find-ns lib)))
            "namespace '%s' not found after loading '%s'"
            lib (#'core/root-resource lib))
  (when require
    (dosync
     (commute @#'core/*loaded-libs* conj lib))))

(defn- load-all
  "Loads a lib given its name and forces a load of any libs it directly or
  indirectly loads. If need-ns, ensures that the associated namespace
  exists after loading. If require, records the load so any duplicate loads
  can be skipped."
  [lib need-ns require]
  (dosync
   (commute @#'core/*loaded-libs* #(#'core/reduce1 conj %1 %2)
            (with-bindings {#'core/*loaded-libs* (ref (sorted-set))}
              (load-one lib need-ns require)
              @@#'core/*loaded-libs*))))

(defn- load-lib
  "Loads a lib with options"
  [prefix lib & options]
  (@#'core/throw-if (and prefix (pos? (.indexOf (name lib) (int \.))))
            "Found lib name '%s' containing period with prefix '%s'.  lib names inside prefix lists must not contain periods"
            (name lib) prefix)
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)
        {:keys [as reload reload-all require use verbose]} opts
        loaded (contains? @@#'core/*loaded-libs* lib)
        load (cond reload-all
                   load-all
                   (or reload (not require) (not loaded))
                   load-one)
        need-ns (or as use)
        filter-opts (select-keys opts '(:exclude :only :rename :refer))
        undefined-on-entry (not (find-ns lib))]
    (with-bindings {#'core/*loading-verbosely* (or @#'core/*loading-verbosely* verbose)}
      (if load
        (try
          (load lib need-ns require)
          (catch Exception e
            (when undefined-on-entry
              (remove-ns lib))
            (throw e)))
        (@#'core/throw-if (and need-ns (not (find-ns lib)))
                  "namespace '%s' not found" lib))
      (when (and need-ns @#'core/*loading-verbosely*)
        (printf "(clojure.core/in-ns '%s)\n" (ns-name *ns*)))
      (when as
        (when @#'core/*loading-verbosely*
          (printf "(clojure.core/alias '%s '%s)\n" as lib))
        (alias as lib))
      (when (or use (:refer filter-opts))
        (when @#'core/*loading-verbosely*
          (printf "(clojure.core/refer '%s" lib)
          (doseq [opt filter-opts]
            (printf " %s '%s" (key opt) (print-str (val opt))))
          (printf ")\n"))
        (apply refer lib (mapcat seq filter-opts))))))

(defn- load-libs
  "Loads libs, interpreting libspecs, prefix lists, and flags for
  forwarding to load-lib"
  [& args]
  (let [flags (filter keyword? args)
        opts (interleave flags (repeat true))
        args (filter (complement keyword?) args)]
    ; check for unsupported options
    (let [supported #{:as :reload :reload-all :require :use :verbose :refer}
          unsupported (seq (remove supported flags))]
      (@#'core/throw-if unsupported
                (apply str "Unsupported option(s) supplied: "
                     (interpose \, unsupported))))
    ; check a load target was specified
    (@#'core/throw-if (not (seq args)) "Nothing specified to load")
    (doseq [arg args]
      (if (@#'core/libspec? arg)
        (apply load-lib nil (@#'core/prependss arg opts))
        (let [[prefix & args] arg]
          (@#'core/throw-if (nil? prefix) "prefix cannot be nil")
          (doseq [arg args]
            (apply load-lib prefix (@#'core/prependss arg opts))))))))

(defn- check-cyclic-dependency
  "Detects and rejects non-trivial cyclic load dependencies. The
  exception message shows the dependency chain with the cycle
  highlighted. Ignores the trivial case of a file attempting to load
  itself because that can occur when a gen-class'd class loads its
  implementation."
  [path]
  (when (some #{path} (rest @#'core/*pending-paths*))
    (let [pending (map #(if (= % path) (str "[ " % " ]") %)
                       (cons path @#'core/*pending-paths*))
          chain (apply str (interpose "->" pending))]
      (@#'core/throw-if true "Cyclic load dependency: %s" chain))))

;; Public

(defn require
  "Loads libs, skipping any that are already loaded. Each argument is
  either a libspec that identifies a lib, a prefix list that identifies
  multiple libs whose names share a common prefix, or a flag that modifies
  how all the identified libs are loaded. Use :require in the ns macro
  in preference to calling this directly.

  Libs

  A 'lib' is a named set of resources in classpath whose contents define a
  library of Clojure code. Lib names are symbols and each lib is associated
  with a Clojure namespace and a Java package that share its name. A lib's
  name also locates its root directory within classpath using Java's
  package name to classpath-relative path mapping. All resources in a lib
  should be contained in the directory structure under its root directory.
  All definitions a lib makes should be in its associated namespace.

  'require loads a lib by loading its root resource. The root resource path
  is derived from the lib name in the following manner:
  Consider a lib named by the symbol 'x.y.z; it has the root directory
  <classpath>/x/y/, and its root resource is <classpath>/x/y/z.clj. The root
  resource should contain code to create the lib's namespace (usually by using
  the ns macro) and load any additional lib resources.

  Libspecs

  A libspec is a lib name or a vector containing a lib name followed by
  options expressed as sequential keywords and arguments.

  Recognized options:
  :as takes a symbol as its argument and makes that symbol an alias to the
    lib's namespace in the current namespace.
  :refer takes a list of symbols to refer from the namespace or the :all
    keyword to bring in all public vars.

  Prefix Lists

  It's common for Clojure code to depend on several libs whose names have
  the same prefix. When specifying libs, prefix lists can be used to reduce
  repetition. A prefix list contains the shared prefix followed by libspecs
  with the shared prefix removed from the lib names. After removing the
  prefix, the names that remain must not contain any periods.

  Flags

  A flag is a keyword.
  Recognized flags: :reload, :reload-all, :verbose
  :reload forces loading of all the identified libs even if they are
    already loaded
  :reload-all implies :reload and also forces loading of all libs that the
    identified libs directly or indirectly load via require or use
  :verbose triggers printing information about each load, alias, and refer

  Example:

  The following would load the libraries clojure.zip and clojure.set
  abbreviated as 's'.

  (require '(clojure zip [set :as s]))"
  {:added "1.0"}

  [& args]
  (apply load-libs :require args))

(defn use
  "Like 'require, but also refers to each lib's namespace using
  clojure.core/refer. Use :use in the ns macro in preference to calling
  this directly.

  'use accepts additional options in libspecs: :exclude, :only, :rename.
  The arguments and semantics for :exclude, :only, and :rename are the same
  as those documented for clojure.core/refer."
  {:added "1.0"}
  [& args] (apply load-libs :require :use args))

;; End clojure.core copy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; [-> nil]
(def monkey-patch-extensible-load
  "A no-argument function that installs the core.typed `load` function
  over clojure.core/load."
  (let [l (delay
            (alter-var-root #'load (constantly #'extensible-load))
            (alter-var-root #'core/require (constantly #'require))
            (alter-var-root #'core/use (constantly #'use)))]
    (fn []
      @l
      nil)))

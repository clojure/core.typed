;; Copyright (c) Stuart Sierra, 2012. All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution. By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license. You must not
;; remove this notice, or any other, from this software.

(ns 
  ^{:author "Stuart Sierra",
     :doc "Search for namespace declarations in directories and JAR files."} 
  clojure.core.typed.deps.clojure.tools.namespace.find
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.core.typed.deps.clojure.tools.namespace.file :as file]
            [clojure.core.typed.deps.clojure.tools.namespace.parse :as parse])
  (:import (java.io File FileReader BufferedReader PushbackReader
                    InputStreamReader)
           (java.util.jar JarFile JarEntry)))

;;; JAR-file utilities, adapted from clojure.java.classpath

(defn- jar-file?
  "Returns true if file is a normal file with a .jar or .JAR extension."
  [f]
  (let [file (io/file f)]
    (and (.isFile file)
         (or (.endsWith (.getName file) ".jar")
             (.endsWith (.getName file) ".JAR")))))

(defn- jar-files
  "Given a sequence of File objects, filters it for JAR files, returns
  a sequence of java.util.jar.JarFile objects."
  [files]
  (map #(JarFile. ^File %) (filter jar-file? files)))

(defn- filenames-in-jar
  "Returns a sequence of Strings naming the non-directory entries in
  the JAR file."
  [^JarFile jar-file]
  (map #(.getName ^JarEntry %)
       (filter #(not (.isDirectory ^JarEntry %))
               (enumeration-seq (.entries jar-file)))))

;;; Finding namespaces in a directory tree

(defn find-clojure-sources-in-dir
  "Searches recursively under dir for Clojure source files (.clj).
  Returns a sequence of File objects, in breadth-first sort order."
  [^File dir]
  ;; Use sort by absolute path to get breadth-first search.
  (sort-by #(.getAbsolutePath ^File %)
           (filter file/clojure-file? (file-seq dir))))

(defn find-ns-decls-in-dir
  "Searches dir recursively for (ns ...) declarations in Clojure
  source files; returns the unevaluated ns declarations."
  [^File dir]
  (keep file/read-file-ns-decl (find-clojure-sources-in-dir dir)))

(defn find-namespaces-in-dir
  "Searches dir recursively for (ns ...) declarations in Clojure
  source files; returns the symbol names of the declared namespaces."
  [^File dir]
  (map second (find-ns-decls-in-dir dir)))

;;; Finding namespaces in JAR files

(defn clojure-sources-in-jar
  "Returns a sequence of filenames ending in .clj found in the JAR file."
  [^JarFile jar-file]
  (filter #(.endsWith ^String % ".clj") (filenames-in-jar jar-file)))

(defn read-ns-decl-from-jarfile-entry
  "Attempts to read a (ns ...) declaration from the named entry in the
  JAR file, and returns the unevaluated form.  Returns nil if the read
  fails, or if the first form is not a ns declaration."
  [^JarFile jarfile ^String entry-name]
  (with-open [rdr (PushbackReader.
                   (io/reader
                    (.getInputStream jarfile (.getEntry jarfile entry-name))))]
    (parse/read-ns-decl rdr)))

(defn find-ns-decls-in-jarfile
  "Searches the JAR file for Clojure source files containing (ns ...)
  declarations; returns the unevaluated ns declarations."
  [^JarFile jarfile]
  (filter identity
          (map #(read-ns-decl-from-jarfile-entry jarfile %)
               (clojure-sources-in-jar jarfile))))

(defn find-namespaces-in-jarfile
  "Searches the JAR file for Clojure source files containing (ns ...)
  declarations.  Returns a sequence of the symbol names of the
  declared namespaces."
  [^JarFile jarfile]
  (map second (find-ns-decls-in-jarfile jarfile)))


;;; Finding namespaces

(defn find-ns-decls
  "Searches a sequence of java.io.File objects (both directories and
  JAR files) for .clj source files containing (ns...) declarations.
  Returns a sequence of the unevaluated ns declaration forms. Use with
  clojure.java.classpath to search Clojure's classpath."
  [files]
  (concat
   (mapcat find-ns-decls-in-dir (filter #(.isDirectory ^File %) files))
   (mapcat find-ns-decls-in-jarfile (jar-files files))))

(defn find-namespaces
  "Searches a sequence of java.io.File objects (both directories and
  JAR files) for .clj source files containing (ns...) declarations.
  Returns a sequence of the symbol names of the declared
  namespaces. Use with clojure.java.classpath to search Clojure's
  classpath."
  [files]
  (map second (find-ns-decls files)))


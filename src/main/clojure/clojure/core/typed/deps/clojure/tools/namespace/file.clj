;; Copyright (c) Stuart Sierra, 2012. All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution. By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license. You must not
;; remove this notice, or any other, from this software.

(ns ^{:author "Stuart Sierra"
      :doc "Read and track namespace information from files"}
  clojure.core.typed.deps.clojure.tools.namespace.file
  (:require [clojure.java.io :as io]
            [clojure.core.typed.deps.clojure.tools.namespace.parse :as parse]
            [clojure.core.typed.deps.clojure.tools.namespace.track :as track])
  (:import (java.io PushbackReader)))

(defn read-file-ns-decl
  "Attempts to read a (ns ...) declaration from file, and returns the
  unevaluated form.  Returns nil if read fails, or if the first form
  is not a ns declaration."
  [file]
  (with-open [rdr (PushbackReader. (io/reader file))]
    (parse/read-ns-decl rdr)))

(defn clojure-file?
  "Returns true if the java.io.File represents a normal Clojure source
  file."
  [^java.io.File file]
  (and (.isFile file)
       (.endsWith (.getName file) ".clj")))

;;; Dependency tracker

(defn- files-and-deps [files]
  (reduce (fn [m file]
            (if-let [decl (read-file-ns-decl file)]
              (let [deps (parse/deps-from-ns-decl decl)
                    name (second decl)]
                (-> m
                    (assoc-in [:depmap name] deps)
                    (assoc-in [:filemap file] name)))
              m))
          {} files))

(defn add-files
  "Reads ns declarations from files; returns an updated dependency
  tracker with those files added."
  [tracker files]
  (let [{:keys [depmap filemap]} (files-and-deps files)]
    (-> tracker
        (track/add depmap)
        (update-in [::filemap] (fnil merge {}) filemap))))

(defn remove-files
  "Returns an updated dependency tracker with files removed. The files
  must have been previously added with add-files."
  [tracker files]
  (-> tracker
      (track/remove (keep (::filemap tracker {}) files))
      (update-in [::filemap] #(apply dissoc % files))))


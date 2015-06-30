;; Copyright (c) Stuart Sierra, 2012. All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution. By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license. You must not
;; remove this notice, or any other, from this software.

(ns ^{:author "Stuart Sierra"
      :doc "Force reloading namespaces on demand or through a
      dependency tracker"}
  clojure.core.typed.deps.clojure.tools.namespace.reload
  (:require [clojure.core.typed.deps.clojure.tools.namespace.track :as track]))

(defn remove-lib
  "Remove lib's namespace and remove lib from the set of loaded libs."
  [lib]
  (remove-ns lib)
  (dosync (alter @#'clojure.core/*loaded-libs* disj lib)))

(defn track-reload-one
  "Executes the next pending unload/reload operation in the dependency
  tracker. Returns the updated dependency tracker. If reloading caused
  an error, it is captured as ::error and the namespace which caused
  the error is ::error-ns."
  [tracker]
  (let [{unload ::track/unload, load ::track/load} tracker]
    (cond
      (seq unload)
        (let [n (first unload)]
          (remove-lib n)
          (update-in tracker [::track/unload] rest))
      (seq load)
        (let [n (first load)]
          (try (require n :reload)
               (update-in tracker [::track/load] rest)
               (catch Throwable t
                 (assoc tracker
                   ::error t ::error-ns n ::track/unload load))))
      :else
        tracker)))

(defn track-reload
  "Executes all pending unload/reload operations on dependency tracker
  until either an error is encountered or there are no more pending
  operations."
  [tracker]
  (loop [tracker (dissoc tracker ::error ::error-ns)]
    (let [{error ::error, unload ::track/unload, load ::track/load} tracker]
      (if (and (not error)
               (or (seq load) (seq unload)))
        (recur (track-reload-one tracker))
        tracker))))

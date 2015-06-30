;; Copyright (c) Stuart Sierra, 2012. All rights reserved. The use and
;; distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution. By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license. You must not
;; remove this notice, or any other, from this software.

(ns ^{:author "Stuart Sierra"
      :doc "REPL utilities for working with namespaces"}
  clojure.core.typed.deps.clojure.tools.namespace.repl
  (:require [clojure.core.typed.deps.clojure.tools.namespace.track :as track]
            [clojure.core.typed.deps.clojure.tools.namespace.dir :as dir]
            [clojure.core.typed.deps.clojure.tools.namespace.reload :as reload]))

(defonce ^:private refresh-tracker (track/tracker))

(defonce ^:private refresh-dirs [])

(defn- print-and-return [tracker]
  (if-let [e (::reload/error tracker)]
    (do (when (thread-bound? #'*e)
          (set! *e e))
        (prn :error-while-loading (::reload/error-ns tracker))
        e)
    :ok))

(defn- print-pending-reloads [tracker]
  (prn :reloading (::track/load tracker)))

(defn- load-disabled? [sym]
  (false? (::load (meta (find-ns sym)))))

(defn- unload-disabled? [sym]
  (or (false? (::unload (meta (find-ns sym))))
      (load-disabled? sym)))

(defn- remove-disabled [tracker]
  (-> tracker
      (update-in [::track/unload] #(remove unload-disabled? %))
      (update-in [::track/load] #(remove load-disabled? %))))

(defn- referred
  "Given a Namespace object, returns a map of symbols describing the
  Vars it refers from other namespaces, in the following form:

     {other-namespace-name {symbol-in-other-ns symbol-in-this-ns}}"
  [ns]
  (reduce (fn [m [sym var]]
            (let [ns-name (ns-name (:ns (meta var)))
                  var-name (:name (meta var))]
              (assoc-in m [ns-name var-name] sym)))
          {}
          (ns-refers ns)))

(defn- aliased
  "Given a namespace object, returns a map of symbols describing its
  aliases, in the following form:

      {alias-symbol namespace-name}"
  [ns]
  (reduce (fn [m [alias n]] (assoc m alias (ns-name n)))
          {} (ns-aliases ns)))

(defn- recover-ns
  "Given the maps returned by 'referred' and 'aliased', attempts to
  restore as many bindings as possible into the current namespace. Any
  bindings to namespaces or Vars which do not currently exist will be
  ignored."
  [refers aliases]
  (doseq [[ns-name symbol-map] refers]
    (when-let [ns (find-ns ns-name)]
      (doseq [[source-name target-name] symbol-map]
        (when (ns-resolve ns source-name)
          (if (= source-name target-name)
            (refer ns-name :only (list source-name))
            (refer ns-name :only () :rename {source-name target-name}))))))
  (doseq [[alias ns-name] aliases]
    (when (find-ns ns-name)
      (alias alias ns-name))))

(defn- do-refresh [scan-fn after-sym]
  (when after-sym
    (assert (symbol? after-sym) ":after value must be a symbol")
    (assert (namespace after-sym)
            ":after value must be a namespace-qualified symbol"))
  (let [current-ns-name (ns-name *ns*)
        current-ns-refers (referred *ns*)
        current-ns-aliases (aliased *ns*)]
    (alter-var-root #'refresh-tracker
                    #(apply scan-fn % refresh-dirs))
    (alter-var-root #'refresh-tracker remove-disabled)
    (print-pending-reloads refresh-tracker)
    (alter-var-root #'refresh-tracker reload/track-reload)
    (in-ns current-ns-name)
    (let [result (print-and-return refresh-tracker)]
      (if (= :ok result)
        (if after-sym
          ((ns-resolve *ns* after-sym))
          result)
        ;; There was an error, recover as much as we can:
        (do (when-not (or (false? (::unload (meta *ns*)))
                          (false? (::load (meta *ns*))))
              (recover-ns current-ns-refers current-ns-aliases))
            ;; Return the Exception to the REPL:
            result)))))

(defn disable-unload!
  "Adds metadata to namespace (or *ns* if unspecified) telling
  'refresh' not to unload it. The namespace may still be reloaded, it
  just won't be removed first.

  Warning: Aliases to reloaded namespaces will break."
  ([] (disable-unload! *ns*))
  ([namespace] (alter-meta! namespace assoc ::unload false)))

(defn disable-reload!
  "Adds metadata to namespace (or *ns* if unspecified) telling
  'refresh' not to load it. Implies disable-unload! also.

  Warning: Aliases to reloaded namespaces will break."
  ([] (disable-reload! *ns*))
  ([namespace] (alter-meta! namespace assoc ::load false)))

(defn refresh
  "Scans source code directories for files which have changed (since
  the last time this function was run) and reloads them in dependency
  order. Returns :ok or an error; sets the latest exception to
  clojure.core/*e (if *e is thread-bound).

  The directories to be scanned are controlled by 'set-refresh-dirs';
  defaults to all directories on the Java classpath.

  Options are key-value pairs. Valid options are:

      :after   Namespace-qualified symbol naming a zero-argument
               function to be invoked after a successful refresh. This
               symbol will be resolved *after* all namespaces have
               been reloaded."
  [& options]
  (let [{:keys [after]} options]
    (do-refresh dir/scan after)))

(defn refresh-all
  "Scans source code directories for all Clojure source files and
  reloads them in dependency order.

  The directories to be scanned are controlled by 'set-refresh-dirs';
  defaults to all directories on the Java classpath.

  Options are key-value pairs. Valid options are:

      :after   Namespace-qualified symbol naming a zero-argument
               function to be invoked after a successful refresh. This
               symbol will be resolved *after* all namespaces have
               been reloaded."
  [& options]
  (let [{:keys [after]} options]
    (do-refresh dir/scan-all after)))

(defn set-refresh-dirs
  "Sets the directories which are scanned by 'refresh'. Supports the
  same types as clojure.java.io/file."
  [& dirs]
  (alter-var-root #'refresh-dirs (constantly dirs)))

(defn clear
  "Clears all state from the namespace/file tracker. This may help
  repair the namespace tracker when it gets into an inconsistent
  state, without restarting the Clojure process. The next call to
  'refresh' will reload all source files, but may not completely
  remove stale code from deleted files."
  []
  (alter-var-root #'refresh-tracker (constantly (track/tracker))))

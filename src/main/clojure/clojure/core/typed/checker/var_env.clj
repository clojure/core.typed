;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.var-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.indirect-utils :as indu]
            [clojure.core.typed.checker.indirect-ops :as ind]
            [clojure.core.typed.env :as env]
            [clojure.core.typed.checker.name-env :as name-env]
            [clojure.set :as set]))

(defn clj-var-annotations []
  (get @(impl/clj-checker) impl/current-var-annotations-kw {}))

(defn clj-nocheck-var? []
  (get @(impl/clj-checker) impl/current-nocheck-var?-kw {}))

(defn clj-used-vars []
  (get @(impl/clj-checker) impl/current-used-vars-kw {}))

(def var-annotations-con (con/hash-c? (every-pred symbol? namespace) (some-fn delay? r/Type?)))
(def nocheck-var-con (con/set-c? (every-pred symbol? namespace)))
(def used-vars-con (con/set-c? (every-pred symbol? namespace)))
(def checked-var-defs-con (con/set-c? (every-pred symbol? namespace)))
(def cljs-jsvar-annotations-con (con/hash-c? symbol? r/Type?))

(defmacro with-lexical-env [env & body]
  `(binding [vs/*lexical-env* ~env]
     ~@body))

(defn var-annotations []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/current-var-annotations-kw {}))

(def var-no-checks impl/var-no-checks)

(defn used-vars []
  {:post [(set? %)]}
  (get (env/deref-checker) impl/current-used-vars-kw #{}))

(defn checked-vars []
  {:post [(set? %)]}
  (get (env/deref-checker) impl/current-checked-var-defs-kw #{}))

(defn untyped-var-annotations []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/untyped-var-annotations-kw {}))

(defn add-var-type [sym type]
  (when-let [old-t ((var-annotations) sym)]
    ;; if old type is realized, it's probably been
    ;; used. We should force the new type to ensure
    ;; it's the same.
    (when (and (delay? old-t)
               (realized? old-t))
      (when (not= (force old-t) (force type))
        (println (str "WARNING: Duplicate var annotation: " sym))
        (flush))))
  (env/swap-checker! assoc-in [impl/current-var-annotations-kw sym] type)
  nil)

(def add-untyped-var impl/add-untyped-var)

(def check-var? impl/check-var?)

(defn checked-var-def? [sym]
  (contains? (checked-vars) sym))

(defn used-var? [sym]
  (contains? (used-vars) sym))

(def add-nocheck-var impl/add-nocheck-var)
(def remove-nocheck-var impl/remove-nocheck-var)

(defn add-used-var [sym]
  (env/swap-checker! update impl/current-used-vars-kw (fnil conj #{}) sym)
  nil)

(defn add-checked-var-def [sym]
  (env/swap-checker! update impl/current-checked-var-defs-kw (fnil conj #{}) sym)
  nil)

(defn vars-with-unchecked-defs []
  (set/difference (used-vars)
                  (checked-vars)
                  (var-no-checks)))

(defn reset-current-var-annotations! [m]
  (env/swap-checker! assoc impl/current-var-annotations-kw m)
  nil)

(defn merge-current-var-annotations! [m]
  {:pre [(map? m)]}
  (env/swap-checker! update impl/current-var-annotations-kw merge m)
  nil)

(defn reset-current-nocheck-var?! [nocheck]
  (env/swap-checker! assoc impl/current-nocheck-var?-kw nocheck)
  nil)

(defn merge-current-nocheck-var?! [nocheck]
  (env/swap-checker! update impl/current-nocheck-var?-kw (fnil into #{}) nocheck)
  nil)

(defn reset-current-used-vars! [s]
  (env/swap-checker! assoc impl/current-used-vars-kw s)
  nil)

(defn reset-current-checked-var-defs! [s]
  (env/swap-checker! assoc impl/current-checked-var-defs-kw s)
  nil)

(defn reset-var-type-env! [m nocheck]
  (reset-current-var-annotations! m)
  (reset-current-nocheck-var?! nocheck)
  (reset-current-used-vars! #{})
  (reset-current-checked-var-defs! #{})
  nil)

(defn refresh-var-type-env! [m nocheck]
  (merge-current-var-annotations! m)
  (merge-current-nocheck-var?! nocheck)
  (reset-current-used-vars! #{})
  (reset-current-checked-var-defs! #{})
  nil)

(defn reset-jsvar-type-env! [m]
  {:pre [(cljs-jsvar-annotations-con m)]
   :post [(nil? %)]}
  (env/swap-checker! assoc impl/cljs-jsvar-annotations-kw m)
  nil)

(defn jsvar-annotations []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/cljs-jsvar-annotations-kw {}))

(defn lookup-Var-nofail [nsym]
  {:post [((some-fn nil? r/Type?) %)]}
  (or (let [e (var-annotations)]
        (force (e nsym)))
      (when (impl/checking-clojurescript?)
        (or (nsym (name-env/name-env))
            ((jsvar-annotations) nsym)))))

(defn lookup-Var [nsym]
  {:post [((some-fn nil? r/Type?) %)]}
  (if-let [t (lookup-Var-nofail nsym)]
    t
    (err/int-error
      (str "Untyped var reference: " nsym))))

(defn type-of-nofail [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn nil? r/Type?) %)]}
  (or (when (and (not (namespace sym))
                 (not-any? #{\.} (str sym)))
        (lex/lookup-local sym))
      (lookup-Var-nofail sym)))

(indu/add-indirection ind/type-of-nofail type-of-nofail)

(defn type-of [sym]
  {:pre [(symbol? sym)]
   :post [(r/Type? %)]}
  (if-let [t (type-of-nofail sym)]
    t
    (err/int-error (str (when vs/*current-env*
                          (str (:line vs/*current-env*) ": "))
                        "Reference to untyped binding: " sym
                        "\nHint: Add the annotation for " sym
                        " via check-ns or cf"))))

(defn get-untyped-var [nsym sym]
  {:pre [(symbol? nsym)
         (symbol? sym)]
   :post [(or (nil? %)
              (r/Type? %))]}
  (some-> (untyped-var-annotations)
          (get nsym)
          (get sym)
          force))

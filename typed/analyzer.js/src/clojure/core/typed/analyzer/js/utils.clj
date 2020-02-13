;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from tools.analyzer.js
(ns clojure.core.typed.analyzer.js.utils
  (:require [clojure.string :as s]
            [clojure.tools.analyzer.utils :refer [-source-info]]
            [clojure.java.io :as io])
  (:import java.io.File
           java.net.URL))

(defn desugar-macros [{:keys [require] :as ns-opts}]
  (let [sugar-keys #{:include-macros :refer-macros}]
    (reduce-kv (fn [ns-opts ns opts]
                 (if (seq (select-keys opts sugar-keys))
                   (-> ns-opts
                     (update-in [:require] assoc ns (apply dissoc opts sugar-keys))
                     (update-in [:require-macros] assoc ns (select-keys opts #{:refer-macros :as})))
                   ns-opts))
               ns-opts require)))

;;TODO: assumes the libspecs are valid, crashes otherwise
;; needs to validate them
(defn desugar-use [{:keys [use use-macros] :as ns-opts}]
  (let [ns-opts (reduce (fn [ns-opts [lib only syms]]
                          (update-in ns-opts [:require] assoc lib {:refer syms}))
                        ns-opts use)]
    (reduce (fn [ns-opts [lib only syms]]
              (update-in ns-opts [:require-macros] assoc lib {:refer syms}))
            ns-opts use)))

(defn desugar-import [imports]
  (reduce (fn [imports import]
            (if (symbol? import)
              (let [s (s/split (name import) #"\.")]
                (assoc imports (symbol (s/join "." (butlast s))) #{(symbol (last s))}))
              (assoc imports (first import) (set (rest import)))))
          {} imports))

(defn mapify-ns-specs [ns-opts form env]
  (reduce (fn [m [k & specs]]
            (when (get m k)
              (throw (ex-info (str "Only one " k " form is allowed per namespace definition")
                              (merge {:form form}
                                     (-source-info form env)))))
            (case k
              :refer-clojure
              (assoc m k (apply hash-map specs))
              :import
              (assoc m k (desugar-import specs))

              (assoc m k (reduce (fn [m s]
                                   (if (sequential? s)
                                     (assoc m (first s) (apply hash-map (rest s)))
                                     (assoc m s {}))) {} specs)))) {} ns-opts))

;; desugars :include-macros/:refer-mcros into :require/:require-macros
;; and :use/:use-macros into :require/:require-macros
(defn desugar-ns-specs [ns-opts form env]
  (-> ns-opts
    (mapify-ns-specs form env)
    desugar-macros
    desugar-use))

;; TODO: validate
(defn validate-ns-specs [ns-opts form env]
  (when-let [invalid (seq (dissoc ns-opts :require :require-macros :import :refer-clojure))]
    (throw (ex-info (str "Unsupported ns spec(s): " invalid)
                    (merge {:form form}
                           (-source-info form env))))))

(defn source-path [x]
  (if (instance? File x)
    (.getAbsolutePath ^File x)
    (str x)))

(defn ns->relpath [s]
  (str (s/replace (munge (str s)) \. \/) ".cljs"))

(defn ns-resource [ns]
  (let [f (ns->relpath ns)]
   (cond
    (instance? File f) f
    (instance? URL f) f
    (re-find #"^file://" f) (URL. f)
    :else (io/resource f))))

(defn res-path [res]
  (if (instance? File res)
    (.getPath ^File res)
    (.getPath ^URL res)))


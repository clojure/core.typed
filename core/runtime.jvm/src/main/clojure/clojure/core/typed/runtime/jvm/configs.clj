;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; mostly copied from clojure.core's data-reader discovery impl
(ns ^:skip-wiki clojure.core.typed.runtime.jvm.configs
  "Alpha - wip, subject to change"
  (:import [clojure.lang LineNumberingPushbackReader]
           [java.io InputStreamReader]
           [java.net URL]))

(defn- config-urls []
  (let [cl (.. Thread currentThread getContextClassLoader)]
    (concat
      (enumeration-seq (.getResources cl "typedclojure_config.clj"))
      (enumeration-seq (.getResources cl "typedclojure_config.cljc")))))

(defn- load-config-files [^URL url]
  (with-open [rdr (LineNumberingPushbackReader.
                    (InputStreamReader.
                      (.openStream url) "UTF-8"))]
    (binding [*file* (.getFile url)]
      (let [read-opts (if (.endsWith (.getPath url) "cljc")
                        {:eof nil :read-cond :allow}
                        {:eof nil})
            new-config (read read-opts rdr)]
        (when (not (map? new-config))
          (throw (ex-info (str "Not a valid Typed Clojure config map")
                          {:url url})))
        new-config))))

(defn- load-configs []
  (reduce (fn [configs url]
            (conj configs (load-config-files url)))
          #{} (config-urls)))

(def *configs 
  (delay (load-configs)))

(defn register-config-anns []
  (run! (fn [{:keys [ann]}]
          (run! require ann))
        @*configs))

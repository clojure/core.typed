(ns clojure.core.typed.test.edn-parser
  "Tools for loading runtime settings."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.core.typed :as t])
  (:import (java.io PushbackReader Reader)))


(t/ann clojure.java.io/reader [t/Any -> Reader])
;FIXME needs more cases
(t/ann clojure.edn/read [PushbackReader -> t/Any])

(t/defalias Settings (t/Map t/Kw t/Any))

(t/ann defaults Settings)
(def ^:private defaults
  "A set of default settings values"
  {})

(t/ann settings (t/Atom1 (t/U Settings nil)))
(def ^:private settings
  "This atom hosts the loaded settings."
  (atom nil))

(t/ann typed-read [PushbackReader -> Settings])
(t/tc-ignore
(defn typed-read [r]
  (edn/read r))
  )

(defn- load-settings
  "load the settings map for this application"
  []
  (with-open
    [r (PushbackReader. (io/reader
                          (or (System/getenv "KODIAK_SETTINGS")
                              "settings.edn")))]    
    (reset! settings (typed-read r))))

(t/ann load-settings [-> t/Any])

(t/ann read-setting [clojure.lang.Keyword -> t/Any])
(defn read-setting
  "reads a setting from the configuration file (defined by the environment
   variable KODIAK_SETTINGS), loading it if necessary."
  [skey]
  (when-not @settings (load-settings))
  (or (when-let [s @settings] (get s skey))
      (get defaults skey)))

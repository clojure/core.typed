(ns clojure.core.typed.test.edn-parser
  "Tools for loading runtime settings."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.core.typed :refer [;types
                                        Atom1
                                        ;vars
                                        cf ann ann-form tc-ignore def-alias check-ns]])
  (:import [java.io PushbackReader
            Reader]
           (clojure.lang Keyword APersistentMap)))


(ann clojure.java.io/reader [Any -> Reader])
;FIXME needs more cases
(ann clojure.edn/read [PushbackReader -> Any])

(def-alias Settings (APersistentMap Keyword Any))

(ann defaults Settings)
(def ^:private defaults
  "A set of default settings values"
  {})

(ann settings (Atom1 (U Settings nil)))
(def ^:private settings
  "This atom hosts the loaded settings."
  (atom nil))

(ann typed-read [PushbackReader -> Settings])
(tc-ignore
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

(ann load-settings [-> Any])

(ann read-setting [clojure.lang.Keyword -> Any])
(defn read-setting
  "reads a setting from the configuration file (defined by the environment
   variable KODIAK_SETTINGS), loading it if necessary."
  [skey]
  (when-not @settings (load-settings))
  (or (when-let [s @settings] (get s skey))
      (get defaults skey)))

(ns clojure.core.typed.test.CTYP-101-mapentry
  (:require [clojure.core.typed :as typed :refer [ann Keyword Int Option]]))

(ann f [(clojure.lang.IPersistentMap Keyword Int) -> (Option (clojure.lang.IMapEntry Keyword Int))])
(defn f [m] (first m))

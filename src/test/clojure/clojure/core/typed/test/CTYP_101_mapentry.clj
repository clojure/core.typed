(ns clojure.core.typed.test.CTYP-101-mapentry
  (:require [clojure.core.typed :as typed :refer [ann]]))

(ann f [(clojure.lang.IPersistentMap t/Keyword t/Int) -> (t/Option (clojure.lang.IMapEntry t/Keyword t/Int))])
(defn f [m] (first m))

(ns clojure.core.typed.contract-ann
  (:require [clojure.core.typed :as t]))

(t/ann ^:no-check clojure.core.typed.contract-utils/nat? [Any * -> Boolean])
(t/ann ^:no-check clojure.core.typed.contract-utils/hash-c? [[Any -> Any] [Any -> Any] -> [Any -> Any]])
;can't express alternating args
(t/ann ^:no-check clojure.core.typed.contract-utils/hmap-c? [Any * -> [Any -> Any]])
(t/ann ^:no-check clojure.core.typed.contract-utils/set-c? [[Any -> Any] -> [Any -> Any]])
(t/ann ^:no-check clojure.core.typed.contract-utils/every-c? [[Any -> Any] -> [(U nil (t/Seqable Any)) -> Any]])

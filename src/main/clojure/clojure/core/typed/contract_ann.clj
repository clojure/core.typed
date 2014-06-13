(ns clojure.core.typed.contract-ann
  (:require [clojure.core.typed :as t]))

(t/ann ^:no-check clojure.core.typed.contract-utils/nat? [t/Any * -> Boolean])
(t/ann ^:no-check clojure.core.typed.contract-utils/hash-c? [[t/Any -> t/Any] [t/Any -> t/Any] -> [t/Any -> t/Any]])
;can't express alternating args
(t/ann ^:no-check clojure.core.typed.contract-utils/hmap-c? [t/Any * -> [t/Any -> t/Any]])
(t/ann ^:no-check clojure.core.typed.contract-utils/set-c? [[t/Any -> t/Any] -> [t/Any -> t/Any]])
(t/ann ^:no-check clojure.core.typed.contract-utils/every-c? [[t/Any -> t/Any] -> [(t/U nil (t/Seqable t/Any)) -> t/Any]])

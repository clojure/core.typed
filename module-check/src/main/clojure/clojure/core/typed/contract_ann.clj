;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.contract-ann
  (:require [clojure.core.typed :as t]))

(t/ann ^:no-check clojure.core.typed.contract-utils/nat? [t/Any * -> Boolean])
(t/ann ^:no-check clojure.core.typed.contract-utils/hash-c? [[t/Any -> t/Any] [t/Any -> t/Any] -> [t/Any -> t/Any]])
;can't express alternating args
(t/ann ^:no-check clojure.core.typed.contract-utils/hmap-c? [t/Any * -> [t/Any -> t/Any]])
(t/ann ^:no-check clojure.core.typed.contract-utils/set-c? [[t/Any -> t/Any] -> [t/Any -> t/Any]])
(t/ann ^:no-check clojure.core.typed.contract-utils/every-c? [[t/Any -> t/Any] -> [(t/U nil (t/Seqable t/Any)) -> t/Any]])

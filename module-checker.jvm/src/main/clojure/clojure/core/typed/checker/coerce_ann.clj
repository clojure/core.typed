;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.coerce-ann
  (:require [clojure.core.typed :as t]
            clojure.core.typed.coerce-utils))

(t/ann ^:no-check clojure.core.typed.coerce-utils/Class->symbol [Class -> t/Sym])
(t/ann ^:no-check clojure.core.typed.coerce-utils/symbol->Class [t/Sym -> Class])

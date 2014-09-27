(ns ^:skip-wiki clojure.core.typed.coerce-ann
  (:require [clojure.core.typed :as t]
            clojure.core.typed.coerce-utils))

(t/ann ^:no-check clojure.core.typed.coerce-utils/Class->symbol [Class -> t/Sym])
(t/ann ^:no-check clojure.core.typed.coerce-utils/symbol->Class [t/Sym -> Class])

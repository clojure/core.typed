(ns clojure.core.typed.base-env-cljs
  (:require [clojure.core.typed.base-env-helper-cljs :as h]
            [clojure.core.typed.current-impl :as impl :refer [v]]))

(def init-var-env
  (merge
    (h/var-mappings

;internal vars
cljs.core.typed/ann* [Any Any -> Any]
cljs.core.typed/ann-protocol* [Any Any Any -> Any]

cljs.core/+ [number * -> number]
      )))

(def init-var-nochecks
  (set (keys init-var-env)))

(def init-protocol-env 
  (h/protocol-mappings
    
cljs.core/ISeqable [[[x :variance :covariant]]]
cljs.core/ISeq [[[x :variance :covariant]]]
cljs.core/IMap [[[k :variance :covariant]
                 [v :variance :covariant]]]
cljs.core/ISet [[[x :variance :covariant]]]
cljs.core/IVector [[[x :variance :covariant]]]
cljs.core/IList [[[x :variance :covariant]]]
    
    ))

(def init-declared-kinds {})

(defn reset-cljs-envs! []
  (impl/with-cljs-impl
    ((v 'clojure.core.typed.var-env/reset-var-type-env!)
     init-var-env init-var-nochecks)
    ((v 'clojure.core.typed.protocol-env/reset-protocol-env!) 
     init-protocol-env)
    ((v 'clojure.core.typed.declared-kind-env/reset-declared-kinds!) 
     init-declared-kinds))
  nil)

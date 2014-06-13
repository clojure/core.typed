(ns ^:skip-wiki clojure.core.typed.nilsafe-utils
  (:require [clojure.set :as set]
            [clojure.core.typed :as t]))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
)

(t/ann ^:no-check set-union 
       (t/All [x] 
              (t/IFn [-> (t/Set x)]
                        [(t/U nil (t/Set x)) -> (t/Set x)]
                        [(t/U nil (t/Set x)) (t/Set x) * -> (t/Set x)])))
(def set-union (fnil set/union #{}))

(t/ann ^:no-check set-difference 
       (t/All [x] 
              (t/IFn [(t/U nil (t/Set x)) -> (t/Set x)]
                        [(t/U nil (t/Set x)) (t/Set t/Any) * -> (t/Set x)])))
(def set-difference (fnil set/difference #{}))

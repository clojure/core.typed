(in-ns 'clojure.core.typed)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DataType Ancestor Env

(defonce DATATYPE-ANCESTOR-ENV (atom {}))
(set-validator! DATATYPE-ANCESTOR-ENV (hash-c? (every-pred symbol? #(some #{\.} (str %)))
                                               (set-c? Type?)))

(defn add-datatype-ancestors [sym tset]
  (swap! DATATYPE-ANCESTOR-ENV update-in [sym] #(set/union (or % #{}) tset))
  nil)

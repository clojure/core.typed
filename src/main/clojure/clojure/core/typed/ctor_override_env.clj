(in-ns 'clojure.core.typed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor Override Env

(defonce CONSTRUCTOR-OVERRIDE-ENV 
  (atom {}
        :validator (hash-c? symbol? Type?)))

(defn add-constructor-override [sym t]
  (swap! CONSTRUCTOR-OVERRIDE-ENV assoc sym t)
  nil)



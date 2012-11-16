
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Param nilables

(defonce METHOD-PARAM-NILABLE-ENV (atom {}))
(set-validator! METHOD-PARAM-NILABLE-ENV (hash-c? (every-pred namespace symbol?)
                                                  (hash-c? (some-fn #(= :all %) nat?)
                                                           (some-fn #(= :all %) (set-c? nat?)))))

(defn add-method-nilable-param [sym a]
  (swap! METHOD-PARAM-NILABLE-ENV assoc sym a)
  nil)

(defn nilable-param? [sym arity param]
  (boolean 
    (when-let [nilables (@METHOD-PARAM-NILABLE-ENV sym)]
      (when-let [params (or (nilables :all)
                            (nilables arity))]
        (or (= :all params)
            (params param))))))


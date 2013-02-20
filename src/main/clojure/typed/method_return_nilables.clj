(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Return non-nilables

(defonce METHOD-RETURN-NONNILABLE-ENV (atom {}))
(set-validator! METHOD-RETURN-NONNILABLE-ENV (hash-c? (every-pred namespace symbol?)
                                                      (some-fn #(= :all %)
                                                               (set-c? nat?))))

(defn add-nonnilable-method-return [sym m]
  (swap! METHOD-RETURN-NONNILABLE-ENV assoc sym m)
  nil)

(defn nonnilable-return? [sym arity]
  (let [as (@METHOD-RETURN-NONNILABLE-ENV sym)]
    (boolean (or (= :all as)
                 (when as
                   (as arity))))))

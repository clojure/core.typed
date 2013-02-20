(in-ns 'typed.core)

;; Environment for storing multimethod types and inferred filters

; (Atom (Seqable (IPersistentMap Symbol '{:fn-type Type, :dispatch-result (U nil Type)})))
(defonce MULTIMETHOD-DISPATCH-ENV (atom {}
                                        :validator (hash-c?
                                                     (every-pred symbol? namespace)
                                                     Type?)))

; [Symbol Filter -> nil]
(defn add-multimethod-dispatch-type
  "Add the type of the dispatch function of the multimethod named by mmsym
  to the environment. If already exists, must be identical."
  [mmsym dtype]
  {:pre [(symbol? mmsym)
         (Type? dtype)]}
  (when-let [old (@MULTIMETHOD-DISPATCH-ENV mmsym)]
    (assert (= old dtype)
            (str "Cannot assign multimethod a different dispatch result: "
                 " Old: " (unparse-type old)
                 " New: " (unparse-type dtype))))
  (swap! MULTIMETHOD-DISPATCH-ENV assoc mmsym dtype)
  nil)

(defn get-multimethod-dispatch-type [mmsym]
  {:pre [(symbol? mmsym)]
   :post [(Type? %)]}
  (let [t (@MULTIMETHOD-DISPATCH-ENV mmsym)]
    (assert t (str "Multimethod requires dispatch type: " mmsym))
    t))

(in-ns 'typed.core)

;; Environment for storing multimethod types and inferred filters

; (Atom (Seqable (IPersistentMap Symbol '{:fn-type Type, :dispatch-result (U nil Type)})))
(defonce MULTIMETHOD-ENV (atom {}
                               :validator (hash-c?
                                            (every-pred symbol? namespace)
                                            (hmap-c? :fn-type Type?
                                                     :dispatch-type (some-fn nil? Type?)))))

; [Symbol FnIntersection -> nil]
(defn add-multimethod-fn-type 
  "Add the actual function type for a multimethod to the environment.
  If already exists, must be identical."
  [mmsym fnt]
  {:pre [(symbol? mmsym)
         (Type? fnt)]}
  (when-let [old-info (@MULTIMETHOD-ENV mmsym)]
    (assert (= (:fn-type old-info) fnt)
            (str "Cannot assign multimethod a different type")))
  (swap! MULTIMETHOD-ENV update-in [mmsym] #(hash-map :fn-type fnt
                                                      :dispatch-type (:dispatch-type %)))
  nil)

(defn get-multimethod-fn-type [mmsym]
  {:post [(Type? %)]}
  (let [t (:fn-type (@MULTIMETHOD-ENV mmsym))]
    (assert t (str "Multimethod requires annotation: " mmsym))
    t))

; [Symbol Filter -> nil]
(defn add-multimethod-dispatch-type
  "Add the type of the dispatch function of the multimethod named by mmsym
  to the environment. If already exists, must be identical."
  [mmsym dtype]
  {:pre [(symbol? mmsym)
         (Type? dtype)]}
  (when-let [old (:dispatch-type (@MULTIMETHOD-ENV mmsym))]
    (assert (= old dtype)
            (str "Cannot assign multimethod a different dispatch result: "
                 " Old: " (unparse-type old)
                 " New: " (unparse-type dtype))))
  (swap! MULTIMETHOD-ENV update-in [mmsym] #(hash-map :dispatch-type dtype
                                                      :fn-type (:fn-type %)))
  nil)

(defn get-multimethod-dispatch-type [mmsym]
  {:post [(Type? %)]}
  (let [t (:dispatch-type (@MULTIMETHOD-ENV mmsym))]
    (assert t (str "Multimethod requires dispatch type: " mmsym))
    t))

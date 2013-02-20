(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Name Env

(def declared-name-type ::declared-name)
(def protocol-name-type ::protocol-name)
(def datatype-name-type ::datatype-name)

(def temp-binding ::temp-binding)

(doseq [k [declared-name-type protocol-name-type datatype-name-type]]
  (derive k temp-binding))

(defonce TYPE-NAME-ENV (atom {}))
(set-validator! TYPE-NAME-ENV #(and (every? (every-pred (some-fn namespace 
                                                                 (fn [k] (some (fn [a] (= \. a)) (str k))))
                                                        symbol?) 
                                            (keys %))
                                    (every? (some-fn Type? (fn [a] (isa? a temp-binding)))
                                            (vals %))))

(defn add-type-name [sym ty]
  (swap! TYPE-NAME-ENV assoc sym (if (Type? ty)
                                   (vary-meta ty assoc :from-name sym)
                                   ty))
  nil)

(defn declare-name* [sym]
  {:pre [(symbol? sym)
         (namespace sym)]}
  (add-type-name sym declared-name-type)
  nil)

(defn declare-protocol* [sym]
  {:pre [(symbol? sym)
         (some #(= \. %) (str sym))]}
  (add-type-name sym protocol-name-type)
  nil)

(defn declare-datatype* [sym]
  (add-type-name sym datatype-name-type)
  nil)

(defn- resolve-name* [sym]
  (let [t (@TYPE-NAME-ENV sym)]
    (cond
      (= protocol-name-type t) (resolve-protocol sym)
      (= datatype-name-type t) (resolve-datatype sym)
      (= declared-name-type t) (throw (IllegalArgumentException. (str "Reference to declared but undefined name " sym)))
      (Type? t) (vary-meta t assoc :source-Name sym)
      :else (throw (IllegalArgumentException. (error-msg "Cannot resolve name " sym))))))

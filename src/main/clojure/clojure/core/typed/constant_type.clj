(in-ns 'clojure.core.typed)

;[Any -> Type]
(defmulti constant-type class)

(defmethod constant-type nil [_] -nil)
(defmethod constant-type Class [v] (-val v))
(defmethod constant-type Symbol [v] (-val v))
(defmethod constant-type Long [v] (-val v))
(defmethod constant-type Double [v] (-val v))
(defmethod constant-type Integer [v] (-val v))
(defmethod constant-type java.math.BigDecimal [v] (-val v))
(defmethod constant-type clojure.lang.BigInt [v] (-val v))
(defmethod constant-type String [v] (-val v))
(defmethod constant-type Character [v] (-val v))
(defmethod constant-type clojure.lang.Keyword [v] (-val v))
(defmethod constant-type java.util.regex.Pattern [v] (RClass-of java.util.regex.Pattern))

(defmethod constant-type Boolean [v] (if v -true -false))
(defmethod constant-type PersistentHashSet [v] (RClass-of PersistentHashSet [(apply Un (map constant-type v))]))

;nothing specific, Cons seems like an implementation detail
(defmethod constant-type Cons [v] (RClass-of Seqable [(apply Un (map constant-type v))]))

(defmethod constant-type IPersistentList
  [clist]
  (->HeterogeneousList (apply list (map constant-type clist))))

(defmethod constant-type IPersistentVector
  [cvec]
  (-hvec (mapv constant-type cvec)))

(defmethod constant-type IPersistentMap
  [cmap]
  (let [kts (map constant-type (keys cmap))
        vts (map constant-type (vals cmap))]
    (if (every? Value? kts)
      (-complete-hmap (zipmap kts vts))
      (RClass-of IPersistentMap 
                 [(apply Un kts)
                  (apply Un vts)]))))


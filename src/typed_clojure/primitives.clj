(ns typed-clojure.primitives)

(defmacro new-type [& body]
  `(def ~@body))

(def union vector)

(def type-db (atom {}))

(defmacro deftypeT [nme flds & body]
  `(do
     (deftype ~nme ~(vec (map first flds)) ~@body)
     (swap! type-db (assoc @type-db (resolve '~nme)
                           '~(apply merge (map (fn [[n _ t]] {n t}) flds))))))

(defmacro +T [nme type]
  `(do
     (declare ~nme)
     (swap! type-db (assoc @type-db (resolve '~nme) ~type))))

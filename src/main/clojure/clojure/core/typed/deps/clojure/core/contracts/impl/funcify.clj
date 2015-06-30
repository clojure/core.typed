(ns clojure.core.typed.deps.clojure.core.contracts.impl.funcify)

(declare funcify*)
(declare funcify-factor)

(defn funcify
  [args cnstr]
  (vec (map #(funcify* % args) cnstr)))


(defmulti funcify* (fn [e _] (class e)))

(defmethod funcify* clojure.lang.IFn        [e args] (list* e args))
(defmethod funcify* java.util.regex.Pattern [e args] (list* 'clojure.core/re-matches e args))
(defmethod funcify* java.lang.String        [e args] (list* 'clojure.core/= e args))
(defmethod funcify* java.lang.Number        [e args] (list* 'clojure.core/= e args))
(defmethod funcify* :default                [e args] (funcify-factor e args))


;; funcify-factor

(defmulti funcify-factor (fn [[h & _] _] h))

(defmethod funcify-factor 'or
  [e args]
  (list* 'or (funcify args (rest e))))

(defmethod funcify-factor 'in
  [e args]
  (concat (list* 'in args) (rest e)))

(defmethod funcify-factor 'whitelist
  [e args]
  (concat (list* 'whitelist args) (rest e)))

(defmethod funcify-factor :default
  [e args]
    e)
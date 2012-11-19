(in-ns 'typed.core)

(defmulti check-cljs (fn [expr & [expected]] (:op expr)))

(defmethod check-cljs :constant
  [{:keys [form] :as expr} & [expected]]
  (assert (not expected))
  (assoc expr
         expr-type (ret (->Value form))))

(defmethod check-cljs :vector
  [{:keys [items] :as expr} & [expected]]
  (assert (not expected))
  (let [citems (mapv check-cljs items)]
    (assoc expr
           expr-type (ret (->HeterogeneousVector (mapv (comp ret-t expr-type) citems))))))

(defmethod check-cljs :map
  [{mkeys :keys mvals :vals :as expr} & [expected]]
  (assert (not expected))
  (let [ckeys (mapv check-cljs mkeys)
        cvals (mapv check-cljs mvals)
        ;only handle keyword keys for now
        _ (assert (every? (every-pred Value? #(keyword? (.val %)))
                          (map (comp ret-t expr-type) ckeys)))]
    (assoc expr
           expr-type (ret (->HeterogeneousMap (zipmap (map (comp ret-t expr-type) ckeys)
                                                      (map (comp ret-t expr-type) cvals)))))))

(def CLJS-VAR-ENV (atom {}))
(set-validator! CLJS-VAR-ENV (hash-c? symbol? Type?))

(defn type-of [vname]
  (let [t (@CLJS-VAR-ENV vname)]
    (if t
      t
      (throw (Exception. (str "Untyped var: " vname))))))

(defn cljs-ann* [vname tsyn]
  (let [vtype (parse-type tsyn)]
    (swap! CLJS-VAR-ENV assoc vname vtype)
    [vname (unparse-type vtype)]))

(defmacro cljs-ann [vname tsyn]
  `(cljs-ann* '~vname '~tsyn))

(defmethod check-cljs :def
  [{:keys [init] vname :name :as expr} & [expected]]
  (assert init "declare NYI")
  (assert (not expected))
  (let [ann-type (type-of vname)
        cinit (check-cljs init expected)
        _ (assert (subtype? (-> cinit expr-type ret-t)
                            ann-type)
                  (str "Var definition did not match annotation." \n
                       " Expected: " (unparse-type ann-type) \n
                       " Actual" (unparse-type ann-type)))]
    (assoc expr
           ;FIXME should really be Var, change when protocols are implemented
           expr-type (ret -any))))

;(defmethod check-cljs :invoke
;  [{:keys [f args] :as expr} & [expected]]
;  (assert (not expected))

(comment
  ;; TODO there's a bug in the docstring for cljs.analyzer/analyze: it says :ns is a symbol, when instead it's {:name nsym}
  (def denv {:locals {} :context :expr :ns {:name 'user}})

(cljs/analyze denv 1)
  (cf-cljs 1)

(cljs/analyze denv [1])
  (cf-cljs [1])

(cljs/analyze denv {:a 1})
(cf-cljs {:a 1})

(cljs-ann user/a Any)
  (@CLJS-VAR-ENV 'user/a)

(cljs/analyze denv '(def a 1))
(cf-cljs (def a 1))

(cljs/analyze denv '(1))
(cf-cljs (1))
  )

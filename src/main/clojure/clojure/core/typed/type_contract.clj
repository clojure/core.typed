(ns clojure.core.typed.type-contract
  (:require [clojure.core.typed.parse-ast :as ast]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.set :as set]))

(defn keyword-singleton? [{:keys [op val]}]
  (when ('#{:singleton} op)
    (keyword? val)))

(defn ast->pred 
  "Returns syntax representing a runtime predicate on the
  given type astkj."
  [t]
  (letfn [(gen-inner [{:keys [op] :as t} arg]
            (cond
              ('#{:F} op) (err/int-error "Cannot generate predicate for free variable")
              ('#{:Poly} op) (err/int-error "Cannot generate predicate for polymorphic type")
              ('#{:PolyDots} op) (err/int-error "Cannot generate predicate for dotted polymorphic type")
              ('#{:Fn} op) (err/int-error "Cannot generate predicate for function type")
              ('#{:Class} op) `(instance? ~(:name t) ~arg)
              ('#{:Name} op) 
                (impl/impl-case
                  :clojure
                    (let [a (@impl/alias-env (:name t))]
                      (assert a (str "No alias for " (:name t)))
                      (gen-inner a arg))
                  :cljs (assert nil))
;              (cond
;                              (empty? (:poly? t)) `(instance? ~(:the-class t) ~arg)
;                              :else (err/int-error (str "Cannot generate predicate for polymorphic Class")))
              ('#{:Any} op) `true
              ('#{:U} op) `(or ~@(mapv gen-inner (:types t) (repeat arg)))
              ('#{:I} op) `(and ~@(mapv gen-inner (:types t) (repeat arg)))
              ('#{:HVec} op) `(and (vector? ~arg)
                                   ~(cond
                                      (:rest t)
                                      `(<= ~(count (:types t)) (count ~arg))
                                      (:drest t)
                                      (err/int-error (str "Cannot generate predicate for dotted HVec"))
                                      :else
                                      `(== ~(count (:types t)) (count ~arg)))
                                   ~@(doall
                                       (map-indexed 
                                         (fn [i t*]
                                           (let [vlocal (gensym "vlocal")]
                                             `(let [~vlocal (nth ~arg ~i)]
                                                ~(gen-inner t* vlocal))))
                                         (:types t)))
                                   ~@(when (:rest t)
                                       (let [nfixed (count (:types t))]
                                         [`(let [rstvec# (subvec ~arg ~nfixed)]
                                             (every? ~(let [vlocal (gensym "vlocal")]
                                                        `(fn [~vlocal] 
                                                           ~(gen-inner (:rest t) vlocal)))
                                                     rstvec#))])))
              ('#{:CountRange} op) (let [cnt (gensym "cnt")]
                                     `(let [~cnt (count ~arg)]
                                        (<= ~@(let [{:keys [lower upper]} t]
                                                (concat [lower cnt]
                                                        (when upper
                                                          [upper]))))))
              ('#{:singleton} op) (let [v (:val t)]
                                    (cond
                                      (nil? v) `(nil? ~arg)
                                      (symbol? v) `(= '~v ~arg)
                                      (keyword? v) `(identical? '~v ~arg)
                                      (number? v) `(when (number? ~arg)
                                                     (== '~v ~arg))
                                      :else (err/int-error 
                                              (str "Cannot generate predicate for value type: " v))))
              ('#{:HMap} op) `(and (map? ~arg)
                                   ; check keys match
                                   ~(let [req-ks (set (keys (:mandatory t)))
                                          opt-ks (set (keys (:optional t)))
                                          absent-ks (set (:absent-keys t))
                                          _ (when-not (every? keyword-singleton? (concat req-ks absent-ks))
                                              (err/int-error 
                                                (str "HMap keys must be keywords")))]
                                      (cond
                                        (:complete? t)
                                        `(set/subset? (set (keys ~arg))
                                                      ~(set (map :val (concat req-ks opt-ks))))
                                        :else
                                        `(let [actual-ks# (set (keys ~arg))]
                                           (and 
                                             ;required keys are a subset of actual keys
                                             (set/subset? 
                                               ~req-ks
                                               actual-ks#)
                                             ;no absent-keys are present
                                             (empty?
                                               (set/intersection
                                                 ~req-ks
                                                 actual-ks#))))))
                                   ;check mandatory keys match
                                   ~@(mapv (fn [[k v]]
                                             {:pre [(keyword-singleton? k)]}
                                             (let [klocal (gensym "klocal")]
                                               `(let [[_ ~klocal :as f#] (find ~arg ~(:val k))]
                                                  (when f#
                                                    ~(gen-inner v klocal)))))
                                           (:mandatory t))
                                   ;check optional keys match
                                   ~@(mapv (fn [[k v]]
                                             {:pre [(keyword-singleton? k)]}
                                             (let [klocal (gensym "klocal")]
                                               `(let [[_ ~klocal :as f#] (find ~arg ~(:val k))]
                                                  (or (not f#)
                                                      ~(gen-inner v klocal)))))
                                           (:optional t)))

              :else (err/int-error (str op " not supported in type->pred: " (:form t)))))]
    (let [arg (gensym "arg")]
      `(fn [~arg] 
         (boolean
           ~(gen-inner t arg))))))

(defn type-syntax->pred [t]
  (-> (ast/parse t)
      ast->pred))

(comment
        (type-syntax->pred 'Any)
        (type-syntax->pred 'Nothing)
        (type-syntax->pred '(U Number Boolean))
  )

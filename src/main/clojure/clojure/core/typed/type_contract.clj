;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;flat contracts only
(ns ^:no-doc ^:skip-wiki clojure.core.typed.type-contract
  (:require [clojure.core.typed.parse-ast :as ast]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.ast-ops :as ops]
            [clojure.core.typed.contract :as con]
            ;used in contracts
            [clojure.set :as set]))

(defn keyword-singleton? [{:keys [op val]}]
  (when ('#{:singleton} op)
    (keyword? val)))

(def ^:dynamic *inside-rec* #{})

(defn ast->pred 
  "Returns syntax representing a runtime predicate on the
  given type ast."
  [t]
  (letfn [(gen-inner [{:keys [op] :as t} arg]
            (case op
              (:F) (err/int-error "Cannot generate predicate for free variable")
              (:Poly) (err/int-error "Cannot generate predicate for polymorphic type")
              (:PolyDots) (err/int-error "Cannot generate predicate for dotted polymorphic type")
              (:Fn) (err/int-error "Cannot generate predicate for function type")
              (:TApp) (let [{:keys [rator rands]} t]
                        (cond 
                          ;needs resolving
                          (#{:Name} (:op rator))
                          (gen-inner (update-in t [:rator] ops/resolve-Name) arg)
                          ;polymorphic class
                          (#{:Class} (:op rator))
                            (let [{:keys [args pred] :as rcls} (get (impl/rclass-env) (:name rator))
                                  _ (when-not rcls
                                      (err/int-error (str "Class does not take arguments: "
                                                          (:name rator))))
                                  _ (when-not (args (count rands))
                                      (err/int-error (str "Wrong number of arguments to "
                                                          (:name rator) ", expected " args
                                                          " actual " (count rands))))
                                  rands-args (repeatedly (count rands) gensym)
                                  rands-p (mapv (fn [ast gsym]
                                                  `(fn [~gsym] ~(gen-inner ast gsym))) 
                                                rands rands-args)]
                              `(and (instance? ~(:name rator) ~arg)
                                    ~(apply pred arg rands-p)))
                          ;substitute
                          (#{:TFn} (:op rator))
                          (gen-inner (ops/instantiate-TFn rator rands) arg)
                          :else
                          (err/int-error (str "Don't know how to apply type: " (:form t)))))
              (:Class) `(instance? ~(:name t) ~arg)
              (:Name) 
              (impl/impl-case
                :clojure (gen-inner (ops/resolve-Name t) arg)
                :cljs (err/int-error (str "TODO CLJS Name")))
              ;              (cond
              ;                              (empty? (:poly? t)) `(instance? ~(:the-class t) ~arg)
              ;                              :else (err/int-error (str "Cannot generate predicate for polymorphic Class")))
              (:Any) `true
              ;TODO special case for union of HMap, and unions of constants
              (:U) `(or ~@(mapv gen-inner (:types t) (repeat arg)))
              (:I) `(and ~@(mapv gen-inner (:types t) (repeat arg)))
              (:HVec) `(and (vector? ~arg)
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
              (:CountRange) (let [cnt (gensym "cnt")]
                              `(and (or (nil? ~arg)
                                        (coll? ~arg))
                                    (let [~cnt (count ~arg)]
                                      (<= ~@(let [{:keys [lower upper]} t]
                                              (concat [lower cnt]
                                                      (when upper
                                                        [upper])))))))
              (:singleton) (let [v (:val t)]
                             (cond
                               (nil? v) `(nil? ~arg)
                               (symbol? v) `(= '~v ~arg)
                               (keyword? v) `(identical? '~v ~arg)
                               ((some-fn true? false?) v) `(identical? '~v ~arg)
                               (number? v) `(when (number? ~arg)
                                              ; I think = models the type system's behaviour better than ==
                                              (= '~v ~arg))
                               :else (err/int-error 
                                       (str "Cannot generate predicate for value type: " v))))
              (:HMap) (let [mandatory (apply hash-map (:mandatory t))
                            optional (apply hash-map (:optional t))
                            absent-keys (:absent-keys t)
                            valgen (fn [tmap]
                                     (zipmap (map :val (keys tmap))
                                             (mapv (fn [tsyn gi]
                                                     `(fn [~gi]
                                                        ~(gen-inner tsyn gi)))
                                                   (vals tmap)
                                                   (repeatedly (count tmap) gensym))))]
                        `((impl/hmap-c? :mandatory ~(valgen mandatory)
                                        :optional ~(valgen optional)
                                        :absent-keys ~(set (map :val absent-keys))
                                        :complete? ~(:complete? t))
                          ~arg))
              (:Rec) (cond
                       ;we're already inside this rec
                       (contains? *inside-rec* (:unwrap-id t))
                         (let [{:keys [unwrap-id]} t]
                           `(~unwrap-id ~arg))
                       
                       :else
                        (let [unwrap-id (gensym 'Rec-id)
                              body (ops/unwrap-rec t unwrap-id)
                              garg (gensym 'garg)]
                          (binding [*inside-rec* (conj *inside-rec* unwrap-id)]
                            `((fn ~unwrap-id
                                [~garg]
                                ~(gen-inner body garg))
                              ~arg))))
              (err/int-error (str op " not supported in type->pred: " (:form t)))))]
    (let [arg (gensym "arg")]
      `(fn [~arg] 
         (boolean
           ~(gen-inner t arg))))))

(defn ast->contract 
  "Returns syntax representing a runtime predicate on the
  given type ast."
  [t]
  (letfn [(gen-inner [{:keys [op] :as t} arg]
            (case op
              (:F) (err/int-error "Cannot generate predicate for free variable")
              (:Poly) (err/int-error "Cannot generate predicate for polymorphic type")
              (:PolyDots) (err/int-error "Cannot generate predicate for dotted polymorphic type")
              (:Fn) (cond
                      (== 1 (count (:arities t)))
                      (let [{:keys [dom rng filter object flow rest drest] :as method}
                            (first (:arities t))]
                        (if (or rest drest filter object flow)
                          (err/int-error "Cannot generate predicate for this function type")
                          `(con/ifn-c ~(mapv #(gen-inner % arg) dom)
                                      ~(gen-inner rng arg))))
                      :else (err/int-error "Cannot generate predicate for function type"))
              (:TApp) (let [{:keys [rator rands]} t]
                        (cond 
                          ;needs resolving
                          (#{:Name} (:op rator))
                          (gen-inner (update-in t [:rator] ops/resolve-Name) arg)
                          ;polymorphic class
                          ;(#{:Class} (:op rator))
                          ;  (let [{:keys [args pred] :as rcls} (get (impl/rclass-env) (:name rator))
                          ;        _ (when-not rcls
                          ;            (err/int-error (str "Class does not take arguments: "
                          ;                                (:name rator))))
                          ;        _ (when-not (args (count rands))
                          ;            (err/int-error (str "Wrong number of arguments to "
                          ;                                (:name rator) ", expected " args
                          ;                                " actual " (count rands))))
                          ;        rands-args (repeatedly (count rands) gensym)
                          ;        rands-p (mapv (fn [ast gsym]
                          ;                        `(fn [~gsym] ~(gen-inner ast gsym))) 
                          ;                      rands rands-args)]
                          ;    `(and (instance? ~(:name rator) ~arg)
                          ;          ~(apply pred arg rands-p)))
                          ;substitute
                          (#{:TFn} (:op rator))
                          (gen-inner (ops/instantiate-TFn rator rands) arg)
                          :else
                          (err/int-error (str "Don't know how to apply type: " (:form t)))))
              (:Class) `(con/instance-c
                          (Class/forName ~(str (:name t))))
              (:Name) 
              (impl/impl-case
                :clojure (gen-inner (ops/resolve-Name t) arg)
                :cljs (err/int-error (str "TODO CLJS Name")))
              ;              (cond
              ;                              (empty? (:poly? t)) `(instance? ~(:the-class t) ~arg)
              ;                              :else (err/int-error (str "Cannot generate predicate for polymorphic Class")))
              (:Any) `con/any-c
              ;TODO special case for union of HMap, and unions of constants
              (:U) `(con/or-c
                      ;; TODO flatten unions, ensuring Names are resolved
                      ~@(mapv #(gen-inner % arg) (:types t)))
              (:I) `(con/and-c
                      ~@(mapv #(gen-inner % arg) (:types t)))
              ;(:HVec) `(and (vector? ~arg)
              ;              ~(cond
              ;                 (:rest t)
              ;                 `(<= ~(count (:types t)) (count ~arg))
              ;                 (:drest t)
              ;                 (err/int-error (str "Cannot generate predicate for dotted HVec"))
              ;                 :else
              ;                 `(== ~(count (:types t)) (count ~arg)))
              ;              ~@(doall
              ;                  (map-indexed 
              ;                    (fn [i t*]
              ;                      (let [vlocal (gensym "vlocal")]
              ;                        `(let [~vlocal (nth ~arg ~i)]
              ;                           ~(gen-inner t* vlocal))))
              ;                    (:types t)))
              ;              ~@(when (:rest t)
              ;                  (let [nfixed (count (:types t))]
              ;                    [`(let [rstvec# (subvec ~arg ~nfixed)]
              ;                        (every? ~(let [vlocal (gensym "vlocal")]
              ;                                   `(fn [~vlocal] 
              ;                                      ~(gen-inner (:rest t) vlocal)))
              ;                                rstvec#))])))
              (:CountRange) `(con/count-range-c ~(:lower t) ~(:upper t))
              (:singleton) (let [v (:val t)]
                             (cond
                               (nil? v) `con/nil-c
                               (symbol? v) `(con/equiv-c ~v)
                               (keyword? v) `(con/identical-c ~v)
                               ((some-fn true? false?) v) `(con/identical-c ~v)
                               (number? v) ; I think = models the type system's behaviour better than ==
                               `(con/equiv-c ~v)

                               :else (err/int-error 
                                       (str "Cannot generate predicate for value type: " v))))

              (:HMap) (let [mandatory (apply hash-map (:mandatory t))
                            optional (apply hash-map (:optional t))
                            absent-keys (:absent-keys t)
                            congen (fn [tmap]
                                     (zipmap (map :val (keys tmap))
                                             (map #(gen-inner % arg) (vals tmap))))]
                        `(con/hmap-c :mandatory ~(congen mandatory)
                                     :optional ~(congen optional)
                                     :absent-keys ~(set (map :val absent-keys))
                                     :complete? ~(:complete? t)))

              ;(:Rec) (cond
              ;         ;we're already inside this rec
              ;         (contains? *inside-rec* (:unwrap-id t))
              ;           (let [{:keys [unwrap-id]} t]
              ;             `(~unwrap-id ~arg))
              ;         
              ;         :else
              ;          (let [unwrap-id (gensym 'Rec-id)
              ;                body (ops/unwrap-rec t unwrap-id)
              ;                garg (gensym 'garg)]
              ;            (binding [*inside-rec* (conj *inside-rec* unwrap-id)]
              ;              `((fn ~unwrap-id
              ;                  [~garg]
              ;                  ~(gen-inner body garg))
              ;                ~arg))))
              (err/int-error (str op " not supported in type->pred: " (:form t)))))]
    (gen-inner t nil)))

(defn type-syntax->pred [t]
  (impl/with-impl impl/clojure
    (-> (ast/parse t)
        ast->pred)))

(defn type-syntax->contract [t]
  (impl/with-impl impl/clojure
    (-> (ast/parse t)
        ast->contract)))

(comment
        (type-syntax->pred 'Any)
        (type-syntax->pred 'Nothing)
        (type-syntax->pred '(U Number Boolean))

        (con/contract (type-syntax->contract 'nil) 1)

  (clojure.pprint/pprint (type-syntax->pred '(HMap :optional {:c Number})))
  (clojure.pprint/pprint (type-syntax->pred '(HMap :mandatory {:c Number})))
  (clojure.pprint/pprint (type-syntax->pred ''[Number]))
  (clojure.pprint/pprint (type-syntax->pred '(Rec [x] (U '[x] Number))))
  (clojure.pprint/pprint (type-syntax->pred '(clojure.core.typed/Option Number)))
  
  (walk (type-syntax->pred '(HMap :optional {:c Number}))
        (fn [e] (prn 'pre (:op e)))
        (fn [e] (prn 'post (:op e))))

  (def ast (ast/parse-clj '(HMap :optional {:c Number})))

  (:children ast)

  (ops/walk ast
            (fn f [e] (prn 'pre (:op e)))
            (fn [e] (prn 'post (:op e))))
  (ops/unwrap-rec (ast/parse-clj '(Rec [x] (U '[x] Number))) 'abc)
  )

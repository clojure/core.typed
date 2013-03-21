(set! *warn-on-reflection* true)

(in-ns 'clojure.core.typed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker

(declare ret-t ret-f ret-o)

;[TCResult -> Any]
(defn unparse-TCResult [r]
  (let [t (unparse-type (ret-t r))
        fs (unparse-filter-set (ret-f r))
        o (unparse-object (ret-o r))]
    (if (and (= (-FS -top -top) (ret-f r))
             (= (ret-o r) -empty))
      t
      (if (= (ret-o r) -empty)
        [t fs]
        [t fs o]))))

(def expr-type ::expr-type)

(defmulti check (fn [expr & [expected]]
                  {:pre [((some-fn nil? TCResult?) expected)]}
                  (:op expr)))

(defn check-expr [expr & [expected]]
  (prn "Checking line:" (-> expr :env :line))
  (check expr expected))

;[Symbol Any -> Expr]
(defn check-top-level [nsym form]
  (ensure-clojure)
  (let [ast (-> (analyze/analyze-form-in-ns nsym form)
              hygienic/ast-hy)]
    (check ast)))

(defmacro tc-t [form]
  `(do (ensure-clojure)
     (-> (check-top-level (symbol (ns-name *ns*))
                          '~form)
       expr-type)))

(defmacro tc [form]
  `(do (ensure-clojure)
     (-> (check-top-level (symbol (ns-name *ns*))
                          '~form)
       expr-type unparse-type)))

(defn flow-for-value []
  (let [props (.props ^PropEnv *lexical-env*)
        flow (-flow (apply -and -top props))]
    flow))

(defn check-value
  [{:keys [val] :as expr} & [expected]]
  (let [actual-type (constant-type val)
        _ (when expected
            (binding [*current-expr* expr]
              (subtype actual-type (ret-t expected))))
        flow (flow-for-value)]
    (assoc expr
           expr-type (if val
                       (ret actual-type
                            (-FS -top -bot)
                            -empty
                            flow)
                       (ret actual-type
                            (-FS -bot -top)
                            -empty
                            flow)))))

(defmethod check :constant [& args] (apply check-value args))
(defmethod check :number [& args] (apply check-value args))
(defmethod check :string [& args] (apply check-value args))
(defmethod check :keyword [& args] (apply check-value args))

(defmethod check :boolean
  [{:keys [val] :as expr} & [expected]]
  (assoc expr
         expr-type (if val
                     (ret -true
                          (-FS -top -bot)
                          -empty
                          (flow-for-value))
                     (ret -false
                          (-FS -bot -top)
                          -empty
                          (flow-for-value)))))

(defmethod check :nil 
  [expr & [expected]]
  (assoc expr
         expr-type (ret -nil (-FS -bot -top) -empty (flow-for-value))))

(defmethod check :map
  [{:keys [keyvals] :as expr} & [expected]]
  (let [expected (when expected 
                   (ret-t expected))
        ts (apply hash-map (map (comp ret-t expr-type) (mapv check keyvals)))

        actual (if (every? keyword-value? (keys ts))
                 (-complete-hmap ts)
                 (RClass-of APersistentMap [(apply Un (keys ts))
                                            (apply Un (vals ts))]))
        _ (assert (when expected
                    (subtype? actual expected)) (type-error actual expected))]
    (assoc expr
           expr-type (ret actual (-FS -top -bot)))))

(defmethod check :set
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (mapv check args)
        res-type (RClass-of IPersistentSet [(apply Un (mapv (comp ret-t expr-type) cargs))])
        _ (when expected
            (assert (subtype? res-type (ret-t expected))
                    (type-error res-type (ret-t expected))))]
    (assoc expr
           expr-type (ret res-type (-FS -top -bot)))))

(defmethod check :vector
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (mapv check args)
        res-type (-hvec (mapv (comp ret-t expr-type) cargs))
        _ (when expected
            (assert (subtype? res-type (ret-t expected))
                    (type-error res-type (ret-t expected))))]
    (assoc expr
           expr-type (ret res-type (-FS -top -bot)))))

(defmethod check :empty-expr 
  [{coll :coll :as expr} & [expected]]
  (assoc expr
         expr-type (ret (constant-type coll) (-FS -top -bot))))

;; check-below : (/\ (Results Type -> Result)
;;                   (Results Results -> Result)
;;                   (Type Results -> Type)
;;                   (Type Type -> Type))

;check that arg type tr1 is under expected
(defn check-below [tr1 expected]
  {:pre [((some-fn TCResult? Type?) tr1)
         ((some-fn TCResult? Type?) expected)]
   :post [((some-fn TCResult? Type?) %)]}
  (letfn [(filter-better? [{f1+ :then f1- :else :as f1}
                           {f2+ :then f2- :else :as f2}]
            {:pre [(Filter? f1)
                   (Filter? f2)]
             :post [(boolean? %)]}
            (cond
              (= f1 f2) true
              (and (implied-atomic? f2+ f1+)
                   (implied-atomic? f2- f1-)) true
              :else false))
          (object-better? [o1 o2]
            {:pre [(RObject? o1)
                   (RObject? o2)]
             :post [(boolean? %)]}
            (cond
              (= o1 o2) true
              ((some-fn NoObject? EmptyObject?) o2) true
              :else false))]
    ;tr1 = arg
    ;expected = dom
    ; Omitted some cases dealing with multiple return values
    (cond
      (and (TCResult? tr1)
           (TCResult? expected)
           (= (Un) (ret-t tr1))
           (NoFilter? (ret-f expected))
           (NoObject? (ret-o expected)))
      (let [ts2 (:t tr1)]
        (ret ts2))

      (and (TCResult? tr1)
           (= (Un) (ret-t tr1)))
      expected

      (and (TCResult? tr1)
           (TCResult? expected)
           (= (-FS -top -top)
              (ret-f expected))
           (EmptyObject? (ret-o expected)))
      (let [{t1 :t f1 :fl o1 :o} tr1
            {t2 :t} expected]
        (when-not (subtype? t1 t2)
          (type-error t1 t2))
        expected)

      (and (TCResult? tr1)
           (TCResult? expected))
      (let [{t1 :t f1 :fl o1 :o} tr1
            {t2 :t f2 :fl o2 :o} expected]
        (cond
          (not (subtype? t1 t2)) (type-error t1 t2)

          (and (not (filter-better? f1 f2))
               (object-better? o1 o2))
          (throw (Exception. (str "Expected result with filter " f2 ", got filter"  f1)))

          (and (filter-better? f1 f2)
               (not (object-better? o1 o2)))
          (throw (Exception. (str "Expected result with object " o2 ", got object"  o1)))

          (and (not (filter-better? f1 f2))
               (not (object-better? o1 o2)))
          (throw (Exception. (str "Expected result with object " o2 ", got object"  o1 " and filter "
                                  f2 " got filter " f1))))
        expected)

      (and (TCResult? tr1)
           (Type? expected))
      (let [{t1 :t f :fl o :o} tr1
            t2 expected]
        (when-not (subtype? t1 t2)
          (type-error t1 t2))
        (ret t2 f o))

      ;FIXME
      ;; erm.. ? What is (FilterSet: (list) (list))
      ;; TODO this case goes here, but not sure what it means 
      ;
      ;[((? Type? t1) (tc-result1: t2 (FilterSet: (list) (list)) (Empty:)))
      ; (unless (subtype t1 t2)
      ;   (tc-error/expr "Expected ~a, but got ~a" t2 t1))
      ; t1]

      (and (Type? tr1)
           (TCResult? expected))
      (let [t1 tr1
            {t2 :t f :fl o :o} expected]
        (if (subtype? t1 t2)
          (throw (Exception. (str "Expected result with filter " f " and " o ", got " t1)))
          (type-error t1 t2))
        t1)

      (and (Type? tr1)
           (Type? expected))
      (let [t1 tr1
            t2 expected]
        (when-not (subtype? t1 t2)
          (type-error t1 t2))
        expected)

      :else (let [a tr1
                  b expected]
              (throw (Exception. (str "Unexpected input for check-below " a b)))))))

(derive ::free-in-for-object fold-rhs-default)

(add-fold-case ::free-in-for-object
               Path
               (fn [{p :path i :id :as o} {{:keys [free-in? k]} :locals}]
                 (if (= i k)
                   (reset! free-in? true)
                   o)))

(derive ::free-in-for-filter fold-rhs-default)

(add-fold-case ::free-in-for-filter
               NotTypeFilter
               (fn [{t :type p :path i :id :as t} {{:keys [k free-in?]} :locals}]
                 (if (= i k)
                   (reset! free-in? true)
                   t)))

(add-fold-case ::free-in-for-filter
               TypeFilter
               (fn [{t :type p :path i :id :as t} {{:keys [k free-in?]} :locals}]
                 (if (= i k)
                   (reset! free-in? true)
                   t)))

(derive ::free-in-for-type fold-rhs-default)

(declare index-free-in?)

(add-fold-case ::free-in-for-type
               Function
               (fn [{:keys [dom rng rest drest kws]} {{:keys [k free-in? for-type]} :locals}]
                 ;; here we have to increment the count for the domain, where the new bindings are in scope
                 (let [arg-count (+ (count dom) (if rest 1 0) (if drest 1 0) (count (concat (:mandatory kws)
                                                                                            (:optional kws))))
                       st* (fn [t] (index-free-in? (if (number? k) (+ arg-count k) k) t))]
                   (doseq [d dom]
                     (for-type d))
                   (st* rng)
                   (and rest (for-type rest))
                   (and rest (for-type (:pre-type drest)))
                   (doseq [[_ v] (concat (:mandatory kws)
                                         (:optional kws))]
                     (for-type v))
                   ;dummy return value
                   (make-Function [] -any))))

;[AnyInteger Type -> Boolean]
(defn index-free-in? [k type]
  (let [free-in? (atom false :validator boolean?)]
    (letfn [(for-object [o]
              (fold-rhs ::free-in-for-object
                        {:type-rec for-type
                         :locals {:free-in? free-in?
                                  :k k}}
                        o))
            (for-filter [o]
              (fold-rhs ::free-in-for-filter
                        {:type-rec for-type
                         :filter-rec for-filter
                         :locals {:free-in? free-in?
                                  :k k}}
                         o))
            (for-type [t]
              (fold-rhs ::free-in-for-type
                        {:type-rec for-type
                         :filter-rec for-filter
                         :object-rec for-object
                         :locals {:free-in? free-in?
                                  :k k
                                  :for-type for-type}}
                        t))]
      (for-type type)
      @free-in?)))

(declare subst-type)

;[Filter (U Number Symbol) RObject Boolean -> Filter]
(defn subst-filter [f k o polarity]
  {:pre [(Filter? f)
         (name-ref? k)
         (RObject? o)
         (boolean? polarity)]
   :post [(Filter? %)]}
  (letfn [(ap [f] (subst-filter f k o polarity))
          (tf-matcher [t p i k o polarity maker]
            {:pre [(Type? t)
                   ((some-fn EmptyObject? NoObject? Path?) o)]
             :post [(Filter? %)]}
            (cond
              ((some-fn EmptyObject? NoObject?)
                 o)
              (cond 
                (= i k) (if polarity -top -bot)
                (index-free-in? k t) (if polarity -top -bot)
                :else f)

              (Path? o) (let [{p* :path i* :id} o]
                          (cond
                            (= i k) (maker 
                                      (subst-type t k o polarity)
                                      i*
                                      (concat p p*))
                            (index-free-in? k t) (if polarity -top -bot)
                            :else f))
              :else (throw (Exception. (str "what is this? " o)))))]
    (cond
      (ImpFilter? f) (let [{ant :a consq :c} f]
                       (-imp (subst-filter ant k o (not polarity)) (ap consq)))
      (AndFilter? f) (let [fs (:fs f)] 
                       (apply -and (map ap fs)))
      (OrFilter? f) (let [fs (:fs f)]
                       (apply -or (map ap fs)))
      (BotFilter? f) -bot
      (TopFilter? f) -top

      (TypeFilter? f) 
      (let [{t :type p :path i :id} f]
        (tf-matcher t p i k o polarity -filter))

      (NotTypeFilter? f) 
      (let [{t :type p :path i :id} f]
        (tf-matcher t p i k o polarity -not-filter)))))

;[FilterSet Number RObject Boolean (Option Type) -> FilterSet]
(defn subst-filter-set [fs k o polarity & [t]]
  {:pre [((some-fn FilterSet? NoFilter?) fs)
         (name-ref? k)
         (RObject? o)
         ((some-fn nil? Type?) t)]
   :post [(FilterSet? %)]}
;  (prn "subst-filter-set")
;  (prn "fs" (unparse-filter-set fs))
;  (prn "k" k) 
;  (prn "o" o)
;  (prn "polarity" polarity) 
;  (prn "t" (when t (unparse-type t)))
  (let [extra-filter (if t (->TypeFilter t nil k) -top)]
    (letfn [(add-extra-filter [f]
              {:pre [(Filter? f)]
               :post [(Filter? %)]}
              (let [f* (-and extra-filter f)]
                (if (BotFilter? f*)
                  f*
                  f)))]
      (cond
        (FilterSet? fs) (let [^FilterSet fs fs]
                          (-FS (subst-filter (add-extra-filter (.then fs)) k o polarity)
                               (subst-filter (add-extra-filter (.else fs)) k o polarity)))
        :else (-FS -top -top)))))

;[Type Number RObject Boolean -> RObject]
(defn subst-object [t k o polarity]
  {:pre [(RObject? t)
         (name-ref? k)
         (RObject? o)
         (boolean? polarity)]
   :post [(RObject? %)]}
  (cond
    ((some-fn NoObject? EmptyObject?) t) t
    (Path? t) (let [{p :path i :id} t]
                (if (= i k)
                  (cond
                    (EmptyObject? o) (->EmptyObject)
                    ;; the result is not from an annotation, so it isn't a NoObject
                    (NoObject? o) (->EmptyObject)
                    (Path? o) (let [{p* :path i* :id} o]
                                (->Path (seq (concat p p*)) i*)))
                  t))))

(derive ::subst-type fold-rhs-default)

(add-fold-case ::subst-type
               Function
               (fn [{:keys [dom rng rest drest kws] :as ty} {{:keys [st k o polarity]} :locals}]
                 ;; here we have to increment the count for the domain, where the new bindings are in scope
                 (let [arg-count (+ (count dom) (if rest 1 0) (if drest 1 0) (count (:mandatory kws)) (count (:optional kws)))
                       st* (if (integer? k)
                             (fn [t] 
                               {:pre [(AnyType? t)]}
                               (subst-type t (if (number? k) (+ arg-count k) k) o polarity))
                             st)]
                   (->Function (map st dom)
                               (st* rng)
                               (and rest (st rest))
                               (when drest
                                 (-> drest
                                   (update-in [:pre-type] st)))
                               (when kws
                                 (-> kws
                                   (update-in [:mandatory] #(into {} (for [[k v] %]
                                                                       [(st k) (st v)])))
                                   (update-in [:optional] #(into {} (for [[k v] %]
                                                                      [(st k) (st v)])))))))))


;[Type (U Symbol Number) RObject Boolean -> Type]
(defn subst-type [t k o polarity]
  {:pre [(AnyType? t)
         (name-ref? k)
         (RObject? o)
         (boolean? polarity)]
   :post [(AnyType? %)]}
  (letfn [(st [t*]
            (subst-type t* k o polarity))
          (sf [fs] 
            {:pre [(FilterSet? fs)] 
             :post [(FilterSet? %)]}
            (subst-filter-set fs k o polarity))]
    (fold-rhs ::subst-type
      {:type-rec st
       :filter-rec sf
       :object-rec (fn [f] (subst-object f k o polarity))
       :locals {:st st
                :k k
                :o o
                :polarity polarity}}
      t)))

;; Used to "instantiate" a Result from a function call.
;; eg. (let [a (ann-form [1] (U nil (Seqable Number)))]
;;       (if (seq a)
;;         ...
;;
;; Here we want to instantiate the result of (seq a).
;; objs is each of the arguments' objects, ie. [-empty]
;; ts is each of the arugments' types, ie. [(U nil (Seqable Number))]
;;
;; The latent result:
; (Option (ISeq x))
; :filters {:then (is (CountRange 1) 0)
;           :else (| (is nil 0)
;                    (is (ExactCount 0) 0))}]))
;; instantiates to 
; (Option (ISeq x))
; :filters {:then (is (CountRange 1) a)
;           :else (| (is nil a)
;                    (is (ExactCount 0) a))}]))
;;
;; Notice the objects are instantiated from 0 -> a
;
;[Result (Seqable RObject) (Option (Seqable Type)) 
;  -> '[Type FilterSet RObject]]
(defn open-Result 
  "Substitute ids for objs in Result t"
  [{t :t fs :fl old-obj :o :as r} objs & [ts]]
  {:pre [(Result? r)
         (every? RObject? objs)
         ((some-fn FilterSet? NoFilter?) fs)
         (RObject? old-obj)
         ((some-fn nil? (every-c? Type?)) ts)]
   :post [((hvector-c? Type? FilterSet? RObject?) %)]}
;  (prn "open-result")
;  (prn "result type" (unparse-type t))
;  (prn "result filterset" (unparse-filter-set fs))
;  (prn "result (old) object" old-obj)
;  (prn "objs" objs)
;  (prn "ts" (mapv unparse-type ts))
  (reduce (fn [[t fs old-obj] [[o k] arg-ty]]
            {:pre [(Type? t)
                   ((some-fn FilterSet? NoFilter?) fs)
                   (RObject? old-obj)
                   (integer? k)
                   (RObject? o)
                   ((some-fn false? Type?) arg-ty)]
             :post [((hvector-c? Type? FilterSet? RObject?) %)]}
            (let [r [(subst-type t k o true)
                     (subst-filter-set fs k o true arg-ty)
                     (subst-object old-obj k o true)]]
;              (prn [(unparse-type t) (unparse-filter-set fs) old-obj])
;              (prn "r" r)
              r))
          [t fs old-obj]
          ; this is just a sequence of pairs of [nat? RObject] and Type?
          ; Represents the object and type of each argument, and its position
          (map vector 
               (map-indexed #(vector %2 %1) ;racket's is opposite..
                            objs)
               (if ts
                 ts
                 (repeat false)))))


;Function TCResult^n (or nil TCResult) -> TCResult
(defn check-funapp1 [fexpr arg-exprs {{optional-kw :optional mandatory-kw :mandatory :as kws} :kws
                                      :keys [dom rng rest drest] :as ftype0}
                     argtys expected & {:keys [check?] :or {check? true}}]
  {:pre [(Function? ftype0)
         (every? TCResult? argtys)
         ((some-fn nil? TCResult?) expected)
         (boolean? check?)]
   :post [(TCResult? %)]}
  (assert (not drest) "funapp with drest args NYI")
  (assert (empty? mandatory-kw) "funapp with mandatory keyword args NYI")
  (assert (empty? optional-kw) "funapp with optional keyword args NYI")
;  (prn "check-funapp1")
;  (prn "argtys objects" (map ret-o argtys))
  ;checking
  (when check?
    (let [nactual (count argtys)]
      (when-not (or (when (and (not rest)
                               (empty? optional-kw)
                               (empty? mandatory-kw))
                      (= (count dom) (count argtys)))
                    (when rest
                      (<= (count dom) nactual))
                    (when kws
                      (let [nexpected (+ (count dom)
                                         (* 2 (count mandatory-kw)))]
                        (and (even? (- nactual (count dom)))
                             ((if (seq optional-kw) <= =)
                                nexpected
                                nactual)))))
        (throw (Exception. (error-msg "Wrong number of arguments, expected " (count dom) " fixed parameters"
                                      (cond
                                        rest " and a rest parameter "
                                        drest " and a dotted rest parameter "
                                        kws (cond
                                              (and (seq mandatory-kw) (seq optional-kw))
                                              (str ", some optional keyword arguments and " (count mandatory-kw) 
                                                   " mandatory keyword arguments")

                                              (seq mandatory-kw) (str "and " (count mandatory-kw) "  mandatory keyword arguments")
                                              (seq optional-kw) " and some optional keyword arguments"))
                                      ", and got " nactual
                                      " for function " (unparse-type ftype0) " and arguments " (mapv (comp unparse-type ret-t) argtys)))))
        (doseq [[arg-t dom-t] (map vector 
                                   (map ret-t argtys) 
                                   (concat dom (when rest (repeat rest))))]
          (check-below arg-t dom-t))))
  (let [dom-count (count dom)
        arg-count (+ dom-count (if rest 1 0) (count optional-kw))
        o-a (map ret-o argtys)
        _ (assert (every? RObject? o-a))
        t-a (map ret-t argtys)
        _ (assert (every? Type? t-a))
        [o-a t-a] (let [rs (for [[nm oa ta] (map vector 
                                                 (range arg-count) 
                                                 (concat o-a (repeatedly ->EmptyObject))
                                                 (concat t-a (repeatedly Un)))]
                             [(if (>= nm dom-count) (->EmptyObject) oa)
                              ta])]
                    [(map first rs) (map second rs)])
        [t-r f-r o-r] (open-Result rng o-a t-a)]
    (ret t-r f-r o-r)))

;[(U Any {kw x}) -> (U nil x) :filters {:then (is {kw Any} 0)}]
(defn keyword->Fn [kw]
  {:pre [(keyword? kw)]
   :post [(Type? %)]}
  (Poly* ['x]
         [no-bounds]
         (make-FnIntersection
           (make-Function
             [(Un -any
                  (-hmap {(-val kw) (make-F 'x)}))]
             (Un -nil (make-F 'x))
             nil nil
             :filter (-FS (-filter (-hmap {(-val kw) -any}) 0) -top)))
         ['x]))

;[TCResult -> Type]
(defn resolve-to-ftype [expected]
  (loop [etype expected 
         seen #{}]
    (if (seen etype)
      (throw (Exception. (error-msg "Stuck in loop, cannot resolve function type "
                                    (unparse-type (ret-t expected)))))
      (let [seen (conj seen etype)]
        (cond
          (Name? etype) (recur (resolve-Name etype) seen)
          (TApp? etype) (recur (resolve-TApp etype) seen)
          :else etype)))))

(declare Method->symbol MethodExpr->qualsym)

;[Expr (Seqable Expr) (Seqable TCResult) (Option TCResult) Boolean
; -> Any]
(defn ^String app-type-error [fexpr args ^FnIntersection fin arg-ret-types expected poly?]
  {:pre [(FnIntersection? fin)]}
  (let [static-method? (= :static-method (:op fexpr))
        instance-method? (= :instance-method (:op fexpr))
        method-sym (when (or static-method? instance-method?)
                     (MethodExpr->qualsym fexpr))]
    (error-msg 
      (if poly? 
        (str "Polymorphic " 
             (cond static-method? "static method "
                   instance-method? "instance method "
                   :else "function "))
        (cond static-method? "Static method "
              instance-method? "Instance method "
              :else "Function "))
      (if (or static-method?
              instance-method?)  
        (or method-sym
            (:method-name fexpr))
        (emit-form-fn fexpr)) 
      " could not be applied to arguments:\n"
      "Domains: \n\t" 
      (clojure.string/join "\n\t" (map (partial apply pr-str) (map (comp #(map unparse-type %) :dom) (.types fin)))) 
      "\n\n"
      "Arguments:\n\t" (apply prn-str (mapv (comp unparse-type ret-t) arg-ret-types)) "\n"
      (when expected (str "with expected type:\n\t" (unparse-type (ret-t expected)) "\n\n"))
      "in: " (if (or static-method? instance-method?)
               (emit-form-fn fexpr)
               (list* (emit-form-fn fexpr)
                      (map emit-form-fn args))))))

(defn ^String polyapp-type-error [fexpr args fexpr-type arg-ret-types expected]
  {:pre [(Poly? fexpr-type)]}
  (let [fin (Poly-body* (Poly-free-names* fexpr-type) fexpr-type)]
    (app-type-error fexpr args fin arg-ret-types expected true)))

(defn ^String plainapp-type-error [fexpr args fexpr-type arg-ret-types expected]
  {:pre [(FnIntersection? fexpr-type)]}
  (app-type-error fexpr args fexpr-type arg-ret-types expected false))

(declare invoke-keyword)

; Expr Expr^n TCResult TCResult^n (U nil TCResult) -> TCResult
(defn check-funapp [fexpr args fexpr-ret-type arg-ret-types expected]
  {:pre [(TCResult? fexpr-ret-type)
         (every? TCResult? arg-ret-types)
         ((some-fn nil? TCResult?) expected)]
   :post [(TCResult? %)]}
  (let [fexpr-type (resolve-to-ftype (ret-t fexpr-ret-type))
        arg-types (mapv ret-t arg-ret-types)]
    ;(prn "check-funapp" (unparse-type fexpr-type) (map unparse-type arg-types) fexpr-type)
    (cond
      ;keyword function
      (and (Value? fexpr-type)
           (keyword? (:val fexpr-type)))
      (let [[target-ret default-ret & more-args] arg-ret-types]
        (assert (empty? more-args))
        (invoke-keyword fexpr-ret-type target-ret default-ret expected))

      ;set function
      ;FIXME yuck
      (and (RClass? fexpr-type)
           (isa? (symbol->Class (.the-class ^RClass fexpr-type)) IPersistentSet))
      (do
        (assert (#{1} (count args)))
        (ret -any))

      (and (RClass? fexpr-type)
           (isa? (symbol->Class (.the-class ^RClass fexpr-type)) IPersistentMap))
      ;rewrite ({..} x) as (f {..} x), where f is some dummy fn
      (let [mapfn (parse-type '(All [x] [(IPersistentMap Any x) Any -> (U nil x)]))]
        (check-funapp fexpr args (ret mapfn) (concat [fexpr-ret-type] arg-ret-types) expected))

      ;Symbol function
      (and (RClass? fexpr-type)
           ('#{clojure.lang.Symbol} (.the-class ^RClass fexpr-type)))
      (let [symfn (parse-type '(All [x] [(U (IPersistentMap Any x) Any) -> (U x nil)]))]
        (check-funapp fexpr args (ret symfn) arg-ret-types expected))

      ;ordinary Function, single case, special cased for improved error msgs
      (and (FnIntersection? fexpr-type)
           (let [[{:keys [drest] :as ft} :as ts] (:types fexpr-type)]
             (and (= 1 (count ts))
                  (not drest))))
      (let [argtys arg-ret-types
            {[t] :types} fexpr-type]
        (check-funapp1 fexpr args t argtys expected))

      ;ordinary Function, multiple cases
      (FnIntersection? fexpr-type)
      (let [ftypes (:types fexpr-type)
            success-ret-type (some #(check-funapp1 fexpr args % arg-ret-types expected :check? false)
                                   (filter (fn [{:keys [dom rest] :as f}]
                                             {:pre [(Function? f)]}
                                             (subtypes-varargs? arg-types dom rest))
                                           ftypes))]
        (if success-ret-type
          success-ret-type
          (throw (Exception. (plainapp-type-error fexpr args fexpr-type arg-ret-types expected)))))

      ;ordinary polymorphic function without dotted rest
      (and (Poly? fexpr-type)
           (let [body (Poly-body* (repeatedly (.nbound ^Poly fexpr-type) gensym) fexpr-type)]
             (and (FnIntersection? body)
                  (every? (complement :drest) (.types ^FnIntersection body)))))
      (let [fs-names (repeatedly (.nbound ^Poly fexpr-type) gensym)
            _ (assert (every? symbol? fs-names))
            ^FnIntersection fin (Poly-body* fs-names fexpr-type)
            bbnds (Poly-bbnds* fs-names fexpr-type)
            _ (assert (FnIntersection? fin))
            ret-type (loop [[{:keys [dom rng rest drest kws] :as ftype} & ftypes] (.types fin)]
                       (when ftype
                         #_(prn "infer poly fn" (unparse-type ftype) (map unparse-type arg-types)
                              (count dom) (count arg-types))
                         #_(when rest (prn "rest" (unparse-type rest)))
                         ;; only try inference if argument types are appropriate and no kws
                         (if-let [substitution (try
                                                 (and (not (or drest kws))
                                                      ((if rest <= =) (count dom) (count arg-types))
                                                      (infer-vararg (zipmap fs-names bbnds) {} arg-types dom rest (Result-type* rng)
                                                                    (and expected (ret-t expected))))
                                                 (catch IllegalArgumentException e
                                                   (throw e))
                                                 (catch Exception e
                                                   #_(prn e)))]
                           (do #_(prn "subst:" substitution)
                             (check-funapp1 fexpr args (subst-all substitution ftype)
                                            arg-ret-types expected :check? false))
                           (if (or drest kws)
                             (throw (Exception. "Cannot infer arguments to polymorphic functions with dotted rest or kw types"))
                             (recur ftypes)))))]
        (if ret-type
          ret-type
          (throw (Exception. (polyapp-type-error fexpr args fexpr-type arg-ret-types expected)))))

      :else ;; any kind of dotted polymorphic function without mandatory keyword args
      (if-let [[pbody fixed-vars fixed-bnds dotted-var dotted-bnd]
               (and (PolyDots? fexpr-type)
                    (let [vars (vec (repeatedly (:nbound fexpr-type) gensym))
                          bbnds (PolyDots-bbnds* vars fexpr-type)
                          [fixed-bnds dotted-bnd] [(butlast bbnds) (last bbnds)]
                          [fixed-vars dotted-var] [(butlast vars) (last vars)]
                          pbody (PolyDots-body* vars fexpr-type)]
                      (and (FnIntersection? pbody)
                           (seq (:types pbody))
                           (not (some :kws (:types pbody)))
                           [pbody fixed-vars fixed-bnds dotted-var dotted-bnd])))]
        (let [inferred-rng (some identity
                                 (for [{:keys [dom rest ^DottedPretype drest rng] :as ftype} (:types pbody)
                                       ;only try inference if argument types match
                                       :when (cond
                                               rest (<= (count dom) (count arg-types))
                                               drest (and (<= (count dom) (count arg-types))
                                                          (= dotted-var (-> drest :name)))
                                               :else (= (count dom) (count arg-types)))]
                                   (do
                                     ;(prn "Inferring dotted fn" (unparse-type ftype))
                                     ;; Only try to infer the free vars of the rng (which includes the vars
                                     ;; in filters/objects).
                                     (let [substitution (cond
                                                          drest (infer-dots (zipmap fixed-vars fixed-bnds) dotted-var dotted-bnd
                                                                            arg-types dom (.pre-type drest) (Result-type* rng) (fv rng)
                                                                            :expected (and expected (ret-t expected)))
                                                          rest (infer-vararg (zipmap fixed-vars fixed-bnds) {dotted-var dotted-bnd}
                                                                             arg-types dom rest (Result-type* rng)
                                                                             (and expected (ret-t expected)))
                                                          :else (infer (zipmap fixed-vars fixed-bnds) {dotted-var dotted-bnd} 
                                                                       arg-types dom (Result-type* rng)
                                                                       (and expected (ret-t expected))))
                                           ;_ (prn "substitution:" substitution)
                                           substituted-type (subst-all substitution ftype)
                                           ;_ (prn "substituted-type" (unparse-type substituted-type))
                                           ;_ (prn "args" (map unparse-type arg-types))
                                           ]
                                       (or (and substitution
                                                (check-funapp1 fexpr args 
                                                               substituted-type arg-ret-types expected :check? false))
                                           (throw (Exception. "Error applying dotted type")))))))]
          ;(prn "inferred-rng"inferred-rng)
          (if inferred-rng
            inferred-rng
            (throw (Exception. (pr-str "Could not apply dotted function " (unparse-type fexpr-type)
                                       " to arguments " (map unparse-type arg-types))))))

        (throw (Exception. (error-msg
                             "Cannot invoke type: " (unparse-type fexpr-type))))))))

(defmethod check :var
  [{:keys [var] :as expr} & [expected]]
  (let [id (var->symbol var)]
    (assoc expr
           expr-type (ret (binding [*var-annotations* VAR-ANNOTATIONS]
                            (lookup-Var (var->symbol var)))
                          (-FS -top -top)
                          -empty))))

(defmethod check :the-var
  [{:keys [var] :as expr} & [expected]]
  (assoc expr
         expr-type (ret (RClass-of Var)
                        (-FS -top -bot)
                        -empty)))

;[Any TCResult * -> TCResult]
(defn tc-equiv [comparator & vs]
  {:pre [(every? TCResult? vs)]
   :post [(TCResult? %)]}
  (assert (#{:=} comparator))
  (let [thn-fls (set (apply concat
                            (for [[{t1 :t fl1 :fl o1 :o}
                                   {t2 :t fl2 :fl o2 :o}]
                                  (comb/combinations vs 2)]
                              (concat (when (Path? o2)
                                        [(-filter t1 (:id o2) (:path o2))])
                                      (when (Path? o1)
                                        [(-filter t2 (:id o1) (:path o1))])))))
        els-fls (set (apply concat
                            (for [[{t1 :t fl1 :fl o1 :o}
                                   {t2 :t fl2 :fl o2 :o}]
                                  (comb/combinations vs 2)]
                              (concat (when (Path? o2)
                                        [(-not-filter t1 (:id o2) (:path o2))])
                                      (when (Path? o1)
                                        [(-not-filter t2 (:id o1) (:path o1))])))))]
  (ret (Un -false -true)
       (-FS (if (empty? thn-fls)
              -top
              (apply -and thn-fls))
            (if (empty? els-fls)
              -top
              (apply -or els-fls)))
       -empty)))

(defmulti invoke-special (fn [expr & args] (-> expr :fexpr :var)))
(defmulti invoke-apply (fn [expr & args] (-> expr :args first :var)))
(defmulti static-method-special (fn [{{:keys [declaring-class name]} :method} & args]
                                  (symbol (str declaring-class) (str name))))
(defmulti instance-method-special (fn [{{:keys [declaring-class name]} :method} & args]
                                    (symbol (str declaring-class) (str name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword lookups

(declare invoke-keyword)

;[Type TCResult -> Type]
(defn- extend-method-expected 
  "Returns the expected type with target-type intersected with the first argument"
  [target-type expected]
  {:pre [(Type? target-type)
         (Type? expected)]
   :post [(Type? %)]}
  (cond
    (FnIntersection? expected)
    (-> expected
      (update-in [:types] #(for [ftype %]
                             (do
                               (assert (<= 1 (count (:dom ftype))))
                               (-> ftype
                                 (update-in [:dom] (fn [dom] 
                                                     (update-in (vec dom) [0] (partial In target-type)))))))))

    (Poly? expected)
    (let [names (repeat (:nbound expected) gensym)
          body (Poly-body* names expected)
          body (extend-method-expected target-type body)]
      (Poly* names 
             (Poly-bbnds* names expected)
             body
             (Poly-free-names* expected)))

    (PolyDots? expected)
    (let [names (repeat (:nbound expected) gensym)
          body (PolyDots-body* names expected)
          body (extend-method-expected target-type body)]
      (PolyDots* names 
                 (PolyDots-bbnds* names expected)
                 body))
    :else (throw (Exception. (str "Expected Function type, found " (unparse-type expected))))))

(defmethod invoke-special #'clojure.core/extend
  [{[atype & protos] :args :as expr} & [expected]]
  (assert (and atype (even? (count protos))) "Wrong arguments to extend")
  (let [catype (check atype)
        target-type (ret-t (expr-type catype))
        _ (assert (and (Value? target-type)
                       (class? (:val target-type)))
                  (str "Must provide Class as first argument to extend, "
                       "got" (unparse-type target-type)))
        ; build expected types for each method map
        extends (into {}
                      (for [[prcl-expr mmap-expr] (apply hash-map protos)]
                        (let [protocol (do (assert (= :var (:op prcl-expr)) "Must reference protocol directly with var in extend")
                                         (resolve-protocol (var->symbol (:var prcl-expr))))
                              expected-mmap (make-HMap {}
                                                       ;get all combinations
                                                       (into {}
                                                             (for [[msym mtype] (:methods protocol)]
                                                               [(-val (keyword (name msym))) 
                                                                (extend-method-expected target-type mtype)])))]
                          [protocol [mmap-expr expected-mmap]])))
        _ (doseq [[protocol [mmap-expr expected-hmap]] extends]
            (check mmap-expr (ret expected-hmap)))]
    (assoc expr
           expr-type (ret -nil))))

;into-array>
;
; Usage: (into-array> javat cljt coll)
;        (into-array> cljt coll)
(defmethod invoke-special #'into-array>*
  [{:keys [args] :as expr} & [expected]]
  (assert (#{2 3} (count args)) (error-msg "Wrong number of args to into-array>*"))
  (let [has-java-syn? (= 3 (count args))
        [javat-syn cljt-syn coll-expr] (if has-java-syn?
                                         args
                                         (cons nil args))
        javat (let [c (-> (or (when has-java-syn? (:val javat-syn))  ; generalise javat-syn if provided, otherwise cljt-syn
                              (:val cljt-syn))
                        parse-type Type->array-member-Class)]
                (assert (class? c))
                c)
        cljt (parse-type (:val cljt-syn))
        ccoll (check coll-expr (ret (Un -nil (RClass-of Seqable [cljt]))))]
    (assoc expr
           expr-type (ret (->PrimitiveArray javat cljt cljt)))))

;not
(defmethod invoke-special #'clojure.core/not
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert (= 1 (count args)) (error-msg "Wrong number of args to clojure.core/not"))
  (let [ctarget (check (first args))
        {fs+ :then fs- :else} (-> ctarget expr-type ret-f)]
    (assoc expr
           expr-type (ret (parse-type 'boolean) 
                          ;flip filters
                          (-FS fs- fs+)
                          -empty))))

(defn invoke-get [{:keys [args] :as expr} & [expected]]
  {:post [((some-fn 
             #(-> % expr-type TCResult?)
             #{::not-special})
             %)]}
  (assert (#{2 3} (count args)) "Wrong number of args to clojure.core/get")
  (let [[target kw default] args
        kwr (expr-type (check kw))]
    (cond
      ((every-pred Value? (comp keyword? :val)) (ret-t kwr))
      (assoc expr
             expr-type (invoke-keyword kwr
                                       (expr-type (check target))
                                       (when default
                                         (expr-type (check default)))
                                       expected))

      ((every-pred Value? (comp integer? :val)) (ret-t kwr))
      (throw (Exception. "get lookup of vector (like nth) NYI"))
      
      :else (do ;(prn "Non-special 'get'")
              ::not-special))))

;get
(defmethod invoke-special #'clojure.core/get
  [expr & [expected]]
  (invoke-get expr expected))

(defmethod static-method-special 'clojure.lang.RT/get
  [expr & [expected]]
  (invoke-get expr expected))

;FIXME should be the same as (apply hash-map ..) in invoke-apply
(defmethod static-method-special 'clojure.lang.PersistentHashMap/create
  [{:keys [args] :as expr} & [expected]]
  (let [_ (assert (= 1 (count args)) "Incorrect number of arguments to clojure.lang.PersistentHashMap/create")
        targett (-> (first args) check expr-type ret-t)
        _ (assert (HeterogeneousSeq? targett) (error-msg "Must pass HeterogeneousSeq to clojure.lang.PersistentHashMap/create given "
                                                         (unparse-type targett)))
        res (reduce (fn [t [kt vt]]
                      {:pre [(Type? t)]}
                      ;preserve bottom
                      (if (= (Un) vt)
                        vt
                        (do (assert (HeterogeneousMap? t))
                          (assoc-in [:types kt] vt))))
                    (-hmap {}) (.types ^HeterogeneousSeq targett))]
    (assoc expr
           expr-type (ret res))))

(defmethod check :keyword-invoke
  [{:keys [kw target] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  (assoc expr
         expr-type (invoke-keyword (expr-type (check kw))
                                   (expr-type (check target))
                                   nil 
                                   expected)))

;[Type Type (Option Type) -> Type]
(defn find-val-type [t k default]
  {:pre [(Type? t)
         (Type? k)
         ((some-fn nil? Type?) default)]
   :post [(Type? %)]}
  (let [t (-resolve t)]
    (cond
      (Nil? t) (or default -nil)
      (HeterogeneousMap? t) (if-let [v (get (:types t) k)]
                              v
                              (do #_(prn (error-msg "WARNING: Map type " (unparse-type t)
                                                  " does not have entry "
                                                  (unparse-type k)))
                                ; hmaps don't record absense of keys, so we don't actually know anything here.
                                #_(or default -nil)
                                -any))

      (Intersection? t) (apply In 
                               (for [t* (:types t)]
                                 (find-val-type t* k default)))
      (Union? t) (apply Un
                        (for [t* (:types t)]
                          (find-val-type t* k default)))
      :else (throw (Exception. (str (when *current-env*
                                      (str (:line *current-env*) ":"))
                                    "Can't get key " (unparse-type k) 
                                    "  from type " (unparse-type t)))))))

;[TCResult TCResult (Option TCResult) (Option TCResult) -> TCResult]
(defn invoke-keyword [kw-ret target-ret default-ret expected-ret]
  {:pre [(TCResult? kw-ret)
         (TCResult? target-ret)
         ((some-fn nil? TCResult?) default-ret)
         ((some-fn nil? TCResult?) expected-ret)]
   :post [(TCResult? %)]}
  (let [targett (-resolve (ret-t target-ret))
        kwt (ret-t kw-ret)
        defaultt (when default-ret
                   (ret-t default-ret))]
    (cond
      ;Keyword must be a singleton with no default
      (and (Value? kwt)
           (keyword? (:val kwt)))
      (let [{{path-hm :path id-hm :id :as o} :o} target-ret
            this-pelem (->KeyPE (:val kwt))
            val-type (find-val-type targett kwt defaultt)]
        (if (not= (Un) val-type)
          (ret val-type
               (-FS (if (Path? o)
                      (-filter val-type id-hm (concat path-hm [this-pelem]))
                      (-filter-at val-type (->EmptyObject)))
                    -top)
               (if (Path? o)
                 (update-in o [:path] #(seq (concat % [this-pelem])))
                 o))
          (throw (Exception. (error-msg "Keyword lookup gave bottom type: "
                                        (:val kwt) " " (unparse-type targett))))))

      :else (throw (Exception. (error-msg "keyword-invoke only supports keyword lookup, no default. Found " 
                                          (unparse-type kwt)))))))

;binding
;FIXME use `check-normal-def`
(defmethod invoke-special #'push-thread-bindings
  [{[bindings-expr & other-args] :args :as expr} & [expected]]
  (assert (empty? other-args))
  ; only support (push-thread-bindings (hash-map @~[var bnd ...]))
  ; like `binding`s expansion
  (assert (#{:invoke} (-> bindings-expr :op)))
  (assert (#{#'hash-map} (-> bindings-expr :fexpr :var)))
  (assert (even? (count (-> bindings-expr :args))))
  (let [new-bindings-exprs (apply hash-map (-> bindings-expr :args))
        _ (binding [*var-annotations* VAR-ANNOTATIONS]
            (doseq [[{:keys [op var] :as var-expr} bnd-expr] new-bindings-exprs]
              (assert (#{:the-var} op))
              (let [expected (type-of (var->symbol var))
                    cexpr (check bnd-expr (ret expected))]
                (subtype (-> cexpr expr-type ret-t) expected))))]
    (assoc expr
           expr-type (ret -any))))

;=
(defmethod invoke-special #'clojure.core/= 
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (apply tc-equiv := (map expr-type cargs)))))

;identical
(defmethod static-method-special 'clojure.lang.Util/identical
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (apply tc-equiv := (map expr-type cargs)))))

;equiv
(defmethod static-method-special 'clojure.lang.Util/equiv
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (apply tc-equiv := (map expr-type cargs)))))

;[TCResult TCResult -> TCResult]
(defn tc-isa? [child-ret parent-ret]
  {:pre [(TCResult? child-ret)
         (TCResult? parent-ret)]
   :post [(TCResult? %)]}
;  (prn "tc-isa?")
;  (prn "child-ret" child-ret)
;  (prn "parent-ret" parent-ret)
  (let [parent-t (ret-t parent-ret)
        fs (-FS (-filter-at parent-t (ret-o child-ret))
                (-not-filter-at parent-t (ret-o child-ret)))]
;    (prn "fs" fs)
;    (prn "child object" (ret-o child-ret))
    (ret (Un -true -false) fs -empty)))


;isa? (2 arity only)
(defmethod invoke-special #'clojure.core/isa?
  [{:keys [args] :as expr} & [expected]]
  (assert (= 2 (count args))
          "Only supports 2 argument invocations of isa?")
  (let [[cchild-expr cparent-expr :as cargs] (mapv check args)]
    (assoc expr
           expr-type (tc-isa? (expr-type cchild-expr)
                              (expr-type cparent-expr)))))

;apply
(defmethod invoke-special #'clojure.core/apply
  [expr & [expected]]
  ;(prn "special apply:")
  (let [e (invoke-apply expr expected)]
    (when (= e ::not-special)
      (throw (Exception. (error-msg "apply must be special: " (emit-form-fn expr)))))
    e))

;manual instantiation
(defmethod invoke-special #'inst-poly
  [{[pexpr targs-exprs] :args :as expr} & [expected]]
  (let [ptype (let [t (-> (check pexpr) expr-type ret-t)]
                (if (Name? t)
                  (resolve-Name t)
                  t))
        _ (assert ((some-fn Poly? PolyDots?) ptype))
        targs (doall (map parse-type (:val targs-exprs)))]
    (assoc expr
           expr-type (ret (manual-inst ptype targs)))))

(def ^:dynamic *inst-ctor-types* nil)
(set-validator! #'*inst-ctor-types* (some-fn nil? (every-c? Type?)))

;manual instantiation for calls to polymorphic constructors
(defmethod invoke-special #'inst-poly-ctor
  [{[ctor-expr targs-exprs] :args :as expr} & [expected]]
  (let [targs (mapv parse-type (:val targs-exprs))
        cexpr (binding [*inst-ctor-types* targs]
                (check ctor-expr))]
    (assoc expr
           expr-type (expr-type cexpr))))


(declare check-anon-fn)

;debug printing
(defmethod invoke-special #'print-env
  [{[debug-string] :args :as expr} & [expected]]
  (assert (= :string (:op debug-string)))
  ;DO NOT REMOVE
  (pr (:val debug-string))
  (print-env*)
  ;DO NOT REMOVE
  (assoc expr
         expr-type (ret -nil -false-filter -empty)))

;filter printing
(defmethod invoke-special #'print-filterset
  [{[debug-string form] :args :as expr} & [expected]]
  (assert (and debug-string form) "Wrong arguments to print-filterset")
  (let [cform (check form expected)
        t (expr-type cform)]
    (assert (= :string (:op debug-string)))
    ;DO NOT REMOVE
    (prn (:val debug-string))
    ;(prn (:fl t))
    (if (FilterSet? (:fl t))
      (do (pprint (unparse-filter-set (:fl t)))
        (flush))
      (prn (:fl t)))
    (prn (unparse-object (:o t)))
    (prn 'Flow (unparse-filter (-> t :flow flow-normal)))
    ;DO NOT REMOVE
    (assoc expr
           expr-type t)))

;unsafe form annotation
(defmethod invoke-special #'unsafe-ann-form*
  [{[frm {tsyn :val}] :args :as expr} & [expected]]
  (let [parsed-ty (parse-type tsyn)]
    (assoc expr
           expr-type (ret parsed-ty))))

;form annotation
(defmethod invoke-special #'ann-form*
  [{[frm {tsyn :val}] :args :as expr} & [expected]]
  (let [parsed-ty (parse-type tsyn)
        cty (check frm (ret parsed-ty))
        checked-type (ret-t (expr-type cty))
        _ (binding [*current-expr* frm]
            (subtype checked-type parsed-ty))
        _ (when expected
            (binding [*current-expr* frm]
              (subtype checked-type (ret-t expected))))]
    (assoc expr
           expr-type (ret parsed-ty))))

;fn literal
(defmethod invoke-special #'fn>-ann
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [[fexpr {type-syns :val}] args
        expected
        (apply
          make-FnIntersection
          (doall
            (for [{:keys [dom-syntax has-rng? rng-syntax]} type-syns]
              (make-Function (mapv parse-type dom-syntax)
                             (if has-rng?
                               (parse-type rng-syntax)
                               -any)))))]
    (check fexpr (ret expected))))

;polymorphic fn literal
(defmethod invoke-special #'pfn>-ann
  [{:keys [fexpr args] :as expr} & [expected]]
  (assert false "pfn> NYI")
  (let [[fexpr {poly-decl :val} {method-types-syn :val}] args
        frees-with-bounds (map parse-free poly-decl)
        method-types (with-bounded-frees frees-with-bounds
                       (doall 
                         (for [{:keys [dom-syntax has-rng? rng-syntax]} method-types-syn]
                           {:dom (doall (map parse-type dom-syntax))
                            :rng (if has-rng?
                                   (parse-type rng-syntax)
                                   -any)})))
        cexpr (-> (check-anon-fn fexpr method-types :poly frees-with-bounds)
                (update-in [expr-type :t] (fn [fin] (Poly* (map first frees-with-bounds) 
                                                           (map second frees-with-bounds)
                                                           fin
                                                           (map first frees-with-bounds)))))]
    cexpr))

(declare binding-init-sym)

;doseq>
;(defmethod invoke-special #'doseq>*
;  [{[_the-doseq_ special-doseq :as args] :args :as expr} & [expected]]
;  {:pre [(= 2 (count args))]}
;  (assert (= :fn-expr (:op special-doseq)))
;  (let [{:keys [binding-inits body] :as special-let} (-> special-doseq :methods first)
;        _ (assert (= :let (:op special-let)))
;        gsyms (map binding-init-sym binding-inits)
;        cinits (map #(-> % :init check) binding-inits)

(declare check-let)

(def ^:dynamic *loop-bnd-anns* nil)
(set-validator! #'*loop-bnd-anns* #(or (nil? %)
                                       (every? Type? %)))

;loop
(defmethod invoke-special #'loop>-ann
  [{:keys [args env] :as expr} & [expected]]
  (let [[expr {expected-bnds-syn :val}] args
        expected-bnds (binding [*ns* (or (-> env :ns :name find-ns)
                                         *ns*)]
                        (mapv parse-type expected-bnds-syn))]
    ;loop may be nested, type the first loop found
    (binding [*loop-bnd-anns* expected-bnds]
      (check expr expected))))

;don't type check
(defmethod invoke-special #'tc-ignore-forms*
  [{:keys [fexpr args] :as expr} & [expected]]
  (assoc (first args)
         expr-type (ret (->Top))))

;seq
(defmethod invoke-special #'clojure.core/seq
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [[ccoll] (doall (map check args))]
    (cond
      ((some-fn HeterogeneousVector? 
                HeterogeneousList? 
                HeterogeneousSeq?)
         (expr-type ccoll))
      (assoc expr
             expr-type (ret (if-let [ts (seq (:types (expr-type ccoll)))]
                              (->HeterogeneousSeq ts)
                              -nil)))
      :else ::not-special)))

;make vector
(defmethod invoke-special #'clojure.core/vector
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (assoc expr
           expr-type (ret (-hvec
                            (mapv (comp ret-t expr-type) cargs))))))

;make hash-map
(defmethod invoke-special #'clojure.core/hash-map
  [{:keys [fexpr args] :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    (cond
      (every? Value? (keys (apply hash-map (mapv (comp ret-t expr-type) cargs))))
      (assoc expr
             expr-type (ret (-hmap
                              (apply hash-map (mapv (comp ret-t expr-type) cargs)))))
      :else ::not-special)))

;apply hash-map
(defmethod invoke-apply #'clojure.core/hash-map
  [{[_ & args] :args :as expr} & [expected]]
  (let [cargs (doall (map check args))]
    #_(prn "apply special (hash-map): "
         (map (comp unparse-type ret-t expr-type) cargs))
    (cond
      (and ((some-fn HeterogeneousVector? HeterogeneousList? HeterogeneousSeq?) 
              (expr-type (last cargs)))
           ;; every key must be a Value
           (every? Value? (keys (apply hash-map (concat (map expr-type (butlast cargs))
                                                        (mapcat vector (:types (expr-type (last cargs)))))))))
      (assoc expr
             expr-type (ret (-hmap
                              (apply hash-map (concat (map expr-type (butlast cargs))
                                                      (mapcat vector (:types (expr-type (last cargs)))))))))
      :else ::not-special)))

(defn invoke-nth [{:keys [args] :as expr} & [expected]]
  (let [_ (assert (#{2 3} (count args)))
        [te ne de :as cargs] (doall (map check args))
        types (let [ts (-resolve (ret-t (expr-type te)))]
                (if (Union? ts)
                  (:types ts)
                  [ts]))
        num-t (ret-t (expr-type ne))
        default-t (when de
                    (ret-t (expr-type de)))]
    (cond
      (and (Value? num-t)
           (integer? (:val num-t))
           (every? (some-fn Nil?
                            HeterogeneousVector?
                            HeterogeneousList?
                            HeterogeneousSeq?)
                   types))
      (assoc expr
             expr-type (ret (apply Un
                                   (doall
                                     (for [t types]
                                       (let [res-t (cond
                                                     (Nil? t) (or default-t -nil)
                                                     :else (apply nth 
                                                                  (:types t)
                                                                  (:val num-t) 
                                                                  (when default-t
                                                                    [default-t])))]
                                         (if res-t
                                           res-t
                                           (throw (Exception. (str "Cannot get index " (:val num-t)
                                                                   " from type " (unparse-type t)))))))))
                            (let [nnth (:val num-t)
                                  target-o (ret-o (expr-type te))
                                  default-o (when de
                                              (ret-o (expr-type de)))
                                  ;; We handle filters for both arities of nth here, with and without default
                                  ;;
                                  ;;With default:
                                  ;; if this is a true value either:
                                  ;;  * target is nil or seq and default is true
                                  ;;  * target is seqable, default is false
                                  ;;    and target is at least (inc nnth) count
                                  default-fs+ (-or (-and (-filter-at (Un -nil (RClass-of (Class->symbol ISeq) [-any])) 
                                                                     target-o)
                                                         (-not-filter-at (Un -false -nil) 
                                                                         default-o))
                                                   (-and (-filter-at (In (RClass-of (Class->symbol Seqable) [-any])
                                                                         (make-CountRange (inc nnth)))
                                                                     target-o)
                                                         (-filter-at (Un -false -nil) 
                                                                     default-o)))
                                  ;;Without default:
                                  ;; if this is a true value: 
                                  ;;  * target is seqable of at least nnth count
                                  nodefault-fs+ (-filter-at (In (RClass-of (Class->symbol Seqable) [-any])
                                                                (make-CountRange (inc nnth)))
                                                            target-o)]
                              (-FS (if default-t
                                     default-fs+
                                     nodefault-fs+)
                                   ; not sure if there's anything worth encoding here
                                   -top))))
      :else ::not-special)))

;nth
(defmethod static-method-special 'clojure.lang.RT/nth
  [expr & [expected]]
  (invoke-nth expr expected))

;assoc
; TODO handle unions of hmaps as the target
; FIXME needs more tests
(defmethod invoke-special #'clojure.core/assoc
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [[target & keyvals] args

        _ (assert (<= 3 (count args))
                  (str "assoc accepts at least 3 arguments, found "
                       (count args)))
        _ (assert (even? (count keyvals))
                  "assoc accepts an even number of keyvals")

        targetun (-> target check expr-type ret-t)
        targett (-resolve targetun)
        hmaps (cond
                (and (Value? targett) (nil? (.val ^Value targett))) #{(-hmap {})}
                ((some-fn HeterogeneousVector? HeterogeneousMap?) targett) #{targett}
                (subtype? targett (RClass-of IPersistentMap [-any -any])) #{targett}
                (subtype? targett (RClass-of IPersistentVector [-any])) #{targett}
                :else (throw (Exception. (str "Must supply map, vector or nil to first argument of assoc, given "
                                              (unparse-type targetun)))))

        _ (assert (every? #(subtype? % (Un (RClass-of IPersistentVector [-any])
                                           (RClass-of IPersistentMap [-any -any]))) 
                          hmaps))

        ; we must already have an even number of keyvals if we've got this far
        ckeyvals (doall (map check keyvals))
        keypair-types (partition 2 (map (comp ret-t expr-type) ckeyvals))

        ; TODO handle unions of hmaps without promoting to IPersistentMap
        new-hmaps (mapv #(reduce (fn [hmap [kt vt]]
                                   (let [is-vec (subtype? hmap (RClass-of IPersistentVector [-any]))
                                         is-map (subtype? hmap (RClass-of IPersistentMap [-any -any]))]
                                     ;check that hmap is either a vector or map
                                     (assert (and (not= is-vec is-map)
                                                  (or is-vec is-map)))
                                     (cond
                                       ;keep hmap if keyword key and already hmap
                                       (and (HeterogeneousMap? hmap)
                                            (Value? kt)
                                            (keyword? (.val ^Value kt)))
                                       (assoc-in hmap [:types kt] vt)

                                       ;keep hvector if number Value key and already hvector
                                       (and (HeterogeneousVector? hmap)
                                            (Value? kt)
                                            (number? (.val ^Value kt)))
                                       (let [^Value kt kt] 
                                         (assert (integer? (.val kt)))
                                         (assoc-in hmap [:types (.val kt)] vt))

                                       ;otherwise just make normal map if already a map, or normal vec if already a vec
                                       is-map (ret-t 
                                                (check-funapp target keyvals
                                                              (ret 
                                                                (parse-type '(All [b c] 
                                                                                  [(clojure.lang.IPersistentMap b c) b c -> 
                                                                                   (clojure.lang.IPersistentMap b c)])))
                                                              (mapv ret [hmap kt vt])
                                                              nil))
                                       :else (ret-t 
                                               (check-funapp target keyvals
                                                             (ret 
                                                               (parse-type '(All [c] 
                                                                                 [(clojure.lang.IPersistentVector c) c -> 
                                                                                  (clojure.lang.IPersistentVector c)])))
                                                             (mapv ret [hmap vt])
                                                             nil)))))
                                 % keypair-types)
                        hmaps)]
    (assoc expr
           expr-type (ret (apply Un new-hmaps)
                          (-FS -top -bot) ;assoc never returns nil
                          -empty))))


;conj
(defmethod invoke-special #'clojure.core/conj
  [{[t & args] :args :keys [fexpr] :as expr} & [expected]]
  (let [t (check t)
        args (doall (map check args))]
    (cond
      ;(conj {...} [a b]) => (merge {...} {a b})
      (and (HeterogeneousMap? (expr-type t))
           (HeterogeneousVector? (expr-type (first args))))
      (let [m (expr-type t)
            arg1 (expr-type (first args))
            _ (assert (= 2 (count (:types arg1)))
                      "Need vector of length 2 to conj to map")
            _ (assert (every? Value? (:types arg1))
                      "NYI Vector must be of Values for now")
            res (-hmap
                  (assoc (:types m)
                         (-> arg1 :types first)
                         (-> arg1 :types second)))]
        (assoc expr
               expr-type (ret res)))

      ;(conj {...} nil) => {...}
      (and (HeterogeneousMap? (expr-type t))
           (Nil? (expr-type (first args))))
      (assoc expr
             expr-type (ret (expr-type t)))

      ;[...]
      (HeterogeneousVector? (expr-type t))
      (assoc expr
             expr-type (ret (-hvec
                              ;vectors conj onto end
                              (vec (concat (:types (expr-type t)) 
                                           [(expr-type (first args))])))))

      :else ::not-special)))

(comment
  (method-expected-type (parse-type '[Any -> Any])
                        (parse-type '(Value :op))
                        (parse-type '(Value :if)))
  ;=> ['{:if Any} -> Any]
  )

(def ^:dynamic *current-mm* nil)
(set-validator! #'*current-mm* (some-fn nil? 
                                        (hmap-c? :dispatch-fn-type Type?
                                                 :dispatch-val-ret TCResult?)))

(defmethod instance-method-special 'clojure.lang.MultiFn/addMethod
  [{[dispatch-val-expr method-expr :as args] :args :keys [target] :as expr} & [expected]]
  (assert (= 2 (count args)))
  (let [_ (assert (#{:var} (:op target)))
        _ (assert (#{:fn-expr} (:op method-expr))
                  "Method must be a fn")
        mmsym (var->symbol (:var target))
        ctarget (check target)
        cdispatch-val-expr (check dispatch-val-expr)
        dispatch-type (get-multimethod-dispatch-type mmsym)
        method-expected (binding [*var-annotations* VAR-ANNOTATIONS]
                          (type-of mmsym))
        cmethod-expr (binding [*current-mm* {:dispatch-fn-type dispatch-type
                                             :dispatch-val-ret (expr-type cdispatch-val-expr)}]
                       (check method-expr (ret method-expected)))]
    (assoc expr
           expr-type (ret (RClass-of clojure.lang.MultiFn)))))

(defmethod invoke-special :default [& args] ::not-special)
(defmethod static-method-special :default [& args] ::not-special)
(defmethod instance-method-special :default [& args] ::not-special)

(defn check-apply
  [{[fexpr & args] :args :as expr} expected]
  {:post [((some-fn TCResult? #{::not-special}) %)]}
  (let [ftype (ret-t (expr-type (check fexpr)))
        [fixed-args tail] [(butlast args) (last args)]]
    (cond
      ;apply of a simple function
      (FnIntersection? ftype)
      (do 
        (when (empty? (:types ftype))
          (throw (Exception. "Empty function intersection given as argument to apply")))
        (let [arg-tres (mapv check fixed-args)
              arg-tys (mapv (comp ret-t expr-type) arg-tres)
              tail-ty (ret-t (expr-type (check tail)))]
          (loop [[{:keys [dom rng rest drest]} :as fs] (:types ftype)]
            (cond
              ;we've run out of cases to try, so error out
              (empty? fs)
              (throw (Exception. (str (when *current-env*
                                        (str (:line *current-env*) ": "))
                                      "Bad arguments to function in apply: " 
                                      (unparse-type ftype) (mapv unparse-type (concat arg-tys [tail-ty])))))

              ;this case of the function type has a rest argument
              (and rest
                   ;; check that the tail expression is a subtype of the rest argument
                   (subtype? tail-ty (Un -nil (RClass-of Seqable [rest])))
                   (subtypes-varargs? arg-tys dom rest))
              (ret (Result-type* rng)
                   (Result-filter* rng)
                   (Result-object* rng))

              ;other cases go here

              ;next case
              :else (recur (next fs))))))

      ;; apply of a simple polymorphic function
      (Poly? ftype)
      (let [vars (repeatedly (:nbound ftype) gensym)
            bbnds (Poly-bbnds* vars ftype)
            body (Poly-body* vars ftype)
            _ (assert (FnIntersection? body))
            arg-tres (mapv check fixed-args)
            arg-tys (mapv (comp ret-t expr-type) arg-tres)
            tail-bound nil
            tail-ty (ret-t (expr-type (check tail)))]
        (loop [[{:keys [dom rng rest drest] :as ftype0} :as fs] (:types body)]
;          (when (seq fs)
;            (prn "checking fn" (unparse-type (first fs))
;                 (mapv unparse-type arg-tys)))
          (cond
            (empty? fs) (throw (Exception. "Bad arguments to polymorphic function in apply"))
            ;the actual work, when we have a * function and a list final argument
            :else 
            (if-let [substitution (try
                                    (and rest (not tail-bound) 
                                         (<= (count dom)
                                             (count arg-tys))
                                         (infer-vararg (zipmap vars bbnds) {}
                                                       (cons tail-ty arg-tys)
                                                       (cons (Un -nil (RClass-of Seqable [rest])) dom)
                                                       rest
                                                       (Result-type* rng)))
                                    (catch IllegalArgumentException e
                                      (throw e))
                                    (catch Exception e
                                      ;(prn "caught failed polymorphic case")
                                      ))]
              (ret (subst-all substitution (Result-type* rng)))
              (recur (next fs))))))

      :else ::not-special)))

;convert apply to normal function application
(defmethod invoke-apply :default 
  [expr & [expected]]
  (let [t (check-apply expr expected)]
    (if (= t ::not-special)
      t
      (assoc expr
             expr-type t))))

(defmethod check :invoke
  [{:keys [fexpr args env] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  #_(prn "invoke:" ((some-fn :var :keyword :op) fexpr))
  (binding [*current-env* env]
    (let [e (invoke-special expr expected)]
      (cond 
        (not= ::not-special e) e

        (let [fexprt (ret-t (expr-type (check fexpr)))]
          (and (Value? fexprt)
               (keyword? (:val fexprt))))
        (let [[target default] args]
          (assert (<= 1 (count args) 2))
          (assoc expr
                 expr-type (invoke-keyword (expr-type (check fexpr))
                                           (expr-type (check target))
                                           (when default
                                             (expr-type (check default))) 
                                           expected)))

        :else
        (let [cfexpr (check fexpr)
              cargs (doall (map check args))
              ftype (expr-type cfexpr)
              argtys (map expr-type cargs)
              actual (check-funapp fexpr args ftype argtys expected)]
          (assoc expr
                 :fexpr cfexpr
                 :args cargs
                 expr-type actual))))))

;lam-result in TR
(defrecord FnResult [args kws rest drest body]
  "Results of checking a fn method"
  [(every? symbol? (map first args))
   (every? Type? (map second args))
   (nil? kws)
   ((some-fn nil? (hvector-c? symbol? Type?)) rest)
   (nil? drest)
   (TCResult? body)])

;[(Seqable Expr) (Option Expr) FnIntersection -> (Seqable Function)]
(defn relevant-Fns
  "Given a set of required-param exprs, rest-param expr, and a FnIntersection,
  returns a seq of Functions containing Function types
  whos arities could be a subtype to the method with the fixed and rest parameters given"
  [required-params rest-param fin]
  {:pre [(FnIntersection? fin)]
   :post [(every? Function? %)]}
  (assert (not (some :drest (:types fin))))
  (let [nreq (count required-params)]
    ;(prn "nreq" nreq)
    ;(prn "rest-param" rest-param)
    (filter (fn [{:keys [dom rest]}]
              (if rest-param 
                (and rest (<= nreq (count dom)))
                (and (not rest) (= nreq (count dom)))))
            (:types fin))))

(declare check-fn)

(def ^:dynamic *check-fn-method1-checkfn*)
; [(U nil Type) (U nil DottedPretype) -> Type]
; takes the current rest or drest argument (only one is non-nil) and returns
; the type to assign the rest parameter
(def ^:dynamic *check-fn-method1-rest-type*)

(defmethod check :fn-expr
  [{:keys [env] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert (:line env))
  (binding [*current-env* env
            *current-expr* expr
            *check-fn-method1-checkfn* check
            *check-fn-method1-rest-type* (fn [rest drest]
                                           {:pre [(or (Type? rest)
                                                      (DottedPretype? drest))
                                                  (not (and rest drest))]
                                            :post [(Type? %)]}
                                           (Un -nil (In (RClass-of Seqable [(or rest (.pre-type ^DottedPretype drest))])
                                                        (make-CountRange 1))))]
    (let [type (check-fn expr (or expected
                                  (ret (make-FnIntersection
                                         (make-Function [] -any -any)))))]
      (assoc expr
             expr-type type))))

(declare check-anon-fn-method abstract-filter abo abstract-object)

;[TCResult (Seqable Symbol) -> Result]
(defn abstract-result [result arg-names]
  {:pre [(TCResult? result)
         (every? symbol? arg-names)]
   :post [(Result? %)]}
  (let [keys (range (count arg-names))]
    (make-Result
      (ret-t result)
      (abstract-filter arg-names keys (ret-f result))
      (abstract-object arg-names keys (ret-o result)))))

;[(Seqable Symbol) (Seqable AnyInteger) RObject -> RObject]
(defn abstract-object [ids keys o]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         (RObject? o)]
   :post [(RObject? %)]}
  (letfn [(lookup [y]
            {:pre [(symbol? y)]
             :post [((some-fn nil? integer?) %)]}
            (some (fn [[x i]] (and (= x y) i))
                  (map vector ids keys)))]
    (cond
      (and (Path? o)
           (lookup (:id o))) (update-in o [:id] lookup)
      :else -empty)))

;[(Seqable Symbol) (Seqable AnyInteger) (U NoFilter FilterSet) 
;  -> (U NoFilter FilterSet)]
(defn abstract-filter [ids keys fs]
  {:pre [(every? symbol? ids)
         (every? integer? keys)
         ((some-fn NoFilter? FilterSet?) fs)]
   :post [((some-fn NoFilter? FilterSet?) %)]}
;  (prn "abstract filter")
;  (prn ids keys fs)
  (cond
    (FilterSet? fs)
    (let [{fs+ :then fs- :else} fs]
      (-FS (abo ids keys fs+)
           (abo ids keys fs-)))
    (NoFilter? fs) (-FS -top -top)))

(derive ::abo fold-rhs-default)

(add-fold-case ::abo
               TypeFilter
               (fn [{:keys [type path id] :as fl} {{:keys [lookup]} :locals}]
                 ;if variable goes out of scope, replace filter with -top
                 (if (lookup id)
                   (-filter type (lookup id) path)
                   -top)))

(add-fold-case ::abo
               NotTypeFilter
               (fn [{:keys [type path id] :as fl} {{:keys [lookup]} :locals}]
                 ;if variable goes out of scope, replace filter with -top
                 (if (lookup id)
                   (-not-filter type (lookup id)  path)
                   -top)))

;[(Seqable Symbol) (Seqable AnyInteger) Filter -> Filter]
(defn abo [xs idxs f]
  {:pre [(every? symbol? xs)
         (every? integer? idxs)
         (Filter? f)]
   :post [(Filter? %)]}
;  (prn "abo")
;  (prn xs idxs f)
  (letfn [(lookup [y]
            {:pre [(symbol? y)]
             :post [((some-fn nil? integer?) %)]}
            (some (fn [[x i]] (and (= x y) i))
                  (map vector xs idxs)))
          (rec [f] (abo xs idxs f))
          (sb-t [t] t)]
    (fold-rhs ::abo
      {:type-rec sb-t 
       :filter-rec rec
       :locals {:lookup lookup}}
      f)))

;[FnResult -> Function]
(defn FnResult->Function [{:keys [args kws rest drest body] :as fres}]
  {:pre [(FnResult? fres)]
   :post [(Function? %)]}
  (assert (not kws))
  (let [arg-names (doall
                    (concat (map first args)
                            (when rest
                              [(first rest)])
                            (when drest
                              [(first drest)]))) ;TODO kws
                            ]
    (->Function
      (map second args)
      (abstract-result body arg-names)
      (when rest
        (second rest))
      (when drest
        (second drest))
      nil)))

;TODO eliminate, only used in pfn>, not needed.
(defn check-anon-fn
  "Check anonymous function, with annotated methods. methods-types
  is a (Seqable (HMap {:dom (Seqable Type) :rng (U nil Type)}))"
  [{:keys [methods] :as expr} methods-types & {:keys [poly]}]
  {:pre [(every? (hmap-c? :dom (every-c? Type?)
                          :rng (some-fn nil? Type?)
                          :rest nil? ;TODO
                          :drest nil?) ;TODO
                 methods-types)
         ((some-fn nil? 
                   (every-c? (hvector-c? symbol? Bounds?)))
            poly)]
   :post [(TCResult? (expr-type %))]}
  (cond
    ; named fns must be fully annotated, and are checked with normal check
    (:name expr) (let [ftype (apply make-FnIntersection 
                                    (doall (for [{:keys [dom rng]} methods-types]
                                             (if rng
                                               (make-Function dom rng)
                                               (throw (Exception. "Named anonymous functions require return type annotation"))))))
                       ftype (if poly
                               (Poly* (map first poly)
                                      (map second poly)
                                      ftype
                                      (map first poly))
                               ftype)]

                   (check expr (ret ftype)))
    :else
    (let [;_ (prn methods methods-types expr)
          ftype (apply make-FnIntersection (mapv FnResult->Function 
                                                 (mapv (fn [m {:keys [dom rng]}]
                                                         (check-anon-fn-method m dom rng))
                                                       methods methods-types)))]
      (assoc expr
             expr-type (ret ftype (-FS -top -bot) -empty)))))

(declare ^:dynamic *recur-target* ->RecurTarget)

;[Type -> '[Type (Option (Seqable Symbol)) (Option (Seqable F)) (Option (Seqable Bounds)) (Option (U :Poly :PolyDots))]
; -> Type]
(defn unwrap-poly
  "Return a pair vector of the instantiated body of the possibly polymorphic
  type and the names used"
  [t]
  {:pre [(Type? t)]
   :post [((hvector-c? Type? 
                       (some-fn nil? (every-c? symbol?))
                       (some-fn nil? (every-c? F?))
                       (some-fn nil? (every-c? Bounds?))
                       (some-fn nil? #{:Poly :PolyDots})) %)]}
  (cond
    (Poly? t) (let [_ (assert (Poly-free-names* t) (unparse-type t))
                    old-nmes (Poly-free-names* t)
                    _ (assert ((every-pred seq (every-c? symbol?)) old-nmes))
                    new-nmes (repeatedly (:nbound t) gensym)
                    new-frees (map make-F new-nmes)]
                (prn "there")
                [(Poly-body* new-nmes t) old-nmes new-frees (Poly-bbnds* new-nmes t) :Poly])
    (PolyDots? t) (let [_ (assert (-> t meta :actual-frees))
                        old-nmes (-> t meta :actual-frees)
                        _ (assert ((every-pred seq (every-c? symbol?)) old-nmes))
                        new-nmes (repeatedly (:nbound t) gensym)
                        new-frees (map make-F new-nmes)]
                    (prn "here")
                    [(PolyDots-body* new-nmes t) old-nmes new-frees (PolyDots-bbnds* new-nmes t) :PolyDots])
    :else [t nil nil nil nil]))

;[Type (Seqable Symbol) (Seqable F) (U :Poly :Polydots nil) -> Type]
(defn rewrap-poly [body orig-names inst-frees bnds poly?]
  {:pre [(Type? body)
         (every? symbol? orig-names)
         (every? F? inst-frees)
         ((some-fn nil? #{:Poly :PolyDots}) poly?)]
   :post [(Type? %)]}
  (case poly?
    :Poly (Poly* (map :name inst-frees) bnds body orig-names)
    :PolyDots (with-meta (PolyDots* (map :name inst-frees) bnds body)
                         {:actual-frees orig-names})
    body))

(declare check-fn-method check-fn-method1)

;[FnExpr (Option Type) -> Expr]
(defn check-fn 
  "Check a fn to be under expected and annotate the inferred type"
  [{:keys [methods] :as fexpr} expected]
  {:pre [(TCResult? expected)]
   :post [(TCResult? %)]}
  (let [; try and unwrap type enough to find function types
        exp (resolve-to-ftype (ret-t expected))
        ; unwrap polymorphic expected types
        [fin orig-names inst-frees bnds poly?] (unwrap-poly exp)
        ; once more to make sure (FIXME is this needed?)
        fin (resolve-to-ftype fin)
        ;ensure a function type
        _ (assert (FnIntersection? fin)
                  (str (when *current-env*
                         (str (:line *current-env*) ": "))
                       (unparse-type fin) " is not a function type"))
        ;collect all inferred Functions
        inferred-fni (with-locals (when-let [name (hygienic/hname-key fexpr)] ;self calls
                                    (assert expected "Recursive methods require full annotation")
                                    {name (ret-t expected)})
                       ;scope type variables from polymorphic type in body
                       (with-free-mappings (case poly?
                                             :Poly (zipmap orig-names (map #(hash-map :F %1 :bnds %2) inst-frees bnds))
                                             :PolyDots (zipmap (next orig-names) 
                                                               (map #(hash-map :F %1 :bnds %2) (next inst-frees) (next bnds)))
                                             nil)
                         (with-dotted-mappings (case poly?
                                                 :PolyDots {(last orig-names) (last inst-frees)}
                                                 nil)
                           (apply make-FnIntersection
                                  (mapcat (fn [method]
                                            (let [fnt (check-fn-method method fin)]
                                              fnt))
                                          methods)))))
        ;rewrap in Poly or PolyDots if needed
        pfni (rewrap-poly inferred-fni orig-names inst-frees bnds poly?)]
    (ret pfni (-FS -top -bot) -empty)))

;[MethodExpr FnIntersection -> (I (Seqable Function) Sequential)]
(defn check-fn-method [{:keys [required-params rest-param] :as method} fin]
  {:pre [(FnIntersection? fin)]
   :post [(seq %)
          (every? Function? %)]}
  (let [mfns (relevant-Fns required-params rest-param fin)]
    #_(prn "relevant-Fns" (map unparse-type mfns))
    (cond
      ;If no matching cases, assign parameters to Any
      (empty? mfns) [(check-fn-method1 method (make-Function (repeat (count required-params) -any)
                                                             -any (when rest-param
                                                                    -any) nil))]
      :else (doall
              (for [f mfns]
                (check-fn-method1 method f))))))

(defmacro with-recur-target [tgt & body]
  `(binding [*recur-target* ~tgt]
     ~@body))

(declare env+)

;check method is under a particular Function, and return inferred Function
;[MethodExpr Function -> Function]
(defn check-fn-method1 [{:keys [body required-params rest-param] :as method} {:keys [dom rest drest] :as expected}]
  {:pre [(Function? expected)]
   :post [(Function? %)]}
  (let [expected-rng (Result->TCResult (:rng expected))
        ;ensure Function fits method
        _ (assert ((if rest <= =) (count required-params) (count dom))
                  (error-msg "Checking method with incorrect number of expected parameters"
                             ", expected " (count dom) " required parameter(s) with"
                             (if rest " a " " no ") "rest parameter, found " (count required-params)
                             " required parameter(s) and" (if rest-param " a " " no ")
                             "rest parameter."))

        _ (assert (or (not rest-param)
                      (some identity [drest rest]))
                  (error-msg "No type for rest parameter"))

;;unhygienic version
;        ; Update filters that reference bindings that the params shadow.
;        ; Abstracting references to parameters is handled later in abstract-result, but
;        ; suffers from bugs due to un-hygienic macroexpansion (see `abstract-result`).
;        ; In short, don't shadow parameters if you want meaningful filters.
;        props (mapv (fn [oldp]
;                      (reduce (fn [p sym]
;                                {:pre [(Filter? p)
;                                       (symbol? sym)]}
;                                (subst-filter p sym -empty true))
;                              oldp (map :sym required-params)))
;                    (:props *lexical-env*))

        _ (assert (every? symbol? (map hygienic/hsym-key required-params))
                  "Unhygienic AST detected")
        props (:props *lexical-env*)
        fixed-entry (map vector (map hygienic/hsym-key required-params) 
                         (concat dom (repeat (or rest 
                                                 (:pre-type drest)))))
        rest-entry (when rest-param
                     [[(hygienic/hsym-key rest-param) 
                       (*check-fn-method1-rest-type* rest drest)]])
        _ (assert ((hash-c? symbol? Type?) (into {} fixed-entry))
                  (into {} fixed-entry))
        _ (assert ((some-fn nil? (hash-c? symbol? Type?)) (when rest-entry
                                                            (into {} rest-entry))))

        ; if this fn method is a multimethod dispatch method, then infer
        ; a new filter that results from being dispatched "here"
        mm-filter (when-let [{:keys [dispatch-fn-type dispatch-val-ret]} *current-mm*]
                    (assert (and dispatch-fn-type dispatch-val-ret))
                    (assert (not (or drest rest rest-param)))
                    (let [disp-app-ret (check-funapp nil nil 
                                                     (ret dispatch-fn-type)
                                                     (map ret dom (repeat (-FS -top -top)) 
                                                          (map (comp #(->Path nil %) hygienic/hsym-key) required-params))
                                                     nil)
                          ;_ (prn "disp-app-ret" disp-app-ret)
                          ;_ (prn "disp-fn-type" (unparse-type dispatch-fn-type))
                          ;_ (prn "dom" dom)
                          isa-ret (tc-isa? disp-app-ret dispatch-val-ret)
                          then-filter (-> isa-ret ret-f :then)
                          _ (assert then-filter)]
                      then-filter))
        ;_ (prn "^^^ mm-filter")

        ;_ (prn "funapp1: inferred mm-filter" mm-filter)

        env (let [env (-> *lexical-env*
                        ;add mm-filter
                        (assoc-in [:props] (concat props (when mm-filter [mm-filter])))
                        ;add parameters to scope
                        ;IF UNHYGIENIC order important, (fn [a a & a]) prefers rightmost name
                        (update-in [:l] merge (into {} fixed-entry) (into {} rest-entry)))
                  flag (atom false :validator boolean?)
                  env (if mm-filter
                        (let [t (env+ env [mm-filter] flag)]
                          t)
                        env)]
              (assert (not @flag) "Unreachable method: Local inferred to be bottom when applying multimethod filter")
              env)
                

        ; rng before adding new filters
        crng-nopass
        (binding [*current-mm* nil]
          (with-lexical-env env
            (with-recur-target (->RecurTarget dom rest drest nil)
              (*check-fn-method1-checkfn* body expected-rng))))

        ; Apply the filters of computed rng to the environment and express
        ; changes to the lexical env as new filters, and conjoin with existing filters.

        ;_ (prn "crng-nopass" crng-nopass)
        {:keys [then else]} (-> crng-nopass expr-type ret-f)
        then-env (env+ env [then] (atom true))
        new-then-props (reduce (fn [fs [sym t]]
                                 {:pre [((set-c? Filter?) fs)]}
                                 (if (= t (get-in env [:l sym]))
                                   ;type hasn't changed, no new propositions
                                   fs
                                   ;new type, add positive proposition
                                   (conj fs (-filter t sym))))
                               #{}
                               (:l then-env))

        crng (update-in crng-nopass [expr-type :fl :then] 
                        (fn [f]
                          (apply -and f new-then-props)))
        _ (binding [*current-expr* body
                    *current-env* (:env body)]
            (subtype (-> crng expr-type ret-t) (ret-t expected-rng)))]
    (FnResult->Function 
      (->FnResult fixed-entry nil 
                  (when (and rest rest-param)
                    [(hygienic/hsym-key rest-param) rest])
                  (when (and drest rest-param) 
                    [(hygienic/hsym-key rest-param) drest])
                  (expr-type crng)))))


(defmethod check :do
  [{:keys [exprs] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  (let [nexprs (count exprs)
        [env cexprs]
        (reduce (fn [[env exprs] [n expr]]
                  {:pre [(PropEnv? env)
                         (integer? n)
                         (< n nexprs)]
                   :post [(hvector-c? PropEnv? vector?)]}
                  (let [cexpr (binding [*current-expr* expr]
                                (with-lexical-env env
                                  (check expr 
                                         ;propagate expected type only to final expression
                                         (when (= (inc n) nexprs)
                                           expected))))
                        flow (-> cexpr expr-type ret-flow flow-normal)
                        flow-atom (atom true)
                        ;add normal flow filter
                        ;Ignore if bottom (exceptional return)
                        nenv (if (= -bot flow)
                               env
                               (-> env
                                 (env+ [flow] flow-atom)))
                        _ (assert @flow-atom (str "Applying flow filter resulted in local being bottom"
                                                  "\n"
                                                  (with-out-str (print-env* nenv))
                                                  "\nOld: "
                                                  (with-out-str (print-env* env))))]
                    [nenv (conj exprs cexpr)]))
                [*lexical-env* []] (map-indexed vector exprs))]
    (assoc expr
           :exprs cexprs
           expr-type (-> cexprs last expr-type)))) ;should be a ret already

(defmethod check :local-binding-expr
  [{:keys [local-binding] :as expr} & [expected]]
  (let [sym (hygienic/hsym-key local-binding)
        t (binding [*var-annotations* VAR-ANNOTATIONS]
            (type-of sym))
        _ (assert (or (not expected)
                      (subtype? t (ret-t expected)))
                  (error-msg "Local binding " sym " expected type " (unparse-type (ret-t expected))
                             ", but actual type " (unparse-type t)))]
    (assoc expr
           expr-type (ret t 
                          (-FS (-not-filter (Un -nil -false) sym)
                               (-filter (Un -nil -false) sym))
                          (->Path nil sym)))))


(declare Java-symbol->Type)

;[Method -> Symbol]
(defn Method->symbol [{name-sym :name :keys [declaring-class] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [((every-pred namespace symbol?) %)]}
  (symbol (name declaring-class) (name name-sym)))

;[Symbol Boolean -> (Option Type)]
(defn symbol->PArray [sym nilable?]
  {:pre [(symbol? sym)
         (boolean? nilable?)]
   :post [((some-fn nil? PrimitiveArray?) %)]}
  (let [s (str sym)]
    (when (.endsWith s "<>")
      (let [^String s-nosuffix (apply str (drop-last 2 s))]
        (assert (not (.contains s-nosuffix "<>")))
        ;Nullable elements
        (let [t (Java-symbol->Type (symbol s-nosuffix) nilable?)
              c (let [c (or (when-let [rclass (primitives (symbol s-nosuffix))]
                              (RClass->Class rclass))
                            (resolve (symbol s-nosuffix)))
                      _ (assert (class? c) s-nosuffix)]
                  c)]
          (->PrimitiveArray c t t))))))

;[Symbol Boolean -> Type]
(defn Java-symbol->Type [sym nilable?]
  {:pre [(symbol? sym)
         (boolean? nilable?)]
   :post [(Type? %)]}
  (if-let [typ (or (primitives sym)
                   (symbol->PArray sym nilable?)
                   (when-let [cls (resolve sym)]
                     (let [rcls-or-poly (@RESTRICTED-CLASS (Class->symbol cls))
                           ; use correct number of arguments. Could be more general by recognising variance.
                           nargs (when rcls-or-poly
                                   (if (Poly? rcls-or-poly)
                                     (let [^RClass body (Poly-body* (repeatedly (:nbound rcls-or-poly) gensym)
                                                                    rcls-or-poly)
                                           _ (assert (RClass? body))]
                                       (count (.poly? body)))
                                     (let [_ (assert (RClass? rcls-or-poly))]
                                       (count (.poly? ^RClass rcls-or-poly)))))]
;                       (prn "class" cls)
;                       (prn "nargs" nargs)
                       (apply Un (apply RClass-of cls (when nargs
                                                        ; fill in arguments with Any
                                                        [(repeat nargs -any)]))
                              (when nilable?
                                [-nil])))))]
    typ
    (throw (Exception. (str "Method symbol " sym " does not resolve to a type")))))

;[clojure.reflect.Method -> Type]
(defn- instance-method->Function [{:keys [parameter-types declaring-class return-type] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [(FnIntersection? %)]}
  (assert (class? (resolve declaring-class)))
  (make-FnIntersection (make-Function (concat [(RClass-of declaring-class nil)]
                                              (doall (map #(Java-symbol->Type % false) parameter-types)))
                                      (Java-symbol->Type return-type true))))

;[clojure.reflect.Field - Type]
(defn- Field->Type [{:keys [type flags] :as field}]
  {:pre [(instance? clojure.reflect.Field field)]
   :post [(Type? %)]}
  (cond
    (flags :enum) (Java-symbol->Type type false)
    :else (Java-symbol->Type type true)))

;[clojure.reflect.Method -> Type]
(defn- Method->Type [{:keys [parameter-types return-type flags] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [(FnIntersection? %)]}
  (let [msym (Method->symbol method)
        nparams (count parameter-types)]
    (make-FnIntersection (make-Function (doall (map (fn [[n tsym]] (Java-symbol->Type 
                                                                     tsym (nilable-param? msym nparams n)))
                                                    (map-indexed vector
                                                                 (if (:varargs flags)
                                                                   (butlast parameter-types)
                                                                   parameter-types))))
                                        (Java-symbol->Type return-type (not (nonnilable-return? msym nparams)))
                                        (when (:varargs flags)
                                          (Java-symbol->Type (last parameter-types) (nilable-param? msym nparams (dec nparams))))))))

;[clojure.reflect.Constructor -> Type]
(defn- Constructor->Function [{:keys [declaring-class parameter-types] :as ctor}]
  {:pre [(instance? clojure.reflect.Constructor ctor)]
   :post [(FnIntersection? %)]}
  (let [cls (resolve declaring-class)
        _ (when-not (class? cls)
            (throw (Exception. (str "Constructor for unresolvable class " (:class ctor)))))]
    (make-FnIntersection (make-Function (doall (map #(Java-symbol->Type % false) parameter-types))
                                    (RClass-of cls nil)
                                    nil nil
                                    :filter (-FS -top -bot))))) ;always a true value

;[MethodExpr -> (U nil NamespacedSymbol)]
(defn MethodExpr->qualsym [{c :class :keys [op method method-name] :as expr}]
  {:pre [(#{:static-method :instance-method} op)]
   :post [((some-fn nil? symbol?) %)]}
  (cond
    method (Method->symbol method)
    (and c method-name) (symbol (str (Class->symbol c))
                                (str method-name))))

;[MethodExpr Type Any -> Expr]
(defn check-invoke-method [{c :class :keys [args tag method env method-name] :as expr} expected inst?]
  {:pre [((some-fn nil? TCResult?) expected)]
   :post [(-> % expr-type TCResult?)]}
  #_(prn "invoke method: " (when method (Method->symbol method)) inst?)
  (binding [*current-env* env]
    (let [msym (MethodExpr->qualsym expr)
          rfin-type (ret (or (when msym
                               (@METHOD-OVERRIDE-ENV msym))
                             (when method
                               (Method->Type method))))
          _ (assert rfin-type (error-msg "Unresolved " (if inst? "instance" "static") 
                                         " method invocation " 
                                         (when c
                                           (str (Class->symbol c) "/"))
                                         method-name 
                                         ;", insufficient type hints."
                                         ;"\n\nForm:\n\t" (emit-form-fn expr))
                                         ))
          _ (when inst?
              (let [ctarget (check (:target expr))]
;                (prn "check target" (unparse-type (ret-t (expr-type ctarget)))
;                     (unparse-type (RClass-of (Class->symbol (resolve (:declaring-class method))) nil)))
                (when-not (subtype? (ret-t (expr-type ctarget)) (RClass-of (Class->symbol (resolve (:declaring-class method)))
                                                                           nil))
                  (throw (Exception. (error-msg "Cannot call instance method " (Method->symbol method)
                                                " on type " (pr-str (unparse-type (ret-t (expr-type ctarget))))
                                                "\n\n"
                                                "Form:"
                                                "\n\t" (emit-form-fn expr)))))))
          cargs (doall (map check args))
          result-type (check-funapp expr args rfin-type (map expr-type cargs) expected)]
      (assoc expr
             expr-type result-type))))

(defmethod check :static-method
  [expr & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  #_(prn "static-method" (-> expr :method :name))
  (let [spec (static-method-special expr expected)]
    (cond
      (not= ::not-special spec) spec
      :else (check-invoke-method expr expected false))))

(defmethod check :instance-method
  [expr & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [spec (instance-method-special expr expected)]
    (cond
      (not= ::not-special spec) spec
      :else (check-invoke-method expr expected true))))

(def COMPILE-STUB-PREFIX "compile__stub")

(declare unwrap-datatype)

(defmethod check :static-field
  [{:keys [field] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert field "Static field requires type hints")
  (let []
    (assoc expr
           expr-type (ret (Field->Type field)))))

(defmethod check :instance-field
  [expr & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  #_(prn "instance-field:" expr)
  (assert (:target-class expr) "Instance fields require type hints")
  (let [; may be prefixed by COMPILE-STUB-PREFIX
        target-class (symbol
                       (str/replace-first (.getName ^Class (:target-class expr))
                                          (str COMPILE-STUB-PREFIX ".")
                                          ""))
        ;_ (prn (:target-class expr))
        ;_ (prn "target class" (str target-class) target-class)
        ;_ (prn (class target-class))
        fsym (symbol (:field-name expr))]
    (if-let [dtp (@DATATYPE-ENV target-class)]
      (let [dt (if (Poly? dtp)
                 ;generate new names
                 (unwrap-datatype dtp (repeatedly (:nbound dtp) gensym))
                 dtp)
            _ (assert (DataType? dt))
            ft (or (-> dt :fields (get fsym))
                   (throw (Exception. (str "No field " fsym " in Datatype " target-class))))]
        (assoc expr
               expr-type (ret ft)))
      (throw (Exception. ":instance-field NYI")))))

;[Symbol -> Type]
(defn DataType-ctor-type [sym]
  (let [dtp (@DATATYPE-ENV sym)]
    (cond
      (DataType? dtp) (let [dt dtp]
                        (make-FnIntersection 
                          (make-Function (-> dt :fields vals) dt)))
      (Poly? dtp) (let [nms (repeatedly (:nbound dtp) gensym)
                        bbnds (Poly-bbnds* nms dtp)
                        dt (unwrap-datatype dtp nms)]
                    (Poly* nms
                           bbnds
                           (make-FnIntersection 
                             (make-Function (-> dt :fields vals) dt))
                           (Poly-free-names* dtp)))
      :else (throw (Exception. (str "Cannot get DataType constructor of " sym))))))

(defmethod check :instance-of
  [{cls :class :keys [the-expr] :as expr} & [expected]]
  (let [cls-stub (symbol (.getName ^Class cls))
        clssym (symbol (str/replace-first (str cls-stub) (str COMPILE-STUB-PREFIX ".") ""))
        inst-of (or (@DATATYPE-ENV clssym)
                    (RClass-of clssym))
        cexpr (check the-expr)
        expr-tr (expr-type cexpr)]
    (assoc expr
           expr-type (ret (Un -true -false)
                          (-FS (-filter-at inst-of (ret-o expr-tr))
                               (-not-filter-at inst-of (ret-o expr-tr)))
                          -empty))))

(defn ctor-Class->symbol 
  "Returns a symbol representing this constructor's Class, removing any compiler stubs."
  [cls]
  (let [cls-stub (Class->symbol cls)]
    (symbol (str/replace-first (str cls-stub) (str COMPILE-STUB-PREFIX ".") ""))))

(declare ^:dynamic *multimethod-expected*)

(defmulti new-special (fn [{:keys [class] :as expr} & [expected]] (ctor-Class->symbol class)))

;; Multimethod definition

(derive ::expected-dispatch-type fold-rhs-default)

(add-fold-case ::expected-dispatch-type
               Function
               (fn [ty _]
                 (assoc ty :rng (make-Result -any))))

;return the expected type for the dispatch fn of the given multimethod's expected type
;[Type -> Type]
(defn expected-dispatch-type [mm-type]
  {:pre [(AnyType? mm-type)]
   :post [(AnyType? %)]}
  (fold-rhs ::expected-dispatch-type
            {:type-rec expected-dispatch-type}
            mm-type))

(defmethod new-special 'clojure.lang.MultiFn
  [{[nme-expr dispatch-expr default-expr hierarchy-expr :as args] :args :as expr} & [expected]]
  (assert expected)
  (assert (= 4 (count args)))
  (assert (= (:val hierarchy-expr) #'clojure.core/global-hierarchy)
          "Multimethod hierarchy cannot be customised")
  (assert (= (:val default-expr) :default)
          "Non :default default dispatch value NYI")
  (let [mm-name (:val nme-expr)
        _ (assert (string? (:val nme-expr)))
        mm-qual (symbol (str (ns-name *ns*)) mm-name)
        ;_ (prn "mm-qual" mm-qual)
        ;_ (prn "expected ret-t" (unparse-type (ret-t expected)))
        ;_ (prn "expected ret-t class" (class (ret-t expected)))
        expected-mm-disp (expected-dispatch-type (ret-t expected))
        cdisp (check dispatch-expr (ret expected-mm-disp))
        _ (add-multimethod-dispatch-type mm-qual (ret-t (expr-type cdisp)))]
    (assoc expr
           expr-type (ret (In (RClass-of clojure.lang.MultiFn) (ret-t expected))))))

(defmethod new-special :default [expr & [expected]] ::not-special)

(defmethod check :new
  [{cls :class :keys [ctor args env] :as expr} & [expected]]
  #_(prn "check: :new" "env" env)
  (binding [*current-env* env]
    (let [spec (new-special expr expected)]
      (cond
        (not= ::not-special spec) spec
        :else
        (let [inst-types *inst-ctor-types*
              clssym (ctor-Class->symbol cls)
              ifn (let [ctor-fn (or (@CONSTRUCTOR-OVERRIDE-ENV clssym)
                                    (and (@DATATYPE-ENV clssym)
                                         (DataType-ctor-type clssym))
                                    (when ctor
                                      (Constructor->Function ctor)))
                        _ (assert ctor-fn ctor-fn)
                        ctor-fn (if inst-types
                                  (manual-inst ctor-fn inst-types)
                                  ctor-fn)]
                    (ret ctor-fn))
              ;_ (prn "Expected constructor" (unparse-type (ret-t ifn)))
              cargs (mapv check args)
              res-type (check-funapp expr args ifn (map expr-type cargs) nil)
              _ (when expected
                  (subtype (ret-t res-type) (ret-t expected)))]
          (assoc expr
                 expr-type res-type))))))

(defmethod check :throw
  [{:keys [exception] :as expr} & [expected]]
  (let [cexception (check exception)
        _ (assert (subtype? (ret-t (expr-type cexception))
                            (RClass-of (Class->symbol Throwable) nil))
                  (str "Can only throw Throwable, found "
                       (unparse-type (ret-t (expr-type cexception)))))]
    (assoc expr
           expr-type (ret (Un) 
                          (-FS -top -top) 
                          -empty
                          ;never returns normally
                          (-flow -bot)))))

(declare combine-props)

(defrecord RecurTarget [dom rest drest kws]
  "A target for recur"
  [(every? Type? dom)
   ((some-fn nil? Type?) rest)
   (nil? drest) ;TODO
   (nil? kws)]) ;TODO

(def ^:dynamic *recur-target* nil)
(set-validator! #'*recur-target* (some-fn nil? RecurTarget?))

;Arguments passed to recur must match recur target exactly. Rest parameter
;equals 1 extra argument, either a Seqable or nil.
(defmethod check :recur
  [{:keys [args env] :as expr} & [expected]]
  (binding [*current-env* env]
    (assert *recur-target* (error-msg "No recur target"))
    (let [{:keys [dom rest] :as recur-target} *recur-target*
          _ (assert (not ((some-fn :drest :kw) recur-target)) "NYI")
          fixed-args (if rest
                       (butlast args)
                       args)
          rest-arg (when rest
                     (last args))
          cargs (mapv check args (map ret (concat dom (when rest-arg
                                                        [(RClass-of Seqable [rest])]))))
          _ (assert (and (= (count fixed-args) (count dom))
                         (= (boolean rest) (boolean rest-arg)))
                    (error-msg "Wrong number of arguments to recur:"
                               " Expected: " ((if rest inc identity) 
                                                (count dom))
                               " Given: " ((if rest-arg inc identity)
                                             (count fixed-args))))]
      (assoc expr
             expr-type (ret (Un))))))

(def ^:dynamic *check-let-checkfn*)

(defn binding-init-sym [binding-init]
  {:pre [(= :binding-init (:op binding-init))]
   :post [(symbol? %)]}
  (-> binding-init
    :local-binding
    hygienic/hsym-key))

(defn check-let [binding-inits body expr is-loop expected & {:keys [expected-bnds]}]
  (assert (or (not is-loop) expected-bnds) (error-msg "Loop requires more annotations"))
  (let [check-let-checkfn *check-let-checkfn*
        env (reduce (fn [env [{{:keys [init]} :local-binding :as expr} expected-bnd]]
                      {:pre [(PropEnv? env)]
                       :post [(PropEnv? env)]}
                      (let [sym (binding-init-sym expr)
                            ; check rhs
                            {:keys [t fl flow]} (expr-type
                                                  (binding [*current-expr* init]
                                                    (with-lexical-env env
                                                      (check-let-checkfn init (when is-loop
                                                                                (ret expected-bnd))))))
                            _ (assert (or (not expected-bnd)
                                          (subtype? t expected-bnd))
                                      (error-msg "Loop variable " sym " initialised to "
                                                 (pr-str (unparse-type t))
                                                 ", expected " (pr-str (unparse-type expected-bnd))
                                                 "\n\nForm:\n\t" (emit-form-fn init)))
                            t (or expected-bnd t)]
                        (cond
                          (FilterSet? fl)
                          (let [{:keys [then else]} fl
                                p* [(-imp (-not-filter (Un -nil -false) sym) then)
                                    (-imp (-filter (Un -nil -false) sym) else)]
                                flow-f (flow-normal flow)
                                flow-atom (atom true)
                                new-env (-> env
                                          ;update binding type
                                          (assoc-in [:l sym] t)
                                          ;update props
                                          (update-in [:props] #(apply concat 
                                                                      (combine-props p* % (atom true))))
                                          (env+ [(if (= -bot flow-f) -top flow-f)] flow-atom))
                                _ (assert @flow-atom "Applying flow filter resulted in local being bottom")]
                            new-env)

                          (NoFilter? fl) (do
                                           (assert (= (-flow -top) flow))
                                           (-> env
                                             ;no propositions to add, just update binding type
                                             (assoc-in [:l sym] t))))))
                    *lexical-env* (map vector binding-inits (or expected-bnds
                                                                (repeat nil))))

        cbody (with-lexical-env env
                (if is-loop
                  (binding [*recur-target* (->RecurTarget expected-bnds nil nil nil)]
                    (check-let-checkfn body expected))
                  (binding [*current-expr* body]
                    (check-let-checkfn body expected))))
        ;now we return a result to the enclosing scope, so we
        ;erase references to any bindings this scope introduces
        unshadowed-ret
        (reduce (fn [ty sym]
                  {:pre [(TCResult? ty)
                         (symbol? sym)]}
                  (-> ty
                    (update-in [:t] subst-type sym -empty true)
                    (update-in [:fl] subst-filter-set sym -empty true)
                    (update-in [:o] subst-object sym -empty true)
                    (update-in [:flow :normal] subst-filter sym -empty true)))
                (expr-type cbody)
                (map (comp hygienic/hsym-key :local-binding) binding-inits))]
    (assoc expr
           expr-type unshadowed-ret)))

;unhygienic version
;(defn check-let [binding-inits body expr is-loop expected & {:keys [expected-bnds]}]
;  (assert (or (not is-loop) expected-bnds) (error-msg "Loop requires more annotations"))
;  (let [check-let-checkfn *check-let-checkfn*
;        env (reduce (fn [env [{{:keys [sym init]} :local-binding} expected-bnd]]
;                      {:pre [(PropEnv? env)]
;                       :post [(PropEnv? env)]}
;                        (let [;TODO optimisation: this should be false when aliasing like (let [a a] ...)
;                              shadows-local? (-> env :l (find sym))
;                              ; check rhs
;                              {:keys [t fl o]} (let [noshadow-ret 
;                                                     (time
;                                                     (->
;                                                       (expr-type
;                                                         (binding [*current-expr* init]
;                                                           (with-lexical-env env
;                                                             (check-let-checkfn init (when is-loop
;                                                                                       (ret expected-bnd)))))))
;                                                       )
;                                                     _ (prn "^^ noshadow-ret")
;                                                     
;                                                     ;substitute previous references to sym with an empty object,
;                                                     ;as old binding is shadowed
;                                                     ; Rather expensive, only perform when necessary (if shadowing actually occurs).
;                                                     shadow-ret
;                                                     (time 
;                                                       (if shadows-local?
;                                                         (-> noshadow-ret
;                                                           (update-in [:t] subst-type sym -empty true)
;                                                           (update-in [:fl] subst-filter-set sym -empty true)
;                                                           (update-in [:o] subst-object sym -empty true))
;                                                         noshadow-ret))
;                                                     _ (prn "^^ shadow-ret")]
;                                                 shadow-ret)
;
;                            ; update old env and new result with previous references of sym (which is now shadowed)
;                            ; replaced with an empty object
;                            ;
;                            ; This is rather expensive with large types, so only perform when another local binding
;                            ; is actually shadowed.
;                            
;                            env (time
;                                  (if shadows-local?
;                                  (-> env
;                                    (update-in [:l] #(let [sc (into {} (for [[oldsym ty] %]
;                                                                         [oldsym (subst-type ty sym -empty true)]))]
;                                                       sc))
;                                    (update-in [:props] (fn [props]
;                                                          (mapv #(subst-filter % sym -empty true) props))))
;                                  env))
;                              _ (prn "^^ calc shadow")]
;                        (cond
;                          (FilterSet? fl)
;                          (let [{:keys [then else]} fl
;                                p* [(-imp (-not-filter (Un -nil -false) sym) then)
;                                    (-imp (-filter (Un -nil -false) sym) else)]
;                                new-env (-> env
;                                          ;update binding type
;                                          (assoc-in [:l sym] t)
;                                          ;update props
;                                          (update-in [:props] #(apply concat 
;                                                                      (combine-props p* % (atom true)))))]
;                            new-env)
;
;                          (NoFilter? fl) (-> env
;                                           ;no propositions to add, just update binding type
;                                           (assoc-in [:l sym] t)))))
;                    *lexical-env* (map vector binding-inits (or expected-bnds
;                                                                (repeat nil))))
;
;        cbody (with-lexical-env env
;                (if is-loop
;                  (binding [*recur-target* (->RecurTarget expected-bnds nil nil nil)]
;                    (check-let-checkfn body expected))
;                  (binding [*current-expr* body]
;                    (check-let-checkfn body expected))))
;
;        ;now we return a result to the enclosing scope, so we
;        ;erase references to any bindings this scope introduces
;        unshadowed-type 
;        (reduce (fn [ty sym]
;                  {:pre [(TCResult? ty)
;                         (symbol? sym)]}
;                  (-> ty
;                    (update-in [:t] subst-type sym -empty true)
;                    (update-in [:fl] subst-filter-set sym -empty true)
;                    (update-in [:o] subst-object sym -empty true)))
;                (expr-type cbody)
;                (map (comp :sym :local-binding) binding-inits))]
;    (assoc expr
;           expr-type unshadowed-type)))

(defmethod check :let
  [{:keys [is-loop binding-inits body] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (binding [*check-let-checkfn* check]
    (if is-loop
      (let [loop-bnd-anns *loop-bnd-anns*]
        (binding [*loop-bnd-anns* nil]
          (check-let binding-inits body expr true expected :expected-bnds loop-bnd-anns)))
      (check-let binding-inits body expr false expected))))

;[(Seqable Filter) Filter -> Filter]
(defn resolve* [atoms prop]
  {:pre [(every? Filter? atoms)
         (Filter? prop)]
   :post [(Filter? %)]}
  (reduce (fn [prop a]
            (cond
              (AndFilter? a)
              (loop [ps (:fs a)
                     result []]
                (if (empty? ps)
                  (apply -and result)
                  (let [p (first ps)]
                    (cond
                      (opposite? a p) -bot
                      (implied-atomic? p a) (recur (next ps) result)
                      :else (recur (next ps) (cons p result))))))
              :else prop))
          prop
          atoms))

;[(Seqable Filter) -> (Seqable Filter)]
(defn flatten-props [ps]
  {:post [(every? Filter? %)]}
  (cond
    (empty? ps) []
    (AndFilter? (first ps)) (flatten-props (concat (-> ps first :fs) (next ps)))
    :else (cons (first ps) (flatten-props (next ps)))))

(def type-equal? =)

;[(Seqable Filter) (Seqable Filter) (Atom Boolean) 
;  -> '[(Seqable (U ImpFilter OrFilter AndFilter))
;       (Seqable (U TypeFilter NotTypeFilter))]]
(defn combine-props [new-props old-props flag]
  {:pre [(every? Filter? (concat new-props old-props))
         (instance? clojure.lang.Atom flag)
         (boolean? @flag)]
   :post [(let [[derived-props derived-atoms] %]
            (and (every? (some-fn ImpFilter? OrFilter? AndFilter?) derived-props)
                 (every? (some-fn TypeFilter? NotTypeFilter?) derived-atoms)))]}
  (let [atomic-prop? (some-fn TypeFilter? NotTypeFilter?)
        {new-atoms true new-formulas false} (group-by atomic-prop? (flatten-props new-props))]
    (loop [derived-props []
           derived-atoms new-atoms
           worklist (concat old-props new-formulas)]
      (if (empty? worklist)
        [derived-props derived-atoms]
        (let [p (first worklist)
              p (resolve* derived-atoms p)]
          (cond
            (AndFilter? p) (recur derived-props derived-atoms (concat (:fs p) (next worklist)))
            (ImpFilter? p) 
            (let [{:keys [a c]} p
                  implied? (some (fn [p] (implied-atomic? a p)) (concat derived-props derived-atoms))]
              #_(prn "combining " (unparse-filter p) " with " (map unparse-filter (concat derived-props
                                                                                        derived-atoms))
                   " and implied:" implied?)
              (if implied?
                (recur derived-props derived-atoms (cons c (rest worklist)))
                (recur (cons p derived-props) derived-atoms (next worklist))))
            (OrFilter? p)
            (let [ps (:fs p)
                  new-or (loop [ps ps
                                result []]
                           (cond
                             (empty? ps) (apply -or result)
                             (some (fn [other-p] (opposite? (first ps) other-p))
                                   (concat derived-props derived-atoms))
                             (recur (next ps) result)
                             (some (fn [other-p] (implied-atomic? (first ps) other-p))
                                   derived-atoms)
                             -top
                             :else (recur (next ps) (cons (first ps) result))))]
              (if (OrFilter? new-or)
                (recur (cons new-or derived-props) derived-atoms (next worklist))
                (recur derived-props derived-atoms (cons new-or (next worklist)))))
            (and (TypeFilter? p)
                 (type-equal? (Un) (:type p)))
            (do 
              ;(prn "Variable set to bottom:" (unparse-filter p))
              (reset! flag false)
              [derived-props derived-atoms])
            (TypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (and (NotTypeFilter? p)
                 (type-equal? (->Top) (:type p)))
            (do 
              ;(prn "Variable set to bottom:" (unparse-filter p))
              (reset! flag false)
              [derived-props derived-atoms])
            (NotTypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (TopFilter? p) (recur derived-props derived-atoms (next worklist))
            (BotFilter? p) (do 
                             ;(prn "Bot filter found")
                             (reset! flag false)
                             [derived-props derived-atoms])
            :else (recur (cons p derived-props) derived-atoms (next worklist))))))))

;; also not yet correct
;; produces old without the contents of rem
;[Type Type -> Type]
(defn remove* [old rem]
  (let [initial (if (subtype? old rem)
                  (Un) ;the empty type
                  (cond
                    ;FIXME TR also tests for App? here. ie (or (Name? old) (App? old))
                    (Name? old) ;; must be different, since they're not subtypes 
                                ;; and n must refer to a distinct struct type
                    old
                    (Union? old) (let [l (:types old)]
                                   (apply Un (map (fn [e] (remove* e rem)) l)))
                    (Mu? old) (remove* (unfold old) rem)
                    (Poly? old) (let [vs (repeatedly (:nbound old) gensym)
                                      b (Poly-body* vs old)]
                                  (Poly* vs 
                                         (Poly-bbnds* vs old)
                                         (remove* b rem)
                                         (Poly-free-names* old)))
                    :else old))]
    (if (subtype? old initial) old initial)))

; This is where filters are applied to existing types to generate more specific ones
;[Type Filter -> Type]
(defn update [t lo]
  (let [t (fully-resolve-type t)]
    (cond
      (and (TypeFilter? lo)
           (empty? (:path lo))) 
      (let [u (:type lo)
            _ (assert (Type? u))
            r (restrict u t)]
        r)

      (and (NotTypeFilter? lo)
           (empty? (:path lo))) 
      (let [u (:type lo)]
        (assert (Type? u))
        (remove* t u))

      ;heterogeneous map ops
      (and (TypeFilter? lo)
           (KeyPE? (first (:path lo)))
           (HeterogeneousMap? t)) 
      (let [{:keys [type path id]} lo
            [{fpth-kw :val} & rstpth] path
            fpth (->Value fpth-kw)
            type-at-pth (get (:types t) fpth)]
        (if type-at-pth 
          (-hmap (assoc (:types t) fpth (update type-at-pth (-filter type id rstpth))))
          (Bottom)))

      (and (NotTypeFilter? lo)
           (KeyPE? (first (:path lo)))
           (HeterogeneousMap? t)) 
      (let [{:keys [type path id]} lo
            [{fpth-kw :val} & rstpth] path
            fpth (->Value fpth-kw)
            type-at-pth (get (:types t) fpth)]
        (if type-at-pth 
          (-hmap (assoc (:types t) fpth (update type-at-pth (-not-filter type id rstpth))))
          (Bottom)))


      (and (TypeFilter? lo)
           (CountPE? (first (:path lo))))
      (let [u (:type lo)]
        (if-let [cnt (when (and (Value? u) (integer? (:val u)))
                       (make-ExactCountRange (:val u)))]
          (restrict cnt t)
          (do (prn "WARNING:" (str "Cannot infer Count from type " (unparse-type u)))
            t)))

      ;can't do much without a NotCountRange type or difference type
      (and (NotTypeFilter? lo)
           (CountPE? (first (:path lo))))
      t

      ;ClassPE
      (and (TypeFilter? lo)
           (ClassPE? (-> lo :path first)))
      (let [_ (assert (empty? (rest (:path lo))))
            u (:type lo)]
        (cond 
          ;restrict the obvious case where the path is the same as a Class Value
          ; eg. #(= (class %) Number)
          (and (Value? u)
               (class? (:val u)))
          (restrict (RClass-of (:val u)) t)

          ; handle (class nil) => nil
          (Nil? u)
          (restrict -nil t)

          :else
          (do (prn "WARNING:" (str "Cannot infer type via ClassPE from type " (unparse-type u)))
            t)))

      ; Does not tell us anything.
      ; eg. (= Number (class x)) ;=> false
      ;     does not reveal whether x is a subtype of Number, eg. (= Integer (class %))
      (and (NotTypeFilter? lo)
           (ClassPE? (-> lo :path first)))
      t

      (Union? t) (let [ts (:types t)
                       new-ts (mapv (fn [t] 
                                      (let [n (update t lo)]
                                        n))
                                    ts)]
                   (apply Un new-ts))
      (Intersection? t) (let [ts (:types t)]
                          (apply In (doall (map (fn [t] (update t lo)) ts))))
      
      ;keyword invoke of non-hmaps
      ;FIXME TypeFilter case can refine type further
      (and (or (TypeFilter? lo)
               (NotTypeFilter? lo))
           (KeyPE? (first (:path lo))))
      (do (assert (= (count (:path lo)) 1) 
                  (str "Further path NYI " (pr-str (:path lo))
                       (unparse-type t)))
        t)

      :else (throw (Exception. (error-msg "update along ill-typed path " (unparse-type t) " " (with-out-str (pr lo))))))))

; f can be a composite filter. bnd-env is a the :l of a PropEnv
; ie. a map of symbols to types
;[(IPersistentMap Symbol Type) Filter -> PropEnv]
(defn update-composite [bnd-env f]
  {:pre [(lex-env? bnd-env)
         (Filter? f)]
   :post [(lex-env? %)]}
  ;(prn "update-composite" bnd-env f)
  (cond
;    (and (AndFilter? f) 
;         (every? atomic-filter? (.fs f)))
;    (reduce (fn [env a] (update-composite env a))
;            bnd-env (.fs f))
;    (and (OrFilter? f) 
;         (every? atomic-filter? (.fs f)))
    (BotFilter? f)
    (zipmap (:keys bnd-env) (Un))

    (or (TypeFilter? f)
        (NotTypeFilter? f))
    (let [x (:id f)]
      (update-in bnd-env [x] (fn [t]
                               ;check if var is ever a target of a set!
                               (if (is-var-mutated? x)
                                 ; if it is, we do nothing
                                 t
                                 ;otherwise, refine the type
                                 (let [t (or t (->Top))
                                       new-t (update t f)]
                                   new-t)))))
    :else bnd-env))

;; sets the flag box to #f if anything becomes (U)
;[PropEnv (Seqable Filter) (Atom Boolean) -> PropEnv]
(defn env+ [env fs flag]
  {:pre [(PropEnv? env)
         (every? Filter? fs)
         (boolean? @flag)]
   :post [(PropEnv? %)
          (boolean? @flag)]}
  #_(prn 'env+ fs)
  (let [[props atoms] (combine-props fs (:props env) flag)]
    (reduce (fn [env f]
              {:pre [(PropEnv? env)
                     (Filter? f)]}
              (let [new-env (update-in env [:l] update-composite f)]
                ; update flag if a variable is now bottom
                (when-let [bs (seq (filter (comp #{(Un)} val) (:l new-env)))]
                  ;(prn "variables are now bottom: " (map key bs))
                  (reset! flag false))
                new-env))
            (assoc env :props (concat atoms props))
            (concat atoms props))))

(def object-equal? =)

(def ^:dynamic *check-if-checkfn*)

;[TCResult Expr Expr (Option Type) -> TCResult]
(defn check-if [tst thn els & [expected]]
  {:pre [(TCResult? tst)
         ((some-fn TCResult? nil?) expected)]
   :post [(TCResult? %)]}
  (letfn [(tc [expr reachable?]
            {:post [(TCResult? %)]}
            (when-not reachable?
              #_(prn "Unreachable code found.. " expr))
            (cond
              ;; if reachable? is #f, then we don't want to verify that this branch has the appropriate type
              ;; in particular, it might be (void)
              (and expected reachable?)
              (-> (*check-if-checkfn* expr (-> expected
                                             (assoc :fl (-FS -top -top))
                                             (assoc :o -empty)
                                             (assoc :flow (-flow -top))))
                expr-type)
              ;; this code is reachable, but we have no expected type
              reachable? (-> (*check-if-checkfn* expr) expr-type)
              ;; otherwise, this code is unreachable
              ;; and the resulting type should be the empty type
              :else (do #_(prn (error-msg "Not checking unreachable code"))
                      (ret (Un)))))]
    (let [{fs+ :then fs- :else :as f1} (ret-f tst)
;          _ (prn "check-if: fs+" (unparse-filter fs+))
;          _ (prn "check-if: fs-" (unparse-filter fs-))
          flag+ (atom true :validator boolean?)
          flag- (atom true :validator boolean?)

          ;_ (print-env)
          idsym (gensym)
          env-thn (env+ *lexical-env* [fs+] flag+)
;          _ (do (pr "check-if: env-thn")
;              (print-env env-thn))
          env-els (env+ *lexical-env* [fs-] flag-)
;          _ (do (pr "check-if: env-els")
;              (print-env env-els))
;          new-thn-props (set
;                          (filter atomic-filter?
;                                  (set/difference
;                                    (set (:props *lexical-env*))
;                                    (set (:props env-thn)))))
          ;_ (prn idsym"env+: new-thn-props" (map unparse-filter new-thn-props))
;          new-els-props (set
;                          (filter atomic-filter?
;                                  (set/difference
;                                    (set (:props *lexical-env*))
;                                    (set (:props env-els)))))
          ;_ (prn idsym"env+: new-els-props" (map unparse-filter new-els-props))
          {ts :t fs2 :fl os2 :o flow2 :flow :as then-ret} 
          (binding [*current-expr* thn]
            (with-lexical-env env-thn
              (tc thn @flag+)))

          {us :t fs3 :fl os3 :o flow3 :flow :as else-ret} 
          (binding [*current-expr* els]
            (with-lexical-env env-els
              (tc els @flag-)))]

      ;some optimization code here, contraditions etc? omitted

;      (prn "check-if: then branch:" (unparse-TCResult then-ret))
;      (prn "check-if: else branch:" (unparse-TCResult else-ret))
      (cond
        ;both branches reachable
        (and (not (type-equal? (Un) ts))
             (not (type-equal? (Un) us)))
        (let [r (let [filter (cond
                               (or (NoFilter? fs2)
                                   (NoFilter? fs3)) (-FS -top -top)
                               (and (FilterSet? fs2)
                                    (FilterSet? fs3))
                               (let [{f2+ :then f2- :else} fs2
                                     {f3+ :then f3- :else} fs3
                                     ; +ve test, +ve then
                                     new-thn-props (:props env-thn)
                                     new-els-props (:props env-els)
                                     +t+t (apply -and fs+ f2+ new-thn-props)
                                     ; -ve test, +ve else
                                     -t+e (apply -and fs- f3+ new-els-props)
                                     ; +ve test, -ve then
                                     +t-t (apply -and fs+ f2- new-thn-props)
                                     ; -ve test, -ve else
                                     -t-e (apply -and fs- f3- new-els-props)

                                     final-thn-prop (-or +t+t -t+e)
                                     final-els-prop (-or +t-t -t-e)
                                     fs (-FS final-thn-prop final-els-prop)]
                                 fs)
                               :else (throw (Exception. (str "What are these?" fs2 fs3))))
                      type (Un ts us)
                      object (if (object-equal? os2 os3) os2 (->EmptyObject))

                      ;only bother with something interesting if a branch is unreachable (the next two cond cases)
                      ;Should be enough for `assert`
                      ;flow (-flow (-or flow2 flow3))
                      flow (-flow -top)
                      ]
                  (ret type filter object flow))]
          ;(prn "check if:" "both branches reachable, with combined result" (unparse-TCResult r))
          (if expected (check-below r expected) r))
        ;; special case if one of the branches is unreachable
        (type-equal? us (Un))
        (if expected (check-below (ret ts fs2 os2 flow2) expected) (ret ts fs2 os2 flow2))
        (type-equal? ts (Un))
        (if expected (check-below (ret us fs3 os3 flow3) expected) (ret us fs3 os3 flow3))
        :else (throw (Exception. "Something happened"))))))

(defmethod check :if
  [{:keys [test then else] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [ctest (binding [*current-expr* test]
                (check test))]
    (assoc expr
           expr-type (binding [*check-if-checkfn* check]
                       (check-if (expr-type ctest) then else)))))

(declare check-multi-def)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multimethods

;[Expr (Option TCResult) -> Expr]
(defn check-normal-def [{:keys [var init init-provided env] :as expr} & [expected]]
  (assert (not expected))
  (assert init-provided)
  (let [t (binding [*var-annotations* VAR-ANNOTATIONS]
            (type-of (var->symbol var)))
        cinit (cond 
                (not init-provided) expr ;handle `declare`
                :else (check init (ret t)))
        _ (when init-provided
            (subtype (ret-t (expr-type cinit)) t))]
    ;def returns a Var
    (assoc expr
           expr-type (ret (RClass-of Var)))))

(defmethod check :def
  [{:keys [var init init-provided env] :as expr} & [expected]]
  (assert (not expected) expected)
  (assert (:line env))
  #_(prn "Checking" var)
  (binding [*current-env* env
            *current-expr* expr]
    (cond 
      ;ignore macro definitions and declare
      (or (.isMacro ^Var var)
          (not init-provided))
      (assoc expr
             expr-type (ret (RClass-of Var)))

      :else (check-normal-def expr expected))))


(declare check-new-instance-method)

;[Type (Seqable Symbol) -> Type]
;[Type -> Type]
(defn unwrap-datatype 
  "Takes a possibly polymorphic DataType and returns the 
  DataType after instantiating it"
  ([dt nms]
   {:pre [((some-fn DataType? Poly?) dt)
          (every? symbol? nms)]
    :post [(DataType? %)]}
   (if (Poly? dt)
     (Poly-body* nms dt)
     dt))
  ([dt] (let [nms (when (Poly? dt)
                    (repeatedly (:nbound dt) gensym))]
          (unwrap-datatype dt nms))))

(defmethod check :deftype*
  [{nme :name :keys [methods] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert nme) ;remove once analyze is released
  ;TODO check fields match
  #_(prn "Checking deftype definition:" nme)
  (let [cmmap (into {} (for [[k v] (:mmap expr)]
                         [[(symbol (first k)) (count (second k))]
                          (@#'clojure.reflect/method->map v)]))
        _ (assert ((hash-c? (hvector-c? (every-pred symbol? (complement namespace))
                                        nat?)
                            #(instance? clojure.reflect.Method %))
                     cmmap))
        dtp (@DATATYPE-ENV nme)
        dt (if (Poly? dtp)
             (do (assert (-> dtp meta :actual-frees))
               (unwrap-datatype dtp (-> dtp meta :actual-frees)))
             dtp)

        _ (assert (DataType? dt))
        _ (assert dt (str "Untyped datatype definition: " nme))
        ; update this deftype's ancestors to include each protocol/interface in this deftype
        old-ancestors (or (@DATATYPE-ANCESTOR-ENV nme) #{})
        ancestor-diff (set/difference
                        (set
                          (for [[_ method] cmmap]
                            (let [tsym (:declaring-class method)]
                              (if-let [cls (when-let [cls (resolve tsym)]
                                             (and (class? cls) 
                                                  cls))]
                                (or (first (filter #(= (:on-class %) tsym) (vals @PROTOCOL-ENV)))
                                    (RClass-of (Class->symbol cls) nil))
                                (resolve-protocol tsym)))))
                        old-ancestors)
        ;_ (prn "ancestor diff" ancestor-diff)
        _ (swap! DATATYPE-ANCESTOR-ENV update-in [nme] set/union ancestor-diff)
        _ (try
            (doseq [inst-method methods]
              #_(prn "Checking deftype* method: "(:name inst-method))
              (let [nme (:name inst-method)
                    _ (assert (symbol? nme))
                    ; minus the target arg
                    method-sig (cmmap [nme (dec (count (:required-params inst-method)))])
                    _ (assert (instance? clojure.reflect.Method method-sig))
                    expected-ifn 
                    (extend-method-expected dt
                                            (or (let [ptype (first
                                                              (filter #(= (:on-class %) (:declaring-class method-sig))
                                                                      (vals @PROTOCOL-ENV)))]
                                                  ;(prn "ptype" ptype)
                                                  (when ptype
                                                    (let [munged-methods (into {} (for [[k v] (:methods ptype)]
                                                                                    [(symbol (munge k)) v]))]
                                                      (munged-methods (:name method-sig)))))
                                                (instance-method->Function method-sig)))]
                (prn "ifn" (unparse-type expected-ifn))
                (with-locals (:fields dt)
                  ;(prn "lexical env when checking method" nme *lexical-env*)
                  ;(prn (:fields dt))
                  (check-new-instance-method
                    inst-method 
                    expected-ifn))))
            (catch Throwable e
              ; reset old ancestors
              (swap! DATATYPE-ANCESTOR-ENV update-in [nme] set/difference ancestor-diff)
              (throw e)))]
    (assoc expr
           expr-type (ret (let [res (resolve nme)]
                            (assert (class? res))
                            (-val res))))))

;[Expr FnIntersection -> Expr]
(defn check-new-instance-method
  [{:keys [body required-params] :as expr} expected-fin]
  {:pre [(FnIntersection? expected-fin)]}
  (let [_ (assert (= 1 (count (:types expected-fin))))
        {:keys [dom rng] :as expected-fn} (-> expected-fin :types first)
        _ (assert (not (:rest expected-fn)))
        cbody (with-locals (zipmap (map hygienic/hsym-key required-params) dom)
                (check body (ret (Result-type* rng)
                                 (Result-filter* rng)
                                 (Result-object* rng))))
        _ (subtype (-> cbody expr-type ret-t)
                   (Result-type* rng))]
    (assoc expr
           expr-type (expr-type cbody))))

(defmethod check :import*
  [{:keys [class-str] :as expr} & [expected]]
  (assoc expr
         expr-type (ret -nil)))

(defmethod check :case*
  [{:keys [] :as expr} & [expected]]
  #_(prn "Checking case")
  ; tests have no duplicates
  (let [;_ (prn (:the-expr expr))
        cthe-expr (check (:the-expr expr))
        etype (expr-type cthe-expr)
        ctests (mapv check (:tests expr))
        cdefault (check (:default expr))
        cthens-and-envs (doall
                          (for [[tst-ret thn] (map vector (map expr-type ctests) (:thens expr))]
                            (let [{{fs+ :then} :fl :as rslt} (tc-equiv := etype tst-ret)
                                  flag+ (atom true)
                                  env-thn (env+ *lexical-env* [fs+] flag+)
                                  then-ret (with-lexical-env env-thn
                                             (check thn))]
                              [(assoc thn
                                      expr-type (expr-type then-ret))
                               env-thn])))
        ;TODO consider tests that failed to refine env
        cdefault (check (:default expr))
        case-result (let [type (apply Un (map (comp :t expr-type) (cons cdefault (map first cthens-and-envs))))
                          ; TODO
                          filter (-FS -top -top)
                          ; TODO
                          object -empty]
                      (ret type filter object))]
    (assoc expr
           expr-type case-result)))

(defmethod check :catch
  [{ecls :class, :keys [handler local-binding] :as expr} & [expected]]
  (let [local-sym (hygienic/hsym-key local-binding)
        local-type (RClass-of ecls)
        chandler (with-locals {local-sym local-type}
                   (check handler expected))]
    (assoc expr
           expr-type (expr-type chandler))))

; filters don't propagate between components of a `try`, nor outside of it.
(defmethod check :try
  [{:keys [try-expr catch-exprs finally-expr] :as expr} & [expected]]
  (let [ctry-expr (check try-expr expected)
        ccatch-exprs (mapv #(check % expected) catch-exprs)
        _cfinally-expr_ (when finally-expr
                          (check finally-expr expected))]
    (assoc expr
           expr-type (ret (apply Un (-> ctry-expr expr-type ret-t) 
                                 (map (comp ret-t expr-type) ccatch-exprs))))))

(defmethod check :set!
  [{:keys [target val] :as expr} & [expected]]
  (let [ctarget (check target)
        cval (check val (expr-type ctarget))
        _ (assert (subtype? 
                    (-> cval expr-type ret-t)
                    (-> ctarget expr-type ret-t))
                  (error-msg "Cannot set! " (-> ctarget expr-type ret-t unparse-type pr-str)
                             " to " (-> cval expr-type ret-t unparse-type pr-str)
                             "\n\nForm:\n\t" (emit-form-fn expr)))]
    (assoc expr
           expr-type -any
           :target ctarget
           :val cval)))

(comment
  ;; error checking
  (cf (if 1 'a 'b) Number)
  )

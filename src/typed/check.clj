(in-ns 'typed.core)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checker

(declare ret-t ret-f ret-o)

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

(defn ret
  "Convenience function for returning the type of an expression"
  ([t] (ret t (-FS -top -top) (->EmptyObject)))
  ([t f] (ret t f (->EmptyObject)))
  ([t f o]
   {:pre [(AnyType? t)
          (FilterSet? f)
          (RObject? o)]
    :post [(TCResult? %)]}
   (->TCResult t f o)))

(defn ret-t [r]
  {:pre [(TCResult? r)]
   :post [(AnyType? %)]}
  (:t r))

(defn ret-f [r]
  {:pre [(TCResult? r)]
   :post [(FilterSet? %)]}
  (:fl r))

(defn ret-o [r]
  {:pre [(TCResult? r)]
   :post [(RObject? %)]}
  (:o r))

(def expr-type ::expr-type)

(defmulti check (fn [expr & [expected]]
                  {:pre [((some-fn nil? TCResult?) expected)]}
                  (:op expr)))

(defn check-top-level [nsym form]
  (ensure-clojure)
  (let [ast (analyze/analyze-form-in-ns nsym form)]
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

(defmulti constant-type class)

(defmethod constant-type nil [_] -nil)
(defmethod constant-type Class [v] (->Value v))
(defmethod constant-type Symbol [v] (->Value v))
(defmethod constant-type Long [v] (->Value v))
(defmethod constant-type Double [v] (->Value v))
(defmethod constant-type Integer [v] (->Value v))
(defmethod constant-type java.math.BigDecimal [v] (->Value v))
(defmethod constant-type clojure.lang.BigInt [v] (->Value v))
(defmethod constant-type String [v] (->Value v))
(defmethod constant-type Character [v] (->Value v))
(defmethod constant-type clojure.lang.Keyword [v] (->Value v))
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
  (-hmap (into {} (map #(vector (constant-type (first %))
                                (constant-type (second %)))
                       cmap))))

(defn check-value
  [{:keys [val] :as expr} & [expected]]
  (let [actual-type (constant-type val)
        _ (when expected
            (binding [*current-expr* expr]
              (subtype actual-type (ret-t expected))))]
    (assoc expr
           expr-type (if val
                       (ret actual-type
                            (-FS -top -bot)
                            -empty)
                       (ret actual-type
                            (-FS -bot -top)
                            -empty)))))

(defmethod check :constant [& args] (apply check-value args))
(defmethod check :number [& args] (apply check-value args))
(defmethod check :string [& args] (apply check-value args))
(defmethod check :keyword [& args] (apply check-value args))

(defmethod check :boolean
  [{:keys [val] :as expr} & [expected]]
  (assoc expr
         expr-type (if val
                     (ret -true
                          (-FS -top -bot))
                     (ret -false
                          (-FS -bot -top)))))

(defmethod check :nil 
  [expr & [expected]]
  (assoc expr
         expr-type (ret -nil (-FS -bot -top) -empty)))

(defmethod check :map
  [{:keys [keyvals] :as expr} & [expected]]
  (let [expected (when expected 
                   (ret-t expected))
        actual (-hmap (apply hash-map (map (comp ret-t expr-type) (mapv check keyvals))))
        _ (assert (or (not expected) (subtype? actual expected)) (type-error actual expected))]
    (assoc expr
           expr-type (ret actual))))

(defmethod check :set
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (mapv check args)
        res-type (RClass-of IPersistentSet [(apply Un (mapv (comp ret-t expr-type) cargs))])
        _ (when expected
            (assert (subtype? res-type (ret-t expected))
                    (type-error res-type (ret-t expected))))]
    (assoc expr
           expr-type (ret res-type))))

(defmethod check :vector
  [{:keys [args] :as expr} & [expected]]
  (let [cargs (mapv check args)
        res-type (-hvec (mapv (comp ret-t expr-type) cargs))
        _ (when expected
            (assert (subtype? res-type (ret-t expected))
                    (type-error res-type (ret-t expected))))]
    (assoc expr
           expr-type (ret res-type))))

(defmethod check :empty-expr 
  [{coll :coll :as expr} & [expected]]
  (assoc expr
         expr-type (ret (constant-type coll)
                        (-FS -top -bot)
                        (->EmptyObject))))

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

(defn index-free-in? [k type]
  (let [free-in? (atom false)]
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
        (FilterSet? fs) (-FS (subst-filter (add-extra-filter (.then fs)) k o polarity)
                             (subst-filter (add-extra-filter (.else fs)) k o polarity))
        :else (-FS -top -top)))))

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


(defn subst-type [t k o polarity]
  {:pre [(AnyType? t)
         (name-ref? k)
         (RObject? o)
         ((some-fn true? false?) polarity)]
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
(defn check-funapp1 [fexpr arg-exprs {:keys [dom rng rest drest kws] :as ftype0} argtys expected & {:keys [check?] :or {check? true}}]
  {:pre [(Function? ftype0)
         (every? TCResult? argtys)
         ((some-fn nil? TCResult?) expected)
         (boolean? check?)]
   :post [(TCResult? %)]}
  (assert (not drest) "funapp with drest args NYI")
  (assert (empty? (:mandatory kws)) "funapp with mandatory keyword args NYI")
;  (prn "check-funapp1")
;  (prn "argtys objects" (map ret-o argtys))
  ;checking
  (when check?
    (when (or (and (not rest) (not (= (count dom) (count argtys))))
              (and rest (< (count argtys) (count dom))))
      (throw (Exception. (error-msg "Wrong number of arguments, expected " (count dom) " and got "(count argtys)
                                    " for function " (unparse-type ftype0) " and arguments " (mapv (comp unparse-type ret-t) argtys)))))
    (doseq [[arg-t dom-t] (map vector (map ret-t argtys) (concat dom (when rest (repeat rest))))]
      (check-below arg-t dom-t)))
  (let [dom-count (count dom)
        arg-count (+ dom-count (if rest 1 0) (count (:optional kws)))
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

(declare Method->symbol)

(defn app-type-error [fexpr args fin arg-ret-types expected poly?]
  {:pre [(FnIntersection? fin)]}
  (let [static-method? (= :static-method (:op fexpr))
        instance-method? (= :instance-method (:op fexpr))
        method-sym (when (or static-method? instance-method?)
                     (Method->symbol (:method fexpr)))]
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
        method-sym
        (ana-frm/map->form fexpr)) 
      " could not be applied to arguments:\n"
      "Domains: \n\t" 
      (clojure.string/join "\n\t" (map (partial apply pr-str) (map (comp #(map unparse-type %) :dom) (.types fin)))) 
      "\n\n"
      "Arguments:\n\t" (apply prn-str (mapv (comp unparse-type ret-t) arg-ret-types)) "\n"
      "in: " (if (or static-method? instance-method?)
               (ana-frm/map->form fexpr)
               (list* (ana-frm/map->form fexpr)
                      (map ana-frm/map->form args))))))

(defn polyapp-type-error [fexpr args fexpr-type arg-ret-types expected]
  {:pre [(Poly? fexpr-type)]}
  (let [fin (Poly-body* (Poly-free-names* fexpr-type) fexpr-type)]
    (app-type-error fexpr args fin arg-ret-types expected true)))

(defn plainapp-type-error [fexpr args fexpr-type arg-ret-types expected]
  {:pre [(FnIntersection? fexpr-type)]}
  (app-type-error fexpr args fexpr-type arg-ret-types expected false))

; TCResult TCResult^n (U nil TCResult) -> TCResult
(defn check-funapp [fexpr args fexpr-ret-type arg-ret-types expected]
  {:pre [(TCResult? fexpr-ret-type)
         (every? TCResult? arg-ret-types)
         ((some-fn nil? TCResult?) expected)]
   :post [(TCResult? %)]}
  (let [fexpr-type (resolve-to-ftype (ret-t fexpr-ret-type))
        arg-types (mapv ret-t arg-ret-types)]
    #_(prn "check-funapp" (unparse-type fexpr-type) (map unparse-type arg-types))
    (cond
      ;ordinary Function, single case, special cased for improved error msgs
      (and (FnIntersection? fexpr-type)
           (= 1 (count (:types fexpr-type))))
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
           (let [body (Poly-body* (repeatedly (.nbound fexpr-type) gensym) fexpr-type)]
             (and (FnIntersection? body)
                  (every? (complement :drest) (.types body)))))
      (let [fs-names (repeatedly (.nbound fexpr-type) gensym)
            _ (assert (every? symbol? fs-names))
            fin (Poly-body* fs-names fexpr-type)
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
                                 (for [{:keys [dom rest drest rng] :as ftype} (:types pbody)
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
         expr-type (ret (RClass-of (Class->symbol Var) nil)
                        (-FS -top -bot)
                        -empty)))

(defn tc-equiv [comparator & vs]
  {:pre [(every? TCResult? vs)]
   :post [(TCResult? %)]}
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyword lookups

(declare invoke-keyword)

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
(defmethod invoke-special #'typed.core/into-array>*
  [{:keys [args] :as expr} & [expected]]
  (assert (= 3 (count args)) (error-msg "Wrong number of args to typed.core/into-array>*"))
  (let [[javat-syn cljt-syn coll-expr] args
        javat (let [c (resolve (:val javat-syn))]
                (assert (class? c) (error-msg "First argument of into-array> must be a Java class, given " (:val javat-syn)))
                c)
        cljt (parse-type (:val javat-syn))
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

;get
(defmethod invoke-special #'clojure.core/get
  [{:keys [args] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (assert (<= 2 (count args) 3) "Wrong number of args to clojure.core/get")
  (let [[target kw default] args
        kwr (expr-type (check kw))]
    (cond
      ((every-pred Value? (comp keyword? :val)) (ret-t kwr))
      (assoc expr
             expr-type (invoke-keyword kwr 
                                       (expr-type (check target)) 
                                       (when default
                                         (expr-type (check target))) 
                                       expected))
      
      :else (do ;(prn "Non-special 'get'")
              ::not-special))))

(defmethod static-method-special 'clojure.lang.PersistentHashMap/create
  [{:keys [args] :as expr} & [expected]]
  (let [_ (assert (= 1 (count args)) "Incorrect number of arguments to clojure.lang.PersistentHashMap/create")
        targett (-> (first args) check expr-type ret-t)
        _ (assert (HeterogeneousSeq? targett) (str "Must pass HeterogeneousSeq to clojure.lang.PersistentHashMap/create given "
                                                   (unparse-type targett)))
        res (reduce (fn [t [kt vt]]
                      {:pre [(HeterogeneousMap? t)]}
                      (assoc-in [:types kt] vt))
                    (-hmap {}) (.types ^HeterogeneousSeq targett))]
    (assoc expr
           expr-type (ret res))))

(defmethod static-method-special 'clojure.lang.RT/get
  [{:keys [args] :as expr} & [expected]]
  (assert (<= 2 (count args) 3) "Wrong number of args to clojure.core/get")
  (let [[target kw default] args
        kwr (expr-type (check kw))]
    (cond
      ((every-pred Value? (comp keyword? :val)) (ret-t kwr))
      (assoc expr
             expr-type (invoke-keyword kwr 
                                       (expr-type (check target)) 
                                       (when default
                                         (expr-type (check target))) 
                                       expected))
      
      :else ::not-special)))

(defmethod check :keyword-invoke
  [{:keys [kw target] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  (assoc expr
         expr-type (invoke-keyword (expr-type (check kw))
                                   (expr-type (check target))
                                   nil 
                                   expected)))

(defn find-val-type [t k]
  {:pre [(Type? t)
         (Type? k)]
   :post [(Type? %)]}
  (let [t (-resolve t)]
    (cond
      (Nil? t) -nil
      (HeterogeneousMap? t) (if-let [v (get (:types t) k)]
                              v
                              (throw (Exception. (str "Map type " (unparse-type t)
                                                      " does not have entry "
                                                      (unparse-type k)))))

      (Intersection? t) (apply In 
                               (for [t* (:types t)]
                                 (find-val-type t* k)))
      (Union? t) (apply Un
                        (for [t* (:types t)]
                          (find-val-type t* k)))
      :else (throw (Exception. (str (when *current-env*
                                      (str (:line *current-env*) ":"))
                                    "Can't get key " (unparse-type k) 
                                    "  from type " (unparse-type t)))))))

(defn invoke-keyword [kw-ret target-ret default-ret expected-ret]
  {:pre [(TCResult? kw-ret)
         (TCResult? target-ret)
         ((some-fn nil? TCResult?) default-ret)
         ((some-fn nil? TCResult?) expected-ret)]
   :post [(TCResult? %)]}
  (let [targett (-resolve (ret-t target-ret))
        kwt (ret-t kw-ret)]
    (cond
      ;Keyword must be a singleton with no default
      (and (Value? kwt)
           (keyword? (:val kwt))
           (not default-ret))
      (let [{{path-hm :path id-hm :id :as o} :o} target-ret
            this-pelem (->KeyPE (:val kwt))
            val-type (find-val-type targett kwt)]
        (if (not= (Un) val-type)
          (ret val-type
               (-FS (if (Path? o)
                      (-filter val-type id-hm (concat path-hm [this-pelem]))
                      (-filter-at val-type (->EmptyObject)))
                    (if (and (not (subtype? -false val-type))
                             (not (subtype? -nil val-type)))
                      -bot
                      -top))
               (if (Path? o)
                 (update-in o [:path] #(seq (concat % [this-pelem])))
                 o))
          (throw (Exception. "Keyword lookup gave bottom type"))))

      :else (throw (Exception. (error-msg "keyword-invoke only supports keyword lookup, no default. Found " 
                                          (unparse-type kwt)))))))

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

;apply
(defmethod invoke-special #'clojure.core/apply
  [expr & [expected]]
  ;(prn "special apply:")
  (let [e (invoke-apply expr expected)]
    (when (= e ::not-special)
      (throw (Exception. (str "apply must be special:" (-> expr :args first :var)))))
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
  (print-env)
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

;for map destructuring
(defmethod invoke-special #'clojure.core/seq?
  [{:keys [args] :as expr} & [expected]]
  (let [_ (assert (= 1 (count args)) "Wrong number of args to seq?")
        cargs (doall (map check args))
        obj (-> (expr-type (first cargs)) ret-o)
        ;_ (prn "seq?: expr" (first args))
        targett (-resolve (ret-t (expr-type (first cargs))))
        tys (cond
                (Union? targett) (into #{}
                                       (apply concat
                                              (for [t (set (map -resolve (:types targett)))]
                                                (if (Union? t)
                                                  (map -resolve (:types t))
                                                  [t]))))
                :else #{targett})
        special? (every? (some-fn HeterogeneousSeq?
                                  HeterogeneousList?
                                  HeterogeneousVector?
                                  HeterogeneousMap?)
                         tys)
        ;_ (prn "specials:" (map unparse-type tys))
        sub? (when special?
               (subtype? targett
                         (RClass-of (Class->symbol ISeq) [-any])))]
    (cond
      (and special? sub?)
      (assoc expr
             expr-type (ret -true 
                            (-FS (-filter-at (RClass-of (Class->symbol ISeq) [-any]) obj)
                                 -bot)
                            -empty))

      (and special? (not sub?))
      (assoc expr
             expr-type (ret -false 
                            (-FS -bot
                                 (-not-filter-at (RClass-of (Class->symbol ISeq) [-any]) obj))
                            -empty))

      :else (do ;(prn "seq? not special")
              ;(prn (unparse-type targett))
              ::not-special))))
;nth
(defmethod static-method-special 'clojure.lang.RT/nth
  [{:keys [args] :as expr} & [expected]]
  (let [_ (assert (<= 2 (count args) 3))
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

;assoc
; TODO handle unions of hmaps as the target
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
                (and (Value? targett) (nil? (.val targett))) #{(-hmap {})}
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
                                            (keyword? (.val kt)))
                                       (assoc-in hmap [:types kt] vt)

                                       ;keep hvector if number Value key and already hvector
                                       (and (HeterogeneousVector? hmap)
                                            (Value? kt)
                                            (number? (.val kt)))
                                       (do (assert (integer? (.val kt)))
                                         (assoc-in hmap [:types (.val kt)] vt))

                                       ;otherwise just make normal map if already a map, or normal vec if already a vec
                                       is-map (ret-t 
                                                (check-funapp target keyvals
                                                              (ret 
                                                                (parse-type '(All [b c] 
                                                                                  [(IPersistentMap b c) b c -> (IPersistentMap b c)])))
                                                              (mapv ret [hmap kt vt])
                                                              nil))
                                       :else (ret-t 
                                               (check-funapp target keyvals
                                                             (ret 
                                                               (parse-type '(All [c] 
                                                                                 [(IPersistentVector c) c -> (IPersistentVector c)])))
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
                      "Vector must be of Values for now")
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

(defmethod invoke-special :default [& args] ::not-special)
(defmethod static-method-special :default [& args] ::not-special)

(defn check-apply
  [{[fexpr & args] :args :as expr} expected]
  {:post [((some-fn TCResult? #(= ::not-special %)) %)]}
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
                                           (Un -nil (In (RClass-of Seqable [(or rest (.pre-type drest))])
                                                        (make-CountRange 1))))]
    (assoc expr
           expr-type (check-fn expr (or expected
                                        (ret (make-FnIntersection
                                               (make-Function [] -any -any))))))))

(declare check-anon-fn-method abstract-filter abo abstract-object)

;If an argument is shadowed and the shadowed binding is referenced
;in filters or object then the shadow is indistinguishable from the parameter
;and parameter will be incorrectly abstracted.
;Solution: 
;1. Simulating hygenic macroexpansion by renaming bindings to unique symbols,
;   should only be necessary for parameter names.
;2. Using equality filters
;
;eg.
;(fn [a]
;  (if (= a 1)
;    (let [a 'foo] ; here this shadows the argument, impossible to recover filters
;      a)          ; in fact any new filters about a will be incorrectly assumed to be the argument
;      false)) 
;
(defn abstract-result [result arg-names]
  {:pre [(TCResult? result)
         (every? symbol? arg-names)]
   :post [(Result? %)]}
  (let [keys (range (count arg-names))]
    (make-Result
      (ret-t result)
      (abstract-filter arg-names keys (ret-f result))
      (abstract-object arg-names keys (ret-o result)))))

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

(defn unwrap-poly
  "Return a pair vector of the instantiated body of the possibly polymorphic
  type and the names used"
  [t]
  {:pre [(Type? t)]
   :post [((hvector-c? Type? 
                       (some-fn nil? (every-c? symbol?))
                       (some-fn nil? (every-c? F?))
                       (some-fn nil? (every-c? Bounds?))
                       (some-fn (=-c? :Poly) 
                                (=-c? :PolyDots) 
                                nil?)) %)]}
  (cond
    (Poly? t) (let [_ (assert (Poly-free-names* t) (unparse-type t))
                    old-nmes (Poly-free-names* t)
                    _ (assert ((every-pred seq (every-c? symbol?)) old-nmes))
                    new-nmes (repeatedly (:nbound t) gensym)
                    new-frees (map make-F new-nmes)]
                [(Poly-body* new-nmes t) old-nmes new-frees (Poly-bbnds* new-nmes t) :Poly])
    (PolyDots? t) (let [_ (assert (-> t meta :actual-frees))
                        old-nmes (-> t meta :actual-frees)
                        _ (assert ((every-pred seq (every-c? symbol?)) old-nmes))
                        new-nmes (repeatedly (:nbound t) gensym)
                        new-frees (map make-F new-nmes)]
                    [(PolyDots-body* new-nmes t) old-nmes new-frees (PolyDots-bbnds* new-nmes t) :PolyDots])
    :else [t nil nil nil nil]))

(defn rewrap-poly [body orig-names inst-frees bnds poly?]
  {:pre [(Type? body)
         (every? symbol? orig-names)
         (every? F? inst-frees)
         ((some-fn (=-c? :Poly) (=-c? :PolyDots) nil?) poly?)]
   :post [(Type? %)]}
  (case poly?
    :Poly (Poly* (map :name inst-frees) bnds body orig-names)
    :PolyDots (with-meta (PolyDots* (map :name inst-frees) bnds body)
                         {:actual-frees orig-names})
    body))

(declare check-fn-method check-fn-method1)

(defn check-fn 
  "Check a fn to be under expected and annotate the inferred type"
  [{:keys [methods variadic-method] :as fexpr} expected]
  {:pre [(TCResult? expected)]
   :post [(TCResult? %)]}
  (let [; try and unwrap type enough to find function types
        exp (resolve-to-ftype (ret-t expected))
        ; unwrap polymorphic expected types
        [fin orig-names inst-frees bnds poly?] (unwrap-poly exp)
        ; once more to make sure
        fin (resolve-to-ftype fin)
        ;ensure a function type
        _ (assert (FnIntersection? fin)
                  (str (when *current-env*
                         (str (:line *current-env*) ": "))
                       (unparse-type fin) " is not a function type"))
        ;collect all inferred Functions
        inferred-fni (with-locals (when-let [name (:name fexpr)] ;self calls
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
                                            (check-fn-method method fin))
                                          (concat methods (when variadic-method
                                                            [variadic-method])))))))
        ;rewrap in Poly or PolyDots if needed
        pfni (rewrap-poly inferred-fni orig-names inst-frees bnds poly?)]
    (ret pfni (-FS -top -bot) -empty)))

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

        ; Update filters that reference bindings that the params shadow.
        ; Abstracting references to parameters is handled later in abstract-result, but
        ; suffers from bugs due to un-hygienic macroexpansion (see `abstract-result`).
        ; In short, don't shadow parameters if you want meaningful filters.
        props (mapv (fn [oldp]
                      (reduce (fn [p sym]
                                {:pre [(Filter? p)
                                       (symbol? sym)]}
                                (subst-filter p sym -empty true))
                              oldp (map :sym required-params)))
                    (:props *lexical-env*))
        fixed-entry (map vector (map :sym required-params) (concat dom (repeat (or rest 
                                                                                   (:pre-type drest)))))
        rest-entry (when rest-param
                     [[(:sym rest-param) 
                       (*check-fn-method1-rest-type* rest drest)]])
        _ (assert ((hash-c? symbol? Type?) (into {} fixed-entry)))
        _ (assert ((some-fn nil? (hash-c? symbol? Type?)) (when rest-entry
                                                            (into {} rest-entry))))

        env (-> *lexical-env*
              (assoc-in [:props] props)
              ;order important, (fn [a a & a]) prefers rightmost name
              (update-in [:l] merge (into {} fixed-entry) (into {} rest-entry)))
        crng-nopass
        (with-lexical-env env
          (with-recur-target (->RecurTarget dom rest drest nil)
            (*check-fn-method1-checkfn* body expected-rng)))

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
        ]
    (FnResult->Function 
      (->FnResult fixed-entry nil 
                  (when (and rest rest-param)
                    [(:sym rest-param) rest])
                  (when (and drest rest-param) 
                    [(:sym rest-param) drest])
                  (expr-type crng)))))


(defmethod check :do
  [{:keys [exprs] :as expr} & [expected]]
  {:post [(TCResult? (expr-type %))]}
  (let [cexprs (concat (doall
                         (for [stmtexpr (butlast exprs)]
                           (binding [*current-expr* stmtexpr]
                             (check stmtexpr))))
                       (let [lexpr (last exprs)]
                         (binding [*current-expr* lexpr]
                           [(check lexpr  expected)])))]
    (assoc expr
           :exprs cexprs
           expr-type (-> cexprs last expr-type)))) ;should be a ret already

(defmethod check :local-binding-expr
  [{:keys [local-binding] :as expr} & [expected]]
  (let [sym (:sym local-binding)
        t (binding [*var-annotations* VAR-ANNOTATIONS]
            (type-of sym))
        _ (assert (or (not expected)
                      (subtype? t (ret-t expected)))
                  (error-msg "Local binding " sym " expected type " (unparse-type (ret-t expected))
                             ", but actual type " (unparse-type t)))]
    (assoc expr
           expr-type (ret t 
                          (-FS (if (subtype? t (Un -false -nil))
                                 -bot
                                 (-not-filter (Un -nil -false) sym))
                               (-filter (Un -nil -false) sym))
                          (->Path nil sym)))))


(declare Method-symbol->Type)

(defn Method->symbol [{name-sym :name :keys [declaring-class] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [((every-pred namespace symbol?) %)]}
  (symbol (name declaring-class) (name name-sym)))

(defn symbol->PArray [sym nilable?]
  {:pre [(symbol? sym)
         (boolean? nilable?)]
   :post [((some-fn nil? PrimitiveArray?) %)]}
  (let [s (str sym)]
    (when (.endsWith s "<>")
      (let [s-nosuffix (apply str (drop-last 2 s))]
        (assert (not (.contains s-nosuffix "<>")))
        ;Nullable elements
        (let [t (Method-symbol->Type (symbol s-nosuffix) nilable?)
              c (let [c (resolve (symbol s-nosuffix))
                      _ (assert (class? c))]
                  c)]
          (->PrimitiveArray c t t))))))

(defn Method-symbol->Type [sym nilable?]
  {:pre [(symbol? sym)
         (boolean? nilable?)]
   :post [(Type? %)]}
  (if-let [typ (or (primitives sym)
                   (symbol->PArray sym nilable?)
                   (when-let [cls (resolve sym)]
                     #_(prn (class cls) cls)
                     (apply Un (RClass-of (Class->symbol cls) nil)
                            (when nilable?
                              [-nil]))))]
    typ
    (throw (Exception. (str "Method symbol " sym " does not resolve to a type")))))

(defn- instance-method->Function [{:keys [parameter-types declaring-class return-type] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [(FnIntersection? %)]}
  (assert (class? (resolve declaring-class)))
  (make-FnIntersection (make-Function (concat [(RClass-of declaring-class nil)]
                                              (doall (map #(Method-symbol->Type % false) parameter-types)))
                                      (Method-symbol->Type return-type true))))

(defn- Method->Function [{:keys [parameter-types return-type flags] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [(FnIntersection? %)]}
  (let [msym (Method->symbol method)
        nparams (count parameter-types)]
    (make-FnIntersection (make-Function (doall (map (fn [[n tsym]] (Method-symbol->Type 
                                                                     tsym (nilable-param? msym nparams n)))
                                                    (map-indexed vector
                                                                 (if (:varargs flags)
                                                                   (butlast parameter-types)
                                                                   parameter-types))))
                                        (Method-symbol->Type return-type (not (nonnilable-return? msym nparams)))
                                        (when (:varargs flags)
                                          (Method-symbol->Type (last parameter-types) (nilable-param? msym nparams (dec nparams))))))))

(defn- Constructor->Function [{:keys [declaring-class parameter-types] :as ctor}]
  {:pre [(instance? clojure.reflect.Constructor ctor)]
   :post [(FnIntersection? %)]}
  (let [cls (resolve declaring-class)
        _ (when-not (class? cls)
            (throw (Exception. (str "Constructor for unresolvable class " (:class ctor)))))]
    (make-FnIntersection (make-Function (doall (map #(Method-symbol->Type % false) parameter-types))
                                    (RClass-of (Class->symbol cls) nil)
                                    nil nil
                                    :filter (-FS -top -bot))))) ;always a true value

(defn check-invoke-method [{:keys [args tag method env] :as expr} expected inst?]
  {:pre [((some-fn nil? TCResult?) expected)]
   :post [(-> % expr-type TCResult?)]}
  (assert method (str "Unresolved method invocation " (:method-name expr) ", insufficient type hints."))
  #_(prn "invoke method: " (Method->symbol method) inst?)
  (binding [*current-env* env]
    (let [rfin-type (ret (or (@METHOD-OVERRIDE-ENV (Method->symbol method))
                             (Method->Function method)))
          _ (when inst?
              (let [ctarget (check (:target expr))]
;                (prn "check target" (unparse-type (ret-t (expr-type ctarget)))
;                     (unparse-type (RClass-of (Class->symbol (resolve (:declaring-class method))) nil)))
                (when-not (subtype? (ret-t (expr-type ctarget)) (RClass-of (Class->symbol (resolve (:declaring-class method)))
                                                                           nil))
                  (throw (Exception. (error-msg "Cannot call instance method " (Method->symbol method)
                                                " on type " (unparse-type (ret-t (expr-type ctarget)))))))))
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
  (check-invoke-method expr expected true))

(def COMPILE-STUB-PREFIX "compile__stub")

(declare unwrap-datatype)

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

(defmethod check :new
  [{cls :class :keys [ctor args env] :as expr} & [expected]]
  #_(prn "check: :new" "env" env)
  (binding [*current-env* env]
    (let [inst-types *inst-ctor-types*
          cls-stub (Class->symbol cls)
          clssym (symbol (str/replace-first (str cls-stub) (str COMPILE-STUB-PREFIX ".") ""))
          ifn (let [ctor-fn (or (@CONSTRUCTOR-OVERRIDE-ENV clssym)
                                (and (@DATATYPE-ENV clssym)
                                     (DataType-ctor-type clssym))
                                (Constructor->Function ctor))
                    _ (assert ctor-fn)
                    ctor-fn (if inst-types
                              (manual-inst ctor-fn inst-types)
                              ctor-fn)]
                    (ret ctor-fn))
          ;_ (prn "Expected constructor" (unparse-type (ret-t ifn)))
          cargs (mapv check args)
          res-type (check-funapp expr args ifn (map expr-type cargs) nil)]
      (assoc expr
             expr-type res-type))))

(defmethod check :throw
  [{:keys [exception] :as expr} & [expected]]
  (let [cexception (check exception)
        _ (assert (subtype? (ret-t (expr-type cexception))
                            (RClass-of (Class->symbol Throwable) nil))
                  (str "Can only throw Throwable, found "
                       (unparse-type (ret-t (expr-type cexception)))))]
    (assoc expr
           expr-type (ret (Un)))))

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
  [{:keys [args] :as expr} & [expected]]
  (assert *recur-target* (error-msg "No recur target"))
  (let [{:keys [dom rest]} *recur-target*
        fixed-args (if rest
                     (butlast args)
                     args)
        rest-arg (when rest
                   (last args))
        cargs (mapv check args (map ret (concat dom (when rest-arg
                                                      [(RClass-of Seqable [rest])]))))
        _ (assert (and (= (count fixed-args) (count dom))
                       (= (boolean rest) (boolean rest-arg)))
                  (error-msg "Wrong number of arguments to recur"))]
    (assoc expr
           expr-type (ret (Un)))))

(def ^:dynamic *check-let-checkfn*)

(defn check-let [binding-inits body expr is-loop expected & {:keys [expected-bnds]}]
  (assert (or (not is-loop) expected-bnds) (error-msg "Loop requires more annotations"))
  (let [env (reduce (fn [env [{{:keys [sym init]} :local-binding} expected-bnd]]
                      {:pre [(PropEnv? env)]
                       :post [(PropEnv? env)]}
                      (let [{:keys [t fl o]} (->
                                               (expr-type
                                                 (binding [*current-expr* init]
                                                   (with-lexical-env env
                                                     (*check-let-checkfn* init (when is-loop
                                                                                 (ret expected-bnd))))))
                                               ;substitute previous references to sym with an empty object,
                                               ;as old binding is shadowed
                                               (update-in [:t] subst-type sym -empty true)
                                               (update-in [:fl] subst-filter-set sym -empty true)
                                               (update-in [:o] subst-object sym -empty true))
                            ; update old env and new result with previous references of sym (which is now shadowed)
                            ; replaced with an empty object
                            
                           ; _ (pr "ENV")
                           ; _ (print-env)
                            env (-> env
                                  (update-in [:l] #(into {} (for [[oldsym ty] %]
                                                              [oldsym (subst-type ty sym -empty true)])))
                                  (update-in [:props] (fn [props]
                                                        (mapv #(subst-filter % sym -empty true) props))))
                            ;_ (do (pr "let: env after") (print-env env))
                            ]
                        (cond
                          (FilterSet? fl)
                          (let [{:keys [then else]} fl
                                p* [(-imp (-not-filter (Un -nil -false) sym) then)
                                    (-imp (-filter (Un -nil -false) sym) else)]]
                            (-> env
                              ;update binding type
                              (assoc-in [:l sym] t)
                              ;update props
                              (update-in [:props] #(apply concat 
                                                          (combine-props p* % (atom true))))))

                          (NoFilter? fl) (-> env
                                           ;no propositions to add, just update binding type
                                           (assoc-in [:l sym] t)))))
                    *lexical-env* (map vector binding-inits (or expected-bnds
                                                                (repeat nil))))
        cbody (with-lexical-env env
                (if is-loop
                  (binding [*recur-target* (->RecurTarget expected-bnds nil nil nil)]
                    (*check-let-checkfn* body expected))
                  (binding [*current-expr* body]
                    (*check-let-checkfn* body expected))))

        ;now we return a result to the enclosing scope, so we
        ;erase references to any bindings this scope introduces
        unshadowed-type 
        (reduce (fn [ty sym]
                  {:pre [(TCResult? ty)
                         (symbol? sym)]}
                  (-> ty
                    (update-in [:t] subst-type sym -empty true)
                    (update-in [:fl] subst-filter-set sym -empty true)
                    (update-in [:o] subst-object sym -empty true)))
                (expr-type cbody)
                (map (comp :sym :local-binding) binding-inits))]
    (assoc expr
           expr-type unshadowed-type)))

(defmethod check :let
  [expr & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (binding [*check-let-checkfn* check]
    (let [is-loop (:is-loop expr)
          binding-inits (:binding-inits expr)
          body (:body expr)]
      (if-let [expected-bnds (and is-loop *loop-bnd-anns*)]
        (binding [*loop-bnd-anns* nil]
          (check-let binding-inits body expr true expected :expected-bnds expected-bnds))
        (check-let binding-inits body expr false expected)))))

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

(defn flatten-props [ps]
  {:post [(every? Filter? %)]}
  (cond
    (empty? ps) []
    (AndFilter? (first ps)) (flatten-props (concat (-> ps first :fs) (next ps)))
    :else (cons (first ps) (flatten-props (next ps)))))

(def type-equal? =)

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
            (do (reset! flag false)
              [derived-props derived-atoms])
            (TypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (and (NotTypeFilter? p)
                 (type-equal? (->Top) (:type p)))
            (do (reset! flag false)
              [derived-props derived-atoms])
            (NotTypeFilter? p) (recur derived-props (cons p derived-atoms) (next worklist))
            (TopFilter? p) (recur derived-props derived-atoms (next worklist))
            (BotFilter? p) (do (reset! flag false)
                             [derived-props derived-atoms])
            :else (recur (cons p derived-props) derived-atoms (next worklist))))))))

;; also not yet correct
;; produces old without the contents of rem
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
(defn update [t lo]
  (let [t (if (requires-resolving? t)
            (-resolve t)
            t)]
    (cond
      ;heterogeneous map ops
      (and (TypeFilter? lo)
           (KeyPE? (first (:path lo)))
           (HeterogeneousMap? t)) (let [{:keys [type path id]} lo
                                        [{fpth-kw :val} & rstpth] path
                                        fpth (->Value fpth-kw)
                                        type-at-pth (get (:types t) fpth)]
                                    (if type-at-pth 
                                      (-hmap (assoc (:types t) fpth (update type-at-pth (-filter type id rstpth))))
                                      (Bottom)))

      (and (NotTypeFilter? lo)
           (KeyPE? (first (:path lo)))
           (HeterogeneousMap? t)) (let [{:keys [type path id]} lo
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

      (and (TypeFilter? lo)
           (empty? (:path lo))) 
      (let [u (:type lo)]
        (restrict u t))

      (and (NotTypeFilter? lo)
           (empty? (:path lo))) (let [u (:type lo)]
                                  (remove* t u))

      (Union? t) (let [ts (:types t)]
                   (apply Un (doall (map (fn [t] (update t lo)) ts))))
      (Intersection? t) (let [ts (:types t)]
                          (apply In (doall (map (fn [t] (update t lo)) ts))))
      :else (throw (Exception. (str "update along ill-typed path " (unparse-type t) " " (with-out-str (pr lo))))))))

; f can be a composite filter. bnd-env is a the :l of a PropEnv
; ie. a map of symbols to types
(defn update-composite [bnd-env f]
  {:pre [(Filter? f)]}
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
(defn env+ [env fs flag]
  {:pre [(PropEnv? env)
         (every? Filter? fs)
         (boolean? @flag)]
   :post [(PropEnv? env)
          (boolean? @flag)]}
  #_(prn 'env+ fs)
  (let [[props atoms] (combine-props fs (:props env) flag)]
    (reduce (fn [env f]
              {:pre [(PropEnv? env)
                     (Filter? f)]}
              (let [env (update-in env [:l] update-composite f)]
                ; update flag if a variable is now bottom
                (when (seq (set/intersection (set (vals (:l env)))
                                             #{(Un)}))
                  (reset! flag false))
                env))
            (assoc env :props (concat atoms props))
            (concat atoms props))))

(def object-equal? =)

(def ^:dynamic *check-if-checkfn*)

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
                                             (update-in [:fl] (constantly (-FS -top -top)))
                                             (update-in [:o] (constantly -empty))))
                expr-type)
              ;; this code is reachable, but we have no expected type
              reachable? (-> (*check-if-checkfn* expr) expr-type)
              ;; otherwise, this code is unreachable
              ;; and the resulting type should be the empty type
              :else (do (prn (error-msg "Not checking unreachable code"))
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
          {ts :t fs2 :fl os2 :o :as then-ret} (binding [*current-expr* thn]
                                                (with-lexical-env env-thn
                                                  (tc thn @flag+)))
          {us :t fs3 :fl os3 :o :as else-ret} (binding [*current-expr* els]
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
                      object (if (object-equal? os2 os3) os2 (->EmptyObject))]
                  (ret type filter object))]
          ;(prn "check if:" "both branches reachable, with combined result" (unparse-TCResult r))
          (if expected (check-below r expected) r))
        ;; special case if one of the branches is unreachable
        (type-equal? us (Un))
        (if expected (check-below (ret ts fs2 os2) expected) (ret ts fs2 os2))
        (type-equal? ts (Un))
        (if expected (check-below (ret us fs3 os3) expected) (ret us fs3 os3))
        :else (throw (Exception. "Something happened"))))))

(defmethod check :if
  [{:keys [test then else] :as expr} & [expected]]
  {:post [(-> % expr-type TCResult?)]}
  (let [ctest (binding [*current-expr* test]
                (check test))]
    (assoc expr
           expr-type (binding [*check-if-checkfn* check]
                       (check-if (expr-type ctest) then else)))))

(defmethod check :def
  [{:keys [var init init-provided env] :as expr} & [expected]]
  (assert (not expected) expected)
  (assert (:line env))
  #_(prn "Checking" var)
  (binding [*current-env* env
            *current-expr* expr]
    (cond 
      ;ignore macro definitions
      (not (.isMacro ^Var var))
      (let [t (binding [*var-annotations* VAR-ANNOTATIONS]
                (type-of (var->symbol var)))
            cexpr (cond 
                    (not init-provided) expr ;handle `declare`
                    :else (check init (ret t)))
            _ (subtype (ret-t (expr-type cexpr)) t)]
        ;def returns a Var
        (assoc cexpr
               expr-type (ret (RClass-of Var))))

      :else
      (assoc expr
             expr-type (ret (RClass-of Var))))))

(declare check-new-instance-method)

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
                    _ (assert (symbol? nme)) ;can remove once new analyze is released
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

(defn check-new-instance-method
  [{:keys [body required-params] :as expr} expected-fin]
  {:pre [(FnIntersection? expected-fin)]}
  (let [_ (assert (= 1 (count (:types expected-fin))))
        {:keys [dom rng] :as expected-fn} (-> expected-fin :types first)
        _ (assert (not (:rest expected-fn)))
        cbody (with-locals (zipmap (map :sym required-params) dom)
                (check body (ret (Result-type* rng)
                                 (Result-filter* rng)
                                 (Result-object* rng))))]
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

(comment
  ;; error checking
  (cf (if 1 'a 'b) Number)
  )

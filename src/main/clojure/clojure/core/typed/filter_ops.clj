(in-ns 'clojure.core.typed)

(defn -filter [t i & [p]]
  {:pre [(Type? t)
         (name-ref? i)
         ((some-fn nil? #(every? PathElem? %)) p)]
   :post [(Filter? %)]}
  (if (or (= (->Top) t) (and (symbol? i) (is-var-mutated? i)))
    -top
    (->TypeFilter t p i)))

(defn -not-filter [t i & [p]]
  {:pre [(Type? t)
         (name-ref? i)
         ((some-fn nil? #(every? PathElem? %)) p)]
   :post [(Filter? %)]}
  (if (or (= (Bottom) t) (and (symbol? i) (is-var-mutated? i)))
    -top
    (->NotTypeFilter t p i)))

(declare Path?)

(defn -filter-at [t o]
  (if (Path? o)
    (let [{p :path i :id} o]
      (-filter t i p))
    -top))
(defn -not-filter-at [t o]
  (if (Path? o)
    (let [{p :path i :id} o]
      (-not-filter t i p))
    -top))

(defn opposite? [f1 f2]
  {:pre [(Filter? f1)
         (Filter? f2)]
   :post [(boolean? %)]}
  (cond
    (and (TypeFilter? f1)
         (NotTypeFilter? f2))
    (let [{t1 :type p1 :path i1 :id} f1
          {t2 :type p2 :path i2 :id} f2]
      (and (= p1 p2)
           (= i1 i2)
           (subtype? t1 t2)))

    (and (NotTypeFilter? f1)
         (TypeFilter? f2))
    (let [{t2 :type p2 :path i2 :id} f1
          {t1 :type p1 :path i1 :id} f2]
      (and (= p1 p2)
           (= i1 i2)
           (subtype? t1 t2)))
    :else false))


;; compact : (Listof prop) bool -> (Listof prop)
;; props : propositions to compress
;; or? : is this an OrFilter (alternative is AndFilter)
(defn compact [props or?]
  {:pre [(every? Filter? props)
         (boolean? or?)]
   :post [(every? Filter? %)]}
  (let [tf-map (atom {})
        ntf-map (atom {})]
    ;; props: the propositions we're processing
    ;; others: props that are neither TF or NTF
    (loop [props props
           others nil]
      (if (empty? props)
        (concat others
                (vals @tf-map)
                (vals @ntf-map))
        (cond
          (and or? (TypeFilter? (first props)))
          (let [{t1 :type f1 :path x :id :as p} (first props)]
            (swap! tf-map (fn [m] (update-in m [[f1 x]] #(if %
                                                           (if (TypeFilter? %)
                                                             (let [t2 (:type %)]
                                                               (-filter (Un t1 t2) x f1))
                                                             (throw (Exception. (str "got something that isn't a type filter" p))))
                                                           p))))
            (recur (rest props) others))

          (and (not or?) (TypeFilter? (first props)))
          (let [{t1 :type f1 :path x :id} (first props)
                fl (@tf-map [f1 x])]
            (cond
              (and (TypeFilter? fl)
                   (let [t2 (:type fl)]
                     (not (overlap t1 (:type fl)))))
              ;; we're in an And, and we got two types for the same path that do not overlap
              [-bot]
              (TypeFilter? fl)
              (let [t2 (:type fl)]
                (swap! tf-map (fn [m] (assoc m [f1 x] (-filter (restrict t1 t2) x f1))))
                (recur (next props) others))
              :else
              (do 
                (swap! tf-map (fn [m] (assoc m [f1 x] (-filter t1 x f1))))
                (recur (next props) others))))

          (and (not or?) 
               (NotTypeFilter? (first props)))
          (let [{t1 :type f1 :path x :id :as p} (first props)]
            (swap! ntf-map (fn [m] (update-in m [[f1 x]]
                                              (fn [n]
                                                (if n
                                                  (if (NotTypeFilter? n)
                                                    (let [t2 (:type n)]
                                                      (-not-filter (Un t1 t2) x f1))
                                                    (throw (Exception. (str "got something that isn't a nottypefilter" p))))
                                                  p)))))
            (recur (next props) others))
          :else
          (let [p (first props)]
            (recur (next props) (cons p others))))))))


(declare -and)

(defn inverse-atom [a]
  {:pre [((some-fn TypeFilter? NotTypeFilter?) a)]
   :post [((some-fn TypeFilter? NotTypeFilter?) a)]}
  (cond
    (TypeFilter? a) (-not-filter (:type a) (:id a) (:path a))
    (NotTypeFilter? a) (-filter (:type a) (:id a) (:path a))))

(defn simplify-prop 
  "Try and use atomic proposition a to simplify composite
  proposition b. a must be correct polarity."
  [a b]
  {:pre [((some-fn TypeFilter? NotTypeFilter?) a)
         ((some-fn AndFilter? OrFilter?) b)]
   :post [(Filter? %)]}
  (cond
    ; assuming a wrapping OrFilter
    (AndFilter? b)
    (let [fs (set (:fs b))
          fs (set
               (for [f fs]
                 (cond
                   ; A ^ (B v A) => A
                   (OrFilter? f) (simplify-prop a f)
                   :else f)))]
      (if (fs a)
        ; A v (notB ^ A) => A v notB
        (apply -and (disj fs a))
        b))

    ; assuming a wrapping AndFilter
    (OrFilter? b)
    (let [fs (set (:fs b))]
      ; A ^ (B v A) => A
      (if (fs a)
        a
        b))))


(comment
  (-or (-not-filter -nil 'a)
       (-and (-filter -nil 'a)
             (-filter -false 'b)))
  (simplify-prop (-filter -nil 'a) (-and (-filter -nil 'a)
                                         (-filter -false 'b)))
  ;=> (-filter -nil 'a)
  '[-or-filter
    [-not-filter (Value :Black) (:tree) 0]
    [-and-filter
     ; or->and, elim -filter (:Black) (:tree 0)
     [-filter (Value :Black) (:tree) 0]
     [-or-filter
      ;and->or,  elim -filter (:Black) (:tree 0)
      [-and-filter
       ;or->and,  elim -not-filter (:Black) (:tree 0)
       [-filter (Value :Black) (:tree) 0]
       [-not-filter (Value :Red) (:left :tree) 0]]

      [-and-filter
       ;or->and,  elim -not-filter (:Black) (:tree 0)
       [-filter (Value :Red) (:left :tree) 0]
       [-filter (Value :Black) (:tree) 0]
       [-or-filter
        [-and-filter
         [-filter (Value :Red) (:left :tree) 0]
         [-filter (Value :Black) (:tree) 0]
         [-not-filter (Value :Red) (:right :tree) 0]]
        [-and-filter
         [-filter (Value :Red) (:left :tree) 0]
         [-filter (Value :Black) (:tree) 0]
         [-filter (Value :Red) (:right :tree) 0]
         [-not-filter (Value :Red) (:right :left :tree) 0]]]]]
     ]
    ]
  )

(declare atomic-filter?)

;remove opposites in and filter
(defn remove-opposite [and-f atom-f]
  {:pre [(Filter? and-f)
         (Filter? atom-f)]
   :post [(Filter? %)]}
  (if (AndFilter? and-f)
    (apply -and (remove #(opposite? % atom-f) (:fs and-f)))
    and-f))

(defn -or [& args]
  (loop [new-props (set args)
         atoms #{}
         last-props #{} ;stop iteration when (= (set/union new-props atoms) last-props)
         ]
    (assert ((set-c? atomic-filter?) atoms))
    (assert (every? (set-c? Filter?) [new-props last-props]))
    (cond
      ;reached fixed point
      (= (set/union new-props atoms) last-props)
      (case (count last-props)
        0 -bot
        1 (first last-props)
        (->OrFilter last-props))

      :else
      (let [;flatten OrFilters
            original-props (set/union new-props atoms)
            original-atoms atoms
            fs (-> (apply concat
                          (for [a (set/union new-props atoms)]
                            (if (OrFilter? a)
                              (:fs a)
                              [a])))
                 set (disj -bot))
            {:keys [atoms] old-props :props} (group-by #(cond
                                                          ((some-fn TypeFilter? NotTypeFilter?) %) :atoms
                                                          :else :props)
                                                       fs)
            ;simplify AndFilters by removing atomic props directly inside the AndFilter
            ;if they are opposite of any atomic props we already have
            next-props (doall
                         (for [p old-props]
                           (reduce (fn [p a] (remove-opposite p a))
                                   p atoms)))
            {:keys [atoms] new-props :props} (group-by #(cond
                                                          ((some-fn TypeFilter? NotTypeFilter?) %) :atoms
                                                          :else :props)
                                                       (set/union (set next-props) (set atoms)))]
        (assert (<= (count original-atoms) (count atoms)))
        (recur (set new-props) (set atoms) (set original-props))))))

;(defn -or [& args]
;  {:pre [(every? Filter? args)]
;   :post [(Filter? %)]}
;  (letfn [(mk [& fs]
;            {:pre [(every? Filter? fs)]
;             :post [(Filter? %)]}
;            (cond
;              (empty? fs) -bot
;              (= 1 (count fs)) (first fs)
;              :else (->OrFilter fs)))
;          (distribute [args]
;            (let [{ands true others false} (group-by AndFilter? args)]
;              (if (empty? ands)
;                (apply mk others)
;                (let [{elems :fs} (first ands)] ;an AndFilter
;                  (apply -and (for [a elems]
;                                (apply -or a (concat (next ands) others))))))))]
;    (loop [fs args
;           result nil]
;      (assert (every? Filter? fs))
;      (assert (every? Filter? result))
;      (if (empty? fs)
;        (cond
;          (empty? result) -bot
;          (= 1 (count result)) (first result)
;          :else (distribute (compact result true)))
;        (cond
;          (Top? (first fs)) (first fs)
;          (OrFilter? (first fs)) (let [fs* (:fs (first fs))]
;                                   (recur (concat fs* (next fs)) result))
;          (BotFilter? (first fs)) (recur (next fs) result)
;          :else (let [t (first fs)]
;                  (assert (Filter? t))
;                  (cond 
;                    (some (fn [f] (opposite? f t)) (concat (rest fs) result))
;                    -top
;                    (some (fn [f] (or (= f t)
;                                      (implied-atomic? f t)))
;                          result)
;                    (recur (next fs) result)
;                    :else
;                    (recur (next fs) (cons t result)))))))))

(declare atomic-filter? combine-props ->ImpFilter)

(defn -imp [a c]
  {:pre [(Filter? a)
         (Filter? c)]
   :post [(Filter? %)]}
  (cond
    (BotFilter? a) -top
    (TopFilter? a) c
    ;; P -> tt = tt for any P
    (TopFilter? c) -top
    :else (->ImpFilter a c)))



;  A ^ (B v ...) -> (simplify A (B v ...))
(defn -and [& args]
             ;flatten direct internal AndFilters
  (let [flat (apply concat
                    (for [fl args]
                      (if (AndFilter? fl)
                        (:fs fl)
                        [fl])))
        fs (set flat)]
    (cond
      (empty? fs) -bot
      (fs -bot) -bot
      (or (= 1 (count fs))
          (= 1 (count (disj fs -top)))) (or (first (disj fs -top))
                                            (first fs))
      :else (->AndFilter (disj fs -top)))))

;(defn -and [& args]
;  {:pre [(every? Filter? args)]
;   :post [(Filter? %)]}
;  (letfn [(mk [& fs]
;            {:pre [(every? Filter? fs)]
;             :post [(Filter? %)]}
;            (cond
;              (empty? fs) -top
;              (= 1 (count fs)) (first fs)
;              :else (->AndFilter fs)))]
;    (loop [fs (set args)
;           result nil]
;      (if (empty? fs)
;        (cond
;          (empty? result) -top
;          (= 1 (count result)) (first result)
;          ;; don't think this is useful here
;          (= 2 (count result)) (let [[f1 f2] result]
;                                 (if (opposite? f1 f2)
;                                   -bot
;                                   (if (= f1 f2)
;                                     f1
;                                     (apply mk (compact [f1 f2] false)))))
;          :else
;           ;; first, remove anything implied by the atomic propositions
;           ;; We commonly see: (And (Or P Q) (Or P R) (Or P S) ... P), which this fixes
;          (let [{atomic true not-atomic false} (group-by atomic-filter? result)
;                not-atomic* (for [p not-atomic
;                                  :when (some (fn [a] (implied-atomic? p a)) atomic)]
;                              p)]
;             ;; `compact' takes care of implications between atomic props
;            (apply mk (compact (concat not-atomic* atomic) false))))
;        (let [ffs (first fs)]
;          (cond
;            (BotFilter? ffs) ffs
;            (AndFilter? ffs) (let [fs* (:fs ffs)]
;                               (recur (next fs) (concat fs* result)))
;            (TopFilter? ffs) (recur (next fs) result)
;            :else (let [t ffs]
;                    (cond
;                      (some (fn [f] (opposite? f ffs)) (concat (rest fs) result)) 
;                      -bot
;                      (some (fn [f] (or (= f t)
;                                        (implied-atomic? t f))) result) 
;                      (recur (rest fs) result)
;                      :else
;                      (recur (rest fs) (cons t result))))))))))

(defn -FS [+ -]
  {:pre [(Filter? +)
         (Filter? -)]
   :post [(FilterSet? %)]}
  (cond
    (BotFilter? +) (->FilterSet -bot -top)
    (BotFilter? -) (->FilterSet -top -bot)
    :else (->FilterSet + -)))

(def atomic-filter? (some-fn TypeFilter? NotTypeFilter?
                             TopFilter? BotFilter?))

(def -true-filter (-FS -top -bot))
(def -false-filter (-FS -bot -top))

(def -false (->Value false))
(def -true (->Value true))
(def -nil (->Value nil))

(defn Nil? [a] (= -nil a))
(defn False? [a] (= -false a))
(defn True? [a] (= -true a))

(defn implied-atomic? [f1 f2]
  (if (= f1 f2)
    true
    (cond
      (OrFilter? f1) (boolean (some #(= % f2) (:fs f1)))
      (and (TypeFilter? f1)
           (TypeFilter? f2)) (and (= (:id f1) (:id f2))
                                  (subtype? (:type f2) (:type f1)))
      (and (NotTypeFilter? f1)
           (NotTypeFilter? f2)) (and (= (:id f1) (:id f2))
                                     (subtype? (:type f1) (:type f2)))
      :else false)))


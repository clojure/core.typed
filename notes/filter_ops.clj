(in-ns 'typed.core)

(declare subtype? compact)

(defn overlap [t1 t2]
  true)

(defn -filter [t i & [p]]
  {:pre [(Type? t)
         (or (symbol? i)
             (var? i))
         (or (nil? p)
             (every? PathElem? p))]
   :post [(Filter? %)]}
  (if (or (= (->Top) t) (subtype? (type-of i) (parse-type 'clojure.lang.IRef)))
    (->Top)
    (->TypeFilter t p i)))

(defn atomic-filter? [a]
  (or (TypeFilter? a)
      (NotTypeFilter? a)
      (TopFilter? a)
      (BotFilter? a)))

(defn opposite? [f1 f2]
  {:pre [(Filter? f1)
         (Filter? f2)]}
  (cond
    (and (TypeFilter? f1)
         (NotTypeFilter? f2)
         (= (:path f1)
            (:path f2)))
    (let [{t1 :type id1 :id} f1
          {t2 :type id2 :id} f2]
      (and (= id1 id2)
           (subtype? t1 t2)))

    (and (NotTypeFilter? f1)
         (TypeFilter? f2)
         (= (:path f1)
            (:path f2)))
    (let [{t2 :type id2 :id} f1
          {t1 :type id1 :id} f2]
      (and (= id1 id2)
           (subtype? t1 t2)))

    :else false))

;; is f1 implied by f2?
(defn implied-atomic? [f1 f2]
  (if (= f1 f2)
    true
    (cond
      (OrFilter? f1)
      (boolean (some #(= % f2) (:fs f1)))

      (and (TypeFilter? f1)
           (TypeFilter? f2)
           (= (:path f1)
              (:path f2)))
      (and (= (:id f1) (:id f2))
           (subtype? (:id f2) (:id f1)))

      (and (NotTypeFilter? f1)
           (NotTypeFilter? f2)
           (= (:path f1)
              (:path f2)))
      (and (= (:id f1) (:id f2))
           (subtype? (:id f1) (:id f2)))

      :else false)))

(defn -imp [p1 p2]
  (cond
    (BotFilter? p1) (->TopFilter)
    (TopFilter? p1) p2
    :else (->ImpFilter p1 p2)))

(declare -and)

(defn -or [& args]
  (letfn [(mk [& fs]
            (cond
              (empty? fs) (->BotFilter)
              (= 1 (count fs)) (first fs)
              :else (->OrFilter fs)))
          (distribute [args]
            (let [{ands :true others :false} (group-by AndFilter? args)]
              (if (empty? others)
                (apply mk others)
                (let [{elems :fs} (first ands)]
                  (apply -and (for [a elems]
                                (apply -or a (concat (rest ands) others))))))))]
  (loop [[f :as fs] args
         result nil]
    (if (empty? fs)
      (cond
        (empty? fs) (->BotFilter)
        (= 1 (count fs)) f
        :else (distribute (compact result true)))
      (cond
        (Top? f) f

        (OrFilter? f)
        (recur (concat (:fs f) (rest fs)) result)

        (BotFilter? f)
        (recur (rest fs) result)

        (some #(opposite? % f) (concat (rest fs) result))
        (->TopFilter)

        ;TODO Is this translated properly? Rep-seq? check filter-ops.rkt
        (some #(or (= % f) (implied-atomic? % f)) result)
        (recur (rest fs) result)

        :else (recur (rest fs) (cons f result)))))))

(defn -and [& args]
  (letfn [(mk [& fs]
            (cond
              (empty? fs) (->TopFilter)
              (= 1 (count fs)) (first fs)
              :else (->AndFilter fs)))]
    (loop [fs (set args) ;TODO remove-duplicates? is set good enough?
           result nil]
      (if (empty? fs)
        (cond
          (empty? result) (->TopFilter)
          (= 1 (count result)) (first result)
          ;; don't think this is useful here
          (= 2 (count result)) (let [[f1 f2] result]
                                 (if (opposite? f1 f2)
                                   (->BotFilter)
                                   (if (= f1 f2)
                                     f1
                                     (apply mk (compact (list f1 f2) false)))))
          :else
          ;; first, remove anything implied by the atomic propositions
          ;; We commonly see: (And (Or P Q) (Or P R) (Or P S) ... P), which this fixes
          (let [{atomic :true not-atomic :false} (group-by atomic-filter? result)
                not-atomic* (for [p not-atomic
                                  :when (not (some #(implied-atomic? p %) atomic))]
                              p)]
            ;; `compact' takes care of implications between atomic props
            (apply mk (compact (concat not-atomic* atomic) false))))
        (let [[fs1] fs]
          (cond
            (BotFilter? fs1) fs1
            (AndFilter? fs1) (let [{fs* :fs} fs1]
                               (recur (rest fs) (concat fs* result)))
            (TopFilter? fs1) (recur (rest fs) result)
            (some #(opposite? % fs1) (concat (rest fs) result)) (->BotFilter)

            ;;TODO is = enough? see Rep-seq
            (some #(or (= % fs1)
                       (implied-atomic? fs1 %))
                  result)
            (recur (rest fs) result)

            :else (recur (rest fs) (cons fs1 result))))))))

(defn compact [props or?]
  {:pre [(every? Filter? props)
         (or (true? or?)
             (false? or?))]
   :post [(every? Filter? %)]}
  (let [tf-map (atom {})
        ntf-map (atom {})]
    (loop [[props1 :as props] props
           others nil]
      (if (empty? props)
        (concat others
                (vals @tf-map)
                (vals @ntf-map))
        (cond
          (and (TypeFilter? props1)
               or?)
          (do
            (swap! tf-map #(update-in % [(list (:type props1)
                                               (:id props1))]
                                      (fn [p]
                                        (or (and (nil? p) props1)
                                            (do
                                              (assert (TypeFilter? p))
                                              (-filter (Un [(:type props1) (:type p)])
                                                       (:id props1)
                                                       (:path props1)))))))
            (recur (rest props) others))

          (and (TypeFilter? props1)
               (not or?))
          (let [f1 (:path props1)
                tf (@tf-map (list f1 (:id props1)))]
            (cond
              (and (TypeFilter? tf)
                   (not (overlap (:type props1) (:type tf))))
              ;; we're in an And, and we got two types for the same path that do not overlap
              (list (->BotFilter))

              (TypeFilter? tf)
              (do (swap! tf-map #(assoc % (list f1 (:id props1))
                                        (-filter (restrict (:type props1) (:type tf))
                                                 (:id props1) f1)))
                (recur (rest props) others))

              :else
              (do (swap! tf-map #(assoc % (list f1 (:id props1))
                                        (-filter (:type props1) (:id props1) f1)))
                (recur (rest props) others))))
          (and (NotTypeFilter? props1)
               (not or?))
          (do (swap! ntf-map
                     #(update-in % [(list f1 (:id props1))]
                                 (fn [p]
                                   (or (and (nil? p) props1)
                                       (do (assert (NotTypeFilter? p))
                                         (-not-filter (Un [(:type props1) (:type p)])
                                                      (:id props1) f1))))))
            (recur (rest props) others))

          :else (recur (rest props) (cons props1 others)))))))

(defn restrict [t1 t2]
  (cond
    (subtype? t1 t2) t1 ;; already a subtype

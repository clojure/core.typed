(ns clojure.core.typed.test.rbt
;  (:refer-clojure :exclude [and])
;  (:require [clojure.core.typed :refer [ann inst cf fn> pfn> def-alias declare-names
;                                        print-env print-filterset check-ns typed-deps
;                                        ann-form]]
;            [clojure.core.typed.test.rbt-types :refer [badRight rbt EntryT Empty
;                                                       Red Black bt]]
;            #_[clojure.core.typed.subtype :as sub]
;            #_[clojure.core.typed.check :as chk]
;            #_[clojure.core.typed.parse-unparse :as prs]
;            #_[clojure.core.typed.filter-ops :as fo]
;            [clojure.repl :refer [pst]]
;            [clojure.math.combinatorics :as comb]
;            [clojure.jvm.tools.analyzer :refer [ast]]))
  )

(comment
(defmacro and
  "Scheme's and. Returns false on a false case."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(if ~x (and ~@next) false)))

(defmacro and-trace
  "Scheme's and. Returns false on a false case."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(print-filterset ~(str "trace result:\n" `(and ~x ~@next))
      (if (print-filterset ~(str "trace test\n" x) ~x) (and-trace ~@next) false))))


; This is an implementation of red-black tree invariant checking.
; Invariants are copied from Rowan Davies' PhD dissertation
; "Practical Refinement Type Checking".

; restore-right (Black (e,l,r)) >=> dict
; where (1) Black(e,l,r) is ordered,
; (2) Black (e,l,r) hash black height n,
; (3) color invariant may be violated at the root of r:
;     one of its children must be red.
;  and dict is re-balanced red/black tree (satisfying all inv's)
;  and the same black height n.

(ann restore-right 
     (Fn [badRight -> rbt]))
(defn restore-right [tmap]
  (cond
    (print-filterset 
      "TEST1 
        FS
        {:then (& (is ':Black tmap [(Key :tree)])
                  (is ':Red tmap [(Key :left) (Key :tree)])
                  (is ':Red tmap [(Key :right) (Key :tree)])
                  (is ':Red tmap [(Key :right) (Key :left) (Key :tree)]))
         :else (| (! ':Black tmap [(Key :tree)])
                  (! ':Red tmap [(Key :left) (Key :tree)])
                  (! ':Red tmap [(Key :right) (Key :tree)])
                  (! ':Red tmap [(Key :right) (Key :left) (Key :tree)]))}"
      (and-trace (= :Black (-> tmap :tree))
           (= :Red (-> tmap :left :tree))
           (= :Red (-> tmap :right :tree))
           (= :Red (-> tmap :right :left :tree))))
    (let [;_ (print-env "down first then")
          {lt :left rt :right e :entry} tmap
          ;re-color
          res {:tree :Red
               :entry e
               :left (assoc lt
                            :tree :Black)
               :right (assoc rt
                             :tree :Black)}]
      ;(print-env "restore-right: output first branch (res)")
      (ann-form res rbt))

    (print-filterset 
      "TEST2
      FS
      {:then
       (&
        (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
        (is (Value :Red) tmap [(Key :right) (Key :tree)])
        (is (Value :Red) tmap [(Key :left) (Key :tree)])
        (is (Value :Black) tmap [(Key :tree)])
        (is (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])),
       :else
       (&
        (|
         (! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
         (! (Value :Red) tmap [(Key :right) (Key :tree)])
         (! (Value :Red) tmap [(Key :left) (Key :tree)])
         (! (Value :Black) tmap [(Key :tree)]))
        (|
         (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
         (! (Value :Red) tmap [(Key :right) (Key :tree)])
         (! (Value :Red) tmap [(Key :left) (Key :tree)])
         (! (Value :Black) tmap [(Key :tree)])))}"
       (and (= :Black (-> tmap :tree))
            (= :Red (-> tmap :left :tree))
            (= :Red (-> tmap :right :tree))
            (= :Red (-> tmap :right :right :tree))))
    (let [; Input to the second case: - Rowan
          ; Empty | Black of 'a entry * 'a rbt * 'a rbt | Red of 'a entry * 'a bt * 'a bt
          ; | Black of 'a entry * 'a rbt * (Red of 'a entry * 'a bt * 'a rbt)
          _ (print-env "down TEST2")
          _ (ann-form tmap
                      (U Empty
                         (Black rbt rbt)
                         (Red bt bt)
                         (Black rbt (Red bt rbt))))
          {lt :left rt :right e :entry} tmap
          ;re-color
          res {:tree :Red
               :entry e
               :left (assoc lt
                            :tree :Black)
               :right (assoc rt
                             :tree :Black)}]
      ;(print-env "restore-right: output second branch (res)")
      (ann-form res rbt))

    (print-filterset "TEST3"
      (and (= :Black (-> tmap :tree))
           (= :Red (-> tmap :right :tree))
           (= :Red (-> tmap :right :left :tree))))
    (let [{e :entry
           l :left
           {re :entry
            {rle :entry
             rll :left
             rlr :right}
            :left
            rr :right}
           :right} tmap
          ;l is black, deep rotate
          res {:tree :Black
               :entry rle
               :left {:tree :Red
                      :entry e
                      :left l
                      :right rll}
               :right {:tree :Red
                       :entry re
                       :left rlr
                       :right rr}}]
      (ann-form res rbt))

    (print-filterset 
      "TEST4
       {:then
        (&
         (|
          (! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
          (! (Value :Red) tmap [(Key :right) (Key :tree)])
          (! (Value :Red) tmap [(Key :left) (Key :tree)]))
         (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
         (is (Value :Red) tmap [(Key :right) (Key :tree)])
         (is (Value :Black) tmap [(Key :tree)])
         (is (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
         (|
          (! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
          (! (Value :Red) tmap [(Key :left) (Key :tree)]))),
        :else
        (&
         (|
          (! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
          (! (Value :Red) tmap [(Key :right) (Key :tree)])
          (! (Value :Black) tmap [(Key :tree)]))
         (|
          (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
          (! (Value :Red) tmap [(Key :right) (Key :tree)])
          (! (Value :Black) tmap [(Key :tree)]))
         (|
          (! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
          (! (Value :Red) tmap [(Key :right) (Key :tree)])
          (! (Value :Red) tmap [(Key :left) (Key :tree)])
          (! (Value :Black) tmap [(Key :tree)]))
         (|
          (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
          (! (Value :Red) tmap [(Key :right) (Key :tree)])
          (! (Value :Red) tmap [(Key :left) (Key :tree)])
          (! (Value :Black) tmap [(Key :tree)])))}"
      (and (= :Black (-> tmap :tree))
           (= :Red (-> tmap :right :tree))
           (= :Red (-> tmap :right :right :tree))))
    (let [_ (print-env "down TEST4")
          {e :entry
           l :left
           {re :entry
            rl :left
            rr :right} 
           :right} tmap]
      ;l is black, shallow rotate
      (ann-form
        {:tree :Black
         :entry re
         :left {:tree :Red
                :entry e
                :left l
                :right rl}
         :right rr}
        rbt))

    (do (print-env "final else:")
      :else)
    (do (print-env "follow final else:")
      (ann-form tmap rbt))))

; Okasaki's simplified rotations for red-black trees
;(Fn [badRight -> rbt])
#_(defn restore-right [tmap]
  (cond
    (and (= :Black (-> tmap :tree))
         (= :Red (-> tmap :right :tree))
         (= :Red (-> tmap :right :right :tree)))
    (let [{e :entry
           lt :left
           {re :entry
            rlt :left
            rrt :right} :right} tmap]
      {:tree :Red
       :entry re
       :left {:tree :Black
              :entry e
              :left lt
              :right rlt}
       :right (assoc rrt
                     :tree :Black)})

    (and (= :Black (-> tmap :tree))
         (= :Red (-> tmap :right :tree))
         (= :Red (-> tmap :right :left :tree)))
    (let [{e :entry
           lt :left
           {re :entry
            {rlte :entry
             rllt :left
             rlrt :right} :left
            rrt :right} :right} tmap]
      {:tree :Red
       :entry rlte
       :left {:tree :Black
              :entry e
              :left lt
              :right rllt}
       :right {:tree :Black
               :entry re
               :left rlrt
               :right rrt}})

    :else tmap))

(ann ^:no-check insert (Fn [rbt EntryT -> rbt]))
(defn insert [dict {:keys [key datum] :as entry}]
  (letfn [;; ins (Red _) may violate color invariant at root
          ;; ins (Black _) or ins (Empty) will be red/black tree
          ;; ins preserves black height

          ;TODO order of Function cases?
          ; (Fn [rbt -> badRoot]
          ;     [bt -> rbt])
          (ins [{:keys [tree] :as tmap}]
            (cond
              (= :Empty tree) {:tree :Red
                               :entry entry
                               :left {:tree :Empty}
                               :right {:tree :Empty}}
              (= :Red tree) (let [{{key1 :key datum1 :datum :as entry1} :entry
                                   :keys [left right]} tmap]
                              (cond
                                (= key key1) {:tree :Red
                                              :entry entry
                                              :left left
                                              :right right}
                                (< key key1) {:tree :Red
                                              :entry entry1
                                              :left (ins left)
                                              :right right}
                                :else {:tree :Red
                                       :entry entry1
                                       :left left
                                       :right (ins right)}))
              (= :Black tree) (let [{{key1 :key datum1 :datum :as e1} :entry
                                     l :left r :right} tmap]
                                (cond
                                  (= key key1) {:tree :Black
                                                :entry entry
                                                :left l
                                                :right r}
;                                  (< key key1) (restore-left {:tree :Black
;                                                              :entry e1
;                                                              :left (ins l)
;                                                              :right r})
                                  :else (restore-right {:tree :Black
                                                        :entry e1
                                                        :left l
                                                        :right (ins r)})))))]


    (let [{:keys [tree l r] :as res} (ins dict)]
      (cond
        (and (= :Red tree) 
             (= :Red (:tree l))
             (= :Red (:tree r))) (assoc res
                                        :tree :Black) ;re-color
        :else res)))) ;depend on sequential matching







(comment 
(update-composite {'tmap (->Name 'typed.test.rbt/badRight)}
  (-or 
   (-not-filter (-val :Black) 'tmap [(->KeyPE :tree)])
   (-and  
    (-filter (-val :Black) 'tmap [(->KeyPE :tree)])

    (-or 
     (-not-filter (-val :Red) 'tmap [(->KeyPE :left) (->KeyPE :tree)])         
     (-and   
      (-filter (-val :Red) 'tmap [(->KeyPE :left) (->KeyPE :tree)])
      (-or 
       (-not-filter (-val :Red) 'tmap [(->KeyPE :right) (->KeyPE :tree)])
       (-and   
        (-filter (-val :Red) 'tmap [(->KeyPE :right) (->KeyPE :tree)])
        (-not-filter (-val :Red) 'tmap [(->KeyPE :right) (->KeyPE :left) (->KeyPE :tree)]))))))))

  #_(-or 
   (-not-filter (-val :Black) [(->KeyPE :tree)] tmap)
   (-and  
    (-filter (-val :Black) [(->KeyPE :tree)] tmap)

    (-or 
     (-not-filter (-val :Red) [(->KeyPE :left) (->KeyPE :tree)] tmap)         
     (-and   
      (-filter (-val :Red) [(->KeyPE :left) (->KeyPE :tree)] tmap)
      (-or 
       (-not-filter (-val :Red) [(->KeyPE :right) (->KeyPE :tree)] tmap)
       (-and   
        (-filter (-val :Red) [(->KeyPE :right) (->KeyPE :tree)] tmap)
        (-not-filter (-val :Red) [(->KeyPE :right) (->KeyPE :left) (->KeyPE :tree)] tmap)))))))

  ;output of the :else of first branch
(let [fs (read-string "#clojure.core.typed.FilterSet{:then #clojure.core.typed.AndFilter{:fs #{#clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :right} #clojure.core.typed.KeyPE{:val :left} #clojure.core.typed.KeyPE{:val :tree}), :id tmap} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :left} #clojure.core.typed.KeyPE{:val :tree}), :id tmap} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Black}, :path (#clojure.core.typed.KeyPE{:val :tree}), :id tmap} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :right} #clojure.core.typed.KeyPE{:val :tree}), :id tmap}}}, :else #clojure.core.typed.OrFilter{:fs #{#clojure.core.typed.AndFilter{:fs #{#clojure.core.typed.OrFilter{:fs #{#clojure.core.typed.AndFilter{:fs #{#clojure.core.typed.NotTypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :left} #clojure.core.typed.KeyPE{:val :tree}), :id tmap} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Black}, :path (#clojure.core.typed.KeyPE{:val :tree}), :id tmap}}} #clojure.core.typed.AndFilter{:fs #{#clojure.core.typed.OrFilter{:fs #{#clojure.core.typed.AndFilter{:fs #{#clojure.core.typed.NotTypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :right} #clojure.core.typed.KeyPE{:val :left} #clojure.core.typed.KeyPE{:val :tree}), :id tmap} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :left} #clojure.core.typed.KeyPE{:val :tree}), :id tmap} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Black}, :path (#clojure.core.typed.KeyPE{:val :tree}), :id tmap} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :right} #clojure.core.typed.KeyPE{:val :tree}), :id tmap}}} #clojure.core.typed.AndFilter{:fs #{#clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :left} #clojure.core.typed.KeyPE{:val :tree}), :id tmap} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Black}, :path (#clojure.core.typed.KeyPE{:val :tree}), :id tmap} #clojure.core.typed.NotTypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :right} #clojure.core.typed.KeyPE{:val :tree}), :id tmap}}}}} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Red}, :path (#clojure.core.typed.KeyPE{:val :left} #clojure.core.typed.KeyPE{:val :tree}), :id tmap} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Black}, :path (#clojure.core.typed.KeyPE{:val :tree}), :id tmap}}}}} #clojure.core.typed.TypeFilter{:type #clojure.core.typed.Value{:val :Black}, :path (#clojure.core.typed.KeyPE{:val :tree}), :id tmap}}} #clojure.core.typed.NotTypeFilter{:type #clojure.core.typed.Value{:val :Black}, :path (#clojure.core.typed.KeyPE{:val :tree}), :id tmap}}}}")]

  (->
    (env+ (-PropEnv {'tmap (->Name 'typed.test.rbt/badRight)}
                     [])
          [(:else fs)]
          (atom true))
    :l (get 'tmap)))
)
)

(ns typed.test.rbt
  (:require [typed.core :refer [ann inst cf fn> pfn> def-alias declare-names
                                tc-pr-env tc-pr-filters]]
            [clojure.repl :refer [pst]]
            [analyze.core :refer [ast]]))

(def-alias EntryT (Map* :mandatory
                        {:key Number
                         :datum Number})) ;TODO is this EntryT type correct? No definition in thesis

(declare-names rbt bt)

;Trees with only black children for red nodes
(def-alias rbt (U 
                 ;Empty
                 (Map* :mandatory
                       {:tree (Value :Empty)})
                 ;Black
                 (Map* :mandatory
                       {:tree (Value :Black)
                        :entry EntryT
                        :left rbt
                        :right rbt})
                 ;Red
                 (Map* :mandatory
                       {:tree (Value :Red)
                        :entry EntryT
                        :left bt
                        :right bt})))

;As above but additionally the root node is black
(def-alias bt (U
                ;Empty
                (Map* :mandatory
                      {:tree (Value :Empty)})
                ;Black
                (Map* :mandatory
                      {:tree (Value :Black)
                       :entry EntryT
                       :left rbt
                       :right rbt})))

; Trees with a red root
(def-alias red (U
                 ;Red
                 (Map* :mandatory
                       {:tree (Value :Red)
                        :entry EntryT
                        :left bt
                        :right bt})))

;invariant possibly violated at the root
(def-alias badRoot (U
                     ;Empty
                     (Map* :mandatory
                           {:tree (Value :Empty)})
                     ;Black
                     (Map* :mandatory
                           {:tree (Value :Black)
                            :entry EntryT
                            :left rbt
                            :right bt})
                     ;Red
                     (Map* :mandatory
                           {:tree (Value :Red)
                            :entry EntryT
                            :left rbt
                            :right bt})
                     ;Red
                     (Map* :mandatory
                           {:tree (Value :Red)
                            :entry EntryT
                            :left bt
                            :right rbt})))

;invariant possibly violated at the left child
(def-alias badLeft (U
                     ;Empty
                     (Map* :mandatory
                           {:tree (Value :Empty)})
                     ;Black
                     (Map* :mandatory
                           {:tree (Value :Black)
                            :entry EntryT
                            :left rbt
                            :right rbt})
                     ;Red
                     (Map* :mandatory
                           {:tree (Value :Red)
                            :entry EntryT
                            :left bt
                            :right bt})
                     ;Black
                     (Map* :mandatory
                           {:tree (Value :Black)
                            :entry EntryT
                            :left badRoot
                            :right rbt})))

;invariant possibly violated at the right child
(def-alias badRight (U
                      ;Empty
                      (Map* :mandatory
                            {:tree (Value :Empty)})
                      ;Black
                      (Map* :mandatory
                            {:tree (Value :Black)
                             :entry EntryT
                             :left rbt
                             :right rbt})
                      ;Red
                      (Map* :mandatory
                            {:tree (Value :Red)
                             :entry EntryT
                             :left bt
                             :right bt})
                      ;Black
                      (Map* :mandatory
                            {:tree (Value :Black)
                             :entry EntryT
                             :left rbt
                             :right badRoot})))

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
(let [fs (read-string "#typed.core.FilterSet{:then #typed.core.AndFilter{:fs #{#typed.core.TypeFilter{:type #typed.core.Value{:val :Red}, :path (#typed.core.KeyPE{:val :right} #typed.core.KeyPE{:val :left} #typed.core.KeyPE{:val :tree}), :id tmap} #typed.core.TypeFilter{:type #typed.core.Value{:val :Red}, :path (#typed.core.KeyPE{:val :left} #typed.core.KeyPE{:val :tree}), :id tmap} #typed.core.TypeFilter{:type #typed.core.Value{:val :Black}, :path (#typed.core.KeyPE{:val :tree}), :id tmap} #typed.core.TypeFilter{:type #typed.core.Value{:val :Red}, :path (#typed.core.KeyPE{:val :right} #typed.core.KeyPE{:val :tree}), :id tmap}}}, :else #typed.core.OrFilter{:fs #{#typed.core.AndFilter{:fs #{#typed.core.OrFilter{:fs #{#typed.core.AndFilter{:fs #{#typed.core.NotTypeFilter{:type #typed.core.Value{:val :Red}, :path (#typed.core.KeyPE{:val :left} #typed.core.KeyPE{:val :tree}), :id tmap} #typed.core.TypeFilter{:type #typed.core.Value{:val :Black}, :path (#typed.core.KeyPE{:val :tree}), :id tmap}}} #typed.core.AndFilter{:fs #{#typed.core.OrFilter{:fs #{#typed.core.AndFilter{:fs #{#typed.core.NotTypeFilter{:type #typed.core.Value{:val :Red}, :path (#typed.core.KeyPE{:val :right} #typed.core.KeyPE{:val :left} #typed.core.KeyPE{:val :tree}), :id tmap} #typed.core.TypeFilter{:type #typed.core.Value{:val :Red}, :path (#typed.core.KeyPE{:val :left} #typed.core.KeyPE{:val :tree}), :id tmap} #typed.core.TypeFilter{:type #typed.core.Value{:val :Black}, :path (#typed.core.KeyPE{:val :tree}), :id tmap} #typed.core.TypeFilter{:type #typed.core.Value{:val :Red}, :path (#typed.core.KeyPE{:val :right} #typed.core.KeyPE{:val :tree}), :id tmap}}} #typed.core.AndFilter{:fs #{#typed.core.TypeFilter{:type #typed.core.Value{:val :Red}, :path (#typed.core.KeyPE{:val :left} #typed.core.KeyPE{:val :tree}), :id tmap} #typed.core.TypeFilter{:type #typed.core.Value{:val :Black}, :path (#typed.core.KeyPE{:val :tree}), :id tmap} #typed.core.NotTypeFilter{:type #typed.core.Value{:val :Red}, :path (#typed.core.KeyPE{:val :right} #typed.core.KeyPE{:val :tree}), :id tmap}}}}} #typed.core.TypeFilter{:type #typed.core.Value{:val :Red}, :path (#typed.core.KeyPE{:val :left} #typed.core.KeyPE{:val :tree}), :id tmap} #typed.core.TypeFilter{:type #typed.core.Value{:val :Black}, :path (#typed.core.KeyPE{:val :tree}), :id tmap}}}}} #typed.core.TypeFilter{:type #typed.core.Value{:val :Black}, :path (#typed.core.KeyPE{:val :tree}), :id tmap}}} #typed.core.NotTypeFilter{:type #typed.core.Value{:val :Black}, :path (#typed.core.KeyPE{:val :tree}), :id tmap}}}}")]

  (->
    (env+ (->PropEnv {'tmap (->Name 'typed.test.rbt/badRight)}
                     [])
          [(:else fs)]
          (atom true))
    :l (get 'tmap)))
)

; restore-right (BLack (e,l,r)) >=> dict
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
    (tc-pr-filters "TEST1"
      (and (= :Black (-> tmap :tree))
           (= :Red (-> tmap :left :tree))
           (= :Red (-> tmap :right :tree))
           (= :Red (-> tmap :right :left :tree))))
    (let [_ (tc-pr-env "down first then")
          {lt :left rt :right e :entry} tmap
          ;re-color
          res {:tree :Red
               :entry e
               :left (assoc lt
                            :tree :Black)
               :right (assoc rt
                             :tree :Black)}]
      (tc-pr-env "restore-right: output first branch (res)")
      res)

    (and (do (tc-pr-env "SECOND CASE")
           true)
         (= :Black (-> tmap :tree))
         (= :Red (-> tmap :left :tree))
         (= :Red (-> tmap :right :tree))
         (= :Red (-> tmap :right :left :tree)))
    (let [{lt :left rt :right e :entry} tmap
          ;re-color
          res {:tree :Red
               :entry e
               :left (assoc lt
                            :tree :Black)
               :right (assoc rt
                             :tree :Black)}]
      (tc-pr-env "restore-right: output second branch (res)")
      res)

    (and (= :Black (-> tmap :tree))
         (= :Red (-> tmap :right :tree))
         (= :Red (-> tmap :right :left :tree)))
    (let [{e :entry
           l :left
           {re :entry
            {rle :entry
             rll :left
             rlr :right} :left
            rr :right} :right} tmap
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
      res)

    (and (= :Black (-> tmap :tree))
         (= :Red (-> tmap :right :tree))
         (= :Red (-> tmap :right :right :tree)))
    (let [{e :entry
           l :left
           {re :entry
            rl :left
            rr :right} :right} tmap]
      ;l is black, shallow rotate
      {:tree :Black
       :left {:tree :Red
              :entry re
              :left l
              :right rl}
       :right rr})

    (do (tc-pr-env "final else:")
      :else) 
    (do (tc-pr-env "follow final else:")
      tmap)))

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

(ann insert (Fn [rbt EntryT -> rbt]))
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

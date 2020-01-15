(ns clojure.core.typed.test.rbt
  (:refer-clojure :exclude [let])
  (:require [clojure.core.typed 
             :refer [ann print-env print-filterset ann-form defalias
                     U IFn let Rec]
             :as t]))

;-------------------------------
; 'Normal' types
;-------------------------------

(defalias EntryT 
  "The payload"
  '{:key Number
    :datum Number})

(defalias Empty
  "A terminating node."
  '{:tree ':Empty})

(defalias Red
  "A red node"
  (t/TFn [[l :variance :covariant]
          [r :variance :covariant]]
    '{:tree ':Red
      :entry EntryT
      :left l
      :right r}))

(defalias Black
  "A black node"
  (t/TFn [[l :variance :covariant]
          [r :variance :covariant]]
    '{:tree ':Black
      :entry EntryT
      :left l
      :right r}))


;-------------------------------
; 'Refinement' types
;-------------------------------

(defalias rbt 
  "Trees with only black children for red nodes"
  (Rec [rbt]
    (U 
      Empty
      (Black rbt rbt)
      ;(Red bt bt)
      (Red (U 
             Empty
             (Black rbt rbt))
           (U 
             Empty
             (Black rbt rbt))))))

(defalias bt 
  "Like rbt but additionally the root node is black"
  (Rec [bt]
       (U 
         Empty
         (Black rbt rbt))))

(defalias red 
  "Trees with a red root"
  (Red bt bt))

(defalias badRoot 
  "Invariant possibly violated at the root"
  (U 
    Empty
    (Black rbt bt)
    (Red rbt bt)
    (Red bt rbt)))

(defalias badLeft 
  "Invariant possibly violated at the left child"
  (U
   Empty
   (Black rbt rbt)
   (Red bt bt)
   (Black badRoot rbt)))

(defalias badRight 
  "Invariant possibly violated at the right child"
  (U
   Empty
   (Black rbt rbt)
   (Red bt bt)
   (Black rbt badRoot)))

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

(ann restore-right (IFn [badRight -> rbt]))
(defn restore-right [tmap]
  (cond
    (and (-> tmap :tree #{:Black})
         (-> tmap :left :tree #{:Red})
         (-> tmap :right :tree #{:Red})
         (-> tmap :right :left :tree #{:Red}))
    (let [{lt :left rt :right e :entry} tmap]
      ;re-color
      {:tree :Red
       :entry e
       :left (assoc lt
                    :tree :Black)
       :right (assoc rt
                     :tree :Black)})

    (and (-> tmap :tree #{:Black})
         (-> tmap :left :tree #{:Red})
         (-> tmap :right :tree #{:Red})
         (-> tmap :right :right :tree #{:Red}))
    (let [{lt :left rt :right e :entry} tmap]
      ;re-color
      {:tree :Red
       :entry e
       :left (assoc lt
                    :tree :Black)
       :right (assoc rt
                     :tree :Black)})

    (and (-> tmap :tree #{:Black})
         (-> tmap :right :tree #{:Red})
         (-> tmap :right :left :tree #{:Red}))
    (let [{e :entry
           l :left
           {re :entry
            {rle :entry
             rll :left
             rlr :right}
            :left
            rr :right}
           :right} tmap]
      ;l is black, deep rotate
      {:tree :Black
       :entry rle
       :left {:tree :Red
              :entry e
              :left l
              :right rll}
       :right {:tree :Red
               :entry re
               :left rlr
               :right rr}})

    (and (-> tmap :tree #{:Black})
         (-> tmap :right :tree #{:Red})
         (-> tmap :right :right :tree #{:Red}))
    (let [{e :entry
           l :left
           {re :entry
            rl :left
            rr :right} 
           :right} tmap]
      ;l is black, shallow rotate
      {:tree :Black
       :entry re
       :left {:tree :Red
              :entry e
              :left l
              :right rl}
       :right rr})

    :else tmap))

;;  (* val ins : 'a dict -> 'a dict  inserts entry *)
;;  (* ins (Red _) may violate color invariant at root *)
;;  (* ins (Black _) or ins (Empty) will be red/black tree *)
;;  (* ins preserves black height *)
(ann insert (IFn [rbt EntryT -> rbt]))
(defn insert [dict {:keys [key datum] :as entry}]
  (let [
        ;;  (*[ ins1 :> 'a rbt -> 'a badRoot 
        ;;            & 'a bt -> 'a rbt  ]*)


         ins1 :-
          (IFn [bt -> rbt]
               [rbt -> (U rbt badRoot)])
          (fn ins1 [{:keys [tree] :as tmap}]
            (cond
              (#{:Empty} tree) {:tree :Red
                                :entry entry
                                :left {:tree :Empty}
                                :right {:tree :Empty}}
              (#{:Red} tree) (let [{{key1 :key datum1 :datum :as entry1} 
                                    :entry
                                    :keys [left right]} tmap]
                               (cond
                                 (= key key1) {:tree :Red
                                               :entry entry
                                               :left left
                                               :right right}
                                 (< key key1) {:tree :Red
                                               :entry entry1
                                               :left (ins1 left)
                                               :right right}
                                 :else {:tree :Red
                                        :entry entry1
                                        :left left
                                        :right (ins1 right)}))
              (#{:Black} tree) (let [{{key1 :key datum1 :datum :as e1} :entry
                                      l :left r :right} tmap]
                                 (cond
                                   (= key key1) {:tree :Black
                                                 :entry entry
                                                 :left l
                                                 :right r}
                                   ; (< key key1) (restore-left {:tree :Black
                                   ;                             :entry e1
                                   ;                             :left (ins1 l)
                                   ;                             :right r})
                                   :else (restore-right {:tree :Black
                                                         :entry e1
                                                         :left l
                                                         :right (ins1 r)})))
              :else (assert nil "Should never happen")))]
    (let [{:keys [tree l r] :as res} (ins1 dict)]
      (cond
        (and (-> tree #{:Red})
             (or (-> l :tree #{:Red})
                 (-> r :tree #{:Red})))
        (assoc res
               :tree :Black) ;re-color
        :else res)))) ;depend on sequential matching

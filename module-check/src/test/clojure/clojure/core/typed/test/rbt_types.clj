(ns clojure.core.typed.test.rbt-types
  (:require [clojure.core.typed :as t :refer [defalias declare-names Rec U]]))

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
         ;(Black rbt rbt)
         (Black 
           (U 
             Empty
             (Black rbt rbt)
             (Red bt bt))
           (U 
             Empty
             (Black rbt rbt)
             (Red bt bt))))))

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

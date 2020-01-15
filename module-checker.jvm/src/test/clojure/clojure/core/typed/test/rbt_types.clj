(ns clojure.core.typed.test.rbt-types
  (:require [clojure.core.typed :as t :refer [defalias declare-names]]))

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
  (t/Rec [rbt]
    (t/U 
      Empty
      (Black rbt rbt)
      ;(Red bt bt)
      (Red (t/U 
             Empty
             (Black rbt rbt))
           (t/U 
             Empty
             (Black rbt rbt))))))

(defalias bt 
  "Like rbt but additionally the root node is black"
  (t/Rec [bt]
       (t/U 
         Empty
         ;(Black rbt rbt)
         (Black 
           (t/U 
             Empty
             (Black rbt rbt)
             (Red bt bt))
           (t/U 
             Empty
             (Black rbt rbt)
             (Red bt bt))))))

(defalias red 
  "Trees with a red root"
  (Red bt bt))

(defalias badRoot 
  "Invariant possibly violated at the root"
  (t/U 
    Empty
    (Black rbt bt)
    (Red rbt bt)
    (Red bt rbt)))

(defalias badLeft 
  "Invariant possibly violated at the left child"
  (t/U
   Empty
   (Black rbt rbt)
   (Red bt bt)
   (Black badRoot rbt)))

(defalias badRight 
  "Invariant possibly violated at the right child"
  (t/U
   Empty
   (Black rbt rbt)
   (Red bt bt)
   (Black rbt badRoot)))

(ns clojure.core.typed.test.rbt-types
  (:require [clojure.core.typed :as t :refer [def-alias declare-names]]))

;-------------------------------
; 'Normal' types
;-------------------------------

(def-alias EntryT 
  "The payload"
  '{:key Number
    :datum Number})

(def-alias Empty
  "A terminating node."
  '{:tree ':Empty})

(def-alias Red
  "A red node"
  (TFn [[l :variance :covariant]
        [r :variance :covariant]]
    '{:tree ':Red
      :entry EntryT
      :left l
      :right r}))

(def-alias Black
  "A black node"
  (TFn [[l :variance :covariant]
        [r :variance :covariant]]
    '{:tree ':Black
      :entry EntryT
      :left l
      :right r}))


;-------------------------------
; 'Refinement' types
;-------------------------------

(def-alias rbt 
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

(def-alias bt 
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

(def-alias red 
  "Trees with a red root"
  (Red bt bt))

(def-alias badRoot 
  "Invariant possibly violated at the root"
  (U 
    Empty
    (Black rbt bt)
    (Red rbt bt)
    (Red bt rbt)))

(def-alias badLeft 
  "Invariant possibly violated at the left child"
  (U
   Empty
   (Black rbt rbt)
   (Red bt bt)
   (Black badRoot rbt)))

(def-alias badRight 
  "Invariant possibly violated at the right child"
  (U
   Empty
   (Black rbt rbt)
   (Red bt bt)
   (Black rbt badRoot)))

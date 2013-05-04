(ns clojure.core.typed.test.rbt-types
  (:require [clojure.core.typed :refer [def-alias declare-names]]))

(def-alias EntryT (HMap :mandatory {:key Number
                         :datum Number})) ;TODO is this EntryT type correct? No definition in thesis

(declare-names rbt bt)

;Trees with only black children for red nodes
(def-alias rbt (U 
                 ;Empty
                 (HMap :mandatory {:tree (Value :Empty)})
                 ;Black
                 (HMap :mandatory {:tree (Value :Black)
                        :entry EntryT
                        :left rbt
                        :right rbt})
                 ;Red
                 (HMap :mandatory {:tree (Value :Red)
                        :entry EntryT
                        :left bt
                        :right bt})))

;As above but additionally the root node is black
(def-alias bt (U
                ;Empty
                (HMap :mandatory {:tree (Value :Empty)})
                ;Black
                (HMap :mandatory {:tree (Value :Black)
                       :entry EntryT
                       :left rbt
                       :right rbt})))

; Trees with a red root
(def-alias red (U
                 ;Red
                 (HMap :mandatory {:tree (Value :Red)
                        :entry EntryT
                        :left bt
                        :right bt})))

;invariant possibly violated at the root
(def-alias badRoot (U
                     ;Empty
                     (HMap :mandatory {:tree (Value :Empty)})
                     ;Black
                     (HMap :mandatory {:tree (Value :Black)
                            :entry EntryT
                            :left rbt
                            :right bt})
                     ;Red
                     (HMap :mandatory {:tree (Value :Red)
                            :entry EntryT
                            :left rbt
                            :right bt})
                     ;Red
                     (HMap :mandatory {:tree (Value :Red)
                            :entry EntryT
                            :left bt
                            :right rbt})))

;invariant possibly violated at the left child
(def-alias badLeft (U
                     ;Empty
                     (HMap :mandatory {:tree (Value :Empty)})
                     ;Black
                     (HMap :mandatory {:tree (Value :Black)
                            :entry EntryT
                            :left rbt
                            :right rbt})
                     ;Red
                     (HMap :mandatory {:tree (Value :Red)
                            :entry EntryT
                            :left bt
                            :right bt})
                     ;Black
                     (HMap :mandatory {:tree (Value :Black)
                            :entry EntryT
                            :left badRoot
                            :right rbt})))

;invariant possibly violated at the right child
(def-alias badRight (U
                      ;Empty
                      (HMap :mandatory {:tree (Value :Empty)})
                      ;Black
                      (HMap :mandatory {:tree (Value :Black)
                             :entry EntryT
                             :left rbt
                             :right rbt})
                      ;Red
                      (HMap :mandatory {:tree (Value :Red)
                             :entry EntryT
                             :left bt
                             :right bt})
                      ;Black
                      (HMap :mandatory {:tree (Value :Black)
                             :entry EntryT
                             :left rbt
                             :right badRoot})))

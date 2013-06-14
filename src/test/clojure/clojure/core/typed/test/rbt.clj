(ns clojure.core.typed.test.rbt
  (:refer-clojure :exclude [and])
  (:require [clojure.core.typed :refer [ann inst cf fn> pfn> def-alias declare-names
                                        print-env print-filterset check-ns typed-deps
                                        ann-form]]
            [clojure.core.typed.test.rbt-types :refer [badRight rbt EntryT Empty
                                                       Red Black bt]]
            #_[clojure.core.typed.subtype :as sub]
            #_[clojure.core.typed.check :as chk]
            #_[clojure.core.typed.parse-unparse :as prs]
            #_[clojure.core.typed.filter-ops :as fo]
            [clojure.repl :refer [pst]]
            [clojure.math.combinatorics :as comb]
            [clojure.tools.analyzer :refer [ast]]))

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

(typed-deps clojure.core.typed.test.rbt-types)

; restore-right (Black (e,l,r)) >=> dict
; where (1) Black(e,l,r) is ordered,
; (2) Black (e,l,r) hash black height n,
; (3) color invariant may be violated at the root of r:
;     one of its children must be red.
;  and dict is re-balanced red/black tree (satisfying all inv's)
;  and the same black height n.

(ann ^:nocheck tsyns Any)
(def tsyns '((HMap
                  :mandatory
                  {:tree (Value :Black),
                   :entry EntryT,
                   :left rbt,
                   :right
                   (HMap
                     :mandatory
                     {:tree (Value :Black),
                      :entry EntryT,
                      :left
                      (U
                       (HMap :mandatory {:tree (Value :Empty)})
                       (HMap
                         :mandatory
                         {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
                      :right
                      (U
                       (HMap :mandatory {:tree (Value :Empty)})
                       (HMap
                         :mandatory
                         {:tree (Value :Black),
                          :entry EntryT,
                          :left rbt,
                          :right rbt}))})})
               (HMap
                 :mandatory
                 {:tree (Value :Black),
                  :entry EntryT,
                  :left
                  (U
                   (HMap :mandatory {:tree (Value :Empty)})
                   (HMap
                     :mandatory
                     {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
                  :right
                  (U
                   (HMap
                     :mandatory
                     {:tree (Value :Red),
                      :entry EntryT,
                      :left rbt,
                      :right
                      (U
                       (HMap :mandatory {:tree (Value :Empty)})
                       (HMap
                         :mandatory
                         {:tree (Value :Black),
                          :entry EntryT,
                          :left rbt,
                          :right rbt}))})
                   (HMap
                     :mandatory
                     {:tree (Value :Red),
                      :entry EntryT,
                      :left bt,
                      :right
                      (U
                       (HMap :mandatory {:tree (Value :Empty)})
                       (HMap
                         :mandatory
                         {:tree (Value :Black),
                          :entry EntryT,
                          :left rbt,
                          :right rbt}))})
                   (HMap
                     :mandatory
                     {:tree (Value :Black),
                      :entry EntryT,
                      :left rbt,
                      :right
                      (U
                       (HMap :mandatory {:tree (Value :Empty)})
                       (HMap
                         :mandatory
                         {:tree (Value :Black),
                          :entry EntryT,
                          :left rbt,
                          :right rbt}))}))})
               (HMap
                 :mandatory
                 {:tree (Value :Black),
                  :entry EntryT,
                  :left
                  (U
                   (HMap :mandatory {:tree (Value :Empty)})
                   (HMap
                     :mandatory
                     {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
                  :right
                  (U
                   (HMap
                     :mandatory
                     {:tree (Value :Red),
                      :entry EntryT,
                      :left bt,
                      :right
                      (U
                       (HMap :mandatory {:tree (Value :Empty)})
                       (HMap
                         :mandatory
                         {:tree (Value :Black),
                          :entry EntryT,
                          :left rbt,
                          :right rbt}))})
                   (HMap
                     :mandatory
                     {:tree (Value :Black),
                      :entry EntryT,
                      :left rbt,
                      :right
                      (U
                       (HMap :mandatory {:tree (Value :Empty)})
                       (HMap
                         :mandatory
                         {:tree (Value :Black),
                          :entry EntryT,
                          :left rbt,
                          :right rbt}))}))})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left rbt,
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left rbt,
      :right
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black),
          :entry EntryT,
          :left rbt,
          :right rbt}))})})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left rbt,
   :right
   (U
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right bt})
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right rbt})
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right bt}))})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left rbt,
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
      :right rbt})})
(HMap
  :mandatory
  {:tree (Value :Red),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt}))})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
      :right
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black),
          :entry EntryT,
          :left rbt,
          :right rbt}))})})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (U
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right bt})
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right rbt})
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right bt}))})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left rbt,
      :right
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black),
          :entry EntryT,
          :left rbt,
          :right rbt}))})})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left rbt,
   :right
   (U
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right rbt}))})
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right rbt}))}))})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right clojure.core.typed.test.rbt-types/badRoot})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
      :right rbt})})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left rbt,
   :right
   (U
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right bt})
    (HMap :mandatory {:tree (Value :Empty)}))})
(HMap
  :mandatory
  {:tree (Value :Red),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left rbt,
      :right
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black),
          :entry EntryT,
          :left rbt,
          :right rbt}))})})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (U
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right rbt}))})
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right rbt}))}))})
(HMap
  :mandatory
  {:tree (Value :Red),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right bt})
(HMap
  :mandatory
  {:tree (Value :Red),
   :entry EntryT,
   :left bt,
   :right
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt}))})
badRight
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (U
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right bt})
    (HMap :mandatory {:tree (Value :Empty)}))})
(HMap
  :mandatory
  {:tree (Value :Red),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
      :right
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black),
          :entry EntryT,
          :left rbt,
          :right rbt}))})})
(HMap
  :mandatory
  {:tree (Value :Red),
   :entry EntryT,
   :left bt,
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left rbt,
      :right
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black),
          :entry EntryT,
          :left rbt,
          :right rbt}))})})
(HMap
  :mandatory
  {:tree (Value :Red), :entry EntryT, :left bt, :right bt})
(HMap :mandatory {:tree (Value :Empty)})
(HMap
  :mandatory
  {:tree (Value :Red),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
      :right rbt})})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt}))})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left rbt,
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
      :right bt})})
(HMap
  :mandatory
  {:tree (Value :Red),
   :entry EntryT,
   :left bt,
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
      :right
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black),
          :entry EntryT,
          :left rbt,
          :right rbt}))})})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left rbt,
   :right
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt}))})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (U
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right bt})
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right rbt}))})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left rbt,
   :right
   (U
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left bt,
       :right
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right rbt}))})
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left rbt,
       :right
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right rbt}))}))})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left rbt,
   :right
   (U
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left rbt,
       :right
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right rbt}))})
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left bt,
       :right
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right rbt}))})
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left rbt,
       :right
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right rbt}))}))})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right rbt})
(HMap
  :mandatory
  {:tree (Value :Red),
   :entry EntryT,
   :left bt,
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
      :right rbt})})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left
   (U
    (HMap :mandatory {:tree (Value :Empty)})
    (HMap
      :mandatory
      {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
   :right
   (HMap
     :mandatory
     {:tree (Value :Black),
      :entry EntryT,
      :left
      (U
       (HMap :mandatory {:tree (Value :Empty)})
       (HMap
         :mandatory
         {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
      :right bt})})
(HMap
  :mandatory
  {:tree (Value :Black),
   :entry EntryT,
   :left rbt,
   :right
   (U
    (HMap
      :mandatory
      {:tree (Value :Red),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right bt})
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right rbt}))})))

#_(doseq [tsyn tsyns]
  (try 
    (sub/subtype
      (prs/parse-type tsyn)
      (prs/parse-type 'rbt))
    (catch clojure.lang.ExceptionInfo e
      (clojure.pprint/pprint tsyn)
      (flush))))

;Filters in final case

;TEST4
(ann ^:nocheck final-case-filters1 Any)
(def final-case-filters1
  '((! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
    (! (Value :Red) tmap [(Key :right) (Key :tree)])
    (! (Value :Black) tmap [(Key :tree)])))

;TEST3
(ann ^:nocheck final-case-filters2 Any)
(def final-case-filters2
  '((! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
    (! (Value :Red) tmap [(Key :right) (Key :tree)])
    (! (Value :Black) tmap [(Key :tree)])))

;TEST2
(ann ^:nocheck final-case-filters3 Any)
(def final-case-filters3
  '((! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
    (! (Value :Red) tmap [(Key :right) (Key :tree)])
    (! (Value :Red) tmap [(Key :left) (Key :tree)])
    (! (Value :Black) tmap [(Key :tree)])))

;TEST1
(ann ^:nocheck final-case-filters4 Any)
(def final-case-filters4
  '((! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
    (! (Value :Red) tmap [(Key :right) (Key :tree)])
    (! (Value :Red) tmap [(Key :left) (Key :tree)])
    (! (Value :Black) tmap [(Key :tree)])))

#_(doseq [f1 (map prs/parse-filter final-case-filters1)
        f2 (map prs/parse-filter final-case-filters2)
        f3 (map prs/parse-filter final-case-filters3)
        f4 (map prs/parse-filter final-case-filters4)]
  (pr "*")
  (flush)
  (let [f (fo/-and f1 f2 f3 f4)]
    (when-not
      (sub/subtype?
        (-> (chk/update-composite {'tmap (prs/parse-type 'badRight)} f)
            (get 'tmap))
        (prs/parse-type 'rbt))
      (pr ".")
      (prn (map prs/unparse-filter [f1 f2 f3 f4]))
      (flush)
      (throw (Exception. "a")))))

; applying this filter to a badRight does not give rbt
(ann ^:nocheck a-failure-combo1 Any)
(def a-failure-combo1
 '(& ;TEST4
     (! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)]) 
     ;TEST3
     (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)]) 
     ;TEST2
     (! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)]) 
     ;TEST1
     (! (Value :Red) tmap [(Key :left) (Key :tree)])))

#_(->
  (chk/update-composite {'tmap (prs/parse-type 'badRight)}
                        (prs/parse-filter a-failure-combo1))
  (get 'tmap)
  prs/unparse-type
  count)

#_(def disj-of-conj-filter
  '(| 
     (& (is (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
        (! (Value :Red) tmap [(Key :right) (Key :tree)])
        (! (Value :Red) tmap [(Key :left) (Key :tree)])
        (! (Value :Black) tmap [(Key :tree)]))
     (& (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
        (is (Value :Red) tmap [(Key :right) (Key :tree)])
        (! (Value :Red) tmap [(Key :left) (Key :tree)])
        (! (Value :Black) tmap [(Key :tree)]))))

#_(clojure.pprint/pprint (prs/unparse-filter (prs/parse-filter disj-of-conj-filter)))

; FAILURE CASES FOR FINAL CASE

(ann ^:nocheck final-failure-cases Any)
(def final-failure-cases
  '((HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right
       (U
        (HMap
          :mandatory
          {:tree (Value :Red),
           :entry EntryT,
           :left rbt,
           :right
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt}))})
        (HMap
          :mandatory
          {:tree (Value :Red),
           :entry EntryT,
           :left bt,
           :right
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt}))})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black),
               :entry EntryT,
               :left rbt,
               :right rbt}))}))})
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left rbt,
       :right
       (U
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
           :right bt})
        (HMap
          :mandatory
          {:tree (Value :Red),
           :entry EntryT,
           :left
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
           :right rbt})
        (HMap
          :mandatory
          {:tree (Value :Red),
           :entry EntryT,
           :left
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
           :right bt}))})
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right
       (U
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
           :right bt})
        (HMap
          :mandatory
          {:tree (Value :Red),
           :entry EntryT,
           :left
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
           :right rbt})
        (HMap
          :mandatory
          {:tree (Value :Red),
           :entry EntryT,
           :left
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
           :right bt}))})
    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left
       (U
        (HMap :mandatory {:tree (Value :Empty)})
        (HMap
          :mandatory
          {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
       :right clojure.core.typed.test.rbt-types/badRoot})

    badRight

    (HMap
      :mandatory
      {:tree (Value :Black),
       :entry EntryT,
       :left rbt,
       :right
       (U
        (HMap
          :mandatory
          {:tree (Value :Red),
           :entry EntryT,
           :left rbt,
           :right
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt}))})
        (HMap
          :mandatory
          {:tree (Value :Red),
           :entry EntryT,
           :left bt,
           :right
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt}))})
        (HMap
          :mandatory
          {:tree (Value :Black),
           :entry EntryT,
           :left rbt,
           :right
           (U
            (HMap :mandatory {:tree (Value :Empty)})
            (HMap
              :mandatory
              {:tree (Value :Black),
               :entry EntryT,
               :left rbt,
               :right rbt}))}))})))


#_(cf
(ann-form 
  (ann-form {:tree :Empty} rbt)
  (HMap
    :mandatory
    {:tree (Value :Black),
     :entry EntryT,
     :left rbt,
     :right
     (HMap
       :mandatory
       {:tree (Value :Black),
        :entry EntryT,
        :left
        (U
         (HMap :mandatory {:tree (Value :Empty)})
         (HMap
           :mandatory
           {:tree (Value :Black), :entry EntryT, :left rbt, :right rbt})),
        :right
        (U
         (HMap :mandatory {:tree (Value :Empty)})
         (HMap
           :mandatory
           {:tree (Value :Black),
            :entry EntryT,
            :left rbt,
            :right rbt}))})})))

(ann restore-right 
     (Fn [badRight -> rbt]))
(defn restore-right [tmap]
  (cond
    (print-filterset 
      "TEST1 
        FS
        {:then (& (is :Black tmap [:tree])
                  (is :Red tmap [:left :tree])
                  (is :Red tmap [:right :tree])
                  (is :Red tmap [:right :left :tree]))
         :else (| (! :Black tmap [:tree])
                  (! :Red tmap [:left :tree])
                  (! :Red tmap [:right :tree])
                  (! :Red tmap [:right :left :tree]))}"
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

    (print-filterset "TEST4"
      (and (= :Black (-> tmap :tree))
           (= :Red (-> tmap :right :tree))
           (= :Red (-> tmap :right :right :tree))))
    (let [{e :entry
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

(ann ^:nocheck insert (Fn [rbt EntryT -> rbt]))
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

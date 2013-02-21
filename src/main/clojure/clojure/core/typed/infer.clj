(in-ns 'clojure.core.logic)


;; like infer, but dotted-var is the bound on the ...
;; and T-dotted is the repeated type
(defn infer-dots [X dotted-var dotted-bnd S T T-dotted R must-vars & {:keys [expected]}]
  {:pre [((hash-c? symbol? Bounds?) X)
         (symbol? dotted-var)
         (Bounds? dotted-bnd)
         (every? #(every? Type? %) [S T])
         (Type? T-dotted) 
         (AnyType? R)
         ((set-c? symbol?) must-vars)
         ((some-fn nil? Type?) expected)]
   :post [(substitution-c? %)]}
  (let [[short-S rest-S] (split-at (count T) S)
;        _ (prn "short-S" (map unparse-type short-S))
;        _ (prn "rest-S" (map unparse-type rest-S))
        expected-cset (if expected
                        (cs-gen #{} X {dotted-var dotted-bnd} R expected)
                        (empty-cset {} {}))
;        _ (prn "expected-cset" expected-cset)
        cs-short (cs-gen-list #{} X {dotted-var dotted-bnd} short-S T
                              :expected-cset expected-cset)
        ;_ (prn "cs-short" cs-short)
        new-vars (var-store-take dotted-var T-dotted (count rest-S))
        new-Ts (doall
                 (for [v new-vars]
                   (let [target (substitute-dots (map make-F new-vars) nil dotted-var T-dotted)]
                     #_(prn "replace" v "with" dotted-var "in" (unparse-type target))
                     (substitute (make-F v) dotted-var target))))
        ;_ (prn "new-Ts" new-Ts)
        cs-dotted (cs-gen-list #{} (merge X (zipmap new-vars (repeat dotted-bnd))) {dotted-var dotted-bnd} rest-S new-Ts
                               :expected-cset expected-cset)
        ;_ (prn "cs-dotted" cs-dotted)
        cs-dotted (move-vars-to-dmap cs-dotted dotted-var new-vars)
        ;_ (prn "cs-dotted" cs-dotted)
        cs (cset-meet cs-short cs-dotted)
        ;_ (prn "cs" cs)
        ]
    (subst-gen (cset-meet cs expected-cset) #{dotted-var} R)))

;; like infer, but T-var is the vararg type:
(defn infer-vararg [X Y S T T-var R & [expected]]
  {:pre [(every? (hash-c? symbol? Bounds?) [X Y])
         (every? (every-c? Type?) [S T])
         ((some-fn nil? Type?) T-var)
         (AnyType? R)
         ((some-fn nil? AnyType?) expected)]
   :post [((some-fn nil? substitution-c?) %)]}
  ;(prn "infer-vararg" "X:" X)
  (let [new-T (if T-var
                ;Pad out T
                (concat T (repeat (- (count S) (count T)) T-var))
                T)]
;    (prn "S" (map unparse-type S))
;    (prn "new-T" (map unparse-type new-T))
;    (prn "R" (unparse-type R))
;    (prn "expected" (class expected) (when expected (unparse-type expected)))
    (and (>= (count S) (count T))
         (infer X Y S new-T R expected))))

;; X : variables to infer
;; Y : indices to infer
;; S : actual argument types
;; T : formal argument types
;; R : result type
;; expected : #f or the expected type
;; returns a substitution
;; if R is nil, we don't care about the substituion
;; just return a boolean result
(defn infer [X Y S T R & [expected]]
  {:pre [(every? (hash-c? symbol? Bounds?) [X Y])
         (every? Type? S)
         (every? Type? T)
         (AnyType? R)
         ((some-fn nil? AnyType?) expected)]
   :post [((some-fn nil? true? substitution-c?) %)]}
;  (prn "infer" )
;  (prn "X:" X) 
;  (prn "Y:" Y) 
;  (prn "S:" (map unparse-type S))
;  (prn "T:" (map unparse-type T))
;  (when R
;    (prn "R:" (class R) (unparse-type R)))
;  (when expected
;    (prn "expected:" (class expected) (unparse-type expected)))
  (let [expected-cset (if expected
                        (cs-gen #{} X Y R expected)
                        (empty-cset {} {}))
        ;_ (prn "expected cset" expected-cset)
        cs (cs-gen-list #{} X Y S T :expected-cset expected-cset)
        cs* (cset-meet cs expected-cset)]
    ;(prn "final cs" cs*)
    (if R
      (subst-gen cs* (set (keys Y)) R)
      true)))


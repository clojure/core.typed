(ns clojure.core.typed.test.hole
  (:require [clojure.repl :refer [pst]]
            [clojure.core.typed :refer [ann ann-form ann-datatype check-ns]]))

(ann-datatype Hole [])
(deftype Hole [])

(ann hole [-> Nothing])

(defn hole [] 
  (throw (Exception. "hole")))

(ann compose (All [a b c] [[b -> c] [a -> b] a -> c]))
#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x b)]
    (hole)))

; 1. give x wrong type (b).

;clojure.core.typed.test.hole=> (check-ns)
;#<AssertionError java.lang.AssertionError: Assert failed: 17: Local binding x expected type b, but actual type a
;(or (not expected) (subtype? t (ret-t expected)))>

#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x a)]
    (hole)))

; 2. Silent hole is silent. Passes type checking

;clojure.core.typed.test.hole=> (check-ns)
;nil

#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x a)]
    (->Hole)))

; 3. Noisy hole complains.

;#<Exception java.lang.Exception: Type Error, clojure.core.typed.test.hole:41
;
;Actual type
;	clojure.core.typed.test.hole.Hole
;is not a subtype of Expected type
;	c
;
;Form: (let* [_ (clojure.core.typed/ann-form* f (quote [b -> c])) _18471 (clojure.core.typed/ann-form* g (quote [a -> b])) _18472 (clojure.core.typed/ann-form* x (quote a))] (clojure.core.typed.test.hole/->Hole))>

#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x a)]
    (f (->Hole))))

; 4. Fill in hole with an expression that returns `c`

;#<Exception java.lang.Exception: Type Error, clojure.core.typed.test.hole:58
;
;Actual type
;	clojure.core.typed.test.hole.Hole
;is not a subtype of Expected type
;	b
;
;Form: (f (clojure.core.typed.test.hole/->Hole))>

#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x a)]
    (f (g (->Hole)))))

; 5. Fill in hole with an expression that returns `b`

;#<Exception java.lang.Exception: Type Error, clojure.core.typed.test.hole:75
;
;Actual type
;	clojure.core.typed.test.hole.Hole
;is not a subtype of Expected type
;	a
;
;Form: (f (g (clojure.core.typed.test.hole/->Hole)))>

#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x a)]
    (f (g x))))

; 7. Correct implementation

;clojure.core.typed.test.hole=> (check-ns)
;nil

(defn compose [f g x]
  (f (g x)))

; 8. Delete type annotations

;clojure.core.typed.test.hole=> (check-ns)
;nil

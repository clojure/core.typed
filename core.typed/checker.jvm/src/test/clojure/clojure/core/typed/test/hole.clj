(ns clojure.core.typed.test.hole
  (:require [clojure.repl :refer [pst]]
            [clojure.core.typed :as t :refer [ann ann-form ann-datatype check-ns typed-deps]]
            [clojure.core.typed.hole :refer [noisy-hole silent-hole]]
            #_[clojure.core.typed.test.monads :refer [;types 
                                                    AnyMonad 
                                                    ;vars
                                                    domonad defmonadfn ann-monadfn]]))

; # Holes
;
; See: http://matthew.brecknell.net/posts/2013/02/26/hole-driven-haskell/
;
; This file roughly follows the video cast with the equivalent core.typed code. Note: master branch has better error msgs for these cases, as of 0.1.7

;; ## Compose

(ann compose (t/All [a b c] [[b -> c] [a -> b] a -> c]))
#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x b)]
    (silent-hole)))

; 1. give x wrong type (b).

;clojure.core.typed.test.hole=> (check-ns)
;#<AssertionError java.lang.AssertionError: Assert failed: 28: Local binding x expected type b, but actual type a
;(or (not expected) (subtype? t (ret-t expected)))>

#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x a)]
    (silent-hole)))

; 2. Silent hole is silent. Passes type checking

;clojure.core.typed.test.hole=> (check-ns)
;nil

#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x a)]
    (noisy-hole)))

; 3. Noisy hole complains.

;#<Exception java.lang.Exception: Type Error, clojure.core.typed.test.hole:53
;
;Actual type
;  clojure.core.typed.test.hole.Hole
;is not a subtype of Expected type
;  c
;
;Form: (let* [_ (clojure.core.typed/ann-form* f (quote [b -> c])) _18471 (clojure.core.typed/ann-form* g (quote [a -> b])) _18472 (clojure.core.typed/ann-form* x (quote a))] (clojure.core.typed.test.hole/->Hole))>

#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x a)]
    (f (noisy-hole))))

; 4. Fill in hole with an expression that returns `c`

;#<Exception java.lang.Exception: Type Error, clojure.core.typed.test.hole:69
;
;Actual type
;  clojure.core.typed.test.hole.Hole
;is not a subtype of Expected type
;  b
;
;Form: (f (clojure.core.typed.test.hole/->Hole))>

#_(defn compose [f g x]
  (let [_ (ann-form f [b -> c])
        _ (ann-form g [a -> b])
        _ (ann-form x a)]
    (f (g (noisy-hole)))))

; 5. Fill in hole with an expression that returns `b`

;#<Exception java.lang.Exception: Type Error, clojure.core.typed.test.hole:86
;
;Actual type
;  clojure.core.typed.test.hole.Hole
;is not a subtype of Expected type
;  a
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

#_(defn compose [f g x]
  (f (g x)))

; 8. Delete type annotations

;clojure.core.typed.test.hole=> (check-ns)
;nil


;; ## monadic apply

#_(ann-monadfn mapply 
             m
             (t/All [a b]
                  [(m [a -> b]) (m a) -> (m b)]))

#_(defmonadfn mapply [mf ma]
  (let [_ (ann-form mf (m [a -> b]))
        _ (ann-form ma (m a))]
    (noisy-hole)))

; 1. Noisy hole

;#<Exception java.lang.Exception: Type Error, clojure.core.typed.test.hole:127
;
;Actual type
;  clojure.core.typed.test.hole.Hole
;is not a subtype of Expected type
;  (m b)
;
;Form: (let* [_ (clojure.core.typed/ann-form* mf (quote (m [a -> b]))) _21382 (clojure.core.typed/ann-form* ma (quote (m a)))] (clojure.core.typed.test.hole/->Hole))>

; 2. Some m-bind sanity checks

#_(defmonadfn mapply [mf ma]
  (let [_ (ann-form mf (m [a -> b]))
        _ (ann-form ma (m a))

        ; core.typed isn't smart enough to figure out the next statement is valid.
        ; This particular All isn't hard to support because `h` and `a` are both in the 0th
        ; position, but there are too many combinations.
        ;
        ;_ (ann-form m-bind (All [h] [(m h) [h -> (m b)] -> (m b)]))

        _ (ann-form #(m-bind mf %) [[[a -> b] -> (m b)] -> (m b)])
        _ (ann-form #(m-bind ma %) [[a -> (m b)] -> (m b)])
        ]
    (noisy-hole)))

; Note: line number sucks :/
;
;#<Exception java.lang.Exception: Type Error, clojure.core.typed.test.hole:145
;
;Actual type
;  clojure.core.typed.test.hole.Hole
;is not a subtype of Expected type
;  (m b)
;
;Form: (let* [_ (clojure.core.typed/ann-form* mf (quote (m [a -> b]))) _21976 (clojure.core.typed/ann-form* ma (quote (m a))) _21977 (clojure.core.typed/ann-form* (fn* ([p1__21960#] (m-bind mf p1__21960#))) (quote [[[a -> b] -> (m b)] -> (m b)]))] (clojure.core.typed.test.hole/->Hole))>

; 3. (8:15) Introduce k (noisy hole)

#_(defmonadfn mapply [mf ma]
  (let [_ (ann-form mf (m [a -> b]))
        _ (ann-form ma (m a))
        _ (ann-form #(m-bind mf %) [[[a -> b] -> (m b)] -> (m b)])
        _ (ann-form #(m-bind ma %) [[a -> (m b)] -> (m b)])
        k (ann-form
            (fn [f]
              (noisy-hole))
            [[a -> b] -> (m b)])]
    (m-bind mf k)))

;clojure.core.typed.test.hole=> (check-ns)
;#<Exception java.lang.Exception: Type Error, clojure.core.typed.test.hole:172
;
;Actual type
;  clojure.core.typed.test.hole.Hole
;is not a subtype of Expected type
;  (m b)
;
;Form: (clojure.core.typed.test.hole/->Hole)>

; TODO the rest of mapply

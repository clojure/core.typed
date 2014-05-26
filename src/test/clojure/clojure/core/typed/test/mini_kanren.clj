;; Mini-Kanren implemented in Clojure

;; by Jim Duey
;; last updated March 23, 2010

;; Copyright (c) Jim Duey, 2009, 2010. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns clojure.core.typed.test.mini-kanren
  (:import (clojure.lang IMeta Symbol Keyword IPersistentList IPersistentMap
                         Seqable))
  (:require #_[clojure.core.typed.test.monads :refer [defmonad with-monad m-chain]]
            [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [pst]]
            [clojure.core.typed :refer [ann inst defalias check-ns declare-names
                                        tc-ignore]]))
(comment

(comment
  This file is the result of implementing the logic programming system described
  in "The Reasoned Schemer", called mini-kanren, in Clojure.  I was struggling
  to understand the material in the book without a concrete system to work with,
  so I decided to implement the system in the book to aid my understanding.

  "The Reasoned Schemere" is the third book in the "Little Schemer" series of books
  that explain the Scheme programming language and various aspects of programming
  in Scheme.  "The Reasoned Schemer" extends Scheme to include logic programming like
  Prolog and the pattern maching in Erlang.  The first six chapters introduce the 
  syntax and semantics of this extension, which is what this file covers.

  I chose to pretty much lift the logic function names directly from the book to make
  learning the material easier.  Though there are some differences which should be 
  easily discerned by comparing the code to the book.

  There are two areas that required some work to implement this system in Clojure.

  First, in the book's implementation, logic variables are represented as vectors.
  I chose to represent logic variables as keywords, for reasons explained later.  So,
  the following definition creates a logic variable.  Each time an lvar is created,
  it is assigned a unique name using the gensym facility provided by Clojure.)

; a couple of special symbols

(ann _ '_)
(ann | '|)

(def _ '_)
(def | '|)

(defalias LVar String)

; working with logic variables
(ann lvar [-> LVar])
(defn lvar
  "Creating a unique logic variable that has no value assigned"
  []
  (str (gensym (str "lvar__"))))

(ann lvar? (predicate LVar))
(defn lvar?
  "Determine if a value is an logic variable or not."
  [x]
  (and (string? x)
       (.startsWith ^String x "lvar")))

(comment
  The second area of work stems from the fact that Clojure does not use cons cells
  to hold elements of lists.  The book made heavy use of this feature of Scheme and
  so a special version of cons had to be defined.  Passing an item and a list caused
  the item to be added at the head of the list.  Both of those cases were easy to
  account for.  The third case is when the second parameter to cons is not a list,
  causing cons to create a dotted pair where the cdr of the cons cell is a value, not
  a pointer to the rest of the list.  This aspect of cons is used heavily in the book
  to represent lists where the cdr may be undefined.  Since this was a required feature,
  I chose to implement an incomplete list by adding a :tail item to the meta data of a
  literal list, if the second parameter to cons is an lvar.  This worked very well.

  This also meant that a special version of 'next' had to be defined to handle incomplete lists.)

(declare-names Expr)

(defalias ImproperSeq 
  (I (Seqable Expr)
     (IMeta '{:tail Expr})))

(defalias LSeq 
  (U ImproperSeq
     (clojure.lang.ISeq Expr)
     (clojure.lang.IPersistentSet Expr)
     (clojure.lang.IPersistentVector Expr)))

(defalias Expr (U LSeq
                   nil
                   LVar
                   Symbol 
                   Keyword))

(ann is-seq? (predicate LSeq))
(defn- is-seq? [l]
  (or (seq? l)
      (set? l)
      (vector? l)))


(ann lcons [Expr Expr -> LSeq])
(tc-ignore
(defn lcons 
  "cons a value onto a logic seq"
  [a b]
  (cond
    (nil? b) (list a)
    (is-seq? b) (with-meta ((inst cons Expr) a b) (meta b))
    (lvar? b) (with-meta (list a) {:tail b})
    :else ((inst cons Expr) a b)))
  )

(ann lseq? (predicate (I LSeq (CountRange 1))))
(tc-ignore
(defn- lseq? 
  "test if a value is logic sequence or not
  an empty sequence is not a valid logic sequence"
  [l]
  (and (is-seq? l)
       (not (empty? l))))
  )

(tc-ignore
(defn incomplete 
  "make a logic sequence that is incomplete, the rest of the
  sequence is in the tail"
  [l t]
  (with-meta l {:tail t}))
  )

(ann lnext [LSeq -> Expr])
(defn- lnext 
  "get everything but the first element of a logic sequence"
  [l]
  (let [nl ((inst rest Expr) l)]
    (cond
      (= (first nl) |) (second nl)
      (and (empty? nl) (:tail (meta l))) (:tail (meta l))
      (empty? nl) []
      :else (with-meta nl (meta l)))))

(comment
  Values are assigned to logic variables through a mechanism called substitution.  In
  the book's implementation, this was accomplished through the use of an associative
  list, which is basically a list of cons cells where the car of the cell is the key
  and the cdr of the cell is the value.  Clojure has the hash-map data type which is
  a natural replacement for an associative list.  Using a hash-map to hold a substitution,
  the use of keywords for logic variables means that retrieval of a variable's value is
  simply a get operation on the map with that variable.)

; operations on substitutions

; this is what a substitution looks like, 'e is a value, :x and :y are
; logic variables:
;
; the keys in a substitution may only be keywords
; {:y 'e :x :y}
;
; the values in a substitution may be:
;   a symbol
;   a keyword (may or may not be a key in the substitution)
;   a literal list

(defalias Substitution (IPersistentMap Keyword Expr))

(ann lget [Substitution Any -> (U LVar Expr)])
(defn lget [s v]
  "Retrieve the value of a logic variable from a substitution"
  (cond
    (= _ v) (lvar)
    (contains? s v) (recur s (get s v))
    (lvar? v) v
    (and (is-seq? v) (:tail (meta v))) (let [tail (lget s (:tail (meta v)))]
                                         (if (is-seq? tail)
                                           (with-meta (seq (concat v tail)) (meta tail))
                                           (with-meta v {:tail tail})))
    :else v))

(ann deep-lget [Substitution (U LVar Expr) -> (U LVar Expr)])
(defn deep-lget [s v]
  "Walk a logic variable through a substitution.  If the value
  of the variable is a list, get each item in the list.
  Repeat recursively until all atoms are either variables
  with no values or values."
  (let [v (lget s v)]
    (cond
      (lvar? v) v
      (lseq? v) (let [sq (if (:tail (meta v))
                           (concat (map (partial deep-lget s) v)
                                   [| (deep-lget s (:tail (meta v)))])
                           (map (partial deep-lget s) v))]
                  (if (vector? v)
                    (vec sq)
                    sq))
      :else v)))

(ann deep-reify [Substitution (U Expr LVar) -> Substitution])
(defn deep-reify [s v]
  "Associate an indeterminate value with 'v' in 's' if
  it does not already have a value assigned to it.
  If the value of 'v' is a list, recursively associate
  values with each logic variable in the list."
  (let [v (lget s v)]
    (cond
      (lvar? v) (assoc s v (symbol (str "_." (count s))))
      (lseq? v) (with-meta (reduce deep-reify s v)
                           {:tail (:tail (meta v))})
      :else s)))

(ann mk-reify [(U Expr LVar) -> Substitution])
(defn mk-reify [v]
  "Assign an indeterminate value to 'v'"
  (deep-lget (deep-reify {} v) v))

(defn- circular?  [s x v]
  "Tests if associating x with v in s will generate a
  circular association"
  (let [v (lget s v)]
    (cond
      (lvar? v) (identical? v x)
      (lseq? v) (some (partial circular? s x) v)
      :else nil)))

(defn- _-to-lvar
  "Replace all the _'s in a value with lvars."
  [v]
  (cond
    (= _ v) (lvar)
    (is-seq? v) (if (contains? (meta v) :tail)
                  (let [new-v (with-meta (map _-to-lvar v) (meta v))]
                    (if (vector? v)
                      (vec new-v)
                      new-v))
                  (let [[sq tail] (split-with (partial not= |) v)
                        new-v (map _-to-lvar sq)
                        new-meta {:tail (_-to-lvar (second tail))}]
                    (if (vector? v)
                      (with-meta (vec new-v) new-meta)
                      (with-meta new-v new-meta))))
    :else v))

(defn- safe-assoc [x v s]
  "Associate x with v in s if it will not create
  a circular association"
  (cond
    (circular? s x v) nil
    :else (assoc s x (_-to-lvar v))))

(defn unify  [v w s]
  "Add an association to a substitution if it is not already there,
  if it does not violate any associations already in the substitution
  and if it won't create a circular association"
  (let [v (lget s v)
        w (lget s w)]
    (cond
      (identical? v w) s
      (lvar? v) (safe-assoc v w s)
      (lvar? w) (safe-assoc w v s)
      (and (lseq? v) (lseq? w)) (when-let [new-s (unify (first v) (first w) s)]
                                  (recur (lnext v) (lnext w) new-s))
      (= v w) s
      :else nil)))

; To unify means to add associations to a substitution according to the following rules:
(comment
  (let [x (lvar)
        y (lvar)
        w (lvar)
        q (lvar)]
    (assert (= {x 'a}         (unify x 'a {})))
    (assert (= {x y}        (unify x y {})))
    (assert (= {x 'b y 'b}        (unify x y {y 'b})))
    (assert (= {x 'c y 'c}        (unify x y {x 'c})))
    (assert (= {x 'a y 'a}        (unify x y {x 'a y 'a})))
    (assert (= {x 'a y x}         (unify x y {x 'a y x})))
    (assert (= {q 1 w 9}      (unify x x {q 1 w 9})))
    (assert (= {q 1 w 9}      (unify '(1 2 3) '(1 2 3) {q 1 w 9})))
    (assert (= {x '(a b)}       (unify x '(a b) {})))
    (assert (= {x (list 'a y)}      (unify x (list 'a y) {})))
    (assert (= {x 'a y 'b}        (unify (list x 'b) (list 'a y) {})))
    (assert (= {x 'a y '(b c)}      (unify (lcons x y) (list 'a 'b 'c) {})))
    (assert (= {x 'b}       (unify (list 'a x 'c) (list 'a 'b 'c) {})))
    (assert (= nil          (unify (lcons x y) () {q 1 w 9})))
    ))

; The monad foundation for the implementation.  It's basically a lazier 
; variant of the sequence-m monad.

(ann logic-m
     '{:m-result (All [x] (Seqable x))
       :m-bind (All [x y] 
                    [(Seqable x) [x -> (Seqable y)] -> (Seqable y)])
       :m-zero (Seqable Nothing)
       :m-plus (All [x]
                    [(Seqable x) * -> (Seqable x)])})
(defmonad logic-m
  [m-result (fn [v]
              (list v))
   m-bind   (fn m-bind-sequence [mv f]
              (lazy-seq
                (when-let [vs (seq mv)]
                  (lazy-cat (f (first vs))
                            (m-bind-sequence (rest vs) f)))))
   m-zero   (list)
   m-plus   (fn [mvs]
              (lazy-seq
                (apply concat mvs)))
   ])

; A similar monad except that it's m-plus function interleaves the values
; from each of the monadic values.

(ann logic-interleave-m
     '{:m-result (All [x] (Seqable x))
       :m-bind (All [x y] 
                    [(Seqable x) [x -> (Seqable y)] -> (Seqable y)])
       :m-zero (Seqable Nothing)
       :m-plus (All [x]
                    [(Seqable x) * -> (Seqable x)])})
(defmonad logic-interleave-m
  [m-result (fn [v]
              (list v))
   m-bind   (fn m-bind-sequence [mv f]
              (lazy-seq
                (when-let [vs (seq mv)]
                  (concat (f (first vs))
                          (m-bind-sequence (rest vs) f)))))
   m-zero   (list)
   m-plus   (fn m-plus-logic [mvs]
              (let [mvs (drop-while empty? mvs)]
                (when-not (empty? mvs)
                  (lazy-seq
                    (cons 
                      (ffirst mvs)
                      (m-plus-logic (concat (rest mvs)
                                            (list (rest (first mvs))))))))))
   ])

; Clojure implementation of 'Reasoned Schemer' goals
; a goal is a function that accepts a substitution and 
; returns a list of substitutions.  They are monadic functions
; under the logic-m monad.

; The two foundational goals.  A goal takes a stream of
; substitutions and returns a stream of substitutions.
(defn fail [s]
  (list))

(defn succeed [s]
  (list s))

; generates a goal that associates a logic variable
; with a value
(defn & [v w]
  (fn [s]
    (if-let [result (unify v w s)]
      (list result)
      (list))))

; &-expr generates a goal that associates a logic variable
; with the value produced by an expression.  value-of is
; a function that can be used in expressions to get the
; current value of a logic variable.
(ann curr-subst Substitution)
(def curr-subst)
(defn value-of [x]
  (deep-lget curr-subst x))

(defmacro &-expr [v expr]
  "Associates a free variable with the result of evaluating 'expr'."
  `(fn [s#]
     (binding [curr-subst s#]
       (when-let [result# (unify ~v ~expr curr-subst)]
         (list result#)))))

; some utility functions to build the mini-kanren operators

(defn- remove-else [clause-list]
  (map #(if (= 'else (first %))
          (next %)
          %)
       clause-list))

(with-monad logic-m
  (defn do-question [& clause]
    (m-chain clause))

  (defn test-question [& clause]
    (let [answer (m-chain (rest clause))]
      (fn [s]
        (let [tested ((first clause) s)]
          (when-not (= m-zero tested)
            (m-bind tested answer)))))))

(defn- build-clauses [c-list]
  (map #(cons 'do-question %)
       (remove-else c-list)))

(defn- build-questions [c-list]
  (map #(cons 'clojure.core.typed.test.mini-kanren/test-question %)
       (remove-else c-list)))

; the mini-kanren operators

(defmacro cond-e [& c-list]
  (let [clauses (build-clauses c-list)]
    `(with-monad logic-m
       (fn [s#]
         (~'m-plus (map (fn [c#] (c# s#))
                        (lazy-seq (list ~@clauses))))))))

(defmacro cond-i [& c-list]
  (let [clauses (build-clauses c-list)]
    `(with-monad logic-interleave-m
       (fn [s#]
         (~'m-plus (map (fn [c#] (c# s#))
                        (lazy-seq (list ~@clauses))))))))

(defmacro cond-a [& c-list]
  (let [questions (build-questions c-list)]
    `(with-monad logic-m
       (fn [s#]
         (first
           (drop-while nil? (map (fn [c#] (c# s#))
                                 (lazy-seq (list ~@questions)))))))))

(defmacro cond-u [& c-list]
  (let [questions (build-questions c-list)]
    `(with-monad logic-m
       (fn [s#]
         (take 1 
               (~'m-plus (map (fn [c#] (c# s#))
                              (lazy-seq (list ~@questions)))))))))

; exist is used to create new logic variables that can then be bound to values
(defmacro exist [v-list & goals]
  `(with-monad logic-m
     (let [~@(mapcat (fn [v]
                       `(~v (lvar)))
                     (seq v-list))] 
       (m-chain (list ~@goals)))))

(defn all [& args]
  (with-monad logic-m
    (m-chain args)))

; run computes the results of a mini-kanren expression
(defmacro run [x & goals]
  `(with-monad logic-m
     (let [~x (lvar)]
       (map (fn [s#]
              (mk-reify (deep-lget s# ~x)))
            (filter (complement nil?)
                    ((m-chain (list ~@goals)) {}))))))

; various logic programming functions from "Reasoned Schemer"

(defn cons-o
  "Generates a goal that associates '(cons f r) with 'l'."
  [f r l]
  (cond 
    (or (nil? r) (= r ())) (& (list f) l)
    (lvar? r) (& (lcons f r) l)
    (is-seq? r) (exist (new-r)
                       (& new-r r)
                       (& (lcons f new-r) l))
    :else (& (lcons f (list r)) l)))

(defn first-o
  "Generates a goal that associates 'f' with
  the first element of 'l'."
  [l f]
  (exist [r]
         (cons-o f r l)))

(defn rest-o
  "Generates a goal that associates 'r' with
  the rest of 'l'."
  [l r]
  (fn [s-list]
    ((exist (f)
            (cons-o f r l))
       s-list)))

(defn null-o [x]
  (fn [s-list]
    ((& [] x) s-list)))

(defn eq-o [x y]
  (& x y))

(defn pair-o [l]
  (if (= l [])
    fail
    (fn [s-list]
      ((exist (f r)
              (cons-o f r l)) s-list))))

(defn list-o [l]
  (cond-e
    ((null-o l) succeed)
    ((pair-o l) (exist (f r)
                       (rest-o l r)
                       (list-o r)))
    (else fail)))

(defn map-o [m]
  (fn [s]
    (when (map? (deep-lget s m))
      (list s))))

(defn vector-o [m]
  (fn [s]
    (when (vector? (deep-lget s m))
      (list s))))

(defn lol-o [l]
  (cond-e
    ((null-o l) succeed)
    ((exist (a)
            (first-o l a)
            (list-o a))
       (exist (d)
              (rest-o l d)
              (lol-o d)))
    (else fail)))

(defn twins-o [s]
  (exist (x)
         (& (list x x) s)))

(defn lot-o [l]
  (cond-e
    ((null-o l) succeed)
    ((exist (f)
            (first-o l f)
            (twins-o f))
       (exist (r)
              (rest-o l r)
              (lot-o r)))
    (else fail)))

(defn listof-o [pred-o l]
  (cond-e
    ((null-o l) succeed)
    ((exist (f)
            (first-o l f)
            (pred-o f))
       (exist (r)
              (rest-o l r)
              (listof-o pred-o r)))
    (else fail)))

(defn lot-o [l]
  (listof-o twins-o l))

(defn lol-o [l]
  (listof-o list-o l))  

(defn member-o [x l]
  (exist [r]
         (cond-e
           ((& [x | _] l))
           ((& [_ | r] l) (member-o x r)))))

(defn pmember-o [x l]
  (cond-e
    ((first-o l x) (rest-o l ()))
    ((first-o l x) (exist (f r)
                          (rest-o l (lcons f r))))
    (else (exist (r)
                 (rest-o l r)
                 (pmember-o x r)))))

(defn mem-o [x l out]
  (cond-e
    ((first-o l x) (& l out))
    (else (exist (r)
                 (rest-o l r)
                 (mem-o x r out)))))

(defn rember-o [x l out]
  (cond-e
    ((null-o l) (& () out))
    ((first-o l x) (rest-o l out))
    (else (exist (f r res)
                 (cons-o f r l)
                 (rember-o x r res)
                 (cons-o f res out)))))

(defn surprise-o [s]
  (rember-o s '(a b c) '(a b c)))

(defn append-o [l s out]
  (cond-e
    ((null-o l) (& s out))
    (else
      (exist (f r res)
             (cons-o f r l)
             (cons-o f res out)
             (append-o r s res)))))

(defn unwrap-o [x out]
  (cond-e
    (succeed (& x out))
    (else (pair-o x) (exist (f)
                            (first-o x f)
                            (unwrap-o f out)))))

(defn flattenrev-o [s out]
  (cond-e
    (succeed (cons-o s () out))
    ((null-o s) (& () out))
    (else (exist (f r res-f res-r)
                 (cons-o f r s)
                 (flattenrev-o f res-f)
                 (flattenrev-o r res-r)
                 (append-o res-f res-r out)))))

(defn any-o [g]
  (cond-e
    (g succeed)
    (else (any-o g))))

(def never-o
  (any-o fail))

(def always-o
  (any-o succeed))

(defn once-o [g]
  (cond-u
    (g succeed)
    (else fail)))

(defn sal-o [g]
  (cond-e
    (succeed succeed)
    (else g)))

(defn not-pasta-o [x]
  (cond-a
    ((& 'pasta x) fail)
    (else succeed)))

)

;; Monads in Clojure

;; Copyright (c) Konrad Hinsen, 2011. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns
  ^{:author "Konrad Hinsen"
     :see-also [["http://onclojure.com/2009/03/05/a-monad-tutorial-for-clojure-programmers-part-1/" "Monad tutorial part 1"]
                ["http://onclojure.com/2009/03/06/a-monad-tutorial-for-clojure-programmers-part-2/" "Monad tutorial part 2"]
                ["http://onclojure.com/2009/03/23/a-monad-tutorial-for-clojure-programmers-part-3/" "Monad tutorial part 3"]
                ["http://onclojure.com/2009/04/24/a-monad-tutorial-for-clojure-programmers-part-4/" "Monad tutorial part 4"]
                ["http://www.clojure.net/tags.html#monads-ref" "Blog posts on monads in Clojure"]]
     :doc "This library contains the most commonly used monads as well
           as macros for defining and using monads and useful monadic
           functions."}
  typed.test.monads
  (:import (clojure.lang Seqable IPersistentSet IPersistentMap Symbol))
  (:require [clojure.set]
            [clojure.repl :refer [pst]]
            [clojure.tools.macro
             :refer (with-symbol-macros defsymbolmacro name-with-attributes)]
            [typed.core 
             :refer (tc-ignore check-ns ann ann-protocol def-alias unsafe-ann-form ann-form inst fn> pfn>
                               AnyInteger print-env cf Option print-filterset)]))

;; Monad Protocols

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Defining monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro monad
  "Define a monad by defining the monad operations. The definitions
   are written like bindings to the monad operations m-bind and
   m-result (required) and m-zero and m-plus (optional)."
  [operations]
  `(let [~'m-bind   ::undefined
         ~'m-result ::undefined
         ~'m-zero   ::undefined
         ~'m-plus   ::undefined
         ~@operations]
     {:m-result ~'m-result
      :m-bind ~'m-bind 
      :m-zero ~'m-zero
      :m-plus ~'m-plus}))

(defmacro defmonad
  "Define a named monad by defining the monad operations. The definitions
   are written like bindings to the monad operations m-bind and
   m-result (required) and m-zero and m-plus (optional)."

  ([name doc-string operations]
   (let [doc-name (with-meta name {:doc doc-string})]
     `(defmonad ~doc-name ~operations)))

  ([name operations]
   `(def ~name (monad ~operations))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Using monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tc-ignore

(defn- ensure-items [n steps]
  "Ensures there are at least n elements on a list, will fill up with nil
  values when list is not big enough."
  (take n (concat steps (repeat nil))))

(defn- each3-steps [steps]
  "Transforms a list in a list of triples following the form:
   [a b c] => [[a b c] [b c nil] [c nil nil]]."
  (let [n (count steps)]
  (map vector (ensure-items n steps)
              (ensure-items n (rest steps))
              (ensure-items n (rest (rest steps))))))

(def ^:private prepare-monadic-steps
     #(->> % (partition 2) reverse each3-steps))

(defn- if-then-else-statement
  "Process an :if :then :else steps when adding a new
  monadic step to the mexrp."
  [[[_          else-mexpr]
    [then-bform then-mexpr]
    [if-bform   if-conditional]] mexpr continuation]
    (cond
      (and (identical? then-bform :then)
           (identical? if-bform   :if))
        `(if ~if-conditional
          ~(reduce continuation
                   mexpr
                   (prepare-monadic-steps then-mexpr))
          ~(reduce continuation
                   mexpr
                   (prepare-monadic-steps else-mexpr)))
      :else
       (throw (Exception. "invalid :if without :then and :else"))))

(defn- merge-cond-branches [cond-branches]
  (let [merger (fn [result cond-branch]
                  (-> result
                      (conj (first cond-branch))
                      (conj (second cond-branch))))]
    (reduce merger [] cond-branches)))

(defn cond-statement
  "Process a :cond steps when adding a new monadic step to the mexrp."
  [expr mexpr continuation]
  (let [cond-sexps (partition 2 expr)
        result (for [[cond-sexp monadic-sexp] cond-sexps]
                     (list cond-sexp
                           (reduce continuation
                                   mexpr
                                   (prepare-monadic-steps monadic-sexp))))]
      `(cond ~@(merge-cond-branches result))))

(defn- add-monad-step
  "Add a monad comprehension step before the already transformed
   monad comprehension expression mexpr."
  [mexpr steps]
  (let [[[bform expr :as step] & _] steps]
    (cond
      (identical? bform :when)  `(if ~expr ~mexpr ~'m-zero)
      (identical? bform :let)   `(let ~expr ~mexpr)
      (identical? bform :cond)  (cond-statement expr mexpr add-monad-step)
      (identical? bform :then)  mexpr
      ; ^ ignore :then step (processed on the :else step)
      (identical? bform :if)    mexpr
      ; ^ ignore :if step (processed on the :else step)
      (identical? bform :else)
        (if-then-else-statement steps mexpr add-monad-step)
      :else
        (list 'm-bind expr 
              (list 'typed.core/fn> 
                    [[bform :- (do #_(assert (contains? :T (meta bform)))
                                 (-> bform meta :T))]] 
                    mexpr)))))

(defn- monad-expr
  "Transforms a monad comprehension, consisting of a list of steps
   and an expression defining the final value, into an expression
   chaining together the steps using :bind and returning the final value
   using :result. The steps are given as a vector of
   binding-variable/monadic-expression pairs."
  [steps expr]
  (when (odd? (count steps))
    (throw (Exception. "Odd number of elements in monad comprehension steps")))

  (let [rsteps  (prepare-monadic-steps steps)
        [[lr ls] & _] (first rsteps)]
    (if (= lr expr)
      ; Optimization: if the result expression is equal to the result
      ; of the last computation step, we can eliminate an m-bind to
      ; m-result.
      (reduce add-monad-step
        ls
        (rest rsteps))
      ; The general case.
      (reduce add-monad-step
        (list 'm-result expr)
        rsteps))))

(defmacro with-monad
  "Evaluates an expression after replacing the keywords defining the
   monad operations by the functions associated with these keywords
   in the monad definition given by name."
  [monad & exprs]
  `(let [name#      ~monad
         ~'m-bind   (:m-bind name#)
         ~'m-result (:m-result name#)
         ~'m-zero   (:m-zero name#)
         ~'m-plus   (:m-plus name#)]
     (with-symbol-macros ~@exprs)))

(defmacro domonad
  "Monad comprehension. Takes the name of a monad, a vector of steps
   given as binding-form/monadic-expression pairs, and a result value
   specified by expr. The monadic-expression terms can use the binding
   variables of the previous steps.
   If the monad contains a definition of m-zero, the step list can also
   contain conditions of the form :when p, where the predicate p can
   contain the binding variables from all previous steps.
   A clause of the form :let [binding-form expr ...], where the bindings
   are given as a vector as for the use in let, establishes additional
   bindings that can be used in the following steps."
  ([steps expr]
    (monad-expr steps expr))
  ([name steps expr]
    (let [mexpr (monad-expr steps expr)]
      `(with-monad ~name ~mexpr))))

)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Defining functions used with monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defmonadfn
  "Like defn, but for functions that use monad operations and are used inside
   a with-monad block."
  {:arglists '([name docstring? attr-map? args expr]
               [name docstring? attr-map? (args expr) ...])}
  [name & options]
  (let [[name options]  (name-with-attributes name options)
        fn-name (symbol (str *ns*) (format "m+%s+m" (str name)))]
      (let [body options]
        `(do
           (defsymbolmacro ~name ((inst ~fn-name ~'m) ~'m-bind ~'m-result ~'m-zero ~'m-plus))
           (def ~fn-name 
             (fn [~'m-bind ~'m-result
                  ~'m-zero ~'m-plus]
               (with-symbol-macros 
                 (fn ~@body))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commonly used monad functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define the four basic monad operations as symbol macros that
; expand to their unqualified symbol equivalents. This makes it possible
; to use them inside macro templates without having to quote them.
(tc-ignore
(defsymbolmacro m-result m-result)
(defsymbolmacro m-bind m-bind)
(defsymbolmacro m-zero m-zero)
(defsymbolmacro m-plus m-plus)
  )

(defmacro m-lift
  "Converts a function f of n arguments into a function of n
  monadic arguments returning a monadic value.
  Takes an optional second parameter arg-tys which is a vector of
  static types to assign to each parameter."
  ([n arg-tys f]
   (let [expr (map #(with-meta %1 {:T %2})
                   (take n (repeatedly #(gensym "x_")))
                   arg-tys)
         arg-names (take n (repeatedly #(gensym "mv_")))
         vars (vec (map #(vector %1 :- (list 'm %2)) arg-names arg-tys))
         steps (vec (interleave expr arg-names))]
     (list `fn> vars (monad-expr steps (cons f expr)))))
  ([n f]
   (let [expr (take n (repeatedly #(gensym "x_")))
         vars (vec (take n (repeatedly #(gensym "mv_"))))
         steps (vec (interleave expr vars))]
     (list `fn vars (monad-expr steps (cons f expr))))))

(defmacro ann-monadfn [name ty]
  (let [fn-name (symbol (format "m+%s+m" (str name)))]
    `(do
       (ann ~name ~'Any)
       (ann ~fn-name ~`(~'All ~'[[m :kind (TFn [[x :variance :covariant]] Any)]]
                           [~'(All [x y]
                                   [(m x) [x -> (m y)] -> (m y)])
                            ~'(All [x]
                                   [x -> (m x)])
                            ~'(m Nothing)
                            ~'(All [x]
                                   [(m x) * -> (m x)])
                            ~'-> ~ty])))))

(ann-monadfn m-join
             (All [x]
               [(m (m x)) -> (m x)]))
(defmonadfn m-join
  "Converts a monadic value containing a monadic value into a 'simple'
   monadic value."
  [m]
  (m-bind m (inst identity (m x))))

(ann-monadfn m-fmap
             (All [x y]
               [[x -> y] (m x) -> (m y)]))
(defmonadfn m-fmap
  "Bind the monadic value m to the function returning (f x) for argument x"
  [f m]
  (m-bind m (ann-form (fn [x] (m-result (f x)))
                      [x -> (m y)])))

(ann-monadfn m-seq 
             (All [x]
               [(Seqable (m x)) -> (m (Seqable x))]))
(defmonadfn m-seq
  "'Executes' the monadic values in ms and returns a sequence of the
  basic values contained in them."
  [ms]
  ((inst reduce (m (Seqable x)) (m x))
     (ann-form
       (fn [q p]
         (m-bind p (ann-form
                     (fn [x]
                       (m-bind q (ann-form 
                                   (fn [y]
                                     (m-result (cons x y)))
                                   [(Seqable x) -> (m (Seqable x))])))
                     [x -> (m (Seqable x))])))
       [(m (Seqable x)) (m x) -> (m (Seqable x))])
     (m-result '())
     (reverse ms)))

(ann-monadfn m-map (All [x y]
                     [[x -> (m y)] (Seqable x) -> (m (Seqable y))]))
(defmonadfn m-map
  "'Executes' the sequence of monadic values resulting from mapping
   f onto the values xs. f must return a monadic value."
  [f xs]
  (m-seq (map f xs)))

(ann-monadfn m-chain (All [x]
                       [(Seqable [x -> (m x)]) -> (m x)]))
;TODO
(tc-ignore
(defmonadfn m-chain
  "Chains together monadic computation steps that are each functions
   of one parameter. Each step is called with the result of the previous
   step as its argument. (m-chain (step1 step2)) is equivalent to
   (fn [x] (domonad [r1 (step1 x) r2 (step2 r1)] r2))."
  [steps]
  ((inst reduce [x -> (m x)] [x -> (m x)])
    (ann-form
      (fn m-chain-link [chain-expr step]
        (fn [v] (m-bind (chain-expr v) step)))
      [[x -> (m x)] [x -> (m x)] -> [x -> (m x)]])
    m-result
    steps))
  )

(ann-monadfn m-reduce
             (All [x y]
               (Fn 
                 #_[[x y -> x] (I (Seqable (m y)) (CountRange 1)) -> (m x)]
                 [(Fn [x y -> x] [-> x]) (Option (Seqable (m y))) -> (m x)]
                 [[x y -> x] x (Option (Seqable (m y))) -> (m x)])))
(defmonadfn m-reduce
  "Return the reduction of (m-lift 2 f) over the list of monadic values mvs
   with initial value (m-result val)."
  ([f mvs]
   (if (empty? mvs)
     (m-result (f))
     (let [m-f (m-lift 2 [x y] f)]
       ((inst reduce (m x) (m y)) m-f mvs))))
  ([f val mvs]
   (let [m-f    (m-lift 2 [x y] f)
         m-val  (m-result val)]
     ((inst reduce (m x) (m y)) m-f m-val mvs))))

(ann-monadfn m-until
             (All [x]
               [[x -> Any] [x -> (m x)] x -> (m x)]))
(defmonadfn m-until
  "While (p x) is false, replace x by the value returned by the
   monadic computation (f x). Return (m-result x) for the first
   x for which (p x) is true."
  [p f x]
  (if (p x)
    (m-result x)
    (domonad
      [^{:T x} y (f x)
       z (m-until p f y)]
      z)))

(defmacro m-when
  "If test is logical true, return monadic value m-expr, else return
   (m-result nil)."
  [test m-expr]
  `(if ~test ~m-expr (~'m-result nil)))

(defmacro m-when-not
  "If test if logical false, return monadic value m-expr, else return
   (m-result nil)."
  [test m-expr]
  `(if ~test (~'m-result nil) ~m-expr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utility functions used in monad definitions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ann flatten* 
     (All [x]
       [(Option (Seqable (Seqable x))) -> (Seqable x)]))
(defn- flatten*
  "Like #(apply concat %), but fully lazy: it evaluates each sublist
   only when it is needed."
  [ss]
  (lazy-seq
   (when-let [s (seq ss)]
     (concat (first s) (flatten* (rest s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Commonly used monads
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-alias Undefined '::undefined)

;possibly missing zero and plus
(def-alias AnyMonad 
  (TFn [[m :kind (TFn [[x :variance :covariant]] Any)]]
    '{:m-bind (All [x y]
                [(m x) [x -> (m y)] -> (m y)])
      :m-result (All [x]
                  [x -> (m x)])
      :m-zero (U (m Nothing) Undefined)
      :m-plus (U (All [x]
                   [(m x) * -> (m x)])
                 Undefined)}))

;always missing zero and plus
(def-alias Monad 
  (TFn [[m :kind (TFn [[x :variance :covariant]] Any)]]
    '{:m-bind (All [x y]
                [(m x) [x -> (m y)] -> (m y)])
      :m-result (All [x]
                  [x -> (m x)])
      :m-zero Undefined
      :m-plus Undefined}))

;missing plus
(def-alias MonadZero
  (TFn [[m :kind (TFn [[x :variance :covariant]] Any)]]
    '{:m-bind (All [x y]
                [(m x) [x -> (m y)] -> (m y)])
      :m-result (All [x]
                  [x -> (m x)])
      :m-zero (m Nothing)
      :m-plus Undefined}))

;missing zero
(def-alias MonadPlus
  (TFn [[m :kind (TFn [[x :variance :covariant]] Any)]]
    '{:m-bind (All [x y]
                [(m x) [x -> (m y)] -> (m y)])
      :m-result (All [x]
                  [x -> (m x)])
      :m-zero Undefined
      :m-plus (All [x]
                [(m x) * -> (m x)])}))

;all four operations
(def-alias MonadPlusZero
  (TFn [[m :kind (TFn [[x :variance :covariant]] Any)]]
    '{:m-bind (All [x y]
                [(m x) [x -> (m y)] -> (m y)])
      :m-result (All [x]
                  [x -> (m x)])
      :m-zero (m Nothing)
      :m-plus (All [x]
                [(m x) * -> (m x)])}))

; Identity monad
(ann identity-m (Monad (TFn [[x :variance :covariant]] x)))
(defmonad identity-m
   "Monad describing plain computations. This monad does in fact nothing
    at all. It is useful for testing, for combination with monad
    transformers, and for code that is parameterized with a monad."
  [m-result identity
   m-bind   (ann-form
              (fn m-result-id [mv f]
                (f mv))
              (All [x y]
                [x [x -> y] -> y]))
  ])

; Maybe monad
(ann maybe-m (MonadPlusZero
               (TFn [[x :variance :covariant]] 
                 (U nil x))))
(defmonad maybe-m
  "Monad describing computations with possible failures. Failure is
  represented by nil, any other value is considered valid. As soon as
  a step returns nil, the whole computation will yield nil as well."
  [m-zero   nil
   m-result (ann-form 
              (fn m-result-maybe [v] v)
              (All [x] 
                [x -> (U nil x)]))
   m-bind   (ann-form 
              (fn m-bind-maybe [mv f]
                (if (nil? mv) nil (f mv)))
              (All [x y]
                [(U nil x) [x -> (U nil y)] -> (U nil y)]))
   m-plus   (ann-form 
              (fn m-plus-maybe [& mvs]
                (first 
                  (filter
                    (ann-form 
                      #(not (nil? %))
                      [(U nil x) -> boolean :filters {:then (is x 0)}])
                    mvs)))
              (All [x]
                [(U nil x) * -> (U nil x)]))
   ])

; Sequence monad (called "list monad" in Haskell)
(ann sequence-m (MonadPlusZero 
                  (TFn [[x :variance :covariant]]
                    (Seqable x))))
(defmonad sequence-m
   "Monad describing multi-valued computations, i.e. computations
    that can yield multiple values. Any object implementing the seq
    protocol can be used as a monadic value."
   [m-result (ann-form
               (fn m-result-sequence [v]
                 (list v))
               (All [x] [x -> (Seqable x)]))
    m-bind   (ann-form
               (fn m-bind-sequence [mv f]
                 (flatten* (map f mv)))
               (All [x y]
                 [(Seqable x) [x -> (Seqable y)] -> (Seqable y)]))
    m-zero   (list)
    m-plus   (ann-form
               (fn m-plus-sequence [& mvs]
                 (flatten* mvs))
               (All [x]
                 [(Seqable x) * -> (Seqable x)]))
    ])

(ann clojure.set/union
     (All [x]
       [(IPersistentSet x) * -> (IPersistentSet x)]))

; Set monad
(ann set-m (MonadPlusZero (TFn [[x :variance :covariant]] 
                            (IPersistentSet x))))
(defmonad set-m
   "Monad describing multi-valued computations, like sequence-m,
    but returning sets of results instead of sequences of results."
   [m-result (ann-form
               (fn m-result-set [v]
                 #{v})
               (All [x] [x -> (IPersistentSet x)]))
    m-bind   (ann-form
               (fn m-bind-set [mv f]
                 (apply clojure.set/union (map f mv)))
               (All [x y]
                 [(IPersistentSet x) [x -> (IPersistentSet y)] -> (IPersistentSet y)]))
    m-zero   #{}
    m-plus   (ann-form
               (fn m-plus-set [& mvs]
                 (apply clojure.set/union mvs))
               (All [x]
                 [(IPersistentSet x) * -> (IPersistentSet x)]))
    ])

; State monad
(def-alias State
  (TFn [[r :variance :covariant]
        [s :variance :invariant]]
    [s -> '[r s]]))
(ann state-m (All [s]
               (Monad (TFn [[x :variance :covariant]]
                        (State x s)))))
(defmonad state-m
   "Monad describing stateful computations. The monadic values have the
    structure (fn [old-state] [result new-state])."
   [m-result  (ann-form
                (fn m-result-state [v]
                  (fn [s] [v s]))
                (All [r s]
                  [r -> (State r s)]))
    m-bind    (ann-form
                (fn m-bind-state [mv f]
                  (fn [s]
                    (let [[v ss] (mv s)]
                      ((f v) ss))))
                (All [s ra rb]
                  [(State ra s) [ra -> (State rb s)] -> (State rb s)]))
   ])

(ann update-state 
     (All [s]
       [[s -> s] -> (State s s)]))
(defn update-state
  "Return a state-monad function that replaces the current state by the
   result of f applied to the current state and that returns the old state."
  [f]
  (fn [s] [s (f s)]))

(ann set-state 
     (All [s]
       [s -> (State s s)]))
(defn set-state
  "Return a state-monad function that replaces the current state by s and
   returns the previous state."
  [s]
  (update-state (ann-form 
                  (fn [_] s)
                  [s -> s])))

(ann fetch-state
     (All [s]
       [-> (State s s)]))
(defn fetch-state
  "Return a state-monad function that returns the current state and does not
   modify it."
  []
  (update-state identity))

(ann fetch-val 
     (All [x y]
       [Any -> (State (U y nil) (IPersistentMap x y))]))
(defn fetch-val
  "Return a state-monad function that assumes the state to be a map and
   returns the value corresponding to the given key. The state is not modified."
  [key]
  (domonad (inst state-m (IPersistentMap x y))
    [^{:T (IPersistentMap x y)} s ((inst fetch-state (IPersistentMap x y)))]
    (get s key)))

(ann update-val
     (All [x y]
       [x [Any -> y] -> (State (U nil y) (IPersistentMap x y))]))
(defn update-val
  "Return a state-monad function that assumes the state to be a map and
   replaces the value associated with the given key by the return value
   of f applied to the old value. The old value is returned."
  [key f]
  (fn [s]
    (let [old-val (get s key)
          new-s   (assoc s key (f old-val))]
      [old-val new-s])))

(ann set-val
     (All [x y]
       [x y -> (State (U nil y) (IPersistentMap x y))]))
(defn set-val
  "Return a state-monad function that assumes the state to be a map and
   replaces the value associated with key by val. The old value is returned."
  [key val]
  ((inst update-val x y) key (fn [_] val)))

(ann with-state-field
     (All [x y r]
       [x [Any -> '[r y]] -> (State r (IPersistentMap x y))]))
(defn with-state-field
  "Returns a state-monad function that expects a map as its state and
   runs statement (another state-monad function) on the state defined by
   the map entry corresponding to key. The map entry is updated with the
   new state returned by statement."
  [key statement]
  (fn [s]
    (let [substate (get s key)
          [result new-substate] (statement substate)
          new-state (assoc s key new-substate)]
      [result new-state])))

;TODO letfn NYI
(tc-ignore
(defn state-m-until
  "An optimized implementation of m-until for the state monad that
   replaces recursion by a loop."
  [p f x]
  (letfn [(until [p f x s]
            (if (p x)
              [x s]
              (let [[x s] ((f x) s)]
                (recur p f x s))))]
    (fn [s] (until p f x s))))
  )
               
;TODO protocol
(tc-ignore
; Writer monad
(defprotocol writer-monad-protocol
  "Accumulation of values into containers"
  (writer-m-add [container value]
  "add value to container, return new container")
  (writer-m-combine [container1 container2]
  "combine two containers, return new container"))

(extend-protocol writer-monad-protocol

  clojure.lang.IPersistentVector
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (vec (concat c1 c2)))

  clojure.lang.IPersistentList
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (concat c1 c2))

  clojure.lang.APersistentSet
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (clojure.set/union c1 c2))

  java.lang.String
  (writer-m-add [c v] (str c v))
  (writer-m-combine [c1 c2] (str c1 c2)))
)

;TODO types for accumulators
(ann writer-m
     (Monad (TFn [[x :variance :covariant]]
              '[x Any])))
(defn writer-m
  "Monad describing computations that accumulate data on the side, e.g. for
   logging. The monadic values have the structure [value log]. Any of the
   accumulators from clojure.contrib.accumulators can be used for storing the
   log data. Its empty value is passed as a parameter."
  [empty-accumulator]
  (monad
   [m-result  (fn m-result-writer [v]
                [v empty-accumulator])
    m-bind    (fn m-bind-writer [mv f]
                (let [[v1 a1] mv
                      [v2 a2] (f v1)]
                  [v2 (writer-m-combine a1 a2)]))
    ]))

;TODO
(tc-ignore
  (defmonadfn write [v]
    (let [[_ a] (m-result nil)]
      [nil (writer-m-add a v)]))

  (defn listen [mv]
    (let [[v a] mv] [[v a] a]))

  (defn censor [f mv]
    (let [[v a] mv] [v (f a)]))
  )

(def-alias Cont1
  (TFn [[a :variance :contravariant]
        [r :variance :covariant]]
    [a -> r]))

; Continuation monad
(def-alias ContM
  (TFn [[a :variance :covariant]
        [r :variance :invariant]]
    (Cont1 (Cont1 a r) r)))
(ann cont-m (All [r]
              (Monad (TFn [[x :variance :covariant]]
                       (ContM x r)))))
(defmonad cont-m
  "Monad describing computations in continuation-passing style. The monadic
   values are functions that are called with a single argument representing
   the continuation of the computation, to which they pass their result."
  [m-result   (->
                (fn m-result-cont [v]
                  (fn [c] (c v)))
                (ann-form (All [x]
                            [x -> (ContM x r)])))
   m-bind     (->
                (fn m-bind-cont [mv f]
                  (fn [c]
                    (mv (->
                          (fn [v] ((f v) c))
                          (ann-form [x -> r])))))
                (ann-form (All [x y]
                            [(ContM x r) [x -> (ContM y r)] -> (ContM y r)])))
   ])

(ann run-cont
     (All [x]
       [(ContM x x) -> x]))
(defn run-cont
  "Execute the computation c in the cont monad and return its result."
  [c]
  (c identity))

(ann call-cc
     (All [x r]
       [[[x -> (ContM x r)] -> (ContM x r)] -> (ContM x r)]))
(defn call-cc
  "A computation in the cont monad that calls function f with a single
   argument representing the current continuation (as a direct function). 
   The function f should
   return a continuation (which becomes the return value of call-cc),
   or call the passed-in current continuation to terminate."
  [f]
  (fn [c]
    (let [cc (->
               (fn [a]
                 (fn [_]
                   (c a)))
               (ann-form [x -> (ContM x r)]))
          rc (f cc)]
      (rc c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Monad transformers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro monad-transformer
  "Define a monad transforer in terms of the monad operations and the base
   monad. The argument which-m-plus chooses if m-zero and m-plus are taken
   from the base monad or from the transformer."
  [base which-m-plus operations]
  `(let [which-m-plus# (cond (= ~which-m-plus :m-plus-default)
                               (if (= ::undefined (with-monad ~base ~'m-plus))
                                 :m-plus-from-transformer
                                 :m-plus-from-base)
                             (or (= ~which-m-plus :m-plus-from-base)
                                 (= ~which-m-plus :m-plus-from-transformer))
                               ~which-m-plus
                             :else
                               (throw (java.lang.IllegalArgumentException.
                                       "undefined m-plus choice")))
         combined-monad# (monad ~operations)]
    (if (= which-m-plus# :m-plus-from-base)
      (assoc combined-monad#
        :m-zero (with-monad ~base ~'m-zero)
        :m-plus (with-monad ~base ~'m-plus))
      combined-monad#)))

(ann maybe-t
     (All [[m :kind (TFn [[x :variance :covariant]] Any)]]
       (Fn 
         [(AnyMonad m) -> (MonadPlusZero (TFn [[y :variance :covariant]]
                                              (m (U nil y))))]
         [(AnyMonad m) nil -> (MonadPlusZero (TFn [[y :variance :covariant]]
                                                  (m (U nil y))))]
         [(AnyMonad m) nil (U ':m-plus-default ':m-plus-from-base)
          -> (MonadPlusZero (TFn [[y :variance :covariant]]
                                 (m (U nil y))))])))
(defn maybe-t
  "Monad transformer that transforms a monad m into a monad in which
   the base values can be invalid (represented by nothing, which defaults
   to nil). The third argument chooses if m-zero and m-plus are inherited
   from the base monad (use :m-plus-from-base) or adopt maybe-like
   behaviour (use :m-plus-from-transformer). The default is :m-plus-from-base
   if the base monad m has a definition for m-plus, and
   :m-plus-from-transformer otherwise."
  ([m] ((inst maybe-t m) m nil :m-plus-default))
  ([m nothing] ((inst maybe-t m) m nothing :m-plus-default))
  ([m nothing which-m-plus]
   (monad-transformer m which-m-plus
     [m-result (with-monad m m-result)
      m-bind   (with-monad m
                 (ann-form
                   (fn m-bind-maybe-t [mv f]
                     (m-bind
                       mv
                       (ann-form
                         (fn [x]
                           (if (nil? x)
                             (m-result nothing) 
                             (f x)))
                         [(U nil a) -> (m (U nil b))])))
                   (All [a b]
                     [(m (U nil a)) [a -> (m (U nil b))] -> (m (U nil b))])))
      m-zero   (with-monad m (m-result nothing))
      m-plus   (with-monad m
                 (ann-form
                   (fn m-plus-maybe-t [& mvs]
                     (if (empty? mvs)
                       (m-result nothing)
                       (m-bind (first mvs)
                               (ann-form
                                 (fn [v]
                                   (if (= v nothing)
                                     (apply m-plus-maybe-t (rest mvs))
                                     (m-result v)))
                                 [(U x nil) -> (m (U x nil))]))))
                   (All [x] [(m (U x nil)) * -> (m (U x nil))])))
      ])))

(comment
(ann seq-maybe-m (Monad 
                   (TFn [[x :variance :covariant]]
                     (Seqable (U nil x)))))
(def seq-maybe-m ((inst maybe-t 
                        (TFn [[x :variance :covariant]] 
                          (Seqable x)))
                    sequence-m nil :m-plus-default))

(domonad seq-maybe-m
  [^{:T AnyInteger} x  (map (fn> [[n :- AnyInteger]] (when (odd? n) n)) (range 10))]
  (inc x))
  )

(ann sequence-t
     (All [[m :kind (TFn [[x :variance :covariant]] Any)]]
       (Fn 
         [(MonadPlusZero m) -> (MonadPlusZero (TFn [[x :variance :covariant]]
                                                (m (Seqable x))))]
         [(MonadPlusZero m) (U ':m-plus-default ':m-plus-from-base)
          -> (MonadPlusZero (TFn [[x :variance :covariant]]
                              (m (Seqable x))))])))
(defn sequence-t
  "Monad transformer that transforms a monad m into a monad in which
   the base values are sequences. The argument which-m-plus chooses
   if m-zero and m-plus are inherited from the base monad
   (use :m-plus-from-base) or adopt sequence-like
   behaviour (use :m-plus-from-transformer). The default is :m-plus-from-base
   if the base monad m has a definition for m-plus, and
   :m-plus-from-transformer otherwise."
  ([m] ((inst sequence-t m) m :m-plus-default))
  ([m which-m-plus]
   (monad-transformer m which-m-plus
     [m-result (with-monad m
                 (ann-form
                   (fn m-result-sequence-t [v]
                     (m-result (list v)))
                   (All [x]
                     [x -> (m (Seqable x))])))
      m-bind   (with-monad m
                 (ann-form
                   (fn m-bind-sequence-t [mv f]
                     (m-bind mv
                             (ann-form 
                               (fn [xs]
                                 (m-fmap (inst flatten* y)
                                         (m-map f xs)))
                               [(Seqable x) -> (m (Seqable y))])))
                   (All [x y]
                     [(m (Seqable x)) [x -> (m (Seqable y))] -> (m (Seqable y))])))
      m-zero   (with-monad m (m-result (list)))
      m-plus   (with-monad m
                 (ann-form
                   (fn m-plus-sequence-t [& mvs]
                     ((inst m-reduce (Seqable x) (Seqable x)) 
                        concat (list) mvs))
                   (All [x]
                     [(m (Seqable x)) * -> (m (Seqable x))])))
      ])))

(ann state-t
     (All [[m :kind (TFn [[x :variance :covariant]] Any)]]
       (Fn 
         [(MonadPlusZero m) -> (All [s]
                                 (MonadPlusZero (TFn [[x :variance :covariant]]
                                                  [s -> (m '[x s])])))])))
;; Contributed by Jim Duey
(defn state-t
  "Monad transformer that transforms a monad m into a monad of stateful
  computations that have the base monad type as their result."
  [m]
  (ann-form ;scopes s in body
    (monad [m-result (with-monad m
                       (ann-form
                         (fn m-result-state-t [v]
                           (ann-form
                             (fn [s]
                               (m-result [v s]))
                             [s -> (m '[x s])]))
                         (All [x]
                              [x -> [s -> (m '[x s])]])))
            m-bind   (with-monad m
                       (ann-form
                         (fn m-bind-state-t [stm f]
                           (fn [s]
                             (m-bind (stm s)
                                     (ann-form
                                       (fn [[v ss]]
                                         ((f v) ss))
                                       ['[x s] -> (m '[y s])]))))
                         (All [x y]
                              [[s -> (m '[x s])] [x -> [s -> (m '[y s])]] -> [s -> (m '[y s])]])))
            m-zero   (with-monad m
                       (if (= ::undefined m-zero)
                         ::undefined
                         (ann-form
                           (fn [s]
                             m-zero)
                           [s -> (m Nothing)])))
            m-plus   (with-monad m
                       (ann-form
                         (if (= ::undefined m-plus)
                           ::undefined
                           (ann-form
                             (fn [& stms]
                               (fn [s]
                                 (apply m-plus (map (ann-form #(% s)
                                                      [[s -> (m '[x s])] -> (m '[x s])])
                                                    stms))))
                             (All [x]
                                  [[s -> (m '[x s])] * -> [s -> (m '[x s])]])))
                         (All [x]
                              [[s -> (m '[x s])] * -> [s -> (m '[x s])]]))) 
            ])
    (All [s]
      (MonadPlusZero (TFn [[x :variance :covariant]]
                       [s -> (m '[x s])])))))

(domonad maybe-m
         [^{:T AnyInteger} a 5
          :let [c 7]
          :if (and (= a 5) (= c 7))
          :then [^{:T AnyInteger} b 6]
          :else [^{:T nil} b nil]]
         [a b])

(tc-ignore

;;; Examples
(domonad sequence-m
  [^{:T AnyInteger} x  (range 5)
   ^{:T AnyInteger} y  (range (+ 1 x))
   :when (= (+ x y) 2)]
  (list x y))
  )















; Next, a function that illustrates how a captured continuation can be
; used as an "emergency exit" out of a computation:
(ann sqrt-as-str [Number -> String])
(tc-ignore
(defn sqrt-as-str [x]
  (call-cc
    (fn> [[k :- [String -> (ContM String String)]]]
      (domonad (inst cont-m String)
        [_ (m-when (< x 0) 
             (k (str "negative argument " x)))]
        (str (. Math sqrt x))))))
  )

;(run-cont ((inst sqrt-as-str String) 2))
;(run-cont (sqrt-as-str -2))

;(domonad maybe-m
;  [a 5
;   :let [c 7]
;   :if (and (= a 5) (= c 7))
;   :then [b 6]
;   :else [b nil]]
;  [a b])

#_(tc-ignore
(cf
(domonad maybe-m
  [^{:T Long} a 5
   ^{:T Long} d 5
   :let [c 7]
   :if (and (= a 5) (= c 7))
   :then [^{:T Long} b 6]
   :else [^{:T Long} b nil]]
  [(+ a b) b d])
  )


(cf (domonad maybe-m
      [^{:T AnyInteger} a 5]
      ;:let [c 7]]
      [a]))

  )

(ann afun (All [x y] [(U x nil) [x -> (U nil y)] -> (U nil y)]))
(ann bfun [Symbol -> (U nil Symbol)])
(declare afun bfun)
#_(tc-ignore (cf (afun 'a bfun)))

;TODO these are test cases
;
;(infer {'x no-bounds 'y no-bounds} {} 
;       [(make-FnIntersection (make-Function [(RClass-of Symbol)] (Un -nil (RClass-of Symbol))))]
;       [(make-FnIntersection (make-Function [(make-F 'x)] (Un -nil (make-F 'y))))]
;       (Un -nil (make-F 'y)))
;
;(infer {'x no-bounds 'y no-bounds} {} 
;       [(parse-type ''5) 
;        (parse-type '[(U Long java.math.BigInteger) -> (U nil (U Long java.math.BigInteger))])]
;       [(Un -nil (make-F 'x))
;        (make-FnIntersection (make-Function [(make-F 'x)] (Un -nil (make-F 'y))))]
;       (Un -nil (make-F 'y)))

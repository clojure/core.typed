(ns clojure.core.typed.test.protocol-monads
  (:refer-clojure :exclude [do seq map])
  (:require [clojure.set :as set]
            [clojure.core.typed :as t]))

#_(t/ann-protocol [[m :variance :covariant]]
                  Monad 
                 :methods
                 {bind (All [a b]
                         [(m a) [a -> (m b)] -> (m b)])
                  do-result (All [a]
                              [(m a) a -> (m a)])})

(defprotocol Monad
  (do-result [_ v])
  (bind [mv f]))

(defprotocol MonadZero
  (zero [_])
  (plus-step [mv mvs]))

(defn plus [[mv & mvs]]
  (plus-step mv mvs))

(defmacro do
  "Monad comprehension. Takes the name of a monad (like vector, hash-set),
   a vector of steps given as binding-form/monadic-expression pairs, and
   a result value specified by expr. The monadic-expression terms can use
   the binding variables of the previous steps.
   If the monad contains a definition of m-zero, the step list can also
   contain conditions of the form :when p, where the predicate p can
   contain the binding variables from all previous steps.
   A clause of the form :let [binding-form expr ...], where the bindings
   are given as a vector as for the use in let, establishes additional
   bindings that can be used in the following steps."
  [result bindings expr]
  (let [steps (partition 2 bindings)]
    `(monads.core/bind (~result nil)
                       (fn [_#]
                         ~(reduce (fn [expr [sym mv]]
                                    (cond
                                     (= :when sym) `(if ~mv
                                                      ~expr
                                                      (monads.core/zero (~result nil)))
                                     (= :let sym) `(let ~mv
                                                     ~expr)
                                     :else `(monads.core/bind ~mv (fn [~sym]
                                                                     ~expr))))
                                  `(monads.core/do-result (~result nil) ~expr)
                                  (reverse steps))))))

(defn- comprehend [f mvs]
  (let [mv (first mvs)
        rest-steps (reduce (fn [steps mv]
                             (fn [acc x]
                               (bind mv (partial steps (conj acc x)))))
                           (fn [acc x]
                             (do-result mv (f (conj acc x))))
                           (reverse (rest mvs)))]
    (bind mv (partial rest-steps []))))

(defn seq
  "'Executes' the monadic values in 'mvs' and returns a sequence of the
   basic values contained in them."
  ([mvs]
     (assert (clojure.core/seq mvs)
             "At least one monadic value is required by monads.core/seq")
     (seq (first mvs) mvs))
  ([m-result mvs]
     (if (clojure.core/seq mvs)
       (comprehend identity mvs)
       (m-result []))))

(defn lift
  "Converts of function f to a function of monadic arguments
   returning a monadic value."
  [f]
  (fn [& mvs]
    (comprehend (partial apply f) mvs)))

(defn join
  "Converts a monadic value containing a monadic value into a 'simple'
   monadic value."
  [mv]
  (bind mv identity))

(defn fmap
  "Bind the monadic value mv to the function f. Returning (f x) for argument x"
  [f mv]
  (bind mv (fn [x] (do-result mv (f x)))))

(defn map
  "'Executes' the sequence of monadic values resulting from mapping
   f onto the values xs. f must return a monadic value."
  [f xs]
  (seq (clojure.core/map f xs)))

(defn chain
  "Chains together monadic computation steps that are each functions
   of one parameter. Each step is called with the result of the previous
   step as its argument. (m-chain (step1 step2)) is equivalent to
   (fn [x] (domonad [r1 (step1 x) r2 (step2 r1)] r2))."
  [steps]
  (fn [x]
    (let [mv ((first steps) x)
          chain (reduce (fn [chain step]
                          (fn [x]
                            (bind (step x) chain)))
                        (partial do-result mv)
                        (reverse (rest steps)))]
      (bind mv chain))))

;; for the writer monad
(defprotocol MonadWriter
  "Accumulation of values into containers"
  (writer-m-empty [_]
    "return an empty container")
  (writer-m-add [container value]
    "add value to container, return new container")
  (writer-m-combine [container1 container2]
    "combine two containers, return new container"))

(extend-type clojure.lang.PersistentList
  Monad
  (do-result [_ v]
    (list v))
  (bind [mv f]
    (mapcat f mv))

  MonadZero
  (zero [_]
    (list))
  (plus-step [mv mvs]
    (apply concat mv mvs))

  MonadWriter
  (writer-m-empty [_] (list))
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (concat c1 c2)))

;; Monads describing multi-valued computations, i.e. computations
;; that can yield multiple values. 
(extend-type clojure.lang.PersistentList$EmptyList
  Monad
  (do-result [_ v]
    (list v))
  (bind [mv f]
    (mapcat f mv))

  MonadZero
  (zero [_]
    (list))
  (plus-step [mv mvs]
    (apply concat mv mvs))

  MonadWriter
  (writer-m-empty [_] (list))
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (concat c1 c2)))

(extend-type clojure.lang.PersistentVector
  Monad
  (do-result [_ v]
    [v])
  (bind [mv f]
    (vec (mapcat f mv)))

  MonadZero
  (zero [_]
    [])
  (plus-step [mv mvs]
    (vec (apply concat mv mvs)))

  MonadWriter
  (writer-m-empty [_] [])
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (vec (concat c1 c2))))

(defn- lazy-concat
  ([l] l)
  ([l ls]
     (lazy-seq
      (cond
       (clojure.core/seq l) (cons (first l)
                                  (lazy-concat (rest l) ls))
       (clojure.core/seq ls) (lazy-concat (first l) (rest ls))
       :else (list)))))

(extend-type clojure.lang.LazySeq
  Monad
  (do-result [_ v]
    (list v))
  (bind [mv f]
    (mapcat f mv))

  MonadZero
  (zero [_]
    [])
  (plus-step [mv mvs]
    (lazy-concat mv mvs))

  MonadWriter
  (writer-m-empty [_] (list))
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (concat c1 c2)))

;; Monad describing multi-valued computations, like the sequence monads,
;; but returning sets of results instead of sequences of results.
(extend-type clojure.lang.PersistentHashSet
  Monad
  (do-result [_ v]
    (hash-set v))
  (bind [mv f]
    (apply set/union
           (clojure.core/map f mv)))

  MonadZero
  (zero [_]
    #{})
  (plus-step [mv mvs]
    (apply set/union mv mvs))

  MonadWriter
  (writer-m-empty [_] #{})
  (writer-m-add [c v] (conj c v))
  (writer-m-combine [c1 c2] (clojure.set/union c1 c2)))


(declare maybe-zero-val)

(deftype maybe-monad [v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (maybe-monad. v))
  (bind [mv f]
    (if (= mv maybe-zero-val)
      maybe-zero-val
      (f @mv)))

  MonadZero
  (zero [_]
    maybe-zero-val)
  (plus-step [mv mvs]
    (let [mv (->> (cons mv mvs)
                  (drop-while #(= maybe-zero-val %))
                  first)]
      (if (nil? mv)
        maybe-zero-val
        mv))))

(def maybe-zero-val (maybe-monad. ::nothing))

(defn maybe
  "Monad describing computations with possible failures. Failure is
   represented by nil, any other value is considered valid. As soon as
   a step returns nil, the whole computation will yield nil as well."
  [v]
  (maybe-monad. v))


(deftype state-monad [v mv f]
  clojure.lang.IFn
  (invoke [_ s]
    (if f
      (let [[v ss] (mv s)]
        ((f v) ss))
      [v s]))

  Monad
  (do-result [_ v]
    (state-monad. v nil nil))
  (bind [mv f]
    (state-monad. nil mv f)))

(defn state
  "Monad describing stateful computations. The monadic values have the
   structure (fn [old-state] [result new-state])."
  [v]
  (state-monad. v nil nil))

(defn update-state
  "Return a state-monad value that replaces the current state by the
   result of f applied to the current state and that returns the old state."
  [f]
  (reify
    clojure.lang.IFn
    (invoke [_ s]
      [s (f s)])

    Monad
    (do-result [_ v]
      (state-monad. v nil nil))
    (bind [mv f]
      (state-monad. nil mv f))))

(defn set-state
  "Return a state-monad value that replaces the current state by s and
   returns the previous state."
  [s]
  (update-state (constantly s)))

(defn get-state
  "Return a state-monad value that returns the current state and does not
   modify it."
  []
  (update-state identity))

(defn get-val
  "Return a state-monad value that assumes the state to be a map and
   returns the value corresponding to the given key. The state is not modified."
  [key]
  (bind (get-state)
        #(state (get % key))))

(defn update-val
  "Return a state-monad value that assumes the state to be a map and
   replaces the value associated with the given key by the return value
   of f applied to the old value and args. The old value is returned."
  [key f & args]
  (bind (update-state #(apply update-in % [key] f args))
        #(state (get % key))))

(defn set-val
  "Return a state-monad value that assumes the state to be a map and
   replaces the value associated with key by val. The old value is returned."
  [key val]
  (update-val key (constantly val)))

(defn get-in-val [path & [default]]
  (bind (get-state)
        #(state (get-in % path default))))

(defn assoc-in-val [path val]
  (bind (update-state #(assoc-in % path val))
        #(state (get-in % path))))

(defn update-in-val [path f & args]
  (bind (update-state #(apply update-in % path f args))
        #(state (get-in % path))))


(deftype cont-monad [v mv f]
  clojure.lang.IDeref
  (deref [mv]
    (mv identity))

  clojure.lang.IFn
  (invoke [_ c]
    (if f
      (mv (fn [v] ((f v) c)))
      (c v)))

  Monad
  (do-result [_ v]
    (cont-monad. v nil nil))
  (bind [mv f]
    (cont-monad. nil mv f)))

(defn cont
  "Monad describing computations in continuation-passing style. The monadic
   values are functions that are called with a single argument representing
   the continuation of the computation, to which they pass their result."
  [v]
  (cont-monad. v nil nil))

;; holding off on implementing this until later
(defn call-cc
  "A computation in the cont monad that calls function f with a single
   argument representing the current continuation. The function f should
   return a continuation (which becomes the return value of call-cc),
   or call the passed-in current continuation to terminate."
  [f]
  )


(extend-type java.lang.String
  MonadWriter
  (writer-m-empty [_] "")
  (writer-m-add [c v] (str c v))
  (writer-m-combine [c1 c2] (str c1 c2)))

(deftype writer-monad [v accumulator]
  clojure.lang.IDeref
  (deref [_]
    [v accumulator])

  Monad
  (do-result [_ v]
    (writer-monad. v (writer-m-empty accumulator)))
  (bind [mv f]
    (let [[v1 a1] (deref mv)
          [v2 a2] (deref (f v1))]
      (writer-monad. v2 (writer-m-combine a1 a2)))))

(defn writer
  "Monad describing computations that accumulate data on the side, e.g. for
   logging. The monadic values have the structure [value log]. Any of the
   accumulators from clojure.contrib.accumulators can be used for storing the
   log data. Its empty value is passed as a parameter."
  [accumulator]
  (fn [v]
    (writer-monad. v accumulator)))

(defn write [m-result val-to-write]
  (let [[_ a] (deref (m-result nil))]
    (writer-monad. nil (writer-m-add a val-to-write))))

(defn listen [mv]
  (let [[v a :as va] (deref mv)]
    (writer-monad. va a)))

(defn censor [f mv]
  (let [[v a] (deref mv)]
    (writer-monad. v (f a))))


(deftype state-transformer [m v mv f alts]
  clojure.lang.IFn
  (invoke [_ s]
    (cond
     alts (plus (clojure.core/map #(% s) alts))
     f (bind (mv s)
             (fn [[v ss]]
               ((f v) ss)))
     :else (if (= v (zero (m nil)))
             v
             (m [v s]))))

  Monad
  (do-result [_ v]
    (state-transformer. m v nil nil nil))
  (bind [mv f]
    (state-transformer. m nil mv f nil))

  MonadZero
  (zero [_]
    (state-transformer. m nil
                        (fn [s] (zero (m nil)))
                        (fn [v]
                          (state-transformer. m v nil nil nil))
                        nil))
  (plus-step [mv mvs]
    (state-transformer. m nil nil nil (cons mv mvs))))

(defn state-t
  "Monad transformer that transforms a monad m into a monad of stateful
  computations that have the base monad type as their result."
  [m]
  (fn [v]
    (state-transformer. m v nil nil nil)))


(deftype maybe-transformer [m v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (maybe-transformer. m (m (maybe v))))
  (bind [mv f]
    (let [v (deref mv)]
      (maybe-transformer. m (bind v (fn [x]
                                      (if (= x maybe-zero-val)
                                        (m maybe-zero-val)
                                        (deref (f (deref x)))))))))

  MonadZero
  (zero [_]
    (maybe-transformer. m (m maybe-zero-val)))
  (plus-step [mv mvs]
    (maybe-transformer.
     m (bind (deref mv)
             (fn [x]
               (cond
                (and (= x maybe-zero-val) (empty? mvs)) (m maybe-zero-val)
                (= x maybe-zero-val) (deref (plus mvs))
                :else (m x)))))))

(defn maybe-t
  "Monad transformer that transforms a monad m into a monad in which
   the base values can be invalid (represented by :nothing)."
  [m]
  (fn [v]
    (maybe-transformer. m (m (maybe v)))))



(deftype list-transformer [m v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (list-transformer. m (m (list v))))
  (bind [mv f]
    (let [v (deref mv)]
      (list-transformer. m (bind v (fn [xs]
                                     (if (clojure.core/seq xs)
                                       (->> xs
                                            (map (comp deref f))
                                            (fmap (partial apply lazy-concat)))
                                       (m '())))))))

  MonadZero
  (zero [_]
    (list-transformer. m (m '())))
  (plus-step [mv mvs]
    (list-transformer.
     m (reduce (lift concat)
               (m '())
               (clojure.core/map deref (cons mv mvs))))))

(defn list-t
  "monad transformer that transforms a monad m into a monad in which
   the base values are lists."
  [m]
  (fn [v]
    (list-transformer. m (m (list v)))))


(deftype vector-transformer [m v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (vector-transformer. m (m (vector v))))
  (bind [mv f]
    (let [v (deref mv)]
      (vector-transformer. m (bind v (fn [xs]
                                       (if (clojure.core/seq xs)
                                         (->> xs
                                              (map (comp deref f))
                                              (fmap (partial apply lazy-concat)))
                                         (m [])))))))

  MonadZero
  (zero [_]
    (vector-transformer. m (m [])))
  (plus-step [mv mvs]
    (vector-transformer.
     m (reduce (lift (comp vec concat))
               (m [])
               (clojure.core/map deref (cons mv mvs))))))

(defn vector-t
  "monad transformer that transforms a monad m into a monad in which
   the base values are vectors."
  [m]
  (fn [v]
    (vector-transformer. m (m (vector v)))))


(deftype set-transformer [m v]
  clojure.lang.IDeref
  (deref [_]
    v)

  Monad
  (do-result [_ v]
    (set-transformer. m (m (hash-set v))))
  (bind [mv f]
    (let [v (deref mv)]
      (set-transformer. m (bind v (fn [xs]
                                    (if (clojure.core/seq xs)
                                      (->> xs
                                           (map (comp deref f))
                                           (fmap (partial apply lazy-concat)))
                                      (m #{})))))))

  MonadZero
  (zero [_]
    (set-transformer. m (m #{})))
  (plus-step [mv mvs]
    (set-transformer.
     m (reduce (lift set/union)
               (m #{})
               (clojure.core/map deref (cons mv mvs))))))

(defn set-t
  "monad transformer that transforms a monad m into a monad in which
   the base values are sets."
  [m]
  (fn [v]
    (set-transformer. m (m (hash-set v)))))


(deftype writer-transformer [m mv writer-m]
  clojure.lang.IDeref
  (deref [_]
    mv)

  Monad
  (do-result [_ v]
    (writer-transformer.
     m (m (writer-m v)) writer-m))
  (bind [mv f]
    (let [mv (deref mv)]
      (writer-transformer.
       m (bind mv (fn [v]
                    (let [[v1 a1] (deref v)]
                      (bind (deref (f v1))
                            (fn [v]
                              (let [[v2 a2] (deref v)]
                                (m (writer-monad. v2 (writer-m-combine a1 a2)))))))))
       writer-m)))

  MonadZero
  (zero [mv]
    (let [v (deref mv)]
      (writer-transformer. m (zero v) writer-m)))
  (plus-step [mv mvs]
    (writer-transformer.
     m (plus (clojure.core/map deref (cons mv mvs)))
     writer-m)))

(defn writer-t [m accumulator]
  (let [writer-m (writer accumulator)]
    (fn [v]
      (writer-transformer. m (m (writer-m v)) writer-m))))

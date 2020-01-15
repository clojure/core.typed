(ns clojure.core.typed.test.frozen-macros
  (:require 
    ; this loads the type system, must go first
    [clojure.core.typed.test.test-utils :as tu]
    [clojure.test :refer :all]
    [clojure.core.typed.analyzer.jvm :as ana]
    [clojure.core.typed.checker.jvm.analyze-clj :as ana-clj]
    [clojure.tools.analyzer.passes.jvm.emit-form :refer [emit-form]]
    [clojure.tools.analyzer.jvm :as taj]
    [clojure.core.typed :as t]
    [clojure.tools.analyzer.jvm.utils :as ju]))

(def tc-config
  {:ns-meta {:core.typed {:experimental #{:custom-expansions}}}
   :check-config {:type-check-eval :simulate}})

(defmacro tc-e [frm & opts]
  `(tu/tc-e ~frm ~@opts ~@(apply concat tc-config)))

(defmacro tc-err [frm & opts]
  `(tu/tc-err ~frm ~@opts ~@(apply concat tc-config)))

(defmacro is-tc-e [& body]
  `(is (do (tc-e ~@body)
           true)))

(defmacro is-tc-err [& body]
  `(is (tc-err ~@body)))

(defmacro chk-frm
  "Like tc-e but doesn't type check ns form"
  [frm & {:as opts}]
  `(binding [*ns* *ns*
             *file* *file*]
     (ns ~(gensym)
       ~@(some-> opts :ns-meta vector))
     (t/check-form-info '~frm
                        :check-config '~(:check-config tc-config)
                        ~@(apply concat (dissoc opts :ns-meta)))))

(deftest simulate-test
  (is (-> (chk-frm 1)
          :result
          #{1}))
  (is (-> (chk-frm [1])
          :result
          #{[1]}))
  (is (-> (chk-frm [(do nil (inc 0))])
          :result
          #{[1]}))
  (is (-> (chk-frm (do (inc 0)))
          :result
          #{1}))
  (is (-> (chk-frm (do 5 (do 6 (inc 0))))
          :result
          #{1}))
  (is (-> (chk-frm (do 1 2 [(inc 0)]))
          :result
          #{[1]}))
  (is (-> (chk-frm (do (defmacro a [b] b)
                       (a (inc 0))))
          :result
          #{1}))
  (is (-> (chk-frm (ns baz))
          :result
          nil?))
  (is (-> (chk-frm (clojure.core.typed/ann-form 1 clojure.core.typed/Int))
          :result
          #{1}))
  (is (-> (chk-frm (clojure.core.typed/ann-form (do 1) clojure.core.typed/Int))
          :result
          #{1}))
  (is (-> (chk-frm (clojure.core.typed/ann-form (do (do 1)) clojure.core.typed/Int))
          :result
          #{1}))
  (is (-> (chk-frm (clojure.core.typed/ann-form (let* [] (do 1)) clojure.core.typed/Int))
          :result
          #{1}))
  (is (-> (chk-frm (clojure.core.typed/ann-form [1] '[clojure.core.typed/Int]))
          :result
          #{[1]}))
  (is (-> (chk-frm (clojure.core.typed/tc-ignore))
          :result
          nil?))
  (is (-> (chk-frm (clojure.core.typed/tc-ignore 1))
          :result
          #{1}))
  (is (-> (chk-frm (clojure.core.typed/tc-ignore
                     (do 1)
                     (do 2)))
          :result
          #{2}))
  (is (-> (chk-frm (clojure.core.typed/tc-ignore
                     (do (defmacro a [b] b)
                         (a (inc 0)))
                     (do (defmacro c [b] b)
                         (c (inc 0)))))
          :result
          #{1}))
  )

(deftest ns-test
  (is-tc-e (ns foo) nil)
  (is-tc-err (ns foo) Symbol))

(deftest ann-form-test
  (is-tc-e (ann-form 1 Integer))
  (is-tc-e (ann-form (do (defmacro a [b] b)
                         (a (inc 0)))
                     Long))
  ;; blames ann-form form
  ;; FIXME add types in msg
  (is-tc-err (ann-form 1 Integer) nil)
  (is-tc-err (ann-form 1 nil)))

(deftest tc-ignore-test
  (is-tc-e (tc-ignore))
  (is-tc-e (tc-ignore #(/ nil nil)))
  (is-tc-e (tc-ignore #(/ nil nil) #(/ nil nil)))
  (is-tc-e (tc-ignore (do (defmacro a [b] b)
                          (a (inc 0)))))
  (is-tc-e (tc-ignore (do (defmacro a [b] b)
                          (a (inc 0)))
                      (do (defmacro c [b] b)
                          (c (inc 0)))))
  (is-tc-err (tc-ignore #(/ nil nil)) nil))

(deftest typed-fn-test
  (is-tc-e (fn [a :- (U nil Number)]))
  ;; inherits column from outer expression
  ;; FIXME use entire form if single arity
  (is-tc-err (fn [] :- Number))
  ;; exact column number
  (is-tc-err (fn ([] :- Number))))

(deftest when-test
  (is-tc-e (when 1 (inc 2)))
  (is-tc-e (fn [a :- (U nil Number)]
             (when a (inc a))))
  (is-tc-e (fn [a :- (U nil Number)]
             (when a (inc a))))
  (is-tc-e (fn [a :- Number] :- Number
             (when a (inc a))))
  ;; better error
  (is-tc-err (fn [a :- (U nil Number)] :- Number,
               (when a (inc a))))
  ;; 'else' expected error
  (is-tc-err (fn [a :- (U nil Number)] :- Number,
               (when a 1)))
  ;; 'then+else' expected error
  ; FIXME duplicated error
  (is-tc-err (fn [a :- (U nil Number)] :- Number,
               (when a))))

(deftest when-not-test
  (is-tc-e (fn [a :- (U nil Number)]
             (when-not (not a) (inc a))))
  (is-tc-e (fn [a :- (U nil Number)]
             (when-not (not a) (inc a))))
  (is-tc-e (fn [a :- Number] :- Number
             (when-not (not a) (inc a))))
  ;; better error
  (is-tc-err (fn [a :- (U nil Number)] :- Number,
               (when-not (not a) (inc a))))
  ;; 'then' expected error
  (is-tc-err (fn [a :- (U nil Number)] :- Number,
               (when-not (not a) 1)))
  ;; 'then+else' expected error
  (is-tc-err (fn [a :- (U nil Number)] :- Number,
               (when-not (not a)))))

(deftest let-test
  ;; better error
  (is-tc-err (let [a 1]) Number)
  (is-tc-e (let [a 1]
             (inc a)))
  (is-tc-e #(let [a (throw (Exception.))]
              (/ nil nil)))
  (is-tc-e #(let [a 1
                  b 2]
              (/ a b)))
  (is-tc-e #(let [a (throw (Exception.))
                  b (/ nil nil)]))
  (is-tc-err #(let [a (/ nil nil)
                    b (throw (Exception.))]
                (/ a b)))
  (is-tc-err #(let [a (/ nil nil)]
                (inc a)))
  (is-tc-err #(let [a 1]
                (/ nil nil)))
  ;destructuring
  (is-tc-e (let [{:keys [a]} {:a 1}]
             (inc a)))
  (is-tc-err (let [{:keys [a]} []]
               (inc a)))

  ;; locals shadow vars
  (is-tc-e (let [identity identity]
             (identity 1))))

(deftest when-let-test
  (is-tc-e (when-let [_ 1]
             (inc 1)))
  (is-tc-e (when-let [a 1]
             (inc a)))
  (is-tc-e (when-let [a (ann-form 1 (U nil Number))]
             (inc a)))
  (is-tc-err (when-let [a (ann-form 1 (U nil Number String))]
               (inc a)))
  (is-tc-err (when-let [a "a"]
               (inc a)))
  (is-tc-err (when-let [a (ann-form nil (U nil Number))]
               (inc a))
             Number)
  )

(deftest if-let-test
  (is-tc-e (if-let [_ 1]
             (inc 1)))
  (is-tc-e (if-let [a (ann-form 1 (U nil Number))]
             (inc a)))
  ; improved error
  (is-tc-err (if-let [a (ann-form 1 (U nil Number))]
               (inc a))
             Number)
  (is-tc-e (if-let [{:keys [a]} {:a 1}]
             (inc a)
             1))
  (is-tc-err (if-let [a (ann-form 1 (U nil Number String))]
               (inc a)))
  (is-tc-err (if-let [a "a"]
               (inc a)))
  )

(deftest assert-test
  (binding [*assert* true]
    (is-tc-e #(assert 1)))
  (binding [*assert* true]
    (is-tc-e #(assert 1 "foo")))
  (binding [*assert* false]
    (is-tc-e #(assert (/ nil nil) "foo")))
  (binding [*assert* false]
    (is-tc-e #(assert (/ nil nil "foo"))))
  (is-tc-err #(assert (/ nil) "foo"))
  (is-tc-err #(assert (/ nil nil) "foo"))
  ;; unreachable message
  (is-tc-e #(assert "foo" (/ nil)))
  (is-tc-err #(assert nil (/ nil))))

(deftest with-open-test
  (is-tc-e #(with-open [r (java.io.FileInputStream. "some/dir")] 
              (.available r)))
  ;; better error
  (is-tc-err #(with-open [r (java.io.FileInputStream. "some/dir")])
             [-> Number]))

(deftest fn-test
  (is-tc-e (clojure.core/fn [a]))
  (is-tc-e (clojure.core/fn [a] a))
  (is-tc-e (clojure.core/fn [a]
             {:pre [(-> a identity)]}
             a))
  (is-tc-e (clojure.core/fn [a]
             {:post [(symbol? %)]}
             a))
  ;; approximates line number from outer form
  (is-tc-err (clojure.core/fn [a])
             [Number -> Number])
  ;; exact line number
  (is-tc-err (clojure.core/fn ([a]))
             [Number -> Number])
  )

(deftest assoc-in-inline-test
  (is-tc-e (assoc-in {} [:a] 1) '{:a Num})
  ;; improved msg
  (is-tc-err (assoc-in {} [:a :b] 1) '{:a Num})
  ;; improved msg
  (is-tc-err (assoc-in 'a [:a] 1))
  ;; improved msg
  (is-tc-err (assoc-in {:a (ann-form 'a Sym)} [:a :b] 1))
  (is-tc-err (assoc-in {:a {:b (ann-form 'a Sym)}} [:a :b :c] 1))
  (is-tc-err (assoc-in {:a []} [:a :b] 1))
  (is-tc-e (assoc-in {:a []} [:a 0] 1) '{:a '[Num]}))

(deftest for-test
  (is (-> (chk-frm (clojure.core/for [a [1 2]] a))
          :result
          #{'(1 2)}))
  (is-tc-e #(clojure.core/for [a [1 2]] a))
  (is-tc-e #(clojure.core/for [a [1 2]] a) [-> (Seqable Number)])
  (is-tc-e #(clojure.core/for [a [1 2]] a) [-> (ASeq Number)])
  (is-tc-e #(clojure.core/for [a '(1 2)] (ann-form a Number)) [-> (Seqable Number)])
  (is-tc-e #(clojure.core/for [a [1 2]] (ann-form a Number)) [-> (Seqable Number)])
  ;; FIXME improve error locality
  (is-tc-err #(clojure.core/for [a [1 2]] a) [-> (Seqable Boolean)])
  (is-tc-err #(clojure.core/for [a [1 2]] a) [-> Number])
  (is-tc-err #(clojure.core/for [a [1 2]] a) [-> nil])
  (is-tc-e #(clojure.core/for [a [1 2] b [2 3]] [a b]))
  (is-tc-e #(clojure.core/for [a [1 2] b [2 3]] [a b]) [-> (Seq '[Num Num])])
  ;FIXME use t/fn instead of fn*
  ;; TODO propagates expected type to body
  #_
  (is-tc-e #(clojure.core/for [a [1 2] b [2 3]] (fn* [c] (+ c a b)))
           [-> (Seq [Num -> Num])])
  ;; FIXME example of bad type propagating to body
  #_
  (is-tc-err #(clojure.core/for [a [1 2] b [2 3]] (fn* [c] (+ c a b))) [-> (Seq [nil -> Num])])
)

(deftest get-in-test
  (is (-> (chk-frm (get-in {:a {:b 1}} [:a :b]))
          :result
          #{1}))
  (is-tc-e (get-in {:a {:b 1}} [:a :b])
           Num)
  ; improved error
  (is-tc-err (get-in {:a {:b 1}} [:a :b])
             Sym)
  ;; FIXME need better messages for 'default'
  (is-tc-err (get-in {:a {:b 1}} [:a :b] 1))
  (is-tc-err (get-in {:a {:b 1}} [:a :b] 1)
             Sym))

(deftest update-in-inline-test
  (is-tc-e (update-in {:a {:b 1}} [:a :b] identity)
           '{:a '{:b Num}})
  (is-tc-e (update-in {:a {:b 1 :c 2}} [:a] dissoc :b)
           '{:a '{:c Num}})
  ;TODO
  #_
  (is-tc-e (let [m {:a {:b {:c 3}}}]
             (update-in m [:a] update-in [:b] update-in [:c] str))
           '{:a '{:b '{:c Str}}})
  ;; error is the eventual call to `inc` on nil
  ;; FIXME garbled error
  (is-tc-err (let [m {:a {:b 1}}]
               (update-in m [:a] update-in [:b] update-in [:c] inc)))
  ;; error is (inc "a") call
  ;; FIXME garbled error
  (is-tc-err (let [m {:a {:b {:c "a"}}}]
               (update-in m [:a] update-in [:b] update-in [:c] inc)))
  (is-tc-e (update-in {:a {:b 1}} [:a :b] inc)
           '{:a '{:b Num}})
  (is-tc-e (update-in {:a {:b 1}} [:a :b] str)
           '{:a '{:b Str}})
  (is-tc-err (update-in {:a []} [:a :b] identity))
  (is-tc-err (let [m {:a []}]
               (update-in m [:a :b] identity))))

(deftest ->-test
  (is-tc-e (-> identity
               (map [1 2 3])))
  (is-tc-err (-> identity
                 (map [1 2 3])
                 (ann-form (t/Seq t/Bool))))
  (is-tc-err (-> identity
                 (map [1 2 3])
                 (ann-form (t/Seq t/Bool))
                 (map [2 3 4])))
  (is-tc-err (-> identity
                 (map [1 2 3]))
             (t/Seq t/Bool))
  ; FIXME big error
  (is-tc-err (-> identity
                 (map [1 2 3])
                 (map [4 5 6])
                 (map [7 8 9]))
             (t/Seq t/Bool))
  ; FIXME line number
  (is-tc-err (-> identity
                 (map [1 2 3])
                 vec)
             (t/Seq t/Bool)))

(deftest proxy-test
  (is-tc-e
    (proxy [Object] []
      (toString [] "a")))
  ;TODO actually check methods
  #_
  (is-tc-err
    (proxy [Object] []
      (toString [] 1)))
  (is-tc-e
    (proxy [Object] []
      (toString [] "a"))
    Object)
  (is (tc-e
        (proxy [Object] [])))
  ;TODO
  #_
  (is (tc-e
        (proxy [Object clojure.lang.ISeq] [])))
  (is-tc-e
    (proxy [Object] [])
    Object)
  (is-tc-err
    (proxy [Object] []
      (toString [] "a"))
    nil)
  )

(comment
  (class
    (proxy [clojure.lang.ASeq clojure.lang.ISeq] []
      (seq [] nil)
      (toString [] "a")))
  (class
    (proxy [clojure.lang.ISeq] []
      (seq [] nil)
      (first [] 1)
      (toString [] "a")))
  )

(comment
(deftest map-test
  (is-tc-e (map '(1 2 3) [1 2 3]))
  (is-tc-e (map identity [1 2 3]))
  (is-tc-e (map identity (map identity [1 2 3])))
  (is-tc-e (map + [1 2 3] [2 3 4]))
  (is-tc-e (map identity [1 2 3])
           (t/Seq t/Num))
  (is-tc-e (map identity [1 2 3])
           (t/HSeq [Num Num Num]))
  (is-tc-e (map identity [])
           (t/Seq Nothing))
  (is-tc-e (map identity [1 2 3])
           (t/Seq t/Bool))
  ;; FIXME better column number
  (is-tc-err (map identity 'a))
  (is-tc-err (map identity identity))
  ;; FIXME line number + source ns
  (is-tc-err (map))

  ;               ;vvvvvvvvvvvvvv
  ;; (map identity ('a asdlfsdf
  ;;                 ;lsdl;fsdlf) 
  ;;              ;^^^^^^^^^^^^^^
  ;;      :a :b)
  ;               ;vv
  ;; (map identity 'a
  ;;              ;^^
  ;;      :a :b)

  (is-tc-e (map identity)
           (t/Transducer t/Num t/Num))
  (is-tc-e (map boolean)
           (t/Transducer t/Num t/Bool))
  (is-tc-err (map boolean [1 2 3])
             (t/Transducer t/Bool t/Num))
  ;; FIXME better blame-form needed
  ;; can we check `expected` earlier? before we check arguments?
  (is-tc-err (map boolean [1 2 3] [2 3 4])
             (t/Transducer t/Bool t/Num))
  ;; FIXME this goes crazy because it inlines to (map (ann-form ... t/Bool))
  ;; FIXME need to override form for inlined inner map
  (is-tc-err (map map)
             (t/Transducer t/Bool t/Num))
  ;; FIXME better blame-form needed
  (is-tc-err (map boolean)
             (t/Transducer t/Bool t/Num))
  (is-tc-err (map (fn [a] (boolean a)))
             (t/Transducer t/Bool t/Num))
  (is-tc-err (map identity))
  (is-tc-err (map (fn [a :- t/Any] a)))
)

(comment
  ; andmap : (All (x) ((x -> y) (List x) -> y))
  (lambda (a b)
    (when (andmap number? (list a b))
      (+ a b)))
  =>
  (lambda (a b)
    (when (and (number? a)
               (number? b))
      (+ a b)))
  )

(deftest every?-test
  (is-tc-e (every? identity [1 2 3]))
  (is-tc-e (core/fn [a]
             (when (number? (first [a]))
               (inc a))))
  (is-tc-e (core/fn [a]
             (when (number? (first [a]))
               (inc a))))
  (is-tc-err (core/fn [a]
               (when (number? (first [0 a]))
                 (inc a))))
  (is-tc-e (core/fn [a]
             (if ((complement number?) (first [a]))
               nil
               (inc a))))
  (is-tc-e (core/fn [a]
             (when (number? (first [a]))
               (inc a))))
  (is-tc-e (core/fn [a]
             (when (number? (first (seq [a])))
               (inc a))))
  (is-tc-e (core/fn [a]
             (when ((fn* [& args] (number? (first args))) a)
               (inc a))))
  (is-tc-e (core/fn [a]
             (when ((fn* [& args] (apply number? args)) a)
               (inc a))))
  (is-tc-e (core/fn [a]
             (when-not ((fn* [& args] (not (apply number? args))) a)
               (inc a))))
  (is-tc-e (core/fn [a]
             (if ((complement (fn* [& args] (apply number? args))) a)
               nil
               (inc a))))
  (is-tc-e (core/fn [a]
             (if (if (apply number? [a]) false true)
               nil
               (inc a))))
  (is-tc-e (core/fn [a]
             (when (apply number? [a])
               (inc a))))
  (is-tc-e (core/fn [a b]
             {:pre [(every? number? [a b])]}
             (+ a b)))
  (is-tc-e (fn [a b]
             {:pre [(every? number? [a b])]}
             (+ a b)))
  (is-tc-e (fn [a b]
             {:pre [(not-any? (complement number?) [a b])]}
             (+ a b)))
  (is-tc-e (fn [a]
             {:pre [(some number? [a])]}
             (inc a)))
)

(deftest juxt-test
  (is-tc-e (fn [a]
             {:pre [(every? identity ((juxt number? integer?) a))]}
             (inc a)))
  (is-tc-e (fn [a]
             {:pre [(first (nthrest [(number? a) (integer? a)] 0))]}
             (inc a)))
  (is-tc-e (fn [a]
             {:pre [(first [(number? a) (integer? a)])]}
             (inc a)))
  (is-tc-e (fn [a]
             {:pre [(first ((juxt number?) a))]}
             (inc a)))
  (is-tc-e (fn [a]
             {:pre [(first ((juxt number? identity #(str %)) a))]}
             (inc a)))
  )

(comment
  (((fn []
      map))
   identity
   [1 2 3])
  ;=>
  (map
    identity
    [1 2 3])

  ((fn [f]
     (f 1))
   (fn [d]
     (inc d)))
  ;=>
  ((let [f (fn [d]
             (inc d))]
     (fn [d]
       (inc d)))
   1)
  ;=>
  ((let [f (fn [d]
             (inc d))]
     (let [d 1]
       inc))
   1)
)

(deftest symbolic-fnapp-test
  (is-tc-e ((fn [f]
              (f 1))
            (fn [d]
              (inc d)))))


(deftest beta-reduce-test
  (is-tc-e ((fn* [a] a) :a) ':a)
  (is-tc-err ((fn* [a] a) :a) ':b)
  (is-tc-e ((fn* [a] ((fn* [a] ((fn* [a] a) a)) a)) :a) ':a)
  ;; TODO preserve original form in error msg somewhere? or indicate
  ;; it's been symbolically expanded
  (is-tc-err ((fn* [a] ((fn* [a] ((fn* [a] a) a)) a)) :a) ':b)
  (is-tc-e (:a {:a :b}) ':b)
  (is-tc-e ((let* [] :a) {:a :b}) ':b)
  (is-tc-e ((let* [] (fn* [a] (:a a))) {:a :b}) ':b)
  (is-tc-e (((fn* [a] a) :a)
            {:a :b})
           ':b)
  (is-tc-e (((fn* [a] a) :a)
            ((fn* [a] a) {:a :b}))
           ':b)
  (is-tc-e (((fn* [f] (f :a)) (fn* [a] a)) {:a :b}) ':b)
  (is-tc-e (((fn* [f b] (f b)) (fn* [c] c) :a) {:a :b}) ':b)
  (is-tc-e (((fn* [f a] (f a)) (fn* [a] a) :a) {:a :b}) ':b)
  (is-tc-e (((fn* [f a] (f a)) (fn* [a] a) :a) ((fn* [a] a) {:a :b})) ':b)
  (is-tc-e (((fn* [f a] (f a)) identity :a) ((fn* [a] a) {:a :b})) ':b)
  (is-tc-e (((fn* [f a] (f a)) (fn* [a] a) :a)
            ((fn* [a] a) {:a :b}))
           ':b)
  (is-tc-e [(((fn* [f a] (f a)) (fn* [a] a) :a)
             ((fn* [a] a) {:a :b}))
            (((fn* [f a] (f a)) (fn* [a] a) :a)
             ((fn* [a] a) {:a :b}))
            (((fn* [f a] (f a)) (fn* [a] a) :a)
             ((fn* [a] a) {:a :b}))
            (((fn* [f a] (f a)) (fn* [a] a) :a)
             ((fn* [a] a) {:a :b}))]
           (Seqable ':b))
  ; real y combinator, all should hit the beta limit
  (is-tc-err (fn* ([] ((fn* [f] ((fn* [x] (f (x x))) (fn* [y] (f (y y))))) inc))))
  (is-tc-err (fn* ([] (inc ((fn* ([x] (inc (x x)))) (fn* ([x] (inc (x x)))))))))
  (is-tc-err (fn* ([] (inc (inc ((fn* ([x] (inc (x x)))) (fn* ([x] (inc (x x))))))))))
  (is-tc-err (fn* ([] (inc (inc (inc ((fn* ([x] (inc (x x)))) (fn* ([x] (inc (x x)))))))))))
  (is-tc-err (fn* ([] (inc (inc (inc (inc ((fn* ([x] (inc (x x)))) (fn* ([x] (inc (x x))))))))))))
  (is-tc-err (fn* ([] (inc (inc (inc (inc (inc ((fn* ([x] (inc (x x)))) (fn* ([x] (inc (x x)))))))))))))
  (is-tc-err (fn* ([] (inc (inc (inc (inc (inc (inc ((fn* ([x] (inc (x x)))) (fn* ([x] (inc (x x))))))))))))))
  ;variadic fn
  (is-tc-e ((fn* [& a] (map inc a)) 1))
  (is-tc-err ((fn* [& a] (map inc a)) :a))
  (is-tc-e ((fn* [a] (map inc (seq [a]))) 1))
  (is-tc-e ((fn* [a z] (map inc (seq [a z]))) 1 2))
  (is-tc-e ((fn* [_ z] (inc z)) nil 1))
  (is-tc-e ((fn* [a z] (inc z)) nil 1))
  (is-tc-e ((fn* [a z] (+ a z)) 2 1))
  ;TODO keyword invocations
  #_(is-tc-e ((:a {:a (fn [a] a)}) :b) ':b)
  ;apply
  (is-tc-e (apply inc [2])
           Int)
  (is-tc-e (apply inc (seq [2])) Int)
  (is-tc-e (apply inc [(second (first {:a 2}))]) Int)
  (is-tc-e (apply map (seq [identity [1]]))
           (Seqable Num))
  (is-tc-e (apply map [identity])
           (Transducer Num Num))
  ;; FIXME error msg
  (is-tc-err (apply map [identity])
             (Transducer Num Bool))
  ;; FIXME error msg
  (is-tc-err (apply map [identity]))
  ;comp
  (is-tc-e ((comp inc dec) 1) Num)
  (is-tc-e ((comp inc :a) {:a 1}) Num)
  (is-tc-e ((comp identity :a) {:a 1}) Num)
  (is-tc-e ((comp identity :a) {:a 1}) '1)
  (is-tc-e ((comp inc (constantly nil)) 1))
  ;; FIXME
  #_
  (is-tc-e (sequence (comp (map inc) (map dec)) [1]))
  (is-tc-e ((fn* [& args]
              (inc (first args)))
            1))
  (is-tc-e ((core/fn [& args]
              (inc (first args)))
            1))
  (is-tc-err ((core/fn [& args]
                (inc (first args)))
              true))
  (is-tc-err ((core/fn [& args]
                (inc (first args)))
              true))
  ;;TODO t/fn should "infer" its rest arg
  #_(is-tc-e ((fn [& args]
              (inc (first args)))
            1))
  ;;TODO play with the map transducer expander, use fn instead of fn*
  ;; and figure out how to play nicely with its mexpansion
  (is-tc-e (comp (map inc) (map dec))
           (Transducer Num Num))
  ;; horrible error msg
  (is-tc-err (comp (map inc) (map dec))
             (Transducer Num Bool))
  ;TODO constantly

  ; ensure we correctly reduce
  ; ((ann-form (fn [a] body)
  ;            [A :-> B])
  ;  v)
  ; =>
  ; (ann-form
  ;   (expected-as e
  ;     (ann-form body[(ann-form v (If e (DomOf (OptsOf e) 0 :arity 2) ^:infer Any))/a]
  ;               (If e (RngOf (TypeOf e) :arity 2) ^:infer Any)))
  ;   [A :-> B])
  ; =>
  ; (ann-form body[(ann-form v A)/a]
  ;           B)
  (is-tc-e ((ann-form
              (fn* [i] (inc i))
              [Int :-> Int])
            1))
  (is-tc-e ((ann-form
              (clojure.core/fn [i] (inc i))
              [Int :-> Int])
            1))
  (is-tc-e ((ann-form
              (clojure.core/fn [i] (inc i))
              [Int :-> Bool])
            1))
  (is-tc-e ((ann-form
              (clojure.core/fn [i] (inc i))
              [Bool :-> Int])
            1))
  ;; FIXME t/fn defaults to Any arguments
  (is-tc-e ((t/fn [i :- Int] (inc i))
            1))
  (is-tc-e ((ann-form
              (t/fn [i :- Int] :- Int (inc i))
              [Int :-> Int])
            1))
  (is-tc-e ((ann-form
              (ann-form
                inc
                [Int :-> Int])
              [Int :-> Int])
            1))
  (is-tc-e ((ann-form
              (ann-form
                (fn* [i] (boolean i))
                [Num :-> Bool])
              [Int :-> Bool])
            1))
  (is-tc-e (ann-form
             (ann-form
               (fn* [i] (boolean i))
               [Num :-> Bool])
             [Int :-> Bool]))
  (is-tc-err (ann-form
               (ann-form
                 (fn* [i] (boolean i))
                 [Int :-> Bool])
               [Num :-> Bool]))
  ;; TODO subst object in return type of beta-reduction

  ;reduce
  (is-tc-e (reduce (fn* [a e] (conj a e))
                   []
                   [1 2 3]))

  ;fixpoint
#_
  (is-tc-e (fixpoint
             (fn* [c e] (concat c [(inc e)]))
             {:subst-var x
              :init [(Seq Nothing) Int :-> ^::t/infer Any]
              :query (All [x] [[x Int :-> x] :-> x])
              :iterate [x Int :-> ^::t/infer Any]
              }))
  )

(comment
  (defn timet
    [expr]
    (let [start (. System (nanoTime))
          ret (expr)]
      (/ (double (- (. System (nanoTime)) start)) 1000000.0)))

  (clojure.pprint/pprint
  (sort-by val
           (into {} (map (fn [v]
                           [v (timet #(clojure.test/test-vars [v]))]))
                 (filter (every-pred var? (comp :test meta)) (vals (ns-publics *ns*)))))
  )
; no custom expansion
  '([#'clojure.core.typed.test.frozen-macros/tc-ignore-test 171.394456]
    [#'clojure.core.typed.test.frozen-macros/with-open-test 181.161775]
    [#'clojure.core.typed.test.frozen-macros/typed-fn-test 233.531726]
    [#'clojure.core.typed.test.frozen-macros/ann-form-test 235.352863]
    [#'clojure.core.typed.test.frozen-macros/ns-test 240.44296]
    [#'clojure.core.typed.test.frozen-macros/get-in-test 341.253694]
    [#'clojure.core.typed.test.frozen-macros/fn-test 495.774091]
    [#'clojure.core.typed.test.frozen-macros/when-not-test 542.922632]
    [#'clojure.core.typed.test.frozen-macros/when-test 546.166276]
    [#'clojure.core.typed.test.frozen-macros/when-let-test 609.879237]
    [#'clojure.core.typed.test.frozen-macros/if-let-test 631.63356]
    [#'clojure.core.typed.test.frozen-macros/assoc-in-inline-test 676.056304]
    [#'clojure.core.typed.test.frozen-macros/assert-test 694.094945]
    [#'clojure.core.typed.test.frozen-macros/update-in-inline-test 765.674776]
    [#'clojure.core.typed.test.frozen-macros/let-test 992.088318]
    [#'clojure.core.typed.test.frozen-macros/for-test 5778.336702])
; yes custom expansion
'([#'clojure.core.typed.test.frozen-macros/ns-test 182.167286]
  [#'clojure.core.typed.test.frozen-macros/tc-ignore-test 188.358344]
  [#'clojure.core.typed.test.frozen-macros/with-open-test 221.02634]
  [#'clojure.core.typed.test.frozen-macros/ann-form-test 274.636581]
  [#'clojure.core.typed.test.frozen-macros/typed-fn-test 330.160597]
  [#'clojure.core.typed.test.frozen-macros/get-in-test 388.410054]
  [#'clojure.core.typed.test.frozen-macros/fn-test 682.037165]
  [#'clojure.core.typed.test.frozen-macros/assert-test 774.38307]
  [#'clojure.core.typed.test.frozen-macros/if-let-test 793.200128]
  [#'clojure.core.typed.test.frozen-macros/when-not-test 807.979324]
  [#'clojure.core.typed.test.frozen-macros/when-let-test 816.350961]
  [#'clojure.core.typed.test.frozen-macros/for-test 819.305905]
  [#'clojure.core.typed.test.frozen-macros/assoc-in-inline-test 820.942907]
  [#'clojure.core.typed.test.frozen-macros/when-test 865.453885]
  [#'clojure.core.typed.test.frozen-macros/let-test 1221.219269]
  [#'clojure.core.typed.test.frozen-macros/update-in-inline-test 1641.337323])

)
)

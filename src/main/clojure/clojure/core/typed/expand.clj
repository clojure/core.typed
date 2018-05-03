(ns clojure.core.typed.expand
  "Rewriting rules for custom expansions, to improve type checking
  error messages and reduce local annotations."
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.special-form :as spc]
            [clojure.pprint :as pp]
            [clojure.core.typed.internal :as internal]))

(defmulti -expand-macro (fn [form {:keys [vsym]}] vsym))
(defmulti -expand-inline (fn [form {:keys [vsym]}] vsym))

(defn custom-expansion? [vsym]
  {:pre [(symbol? vsym)
         (namespace vsym)]}
  (contains? (methods -expand-macro) vsym))

(defn custom-inline? [vsym]
  {:pre [(symbol? vsym)
         (namespace vsym)]}
  (contains? (methods -expand-inline) vsym))

(defn expand-macro [form opts]
  (-expand-macro form opts))

(defn expand-inline [form opts]
  (-expand-inline form opts))

(defmacro check-let-destructure [{:keys [expression]}] expression)
(defmacro check-let-destructure-no-op [_] nil)

(defmethod -expand-macro 'clojure.core/let
  [[_ bindings-form & body-forms :as form] _]
  (let [gs (gensym "b")]
    (reduce
      (fn [form [expression binding]]
        `(let* [~gs (check-let-destructure
                      {:binding ~binding
                       :expression ~expression})
                ~@(destructure [binding gs])]
           ~form))
      `(check-if-empty-body
         (do ~@body-forms)
         {:msg-fn (fn [_#]
                     "This 'let' expression returns nil with an empty body, which does not agree with the expected type")
          :blame-form ~form
          :original-body ~body-forms})
      (partition 2 (rseq bindings-form)))))

(defmacro check-if-empty-body [e opts]
  {:pre [(map? opts)]}
  `(do ~spc/special-form
       ::check-if-empty-body
       '~opts
       ~e))

(defmacro check-expected [e opts]
  {:pre [(map? opts)]}
  `(do ~spc/special-form
       ::check-expected
       '~opts
       ~e))

(defmethod -expand-macro 'clojure.core/when
  [[_ test & bodys :as form] _]
  `(if ~test
     (check-if-empty-body
       (do ~@bodys)
       {:msg-fn (fn [_#]
                  "This 'when' expression returns nil if the test is true, which does not agree with the expected type.")
        :blame-form ~form
        :original-body ~bodys})
     (check-expected
       nil
       {:msg-fn (fn [_#]
                  "This 'when' expression returns nil if the test is false, which does not agree with the expected type.")
        :blame-form ~form})))

(defmethod -expand-macro 'clojure.core/when-not
  [[_ test & bodys :as form] _]
  `(if ~test
     (check-expected
       nil
       {:msg-fn (fn [_#]
                  "This 'when-not' expression returns nil if the test is true, which does not agree with the expected type.")
        :blame-form ~form})
     (check-if-empty-body
       (do ~@bodys)
       {:msg-fn (fn [_#]
                  "This 'when-not' expression returns nil if the test is false, which does not agree with the expected type.")
        :blame-form ~form
        :original-body ~bodys})))

(defmethod -expand-macro 'clojure.core/if-let
  [[_ bindings then else :as original-form] _]
  (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if temp#
         (let [_# (check-let-destructure-no-op
                    {:binding ~form
                     :expression ~tst})
               ;TODO avoid repeated destructuring checks ^::no-check-destructure
               ~form temp#]
           ~then)
         ~(if (= 3 (count original-form))
            `(check-expected
               nil
               {:msg-fn (fn [_#]
                          "This 'if-let' expression returns nil if the test is false, which does not agree with the expected type.")
                :blame-form ~original-form})
            else)))))

(defmethod -expand-macro 'clojure.core/when-let
  [[_ test & bodys :as form] _]
  `(if-let ~test
     (check-if-empty-body
       (do ~@bodys)
       {:msg-fn (fn [_#]
                  "This 'when-let' expression returns nil if the test is true, which does not agree with the expected type.")
        :blame-form ~form
        :original-body ~bodys})
     (check-expected
       nil
       {:msg-fn (fn [_#]
                  "This 'when-let' expression returns nil if the test is false, which does not agree with the expected type.")
        :blame-form ~form})))

(defmethod -expand-macro 'clojure.core/with-open
  [[_ bindings & body :as form] _]
  (let [expand-with-open (fn expand-with-open [bindings body]
                           (cond
                             (= (count bindings) 0) `(check-if-empty-body
                                                       (do ~@body)
                                                       {:msg-fn (fn [_#]
                                                                  (str "This 'with-open' expression returns nil, "
                                                                       "which does not agree with the expected type."))
                                                        :blame-form ~form
                                                        :original-body ~body})
                             (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                                                       (try
                                                         ~(expand-with-open (subvec bindings 2) body)
                                                         (finally
                                                           (. ~(bindings 0) close))))
                             :else (throw (IllegalArgumentException. "with-open only allows Symbols in bindings"))))]
    (expand-with-open bindings body)))

(defmethod -expand-macro 'clojure.core/assert
  [[_ x message :as form] _]
  (let [msg? (= 3 (count form))
        erase-assert? (not *assert*)
        assert-expand (fn
                        ([x]
                         (when-not erase-assert?
                           `(when-not ~x
                              (throw (new AssertionError (str "Assert failed: " (pr-str '~x)))))))
                        ([x message]
                         (when-not erase-assert?
                           `(when-not ~x
                              (throw (new AssertionError (str "Assert failed: " ~message "\n" (pr-str '~x))))))))]
    (apply assert-expand (rest form))))

(defn parse-fn-sigs [[_ & sigs :as form]]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (next sigs) sigs)
        single-arity-syntax? (vector? (first sigs))
        sigs (if (vector? (first sigs))
               (list sigs)
               (if (seq? (first sigs))
                 sigs
                 ;; Assume single arity syntax
                 (throw (IllegalArgumentException.
                          (if (seq sigs)
                            (str "Parameter declaration "
                                 (first sigs)
                                 " should be a vector")
                            (str "Parameter declaration missing"))))))
        process-sigs (fn [[params & body :as sig]]
                       (assert (vector? params) params)
                       (let [conds (when (and (next body) (map? (first body)))
                                     (first body))
                             body (if conds (next body) body)
                             conds-from-params-meta? (not conds)
                             conds (or conds (meta params))
                             pre (:pre conds)
                             post (:post conds)]
                         {;; inherit positional information from params, or
                          ;; overall fn if unavailable. If multi-arity, sig might
                          ;; also have meta.
                          :sig-form (vary-meta sig #(merge (meta form) (meta params) %))
                          :pre pre
                          :post post
                          :conds-from-params-meta? conds
                          :params params
                          :body body}))]
    {:single-arity-syntax? single-arity-syntax?
     :sigs (mapv process-sigs sigs)
     :name name}))

(defmethod -expand-macro 'clojure.core/fn
  [[_ & sigs :as form] _]
  (let [{:keys [sigs name]} (parse-fn-sigs form)
        expand-sig (fn [{:keys [conds-from-params-meta? pre post params body sig-form]}]
                     (assert (vector? params))
                     (let [gsyms (mapv (fn [a]
                                         (if (= '& a)
                                           a
                                           (gensym "a")))
                                       params)
                           fn*-params (with-meta gsyms (meta params))
                           assert-form (fn [p] `(assert ~p))]
                       `(~fn*-params
                         (let [~@(mapcat (fn [param gsym]
                                           (when-not (= '& param)
                                             [param gsym]))
                                         params
                                         gsyms)]
                           ~@(map assert-form pre)
                           ~(let [body `(check-if-empty-body
                                          (do ~@body)
                                          {:msg-fn (fn [_#]
                                                     (str "This 'fn' body returns nil, "
                                                          "which does not agree with the expected type."))
                                           :blame-form ~sig-form
                                           :original-body ~body})]
                              (if post
                                `(let [~'% ~body]
                                   ~@(map assert-form post)
                                   ~'%)
                                body))))))]
    `(fn* ~@(when name [name])
          ~@(map expand-sig sigs))))

(defmacro check-for-seq [expr]
  (throw (Exception. "TODO check-for-seq"))
  `(rand-nth (seq ~expr)))

(defmacro expected-as [s body]
  `(do ~spc/special-form
       ::expected-as
       '{:sym ~s}
       ~body))

(defmacro gather-for-return-type [ret]
  (throw (Exception. "gather-for-return-type Not for expansion")))

(defmacro gather-for-return-type [_ ret]
  `(do ~spc/special-form
       ::gather-for-return-type
       '{}
       ~ret))

(defmacro check-for-expected [{:keys [expr expected-local]}]
  (throw (Exception. "check-for-expected Not for expansion")))

(defmethod -expand-macro `check-for-expected [[_ {:keys [expr expected-local]}] _]
  (assert nil "TODO check-for-expected")
  expr)

(defmethod -expand-macro 'clojure.core/for
  [[_ seq-forms body-form :as form] _]
  (let [expg (gensym 'expected)
        ret (reduce
              (fn [body [expr binding]]
                (case binding
                  :let `(let ~expr ~body)
                  (:while :when) `(when ~expr ~body)
                  (if (keyword? binding)
                    (throw (Exception. (str "Invalid 'for' keyword: " binding)))
                    `(let [~binding (check-for-seq
                                      {:expr ~expr
                                       :binding ~binding})]
                       ~body))))
              `[(check-for-expected
                  {:expr ~body-form
                   ;; FIXME should we blame an outer form (if it exists)
                   ;; if the expected type is incompatible with Seq?
                   :form ~form
                   :expected-local ~expg})]
              (partition 2 (rseq seq-forms)))]
    `(expected-as ~expg
                  (gather-for-return-type ~ret))))

(defmacro ignore-expected-if [tst body] body)

(defmethod -expand-macro `ignore-expected-if
  [[_ tst body :as form] _]
  {:pre [(= 3 (count form))]}
  body)

(defn expand-typed-fn-macro
  [form _]
  (let [{:keys [parsed-methods name poly ann]} (internal/parse-fn* form)
        reassembled-fn-type `(t/IFn ~@(map (fn [{:keys [rest drest dom rng] :as method-ann}]
                                             {:pre [(map? method-ann)]
                                              :post [(vector? %)]}
                                             (vec
                                               (concat
                                                 (map :type dom)
                                                 (cond
                                                   rest [(:type rest) '*]
                                                   drest [(-> drest :pretype :type) '... (:bound drest)])
                                                 [:-> (:type rng)])))
                                           (map :ann parsed-methods)))
        reassembled-fn-type (if-let [forall (:forall poly)]
                              `(t/All ~forall ~reassembled-fn-type)
                              reassembled-fn-type)]
    `(t/ann-form
       (fn ~@(concat
               (when name
                 [name])
               (for [{:keys [original-method body pvec ann]} parsed-methods]
                 (list pvec
                       `(ignore-expected-if ~(boolean (-> ann :rng :default))
                         (check-if-empty-body
                           (do ~@body)
                           {:msg-fn (fn [_#]
                                      "This 't/fn' method returns nil, which does not agree with the expected type.")
                            :blame-form ~original-method
                            :original-body ~body}))))))
      ~reassembled-fn-type)))

(defmethod -expand-macro `t/fn [& args] (apply expand-typed-fn-macro args))
(defmethod -expand-macro 'clojure.core.typed.macros/fn [& args] (apply expand-typed-fn-macro args))

(defmethod -expand-macro 'clojure.core/ns [form _]
  `(check-expected
     nil
     {:msg-fn (fn [_#]
                "This 'ns' expression returns nil, which does not agree with the expected type.")
      :blame-form ~form}))

(comment
(assoc-in 'a [:a] 1)
(assoc 'a :a 1)
)

(defmacro with-post-blame-context [e opts]
  {:pre [(map? opts)]}
  `(do ~spc/special-form
       ::with-post-blame-context
       '~opts
       ~e))

(defn inline-assoc-in
  ([[_ m ks v :as form]] (inline-assoc-in form m ks v))
  ([form m ks v]
   {:pre [(vector? ks)
          (seq ks)]}
   (inline-assoc-in form m ks v []))
  ([form m [k & ks] v seen]
   `(assoc (with-post-blame-context
             ~m
             {:msg-fn (fn [_#]
                        ~(if (seq seen)
                           (str "I traversed the first argument of this 'assoc-in' expression down the path"
                                ;; indent
                                "\n\n  "
                                (binding [*print-level* 4
                                          *print-length* 8]
                                  (with-out-str
                                    (pp/pprint (nth form 2))))
                                "\n"
                                "and expected to find each level to be 'assoc'able. However, I found the result down sub-path"
                                "\n\n  "
                                (binding [*print-level* 4
                                          *print-length* 8]
                                  (with-out-str
                                    (pp/pprint seen)))
                                "\n"
                                "cannot be associated with the next key in the path, which is"
                                "\n\n  "
                                (binding [*print-level* 1
                                          *print-length* 8]
                                  (with-out-str (pp/pprint k))))
                           (str "The first argument of 'assoc-in must be 'assoc'able")))
              :blame-form ~form})
           ~k
           ~(if ks
              (inline-assoc-in form
                               `(get ~m ~k)
                               ks v (conj seen k))
              v))))

(comment 
  (inline-assoc-in `(assoc-in {:a {:b nil}} [:a :b] 2))
  (inline-assoc-in {:a {:b {:c nil}}} [:a :b :c] 2)
)

(defmacro type-error [opts]
  {:pre [(map? opts)]}
  `(do ~spc/special-form
       ::type-error
       '~opts
       nil))

(defn inline-get-in
  ([[_ m ks default :as form]] 
   (if (nil? default)
     (inline-get-in form m ks)
     `(type-error {:msg-fn (fn [_#]
                             "core.typed only supports 'get-in' with 'nil' default value")
                   :form ~form})))
  ([form m [k & ks]]
   (if ks
     (inline-get-in form `(get ~m ~k) ks)
     `(get ~m ~k))))

(defmethod -expand-inline 'clojure.core/assoc-in [form _]
  {:pre [(= 4 (count form))]}
  (prn "-expand-inline clojure.core/assoc-in")
  (let [[_ _ path] form
        _ (assert (and (vector? path) (seq path)) "core.typed only supports non-empty vector paths with assoc-in")]
    `(check-expected
       ~(inline-assoc-in form)
       {:msg-fn (fn [_#]
                  "The return type of this 'assoc-in' expression does not agree with the expected type.")
        :blame-form ~form})))

(defmethod -expand-inline 'clojure.core/get-in [form _]
  {:pre [(#{3 4} (count form))]}
  (let [[_ _ path] form
        _ (assert (and (vector? path) (seq path)) "core.typed only supports non-empty vector paths with get-in")]
    `(check-expected
       ~(inline-get-in form)
       {:msg-fn (fn [_#]
                  "The return type of this 'get-in' expression does not agree with the expected type.")
        :blame-form ~form})))

(defn inline-update-in
  ([[_ m ks f & args :as form]] (inline-update-in form m ks f args))
  ([form m ks f args]
   `(assoc-in ~m ~ks (~f (get-in ~m ~ks) ~@args))))

(defmethod -expand-inline 'clojure.core/update-in [form _]
  {:pre [(<= 4 (count form))]}
  (let [[_ _ path] form
        _ (assert (and (vector? path) (seq path)) "core.typed only supports non-empty vector paths with 'update-in'")]
    `(check-expected
       ~(inline-update-in form)
       {:msg-fn (fn [_#]
                  "The return type of this 'update-in' expression does not agree with the expected type.")
        :blame-form ~form})))


(comment
  (update-in m [:a :b] f x y z)
  (assoc-in m [:a :b]
            (fake-application
              (f (get-in [:a :b]) x y z)
              {:bad-function-blame f
               :bad-argument {0 {:blame-form (update-in m [:a :b] f x y z)
                                 :msg-fn (fn [_]
                                           (str "The implicit first argument of this 'update-in' expression does not"
                                                " match the first argument of the function."))}}}))

  (assoc-in
  )
)

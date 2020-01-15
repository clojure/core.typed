;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.expand
  "Rewriting rules for custom expansions, to improve type checking
  error messages and reduce local annotations."
  (:require [clojure.core.typed :as t]
            [clojure.core :as core]
            [clojure.core.typed.special-form :as spc]
            [clojure.pprint :as pp]
            [clojure.core.typed.internal :as internal]))

(set! *warn-on-reflection* true)

;; TODO need a way to specify a custom expansion for the Gilardi scenario
;;      that's actually evaluated (ie. used instead of :original-form in eval-ast).
;; This would be used only in analyze+eval.
;; Rationale: we assume macroexpansion is idempotent and unordered, but (obviously) not evaluation.
;; To properly type check a top-level form, it much be evaluated using top-level
;; `do` rules, and so either:
;; - the type checking requires evaluation order of top-level `do`'s to be respected,
;;   and type checking fails, or
;;   - eg. custom expansion of (do (defmacro a ...) (a ...))
;; - :original-form will be out of date and subsequently evaluating it
;;    may involve repeated evaluation
;;   - unsure, but this scenario might be impossible.

;; Discussion:
;; we're at a fork in the road at how seriously we need to handle the Gilardi
;; scenario and interleaving macroexpansion and evaluation.
;; If we could simplify type checking to just involve macroexpansion (and
;; never needing to evaluate intermediate forms), things would be simpler.
;; We basically assume that macros and "compile-time" things are static,
;; which, IME, outside of clojure.core bootstrapping (eg., `clojure.core/defn`)
;; is actually a pretty good assumption.
;; Very early versions of Typed Clojure used to evaluate a file before type
;; checking it, and then type check each form using `analyze` to get the AST 
;; instead of `analyze+eval`. IIRC this worked well enough.
;; If we're going to do things like symbolic closures that cross top-level forms,
;; then it's very likely we're going to break the normal macroexpansion/evaluation
;; order (if, eg., reducing the symbolic closure involves macroexpansion, like
;; the following scenario):
;;  (defmacro i-change [] 1)
;;  (defn a [x] (i-change))
;;  (defmacro i-change [] 2)
;;  (defn b [y] (a 1))
;; I find it hard to believe that this happens often, because in almost any Clojure
;; file I've worked with, you can eval a form at any point in the file with no inconsistencies.
;; You can probably rearrange the order of any Clojure expression in the same "phase"
;; and it will mostly work.

;; If we strictly want to adhere to Clojure's evaluation order as we type check,
;; then non-compound top-level forms seem easy to deal with if we want to throw
;; away macro-expansion: just throw away the type checker's macroexpanded code
;; after type checking, and `eval` the originally non-compound top-level expression instead.
;; For compound top-level forms, the "real" and "fake" expansions need to coincide
;; at least in the number and order of their top-level children.
;; eg., (defmacro deftype [..] (do e1  e2  e3))  ;; actual expansion
;;      (defmacro deftype [..] (do e1' e2' e3')) ;; fake expansion
;; We then process each top-level subform in lockstep, evaluating the real expansion
;; before type checking the next one. ie.,
;; 1. type check e1'
;; 2. eval e1
;; 3. type check e2'
;; 4. eval e2
;; etc.
;; If any top-level expression is itself a compound top-level expression, it is also
;; processed with this assumption in mind.

;; However, as above, symbolic closures crossing top-level forms complicates this greatly,
;; I think irreparably. Think: you might need a symbolic closure from further down the
;; same file that you haven't evaluated yet, or even from another namespace that you processed
;; long ago. Even the simple case of using a symbolic closure from a top-level form you
;; just processed might break some assumptions.

;; Is Typed Clojure more a linter or a type system? Well, it's both, but I think
;; it's qualities as a linter are more in demand. Its greatest inheritance from the world of
;; type systems is the notion of soundness "up-to" specific assumptions.
;; I want to add a new assumption: rearranging the order of any forms in the same "stage" is equivalent
;; in any file Typed Clojure type checks. This is a common assumption as devs often copy-paste
;; and re-eval code in their REPL without carefully recreating the precise order of 
;; top-level forms before it. If we can assume it while type checking, it enables the
;; kinds of symbolic execution that I think is necessary to elide many top-level annotations
;; a la Flow lang.

;; The new strategy for type checking a file would be:
;; 1. re-load the file
;; 2. type-check each form in the file without evaluating the end results
;; I guess this also assumes that re-evaluating reader eval #= forms is ok.
;; Using these assumptios to their logical conclusion, we could parallelize
;; the type checking of individual forms.

;; Update: I found a discussion where Rich Hickey explicitly says macros should not have
;; side effects (that are relied on) 
;; - https://groups.google.com/d/msg/clojure/Ch4aaR_pTD0/Gpiqi4JJeK8J
;; (This was basically what I hypothesized in my dissertation, but I would have saved
;; a lot of space if I found this earlier!)

;; eg., perhaps a flag that designates an expansion "safe" for top-level usage.
;; Note: this isn't atomic with the other multimethods, consider single stateful reference
#_(defonce *eval-top-level-expansion (atom #{}))

(defmulti -expand-macro (fn [form {:keys [vsym]}] vsym))
;; TODO equivalent to :inline-arities
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

(defmethod -expand-macro 'clojure.core/ns
  [[_ name & references :as form] _]
  (let [process-reference
        (fn [[kname & args]]
          `(~(symbol "clojure.core" (clojure.core/name kname))
             ~@(map #(list 'quote %) args)))
        docstring  (when (string? (first references)) (first references))
        references (if docstring (next references) references)
        name (if docstring
               (vary-meta name assoc :doc docstring)
               name)
        metadata   (when (map? (first references)) (first references))
        references (if metadata (next references) references)
        name (if metadata
               (vary-meta name merge metadata)
               name)
        name-metadata (meta name)]
    `(do
       nil ;in-ns call
       ~@(when name-metadata
           `(nil)) ;reset-meta call
       nil ;with-loading-context call
       ; a top-level form
       (let* []
         (check-expected
           nil
           {:msg-fn (fn [_#]
                      "This 'ns' expression returns nil, which does not agree with the expected type.")
            :blame-form ~form})))))

(defmacro check-if-empty-body
  "If e is a non-empty do form, this check it with the given options.

  Options:
  - :msg-fn (not quoted)             eval and use this function for error message
  - :blame-form (not quoted)         use this form as a target for blame in type errors
  - :original-body (not quoted)      original list of body members
  "
  [e opts]
  {:pre [(map? opts)]}
  `(do ~spc/special-form
       ::check-if-empty-body
       '~opts
       ~e))

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
      (if (seq body-forms)
        `(do ~@body-forms)
        `(check-if-empty-body
           (do ~@body-forms)
           {:msg-fn (fn [_#]
                      "This 'let' expression returns nil with an empty body, which does not agree with the expected type")
            :blame-form ~form
            :original-body ~body-forms}))
      (partition 2 (rseq bindings-form)))))

(defmacro check-expected
  "Enhance checking of e with extra information.
  
  Options:
  - :default-expected (not quoted)   if no expected type is present, use this one
  - :msg-fn (not quoted)             eval and use this function for error message
  - :blame-form (not quoted)         use this form as a target for blame in type errors
  "
  [e opts]
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
                     {:pre [(vector? params)]}
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
                           ~(let [body (if (seq body)
                                         `(do ~@body)
                                         `(check-if-empty-body
                                            (do ~@body)
                                            {:msg-fn (fn [_#]
                                                       (str "This 'fn' body returns nil, "
                                                            "which does not agree with the expected type."))
                                             :blame-form ~sig-form
                                             :original-body ~body}))]
                              (if post
                                `(let [~'% ~body]
                                   ~@(map assert-form post)
                                   ~'%)
                                body))))))]
    `(fn* ~@(when name [name])
          ~@(map expand-sig sigs))))

(defmacro solve [expr opts]
  expr)

(defmethod -expand-macro `solve
  [[_ expr opts :as form] _]
  {:pre [(= 3 (count form))]}
  `(do ~spc/special-form
       ::solve
       '~opts
       ~expr))

(defmacro expected-type-as [s body & [opts & more]]
  {:pre [((some-fn nil? map?) opts)
         (not more)]}
  `(let* [~s (t/ann-form nil t/Any)]
     (do ~spc/special-form
         ::expected-type-as
         '~(merge opts {:sym s})
         ~body)))

(defmacro ignore-expected-if [tst body] body)

(defmethod -expand-macro `ignore-expected-if
  [[_ tst body :as form] _]
  {:pre [(= 3 (count form))]}
  body)

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
                    `(let [~binding (solve
                                      ~expr
                                      {:query (t/All [a#] [(t/U nil (t/Seqable a#)) :-> a#])
                                       :blame-form ~expr
                                       :msg-fn (fn [_#]
                                                 (str "'for' expects Seqables in binding form"))})]
                       ~body))))
              ;; TODO push inner expected type into body-form
              body-form
              (partition 2 (rseq seq-forms)))]
    `(let* []
       (check-expected
         (solve ~ret
                {:query (t/All [a#] [a# :-> (t/ASeq a#)])})
         {:msg-fn (fn [_#]
                    "The return type of this 'for' expression does not agree with the expected type.")
          :blame-form ~form}))))

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
    `(do nil ;spc/special-form
         nil ;::t/fn
         nil ;{..}
         (let* []
           (t/ann-form
             (fn ~@(concat
                     (when name
                       [name])
                     (for [{:keys [original-method body pvec ann]} parsed-methods]
                       (let [conds (when (and (next body) (map? (first body)))
                                     (first body))
                             body (if conds
                                    (next body)
                                    body)]
                         (list* pvec
                                (concat
                                  (when conds
                                    [conds])
                                  [`(ignore-expected-if ~(boolean (-> ann :rng :default))
                                                        ~(if (seq body)
                                                           `(do ~@body)
                                                           `(check-if-empty-body
                                                              (do ~@body)
                                                              {:msg-fn (fn [_#]
                                                                         "This 't/fn' method returns nil, which does not agree with the expected type.")
                                                               :blame-form ~original-method
                                                               :original-body ~body})))]))))))
             ~reassembled-fn-type)))))

(defmethod -expand-macro `t/fn [& args] (apply expand-typed-fn-macro args))
(defmethod -expand-macro 'clojure.core.typed.macros/fn [& args] (apply expand-typed-fn-macro args))

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
  (inline-assoc-in `(assoc-in {:a {:b {:c nil}}} [:a :b :c] 2))
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

(defmethod -expand-inline 'clojure.core/assoc-in [form {:keys [internal-error]}]
  (when-not (= 4 (count form))
    (internal-error (str "Must provide 3 arguments to clojure.core/assoc-in, found " (dec (count form))
                         ": " form)))
  (let [[_ _ path] form
        _ (assert (and (vector? path) (seq path)) "core.typed only supports non-empty vector paths with assoc-in")]
    `(let* []
       (check-expected
         ~(inline-assoc-in form)
         {:msg-fn (fn [_#]
                    "The return type of this 'assoc-in' expression does not agree with the expected type.")
          :blame-form ~form}))))

(defmethod -expand-inline 'clojure.core/get-in [form {:keys [internal-error]}]
  (when-not (#{3 4} (count form))
    (internal-error (str "Must provide 2 or 3 arguments to clojure.core/get-in, found " (dec (count form))
                         ": " form)))
  (let [[_ _ path] form
        _ (assert (and (vector? path) (seq path)) "core.typed only supports non-empty vector paths with get-in")]
    `(let* []
       (check-expected
         ~(inline-get-in form)
         {:msg-fn (fn [_#]
                    "The return type of this 'get-in' expression does not agree with the expected type.")
          :blame-form ~form}))))

(defn inline-update-in
  ([[_ m ks f & args :as form]] (inline-update-in form m ks f args))
  ([form m ks f args]
   `(assoc-in ~m ~ks (~f (get-in ~m ~ks) ~@args))))

(defmethod -expand-inline 'clojure.core/update-in [form {:keys [internal-error]}]
  (when-not (<= 4 (count form))
    (internal-error (str "Must provide at least 3 arguments to clojure.core/update-in, found " (dec (count form))
                         ": " form)))
  (let [[_ _ path] form
        _ (assert (and (vector? path) (seq path)) "core.typed only supports non-empty vector paths with 'update-in'")]
    `(let* []
       (check-expected
         ~(inline-update-in form)
         {:msg-fn (fn [_#]
                    "The return type of this 'update-in' expression does not agree with the expected type.")
          :blame-form ~form}))))

(defn expand-ann-form [form _]
  (let [[_ frm ty] form]
    ; coincide with top-level `do` macroexpansion of the actual `ann-form` macro
    `(do ~spc/special-form
         ::t/ann-form
         '{:type ~ty
           :inner-check-expected {:blame-form ~frm}
           :outer-check-expected {:msg-fn (fn [_#]
                                            ;; TODO insert actual types in this message
                                            (str "The annotated type for this 'ann-form' expression did not agree "
                                                 "with the expected type from the surrounding context."))
                                  :blame-form ~form}}
         ~frm)))

(defmethod -expand-macro `t/ann-form [& args] (apply expand-ann-form args))
(defmethod -expand-macro 'clojure.core.typed.macros/ann-form [& args] (apply expand-ann-form args))

(defn expand-tc-ignore [[_ & body :as form] _]
  `(do ~spc/special-form
       ::t/tc-ignore
       '{:form ~form
         :outer-check-expected {:msg-fn (fn [_#]
                                          "The surrounding context of this 'tc-ignore' expression expects a more specific type than Any.")
                                :blame-form ~form}}
       (do ~@(or body [nil]))))

(defmethod -expand-macro `t/tc-ignore [& args] (apply expand-tc-ignore args))
(defmethod -expand-macro 'clojure.core.typed.macros/tc-ignore [& args] (apply expand-tc-ignore args))

(defmacro require-expected [expr opts]
  {:pre [(map? opts)]}
  `(do ~spc/special-form
       ::require-expected
       '~opts
       ~expr))

(defn inline-map-transducer [[_ f :as form] {:keys [internal-error]}]
  {:pre [(= 2 (count form))]}
  `(fn* [rf#]
     (fn*
       ([] (rf#))
       ([result#] (rf# result#))
       ([result# input#]
        (rf# result# (~f input#)))))
  #_
  `(expected-type-as expected#
     (let [[in# out#]
           (solve expected#
                  {:query (t/All [a# b#] [(t/Transducer a# b#) :-> '[a# b#]])
                   ;; would be nice to customize this message based on `expected`
                   :msg-fn (fn [_#]
                             (str "'map' transducer arity requires an expected type which is a subtype of (t/Transducer t/Nothing t/Any)"))
                   :blame-form ~form})]
       (fn* [rf#]
         (fn*
           ([] (rf#))
           ([result#] (rf# result#))
           ([result# input#]
            (rf# result#
                 ;; fake invoke
                 (check-expected
                   (~f input#)
                   {:default-expected {:type (t/TypeOf out#)}
                    :msg-fn (fn [{parse-type# :parse-type actual# :actual}]
                              (str "'map' transducer did not return a correct type:"
                                   "\n\nExpected: \t" (pr-str (parse-type# (list 't/Transducer '(t/TypeOf in#) '(t/TypeOf out#))))
                                   "\n\nActual: \t" (pr-str (parse-type# (list 't/Transducer '(t/TypeOf in#) actual#)))
                                   "\n\n"
                                   "in: \t" '~form))
                    :blame-form (~f input#)}))))))
     {:msg-fn (fn [_#]
                (str "Must provide expected type to 'map' transducer arity.\n"
                     "Hint: Try (t/ann-form (map ...) (t/Transducer in out))."))
      :blame-form ~form}))

(defn map-colls-fallthrough [[_ f & colls :as form] {:keys [internal-error splice-seqable-form]}]
  (let [gsyms (repeatedly (count colls) gensym)
        bindings (mapcat (fn [i gsym coll] 
                           [gsym `(solve
                                    ~coll
                                    {:query (t/All [a#] [(t/U nil (t/Seqable a#)) :-> a#])
                                     :msg-fn (fn [{actual# :actual}]
                                               (str "Argument number " ~i " to 'map' must be Seqable, given: "
                                                    actual#))
                                     :blame-form ~coll})])
                         ;; counting argument #'s to this 'map' form
                         (range 2 ##Inf)
                         gsyms colls)]
    `(let ~(vec bindings)
       ;; FIXME can we push the expected type into `f`?
       (check-expected
         (solve
           ((check-expected
              ~f
              {:msg-fn (fn [_#]
                         "Cannot invoke this higher-order function to 'map'")
               :blame-form ~f})
            ~@gsyms)
           {:query (t/All [a#] [a# :-> (t/Seq a#)])})
         {:msg-fn (fn [_#]
                    "The return type of this 'map' expression does not agree with the expected type.")
          :blame-form ~form}))))

(defn inline-map-colls [[_ f & colls :as form] {:keys [internal-error splice-seqable-form] :as opts}]
  {:pre [(seq colls)]}
  #_
  (prn "inline-map-colls")
  (let [splices (mapv splice-seqable-form colls)]
    (if (some nil? splices)
      (do
        #_
        (prn "fall through splices" splices)
        (map-colls-fallthrough form opts))
      (let [smallest-max-count (apply min (map (fn [e]
                                                 (apply + (map :max-count e)))
                                               splices))
            largest-min-count (apply max (map (fn [e]
                                                 (apply + (map :min-count e)))
                                              splices))
            ordered? (:ordered (first splices))
            max-realized-count (max smallest-max-count largest-min-count)
            ;_ (prn "max-realized-count" max-realized-count)
            csyms (repeatedly (count colls) gensym)]
        (if (and (not-any? nil? splices)
                 ordered?
                 (< max-realized-count 10)
                 (< (count colls) 3))
          (if (pos? max-realized-count)
            `(let* [~@(mapcat vector csyms colls)]
               (cons ((check-expected
                        ~f
                        {:msg-fn (fn [_#]
                                   "Cannot invoke this higher-order function to 'map'")
                         :blame-form ~f})
                      ~@(map (fn [csym] `(first ~csym)) csyms))
                     (map ~f ~@(map (fn [csym] `(rest ~csym)) csyms))))
            `(sequence nil))
          (map-colls-fallthrough form opts))))))

(defmethod -expand-inline 'clojure.core/map
  [[_ f & colls :as form] {:keys [internal-error] :as opts}]
  (when-not (<= 2 (count form))
    (internal-error (str "Must provide 1 or more arguments to clojure.core/map, found " (dec (count form))
                         ": " form)))
  (if (empty? colls)
    (inline-map-transducer form opts)
    (inline-map-colls form opts)))

(defmethod -expand-inline 'clojure.core/every? [[_ f coll :as form] {:keys [internal-error splice-seqable-form] :as opts}]
  (when-not (= 3 (count form))
    (internal-error (str "Must provide 2 arguments to clojure.core/every?, found " (dec (count form))
                         ": " form)))
  (if-let [splice (splice-seqable-form coll)]
    (let [min-count (apply + (map :min-count splice))
          max-count (apply + (map :max-count splice))
          ordered? (:ordered (first splice))
          max-realized (max min-count max-count)]
      (if (and ordered?
               (< max-realized 15))
        (if (pos? max-realized)
          `(let* [c# ~coll]
             (and (~f (first c#))
                  (every? ~f (rest c#))))
          true)
        form))
    form))

(defmethod -expand-inline 'clojure.core/some [[_ f coll :as form] {:keys [internal-error splice-seqable-form] :as opts}]
  (when-not (= 3 (count form))
    (internal-error (str "Must provide 2 arguments to clojure.core/every?, found " (dec (count form))
                         ": " form)))
  (if-let [splice (splice-seqable-form coll)]
    (let [min-count (apply + (map :min-count splice))
          max-count (apply + (map :max-count splice))
          ordered? (:ordered (first splice))
          max-realized (max min-count max-count)]
      (if (and ordered?
               (< max-realized 15))
        (if (pos? max-realized)
          `(let* [c# ~coll]
             (or (~f (first c#))
                 (some ~f (rest c#))))
          nil)
        form))
    form))

(defmethod -expand-inline 'clojure.core/not-any? [[_ f coll :as form] {:keys [internal-error splice-seqable-form] :as opts}]
  (when-not (= 3 (count form))
    (internal-error (str "Must provide 2 arguments to clojure.core/not-any?, found " (dec (count form))
                         ": " form)))
  (if-let [splice (splice-seqable-form coll)]
    (let [min-count (apply + (map :min-count splice))
          max-count (apply + (map :max-count splice))
          ordered? (:ordered (first splice))
          max-realized (max min-count max-count)]
      (if (and ordered?
               (< max-realized 15))
        (if (pos? max-realized)
          `(let* [c# ~coll]
             (and (not (~f (first c#)))
                  (not-any? ~f (rest c#))))
          true)
        form))
    form))

(defmethod -expand-inline 'clojure.core/apply [[_ f & args :as form] {:keys [internal-error splice-seqable-form] :as opts}]
  (when-not (<= 3 (count form))
    (internal-error (str "Must provide at least 2 arguments to clojure.core/apply, found " (dec (count form))
                         ": " form)))
  (let [[fixed rst] ((juxt pop peek) (vec args))]
    (if-let [splice (splice-seqable-form rst)]
      (let [min-count (apply + (map :min-count splice))
            max-count (apply + (map :max-count splice))
            ordered? (:ordered (first splice))
            max-realized (max min-count max-count)]
        (prn "apply: found splice" ordered? max-realized)
        (if (and ordered?
                 (< max-realized 15))
          (let [gsym (gensym 'args)]
            `(let* [~gsym ~rst]
               (~f ~@fixed ~@(map (fn [i]
                                    `(first (nthnext ~gsym ~i)))
                                  (range max-realized)))))
          form))
      (do
        (prn "apply: no splice")
        form))))

(defmethod -expand-inline 'clojure.core/complement [[_ f :as form] {:keys [internal-error splice-seqable-form] :as opts}]
  (when-not (= 2 (count form))
    (internal-error (str "Must provide 1 argument to clojure.core/complement, found " (dec (count form))
                         ": " form)))
  `(fn* [& args#]
     (not (apply ~f args#))))

(defmethod -expand-inline 'clojure.core/juxt [[_ & fs :as form] {:keys [internal-error splice-seqable-form] :as opts}]
  (when-not (<= 2 (count form))
    (internal-error (str "Must provide at least 1 argument to clojure.core/juxt, found " (dec (count form))
                         ": " form)))
  (let [gsym (gensym 'args)]
    `(fn* [& ~gsym]
       ~(mapv (fn [f]
                `(apply ~f ~gsym))
              fs))))

(defmethod -expand-inline 'clojure.core/not [[_ x :as form] {:keys [internal-error splice-seqable-form] :as opts}]
  (when-not (= 2 (count form))
    (internal-error (str "Must provide 1 argument to clojure.core/not, found " (dec (count form))
                         ": " form)))
  `(if ~x false true))

(comment
 (-> identity
     (map [1 2 3])
     vec)
 =>
 (check-expected
   (vec
     (check-expected
       (map identity [1 2 3])
       {:msg-fn (fn [_#]
                  "A threaded form with -> threw a type error: (-> ... (map [1 2 3]) ...)")
        :blame-form (map identity [1 2 3])}))
   {:msg-fn (fn [_#]
              "A threaded form with -> threw a type error: (-> ... vec ...)")
    :blame-form (vec (map identity [1 2 3]))})
 (clojure.pprint/pprint (-expand-macro '(-> identity (map [1 2 3]) vec) {:vsym `->}))
 )

(defmethod -expand-macro 'clojure.core/->
  [[_ x & forms :as all-form] _]
  (loop [x x, forms forms, blame-form x]
    (if forms
      (let [form (first forms)
            nforms (next forms)
            insert-in (fn [x]
                        (if (seq? form)
                          (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                          (list form x)))
            threaded (insert-in x)
            blame-form (insert-in blame-form)
            threaded `(check-expected
                        ~threaded
                        {:msg-fn (fn [_#]
                                   (str "A threaded form with -> yielded a type error: "
                                        ~(pr-str (list* '-> '... form
                                                        (when nforms
                                                          ['...])))))
                         :blame-form ~blame-form})]
        (recur threaded nforms blame-form))
      `(let* [] ~x))))

(defmethod -expand-inline 'clojure.core/comp
  [[_ & fs :as all-form] _]
  (let [fs (vec fs)
        args (gensym 'args)]
    `(fn* [& ~args]
       ~(reduce (fn [res g]
                  (list g res))
                `(apply ~(if (seq fs) (peek fs) `identity) ~args)
                (when (seq fs)
                  (rseq (pop fs)))))))


;; Notes:
;; - try `->` next
;;   - good test of inheriting msg-fn etc.
;; - do we need to special-case comp+transducers?
(comment
   (into #{}
         (comp (filter identity)
               (map inc))
         [1 nil 2 nil 3])
   (-expand-inline '(comp) {:vsym 'clojure.core/comp})
   (-expand-inline '(comp inc dec) {:vsym 'clojure.core/comp})
   (-expand-inline '(comp (fn [x] x) inc dec identity) {:vsym 'clojure.core/comp})
   )


(comment
  (update-in m [:a :b] f x y z)
  ;=>
  (assoc-in m [:a :b]
            (fake-application
              (f (get-in [:a :b]) x y z)
              {:bad-function-blame f
               :bad-argument {0 {:blame-form (update-in m [:a :b] f x y z)
                                 :msg-fn (fn [_]
                                           (str "The implicit first argument of this 'update-in' expression does not"
                                                " match the first argument of the function."))}}}))

  (-> a b c)
  ;=>
  (c (b a))

  (-> m
      b
      (update :a inc))
  ;=>
  (update (b m) :a inc)
)

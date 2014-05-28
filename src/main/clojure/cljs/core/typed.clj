(ns cljs.core.typed
  "Macros for Clojurescript type checking"
  (:refer-clojure :exclude [fn])
  (:require [clojure.core.typed.load-if-needed :as load]
            [clojure.core :as core]
            [clojure.core.typed.current-impl :as impl :refer [v]]
            [clojure.core.typed.util-cljs :as u]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.internal :as internal]
            [clojure.core.typed.errors :as err]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]
            [cljs.env :as env]
            [clojure.pprint :as pprint]))

(defn load-if-needed 
  "Load and initialize all of core.typed if not already"
  []
  (load/load-if-needed))

(defn reset-caches
  "Reset internal type caches."
  []
  (load-if-needed)
  ((impl/v 'clojure.core.typed.reset-caches/reset-caches)))


;at the top because the rest of this namespace uses this macro
(defmacro 
  ^{:forms '[(fn name? [param :- type* & param :- type * ?] :- type? exprs*)
             (fn name? ([param :- type* & param :- type * ?] :- type? exprs*)+)]}
  fn
  "Like clojure.core/fn, but with optional annotations.

  eg. ;these forms are equivalent
      (fn [a] b)
      (fn [a :- Any] b)
      (fn [a :- Any] :- Any b)
      (fn [a] :- Any b)

      ;annotate return
      (fn [a :- String] :- String body)

      ;named fn
      (fn fname [a :- String] :- String body)

      ;rest parameter
      (fn [a :- String & b :- Number *] body)

      ;dotted rest parameter
      (fn [a :- String & b :- Number ... x] body)

      ;multi-arity
      (fn fname 
        ([a :- String] :- String ...)
        ([a :- String, b :- Number] :- String ...))"
  [& forms]
  (core/let [{:keys [fn ann]} (internal/parse-fn* false forms)]
    `(do ::special-form
         ::fn
         {:ann '~ann}
         ~fn)))

; many of these macros resolve to CLJS functions in 
; the CLJS ns cljs.core.typed

(defmacro ann-form 
  "Annotate a form with an expected type."
  [form ty]
  `(ann-form* ~form '~ty))

(defmacro ann 
  "Annotate varsym with type. If unqualified, qualify in the current namespace.
  If varsym has metadata {:no-check true}, ignore definitions of varsym while type checking.
  
  eg. ; annotate the var foo in this namespace
      (ann foo [Number -> Number])
  
      ; annotate a var in another namespace
      (ann another.ns/bar [-> nil])
   
      ; don't check this var
      (ann ^:no-check foobar [Integer -> String])"
  [varsym typesyn]
  `(ann* '~varsym '~typesyn))

(defmacro 
  ^{:forms '[(ann-protocol vbnd varsym & methods)
             (ann-protocol varsym & methods)]}
  ann-protocol 
  "Annotate a possibly polymorphic protocol var with method types.
  
  eg. (ann-protocol IFoo
        bar
        [IFoo -> Any]
        baz
        [IFoo -> Number])

      ; polymorphic
      (ann-protocol [[x :variance :covariant]]
        IFoo
        bar
        [IFoo -> Any]
        baz
        [IFoo -> Number])"
  [& args]
  (let [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        varsym (if bnd-provided?
                 (second args)
                 (first args))
        {:as mth} (if bnd-provided?
                    (next (next args))
                    (next args))]
    `(ann-protocol* '~vbnd '~varsym '~mth)))

(defmacro
  ^{:forms '[(ann-datatype dname [field :- type*] opts*)
             (ann-datatype binder dname [field :- type*] opts*)]}
  ann-datatype
  "Annotate datatype Class name dname with expected fields.
  If unqualified, qualify in the current namespace.

  eg. (ann-datatype MyDatatype [a :- Number,
  b :- Long])

  (ann-datatype another.ns.TheirDatatype
  [str :- String,
  vec :- (IPersistentVector Number)])"
  [& args]
  ;[dname fields & {ancests :unchecked-ancestors rplc :replace :as opts}]
  (let [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [dname fields & {ancests :unchecked-ancestors rplc :replace :as opts}]
        (if bnd-provided?
          (next args)
          args)]
    (assert (not rplc) "Replace NYI")
    (assert (symbol? dname)
            (str "Must provide name symbol: " dname))
    `(ann-datatype* '~vbnd '~dname '~fields '~opts)))

(defmacro def-alias 
  "Define a type alias. Takes an optional doc-string as a second
  argument.

  Updates the corresponding var with documentation.
  
  eg. (def-alias MyAlias
        \"Here is my alias\"
        (U nil String))"
  ([sym doc-str t]
   (assert (string? doc-str) "Doc-string passed to def-alias must be a string")
   `(def-alias ~sym ~t))
  ([sym t]
   (assert (symbol? sym) (str "First argument to def-alias must be a symbol: " sym))
   `(do (def-alias* '~sym '~t)
        ~(when-not (namespace sym)
           `(def ~sym)))))

(defmacro inst 
  "Instantiate a polymorphic type with a number of types"
  [inst-of & types]
  `(inst-poly ~inst-of '~types))

(defmacro 
  ^{:forms '[(letfn> [fn-spec-or-annotation*] expr*)]}
  letfn>
  "Like letfn, but each function spec must be annotated.

  eg. (letfn> [a :- [Number -> Number]
               (a [b] 2)

               c :- [Symbol -> nil]
               (c [s] nil)]
        ...)"
  [fn-specs-and-annotations & body]
  (let [bindings fn-specs-and-annotations
        ; (Vector (U '[Symbol TypeSyn] LetFnInit))
        normalised-bindings
        (loop [[fbnd :as bindings] bindings
               norm []]
          (cond
            (empty? bindings) norm
            (symbol? fbnd) (do
                             (assert (#{:-} (second bindings))
                                     "letfn> annotations require :- separator")
                             (assert (<= 3 (count bindings)))
                             (recur 
                               (drop 3 bindings)
                               (conj norm [(nth bindings 0)
                                           (nth bindings 2)])))
            (list? fbnd) (recur
                           (next bindings)
                           (conj norm fbnd))
            :else (throw (Exception. (str "Unknown syntax to letfn>: " fbnd)))))
        {anns false inits true} (group-by list? normalised-bindings)
        ; init-syn unquotes local binding references to be compatible with hygienic expansion
        init-syn (into {}
                   (for [[lb type] anns]
                     [lb `'~type]))]
    `(cljs.core/letfn ~(vec inits)
       ;unquoted to allow bindings to resolve with hygiene
       ~init-syn
       ;preserve letfn empty body
       nil
       ~@body)))

(defmacro 
  ^{:forms '[(loop> [binding :- type, init*] exprs*)]}
  loop>
  "Like loop, except loop variables require annotation.

  Suggested idiom: use a comma between the type and the initial
  expression.

  eg. (loop> [a :- Number, 1
              b :- (U nil Number), nil]
        ...)"
  [bndings* & forms]
  (let [normalise-args
        (fn [seq-exprs]
          (loop [flat-result ()
                 seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result
              (and (vector? (first seq-exprs))
                   (#{:-} (-> seq-exprs first second))) (do
                                                          (prn "DEPRECATED WARNING: loop> syntax has changed, use [b :- t i] for clauses"
                                                               "ns: " *ns* " form:" &form)
                                                          (recur (concat flat-result (take 2 seq-exprs))
                                                                 (drop 2 seq-exprs)))
              :else (do (assert (#{:-} (second seq-exprs))
                                "Incorrect syntax in loop>.")
                        (recur (concat flat-result [(vec (take 3 seq-exprs))
                                                    (nth seq-exprs 3)])
                               (drop 4 seq-exprs))))))
        ;group args in flat pairs
        bndings* (normalise-args bndings*)
        bnds (partition 2 bndings*)
        ; [[lhs :- bnd-ann] rhs]
        lhs (map ffirst bnds)
        rhs (map second bnds)
        bnd-anns (map #(-> % first next second) bnds)]
    `(loop>-ann (cljs.core/loop ~(vec (mapcat vector lhs rhs))
                  ~@forms)
                '~bnd-anns)))

;; `do` is special at the top level in Clojure, same in CLJS?
(defmacro tc-ignore 
  "Ignore forms in body during type checking"
  [& body]
  `(do ~@(map (fn [b] `(tc-ignore-forms* ~b)) body)))

(defmacro typed-deps 
  "Declare namespaces which should be checked before the current namespace.
  Accepts any number of symbols.
  
  eg. (typed-deps clojure.core.typed.holes
                  myns.types)"
  [& args]
  `(typed-deps* '~args))

(defonce ^:dynamic *currently-checking-cljs* nil)
(defonce ^:dynamic *already-collected* nil)

(defn ^:skip-wiki
  -init-delayed-errors 
  "Internal use only"
  []
  (atom [] :validator #(and (vector? %)
                            (every? (fn [a] 
                                      (instance? clojure.lang.ExceptionInfo a))
                                    %))))

(defn cf* 
  "Check a single form with an optional expected type.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see cf."
  [form expected expected-provided?]
  (load-if-needed)
  (reset-caches)
  (env/ensure
    (comp/with-core-cljs
      (if *currently-checking-cljs*
        (throw (Exception. "Found inner call to check-ns or cf"))
        (binding [*currently-checking-cljs* true
                  vs/*delayed-errors* (-init-delayed-errors)]
          (impl/with-cljs-impl
            (let [ast ((v 'clojure.core.typed.analyze-cljs/ast-for-form) form)
                  ;collect
                  _ ((v 'clojure.core.typed.collect-cljs/collect) ast)
                  ;check
                  c-ast ((v 'clojure.core.typed.check-cljs/check) ast
                         (when expected-provided?
                           ((v 'clojure.core.typed.type-rep/ret)
                            ((v 'clojure.core.typed.parse-unparse/parse-type) expected))))]
              ;handle errors
              (if-let [errors (seq @vs/*delayed-errors*)]
                (err/print-errors! errors)
                (-> c-ast 
                    ((v 'clojure.core.typed.utils/expr-type))
                    ((v 'clojure.core.typed.parse-unparse/unparse-TCResult-in-ns) (u/cljs-ns)))))))))))

(defmacro cf
  "Check a single form with an optional expected type."
  ([form] `(cf* '~form nil nil))
  ([form expected] `(cf* '~form '~expected true)))

(defn check-ns*
  "Check a Clojurescript namespace, or the current namespace.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see check-ns."
  ([] (check-ns* (u/cljs-ns)))
  ([nsym]
   (assert (symbol? nsym)
           "Checked namespace must be symbol")
   (load-if-needed)
   (env/ensure
     (comp/with-core-cljs
       (impl/with-cljs-impl
         (reset-caches)
         ((v 'clojure.core.typed.reset-env/reset-envs!))
         (if *currently-checking-cljs*
           (throw (Exception. "Found inner call to check-ns or cf"))
           (do
             (load-if-needed)
             (binding [*currently-checking-cljs* true
                       *already-collected* (atom #{})
                       vs/*delayed-errors* (-init-delayed-errors)]
               (let [_ ((v 'clojure.core.typed.collect-cljs/collect-ns) nsym)
                     _ ((v 'clojure.core.typed.check-cljs/check-ns) nsym)]
                 ;handle errors
                 (if-let [errors (seq @vs/*delayed-errors*)]
                   (err/print-errors! errors)
                   :ok))))))))))

(defmacro check-ns
  "Check a Clojurescript namespace, or the current namespace. This macro
  is intended to be called at the Clojurescript REPL. For the equivalent function see
  check-ns*."
  [& args]
  `~(apply check-ns* args))


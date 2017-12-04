(ns cljs.core.typed
  "Macros for Clojurescript type checking"
  (:refer-clojure :exclude [fn def loop])
  (:require [clojure.core.typed.load-if-needed :as load]
            [clojure.core :as core]
            [clojure.core.typed.current-impl :as impl :refer [v]]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.internal :as internal]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.import-macros :as import-m]
            [clojure.core.typed.macros :as macros]
            [clojure.pprint :as pprint]))

(import-m/import-macros clojure.core.typed.macros
  [fn tc-ignore ann-form def loop])

(defn load-if-needed 
  "Load and initialize all of core.typed if not already"
  []
  (load/load-if-needed true))

(defn reset-caches
  "Reset internal type caches."
  []
  (load-if-needed)
  ((impl/v 'clojure.core.typed.reset-caches/reset-caches)))

; many of these macros resolve to CLJS functions in 
; the CLJS ns cljs.core.typed

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

(defmacro ann-jsnominal
  "Equivalent of TypeScript interface"
  [varsym jsnom]
  (let [qualsym (if (namespace varsym)
                    varsym
                    (symbol (str (ns-name *ns*)) (name varsym)))]
   `(ann-jsnominal* '~qualsym '~jsnom)))

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
        (core/loop [[fbnd :as bindings] bindings
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
       ;;preserve letfn empty body
       ;;nil
       ~@body)))

(defmacro 
  ^{:forms '[(loop> [binding :- type, init*] exprs*)]}
  ^{:deprecated "0.2.61"}
  loop>
  "DEPRECATED: use loop

  Like loop, except loop variables require annotation.

  Suggested idiom: use a comma between the type and the initial
  expression.

  eg. (loop> [a :- Number, 1
              b :- (U nil Number), nil]
        ...)"
  [bndings* & forms]
  (let [normalise-args
        (core/fn [seq-exprs]
          (core/loop [flat-result ()
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

(defmacro typed-deps 
  "Declare namespaces which should be checked before the current namespace.
  Accepts any number of symbols.
  
  eg. (typed-deps clojure.core.typed.holes
                  myns.types)"
  [& args]
  `(typed-deps* '~args))

(defonce ^:dynamic *already-collected* nil)

(defn cf* 
  "Check a single form with an optional expected type.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see cf."
  [form expected expected-provided?]
  (load-if-needed)
  ((impl/v 'clojure.core.typed.check-form-cljs/check-form-cljs)
   form expected expected-provided?))

(defn check-form-info 
  [form & opts]
  (load-if-needed)
  (apply (impl/v 'clojure.core.typed.check-form-cljs/check-form-info)
         form opts))

(defmacro cf
  "Check a single form with an optional expected type."
  ([form] `(cf* '~form nil nil))
  ([form expected] `(cf* '~form '~expected true)))

(defn check-ns-info
  "Check a Clojurescript namespace, or the current namespace.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see check-ns."
  ([]
   (load-if-needed)
   (check-ns-info ((impl/v 'clojure.core.typed.util-cljs/cljs-ns))))
  ([ns-or-syms & opt]
   (load-if-needed)
   (apply (impl/v 'clojure.core.typed.check-ns-cljs/check-ns-info)
          ns-or-syms opt)))

(defn check-ns*
  "Check a Clojurescript namespace, or the current namespace.
  Intended to be called from Clojure. For evaluation at the Clojurescript
  REPL see check-ns."
  ([] 
   (load-if-needed)
   (check-ns* ((impl/v 'clojure.core.typed.util-cljs/cljs-ns))))
  ([ns-or-syms & opt]
   (load-if-needed)
   (apply (impl/v 'clojure.core.typed.check-ns-cljs/check-ns) 
          ns-or-syms opt)))

(defmacro check-ns
  "Check a Clojurescript namespace, or the current namespace. This macro
  is intended to be called at the Clojurescript REPL. For the equivalent function see
  check-ns*.
  
  The symbols *ns* and clojure.core/*ns* are special and refer to the current namespace. Useful if
  providing options for the current namespace."
  ([] 
   (load-if-needed)
   `(check-ns *ns*))
  ([ns-or-syms & args]
   (load-if-needed)
   (let [_ (when (and (list? ns-or-syms)
                      (#{'quote} (first ns-or-syms)))
             (err/int-error "check-ns is a macro, do not quote the first argument"))
         ns-or-syms (if ('#{*ns* clojure.core/*ns*} ns-or-syms)
                      (impl/v 'cljs.analyzer/*cljs-ns*)
                      ns-or-syms)]
     `~(apply check-ns* ns-or-syms args))))

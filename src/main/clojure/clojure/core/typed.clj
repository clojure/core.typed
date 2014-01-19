(ns 
  ^{:doc "This namespace contains typed wrapper macros, type aliases
and functions for type checking Clojure code. check-ns is the interface
for checking namespaces, cf for checking individual forms."}
  clojure.core.typed
  (:require [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.profiling :as p]
            [clojure.java.io :as io])
  (:refer-clojure :exclude [type]))

;=============================================================
; # core.typed
;
; This is the main namespace for core.typed. This project is
; split into many internal namespaces. Here are some of the main ones:
;
; c.c.typed.base-env
;   The base global type environment. All base Var Annotations,
;   Java method annotations, Class overriding and other annotations
;   live here.
;
; c.c.typed.type-{rep,ctors}, c.c.parse-unparse,
; c.c.typed.fold-{rep,default}
;   Internal type representation and operations.
;   
; c.c.typed.check
;   The type checker.
;
; c.c.typed.cs-gen
;   Polymorphic local type inference algorithm.

; ## Base alias vars
;
; Type aliases are resolved using possibly-unbound Vars. We get around
; having to load c.c.typed.base-env by interning each alias' Var in c.c.typed
; here. This way the user can refer the base alias Vars without necessarily loading
; all of core.typed.
;
; ### Consistency
;
; `-base-aliases` is ensured not to get out of date.
; c.c.typed.base-env asserts that -base-aliases is kept in sync with the
; actual set of aliases defined in the c.c.typed namespace. ie. adding
; a new alias in c.c.typed.base-env without updating `-base-aliases` throws an internal error.
;
; ### Visibility level
;
; `-base-aliases` is not private because it is used from c.c.typed.base-env.

(def ^:skip-wiki
  -base-aliases
  "Internal use only."
  '#{Option AnyInteger Int Num Atom1 Id Coll NonEmptyColl Vec NonEmptyVec
     Map Set SortedSet Seqable NonEmptySeqable EmptySeqable Seq NonEmptySeq EmptyCount NonEmptyCount
     NonEmptyLazySeq Hierarchy NilableNonEmptySeq Nilable Var1 Ref1 Keyword Symbol
     Future Promise})

(doseq [v -base-aliases]
  (intern 'clojure.core.typed v))

;=============================================================
; Query functions

; Usually query functions need to force core.typed to fully load.
; To be as lazy as possible, we use `ns-resolve` to grab the Vars
; we need.

(declare load-if-needed)

;(ann method-type [Symbol -> nil])
(defn method-type
  "Given a method symbol, print the core.typed types assigned to it.
  Intended for use at the REPL."
  [mname]
  (load-if-needed)
  (let [type-reflect (impl/v 'clojure.reflect/type-reflect)
        unparse-type (impl/v 'clojure.core.typed.parse-unparse/unparse-type)
        Method->Type (impl/v 'clojure.core.typed.check/Method->Type)
        ms (->> (type-reflect (Class/forName (namespace mname)))
             :members
             (filter #(and (instance? clojure.reflect.Method %)
                           (= (str (:name %)) (name mname))))
             set)
        _ (assert (seq ms) (str "Method " mname " not found"))]
    (println "Method name:" mname)
    (flush)
    (doseq [m ms]
      (println (unparse-type
                 (Method->Type m)))
      (flush))))


;=============================================================
; Special functions

(defn print-filterset
  "During type checking, print the filter set attached to form, 
  preceeded by literal string debug-string.
  Returns nil.
  
  eg. (let [s (seq (get-a-seqable))]
        (print-filterset \"Here now\" s))"
  [debug-string frm]
  frm)

(defn ^:skip-wiki
  inst-poly 
  "Internal use only. Use inst."
  [inst-of types-syn]
  inst-of)

(defn ^:skip-wiki
  inst-poly-ctor 
  "Internal use only. Use inst-ctor"
  [inst-of types-syn]
  inst-of)

(defmacro inst 
  "Instantiate a polymorphic type with a number of types.
  
  eg. (inst foo-fn t1 t2 t3 ...)"
  [inst-of & types]
  `(inst-poly ~inst-of '~types))

(defmacro inst-ctor
  "Instantiate a call to a constructor with a number of types.
  First argument must be an immediate call to a constructor.
  Returns exactly the instantiatee (the first argument).
  
  eg. (inst-ctor (PolyCtor. a b c)
                 t1 t2 ...)"
  [inst-of & types]
  `(inst-poly-ctor ~inst-of '~types))

(defn ^:skip-wiki
  fn>-ann 
  "Internal use only. Use fn>."
  [fn-of param-types-syn]
  fn-of)

(defn ^:skip-wiki
  pfn>-ann 
  "Internal use only. Use pfn>."
  [fn-of polys param-types-syn]
  fn-of)

(defn ^:skip-wiki
  loop>-ann 
  "Internal use only. Use loop>"
  [loop-of bnding-types]
  loop-of)

(defmacro dotimes>
  "Like dotimes.
  
  eg. (dotimes> [_ 100]
        (println \"like normal\"))"
  [bindings & body]
  (@#'clojure.core/assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# (long ~n)]
       (loop> [~i :- (~'U Long Integer) 0]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))

(defmacro for>
  "Like for but requires annotation for each loop variable: [a [1 2]] becomes [a :- Long [1 2]]
  Also requires annotation for return type.
  
  eg. (for> :- Number
        [a :- (U nil AnyInteger) [1 nil 2 3]
         :when a]
        (inc a))"
  [tk ret-ann seq-exprs body-expr]
  (@#'clojure.core/assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (assert (#{:-} tk) "Must provide return type annotation for for>.")
  (let [normalise-args
        ; change [a :- b c] to [[a :- b] c]
        (fn [seq-exprs]
          (loop [flat-result ()
                 seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result
              (keyword? (first seq-exprs)) (recur (concat flat-result (take 2 seq-exprs))
                                                  (drop 2 seq-exprs))
              (and (vector? (first seq-exprs))
                   (#{:-} (-> seq-exprs first second))) (do
                                                          (prn "DEPRECATED WARNING: for> syntax has changed, use [b :- t i] for clauses")
                                                          (recur (concat flat-result (take 2 seq-exprs))
                                                                 (drop 2 seq-exprs)))
              :else (do (assert (#{:-} (second seq-exprs))
                                "Incorrect syntax in for>.")
                        (recur (concat flat-result [(vec (take 3 seq-exprs))
                                                    (nth seq-exprs 3)])
                               (drop 4 seq-exprs))))))

        ; normalise seq-exprs to be flat pairs
        seq-exprs (normalise-args seq-exprs)

        to-groups (fn [seq-exprs]
                    (@#'clojure.core/reduce1 (fn [groups [k v]]
                                               (if (keyword? k)
                                                 (conj (pop groups) (conj (peek groups) [k v]))
                                                 (conj groups [k v])))
                                             [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (IllegalArgumentException. ^String (apply str msg))))
        emit-bind (fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (let [_ (assert (and (vector? bind)
                                         (#{3} (count bind))
                                         (#{:-} (second bind))) 
                                    "Binder must be of the form [lhs :- type]")
                          bind-ann (nth bind 2)
                          bind (nth bind 0)
                          giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                      `(let [iterys# ~(emit-bind next-groups)
                                             fs# (seq (iterys# ~next-expr))]
                                         (if fs#
                                           (concat fs# (~giter (rest ~gxs)))
                                           (recur (rest ~gxs))))
                                     :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))]
                      (if next-groups
                        #_"not the inner-most loop"
                        `(ann-form
                           (fn ~giter [~gxs]
                             (lazy-seq
                               (loop> [~gxs :- (~'clojure.core.typed/Option (~'clojure.lang.Seqable ~bind-ann)) ~gxs]
                                 (when-first [~bind ~gxs]
                                   ~(do-mod mod-pairs)))))
                           [(~'clojure.core.typed/Option (~'clojure.lang.Seqable ~bind-ann)) ~'-> (~'clojure.core.typed/Seq ~ret-ann)])
                        #_"inner-most loop"
                        (let [gi (gensym "i__")
                              gb (gensym "b__")
                              do-cmod (fn do-cmod [[[k v :as pair] & etc]]
                                        (cond
                                          (= k :let) `(let ~v ~(do-cmod etc))
                                          (= k :while) `(when ~v ~(do-cmod etc))
                                          (= k :when) `(if ~v
                                                         ~(do-cmod etc)
                                                         (recur
                                                           (unchecked-inc ~gi)))
                                          (keyword? k)
                                            (err "Invalid 'for' keyword " k)
                                          :else
                                            `(do (chunk-append ~gb 
                                                               ; put an ann-form here so at least one error message
                                                               ; points to code the user can recognise.
                                                               (ann-form ~body-expr
                                                                         ~ret-ann))
                                                 (recur (unchecked-inc ~gi)))))]
                          `(ann-form
                             (fn ~giter [~gxs]
                               (lazy-seq
                                 (loop> [~gxs :- (~'clojure.core.typed/Option (~'clojure.lang.Seqable ~bind-ann)) ~gxs]
                                        (when-let [~gxs (seq ~gxs)]
                                          (if (chunked-seq? ~gxs)
                                            (let [c# (chunk-first ~gxs)
                                                  size# (int (count c#))
                                                  ~gb (ann-form (chunk-buffer size#)
                                                                (~'clojure.lang.ChunkBuffer ~ret-ann))]
                                              (if (loop> [~gi :- (~'U ~'Long ~'Integer) (int 0)]
                                                         (if (< ~gi size#)
                                                           (let [;~bind (.nth c# ~gi)]
                                                                 ~bind (nth c# ~gi)]
                                                             ~(do-cmod mod-pairs))
                                                           true))
                                                (chunk-cons
                                                  (chunk ~gb)
                                                  (~giter (chunk-rest ~gxs)))
                                                (chunk-cons (chunk ~gb) nil)))
                                            (let [~bind (first ~gxs)]
                                              ~(do-mod mod-pairs)))))))
                             [(~'clojure.core.typed/Option (~'clojure.lang.Seqable ~bind-ann)) ~'->
                              (~'clojure.core.typed/Seq ~ret-ann)])))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
        (iter# ~(second seq-exprs)))))

(defmacro doseq>
  "Like doseq but requires annotation for each loop variable: 
  [a [1 2]] becomes [a :- Long [1 2]]
  
  eg.
  (doseq> [a :- (U nil AnyInteger) [1 nil 2 3]
           :when a]
     (inc a))"
  [seq-exprs & body]
  (@#'clojure.core/assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [normalise-args
        ; change [a :- b c] to [[a :- b] c]
        (fn [seq-exprs]
          (loop [flat-result ()
                 seq-exprs seq-exprs]
            (cond
              (empty? seq-exprs) flat-result
              (keyword? (first seq-exprs)) (recur (concat flat-result (take 2 seq-exprs))
                                                  (drop 2 seq-exprs))
              (and (vector? (first seq-exprs))
                   (#{:-} (-> seq-exprs first second))) (do
                                                          (prn "DEPRECATED WARNING: doseq> syntax has changed, use [b :- t i] for clauses")
                                                          (recur (concat flat-result (take 2 seq-exprs))
                                                                 (drop 2 seq-exprs)))
              :else (do (assert (#{:-} (second seq-exprs))
                                "Incorrect syntax in doseq>")
                        (recur (concat flat-result [(vec (take 3 seq-exprs))
                                                    (nth seq-exprs 3)])
                               (drop 4 seq-exprs))))))

        ; normalise seq-exprs to be flat pairs
        seq-exprs (normalise-args seq-exprs)
        step (fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (let [k (first exprs)
                       v (second exprs)]
                   (if (keyword? k)
                     (let [steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)]
                       (cond
                         (= k :let) [needrec `(let ~v ~subform)]
                         (= k :while) [false `(when ~v
                                                ~subform
                                                ~@(when needrec [recform]))]
                         (= k :when) [false `(if ~v
                                               (do
                                                 ~subform
                                                 ~@(when needrec [recform]))
                                               ~recform)]))
                     ;; k is [k :- k-ann]
                     (let [_ (assert (and (vector? k)
                                          (#{3} (count k))
                                          (#{:-} (second k))) 
                                     "Binder must be of the form [lhs :- type]")
                           k-ann (nth k 2)
                           k (nth k 0)
                           ; k is the lhs binding
                           seq- (gensym "seq_")
                           chunk- (with-meta (gensym "chunk_")
                                             {:tag 'clojure.lang.IChunk})
                           count- (gensym "count_")
                           i- (gensym "i_")
                           recform `(recur (next ~seq-) nil 0 0)
                           steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)
                           recform-chunk 
                             `(recur ~seq- ~chunk- ~count- (unchecked-inc ~i-))
                           steppair-chunk (step recform-chunk (nnext exprs))
                           subform-chunk (steppair-chunk 1)]
                       [true
                        `(loop> [~seq- :- (~'U nil (~'clojure.core.typed/Seq ~k-ann)) (seq ~v), 
                                 ~chunk- :- (~'U nil (~'clojure.lang.IChunk ~k-ann)) nil
                                 ~count- :- ~'(U Integer Long) 0,
                                 ~i- :- ~'(U Integer Long) 0]
                           (if (and (< ~i- ~count-)
                                    ;; FIXME review this
                                    ;; core.typed thinks chunk- could be nil here
                                    ~chunk-)
                             (let [;~k (.nth ~chunk- ~i-)
                                   ~k (nth ~chunk- ~i-)]
                               ~subform-chunk
                               ~@(when needrec [recform-chunk]))
                             (when-let [~seq- (seq ~seq-)]
                               (if (chunked-seq? ~seq-)
                                 (let [c# (chunk-first ~seq-)]
                                   (recur (chunk-rest ~seq-) c#
                                          (int (count c#)) (int 0)))
                                 (let [~k (first ~seq-)]
                                   ~subform
                                   ~@(when needrec [recform]))))))])))))]
    (nth (step nil (seq seq-exprs)) 1)))

;(ann parse-fn> [Any (Seqable Any) ->
;                '{:poly Any
;                  :fn Any ;Form
;                  :parsed-methods (Seqable '{:dom-syntax (Seqable Any)
;                                             :dom-lhs (Seqable Any)
;                                             :rng-syntax Any
;                                             :has-rng? Any
;                                             :body Any})}])
;for
(defn- parse-fn>
  "(fn> name? :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [is-poly forms]
  (let [name (when (symbol? (first forms))
               (first forms))
        forms (if name (rest forms) forms)
        poly (when is-poly
               (first forms))
        forms (if poly (rest forms) forms)
        methods (if ((some-fn vector? keyword?) (first forms))
                  (list forms)
                  forms)
        ; turn [param :- type* & param :- type *?]
        ; into [[param :- type]* & [param :- type *]?]
        normalise-args
        (fn [arg-anns]
          (loop [flat-result ()
                 seq-exprs arg-anns]
            (cond
              (empty? seq-exprs) flat-result
              (and (#{'&} (first seq-exprs))
                   ; new syntax
                   (#{:-} (nth seq-exprs 2)))
              (do
                (assert (#{'*} (nth seq-exprs 4)))
                (assert (#{:-} (nth seq-exprs 2)))
                (assert (empty? (drop 5 seq-exprs)))
                (recur (concat flat-result ['& (vec (take 4 (next seq-exprs)))])
                       (drop 4 seq-exprs)))
              ;old syntax
              (#{'&} (first seq-exprs))
              (do 
                (assert (#{2} (count seq-exprs)))
                (prn "DEPRECATED WARNING: fn> syntax has changed, use [& b :- t i *] for rest arguments"
                     "ns: " *ns*)
                (recur (concat flat-result ['& (second seq-exprs)])
                       (drop 1 seq-exprs)))
              (and (vector? (first seq-exprs))
                   (#{:-} (-> seq-exprs first second))) (do
                                                          (prn "DEPRECATED WARNING: fn> syntax has changed, use [b :- t i] for clauses"
                                                               "ns: " *ns*)
                                                          (recur (concat flat-result (take 1 seq-exprs))
                                                                 (drop 1 seq-exprs)))
              :else (do (assert (#{:-} (second seq-exprs))
                                "Incorrect syntax in fn>.")
                        (recur (concat flat-result [(vec (take 3 seq-exprs))])
                               (drop 3 seq-exprs))))))
        ;(fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
        ; (HMap {:dom (Seqable TypeSyntax)
        ;        :rng (U nil TypeSyntax)
        ;        :body Any})
        parsed-methods (doall 
                         (for [method methods]
                           (let [[ret has-ret?] (when (not (vector? (first method)))
                                                  (assert (= :- (first method))
                                                          "Return type for fn> must be prefixed by :-")
                                                  [(second method) true])
                                 method (if ret 
                                          (nnext method)
                                          method)
                                 body (rest method)
                                 arg-anns (normalise-args (first method))
                                 [required-params _ [rest-param]] (split-with #(not= '& %) arg-anns)]
                             (assert (sequential? required-params)
                                     "Must provide a sequence of typed parameters to fn>")
                             (assert (not rest-param) "fn> doesn't support rest parameters yet")
                             {:dom-syntax (doall (map (comp second next) required-params))
                              :dom-lhs (doall (map first required-params))
                              :rng-syntax ret
                              :has-rng? has-ret?
                              :body body})))]
    {:poly poly
     :fn `(fn ~@(concat
                  (when name
                    [name])
                  (for [{:keys [body dom-lhs]} parsed-methods]
                    (apply list (vec dom-lhs) body))))
     :parsed-methods parsed-methods}))

(defmacro pfn> 
  "Define a polymorphic typed anonymous function.
  (pfn> name? [binder+] :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (pfn> name? [binder+] (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [& forms]
  (let [{:keys [poly fn parsed-methods]} (parse-fn> true forms)]
    `(pfn>-ann ~fn '~poly '~parsed-methods)))


(defmacro when-let-fail 
  "Like when-let, but fails if the binding yields a false value."
  [b & body]
  `(if-let ~b
     (do ~@body)
     (throw (ex-info (str "Expression was nil or false") {:form '~(second b)}))))

(defmacro 
  ^{:forms '[(fn> name? :- type? [param :- type* & param :- type * ?] exprs*)
             (fn> name? (:- type? [param :- type* & param :- type * ?] exprs*)+)]}
  fn> 
  "Like fn, but with annotations. Annotations are mandatory
  for parameters, with optional annotations for return type.
  If fn is named, return type annotation is mandatory.

  Suggested idiom: use commas between parameter annotation triples.

  eg. (fn> [a :- Number, b :- (U Symbol nil)] ...)

      ;annotate return
      (fn> :- String [a :- String] ...)

      ;named fn
      (fn> fname :- String [a :- String] ...)

      ;multi-arity
      (fn> fname 
        (:- String [a :- String] ...)
        (:- Long   [a :- String, b :- Number] ...))"
  [& forms]
  (let [{:keys [fn parsed-methods]} (parse-fn> false forms)]
    `(fn>-ann ~fn '~parsed-methods)))

(defn- defn>-parse-typesig 
  "Helper for parsing type signatures out of defn> forms"
  [forms]
  (if (= :- (first forms))
    (let [ret (second forms)
          args (take-nth 3 (drop 2 (first (drop 2 forms))))]
      `[~@args ~'-> ~ret])
    `(~'Fn ~@(map defn>-parse-typesig forms))))

(defn- take-when
  "When pred is true of the head of seq, return [head tail]. Otherwise
  [nil seq]. Used as a helper for parsing optinal typed elements out
  of sequences. Say docstrings out of argument seqs."
  [pred seq]
  (if (pred (first seq))
    ((juxt first rest) seq)
    [nil seq]))

(defmacro
  ^{:forms '[(defn> name docstring? :- type [param :- type *] exprs*)
             (defn> name docstring? (:- type [param :- type *] exprs*)+)]}
  defn>
  "Like defn, but with annotations. Annotations are mandatory for
  parameters and for return type.

  eg. (defn> fname :- Integer [a :- Number, b :- (U Symbol nil)] ...)

  ;annotate return
  (defn> :- String [a :- String] ...)

  ;named fn
  (defn> fname :- String [a :- String] ...)

  ;multi-arity
  (defn> fname 
    (:- String [a :- String] ...)
    (:- Long   [a :- String, b :- Number] ...))"
  [name & fdecl]
  (let [[docstring fdecl] (take-when string? fdecl)
        signature (defn>-parse-typesig fdecl)]
    `(do (ann ~name ~signature)
         ~(list* 'def name 
                 (concat
                   (when docstring [docstring])
                   [`(fn> ~name ~@fdecl)])))))

(defmacro
  ^{:forms '[(def> name docstring? :- type expr)]}
  def>
  "Like def, but with annotations.

  eg. (def> vname :- Long 1)

  ;doc
  (def> vname
    \"Docstring\"
    :- Long
    1)"
  [name & fdecl]
  (let [[docstring fdecl] (take-when string? fdecl)
        _ (assert (and (#{3} (count fdecl))
                       (#{:-} (first fdecl)))
                  (str "Bad def> syntax: " fdecl))
        [_ tsyn body] fdecl]
    `(do (ann ~name ~tsyn)
         ~(list* 'def name 
                 (concat
                   (when docstring [docstring])
                   [body])))))

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
    `(letfn ~(vec inits)
       ;unquoted to allow bindings to resolve with hygiene
       ~init-syn
       ;preserve letfn empty body
       nil
       ~@body)))


(defmacro defprotocol> [& body]
  "Like defprotocol, but required for type checking
  its macroexpansion.
  
  eg. (defprotocol> MyProtocol
        (a [this]))"
  `(tc-ignore
     (defprotocol ~@body)))

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
    `(loop>-ann (loop ~(vec (mapcat vector lhs rhs))
                  ~@forms)
                '~bnd-anns)))

(defn ^:skip-wiki
  declare-datatypes* 
  "Internal use only. Use declare-datatypes."
  [nms]
  nil)

(defmacro declare-datatypes 
  "Declare datatypes, similar to declare but on the type level."
  [& syms]
  `(declare-datatypes* '~syms))

(defn ^:skip-wiki
  declare-protocols* 
  "Internal use only. Use declare-protocols."
  [syms]
  nil)

(defmacro declare-protocols 
  "Declare protocols, similar to declare but on the type level."
  [& syms]
  `(declare-protocols* '~syms))

(defn ^:skip-wiki
  declare-alias-kind* 
  "Internal use only. Use declare-alias-kind."
  [sym ty]
  nil)

(defmacro declare-alias-kind
  "Declare a kind for an alias, similar to declare but on the kind level."
  [sym ty]
  `(do
     (declare ~sym)
     (declare-alias-kind* '~sym '~ty)))

(defn ^:skip-wiki
  declare-names* 
  "Internal use only. Use declare-names."
  [syms]
  nil)

(defmacro declare-names 
  "Declare names, similar to declare but on the type level."
  [& syms]
  `(declare-names* '~syms))

(defn ^:skip-wiki
  def-alias* 
  "Internal use only. Use def-alias."
  [sym type]
  nil)

(defmacro def-alias 
  "Define a type alias. Takes an optional doc-string as a second
  argument.

  Updates the corresponding var with documentation.
  
  eg. (def-alias MyAlias
        \"Here is my alias\"
        (U nil String))"
  ([sym doc-str t]
   (assert (string? doc-str) "Doc-string passed to def-alias must be a string")
   `(def-alias ~(vary-meta sym assoc :doc doc-str) ~t))
  ([sym t]
   (assert (symbol? sym) (str "First argument to def-alias must be a symbol: " sym))
   (let [qsym (if (namespace sym)
                sym
                (symbol (-> *ns* ns-name str) (str sym)))
         m (-> (meta sym)
             (update-in [:doc] #(str #_"Type Alias\n\n" % "\n\n" (with-out-str (pprint/pprint t)))))]
     `(do
        (intern '~(symbol (namespace qsym)) '~(symbol (name qsym)))
        (tc-ignore (alter-meta! (resolve '~qsym) merge '~m))
        (def-alias* '~qsym '~t)))))

(defn ^:skip-wiki
  ann-form* 
  "Internal use only. Use ann-form."
  [form ty]
  form)

(defmacro ann-form 
  "Annotate a form with an expected type."
  [form ty]
  `(ann-form* ~form '~ty))

;(ann unsafe-ann-form* [Any Any -> Any])
(defn- unsafe-ann-form* [form ty]
  form)

(defmacro ^:private unsafe-ann-form [form ty]
  `(unsafe-ann-form* ~form '~ty))

;(ann tc-ignore-forms* [Any -> Any])
(defn ^:skip-wiki
  tc-ignore-forms* 
  "Internal use only. Use tc-ignore"
  [r]
  r)

;; `do` is special at the top level
(defmacro tc-ignore 
  "Ignore forms in body during type checking"
  [& body]
  `(do ~@(map (fn [b] `(tc-ignore-forms* ~b)) body)))

;(ann into-array>* [Any Any -> Any])
(defn ^:skip-wiki
  into-array>*
  "Internal use only. Use into-array>."
  ([cljt coll]
   (load-if-needed)
   (let [parse-type @(ns-resolve (find-ns 'clojure.core.typed.parse-unparse)
                                 'parse-type)
         amc @(ns-resolve (find-ns 'clojure.core.typed.array-ops)
                          'Type->array-member-Class)]
     (impl/with-clojure-impl
       (into-array (-> cljt parse-type amc) coll))))
  ([javat cljt coll]
   (load-if-needed)
   (let [parse-type @(ns-resolve (find-ns 'clojure.core.typed.parse-unparse)
                                 'parse-type)
         amc @(ns-resolve (find-ns 'clojure.core.typed.array-ops)
                          'Type->array-member-Class)]
     (impl/with-clojure-impl
       (into-array (-> javat parse-type amc) coll))))
  ;this is the hacky case to prevent full core.typed from loading
  ([into-array-syn javat cljt coll]
   (into-array (resolve into-array-syn) coll)))

;FIXME hacky 4-arity version to prevent full type system from loading
(defmacro into-array> 
  "Make a Java array with Java class javat and Typed Clojure type
  cljt. Resulting array will be of type javat, but elements of coll must be under
  cljt. cljt should be a subtype of javat (the same or more specific).

  *Temporary hack*
  into-array-syn is exactly the syntax to put as the first argument to into-array.
  Calling resolve on this syntax should give the correct class."
  ([cljt coll]
   `(into-array>* '~cljt ~coll))
  ([javat cljt coll]
   `(into-array>* '~javat '~cljt ~coll))
  ([into-array-syn javat cljt coll]
   `(into-array>* '~javat '~cljt ~coll)))


(defn ^:skip-wiki
  non-nil-return* 
  "Internal use only. Use non-nil-return."
  [msym arities]
  nil)

(defmacro non-nil-return 
  "Override the return type of fully qualified method msym to be non-nil.
  Takes a set of relevant arities,
  represented by the number of parameters it takes (rest parameter counts as one),
  or :all which overrides all arities.
  
  eg. ; must use full class name
      (non-nil-return java.lang.Class/getDeclaredMethod :all)"
  [msym arities]
  `(non-nil-return* '~msym '~arities))

(defn ^:skip-wiki
  nilable-param* 
  "Internal use only. Use nilable-param."
  [msym mmap]
  nil)

(defmacro nilable-param 
  "Override which parameters in qualified method msym may accept
  nilable values. If the parameter is a parameterised type or
  an Array, this also declares the parameterised types and the Array type as nilable.

  mmap is a map mapping arity parameter number to a set of parameter
  positions (integers). If the map contains the key :all then this overrides
  other entries. The key can also be :all, which declares all parameters nilable."
  [msym mmap]
  `(nilable-param* '~msym '~mmap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(defn print-env 
  "During type checking, print the type environment to *out*,
  preceeded by literal string debug-str."
  [debug-str]
  nil)

(declare ann-collect-eval)

(defn ^:skip-wiki
  ann* 
  "Internal use only. Use ann."
  [varsym typesyn check?]
  (ann-collect-eval varsym typesyn check?)
  nil)

(defmacro ann 
  "Annotate varsym with type. If unqualified, qualify in the current namespace.
  If varsym has metadata {:no-check true}, ignore definitions of varsym 
  while type checking.

  If annotating vars in namespaces other than the current one, a fully
  qualified symbol must be provided. Note that namespace aliases are not
  recognised: the *full* namespace must be given in the first part of the symbol.
  
  eg. ; annotate the var foo in this namespace
      (ann foo [Number -> Number])
  
      ; annotate a var in another namespace
      (ann another.ns/bar [-> nil])
   
      ; don't check this var
      (ann ^:no-check foobar [Integer -> String])"
  [varsym typesyn]
  (let [qsym (if (namespace varsym)
               varsym
               (symbol (-> *ns* ns-name str) (str varsym)))
        _ (when (contains? (meta varsym) :nocheck)
            (println (str "DEPRECATED: :nocheck metadata for " varsym " ann. Use :no-check"))
            (flush))
        opts (meta varsym)
        _ (assert (not (and (contains? opts :nocheck)
                            (contains? opts :no-check)))
                  "Cannot provide both :nocheck and :no-check metadata to ann")
        check? (not (or (:no-check opts)
                        (:nocheck opts)))]
    `(ann* '~qsym '~typesyn '~check?)))

(defmacro ann-many
  "Annotate several vars with type t.

  eg. (ann-many FakeSearch
                web1 web2 image1 image2 video1 video2)"
  [t & vs]
  `(do ~@(map #(list `ann % t) vs)))

(declare ann-datatype-collect-eval)

(defonce ^:dynamic 
  ^{:doc 
  "If a true value, global annotations are collected by the
  type checker when their respective forms are evaluated (eg. ann)."}
  *collect-on-eval* 
  true)

(defn ^:skip-wiki
  ann-datatype*
  "Internal use only. Use ann-datatype."
  [vbnd dname fields opts]
  ; ensure that ann-datatype-collect-eval isn't called during the bootstrap
  (when *collect-on-eval*
    (ann-datatype-collect-eval vbnd dname fields opts))
  nil)

(defmacro
  ^{:forms '[(ann-datatype dname [field :- type*] opts*)
             (ann-datatype binder dname [field :- type*] opts*)]}
  ann-datatype
  "Annotate datatype Class name dname with expected fields.
  If unqualified, qualify in the current namespace.
  Takes an optional type variable binder before the name.

  Fields must be specified in the same order as presented 
  in deftype, with exactly the same field names.

  Also annotates datatype factories and constructors.

  Binder is a vector of specs. Each spec is a vector
  with the variable name as the first entry, followed by
  keyword arguments:
  - :variance (mandatory)
    The declared variance of the type variable. Possible
    values are :covariant, :contravariant and :invariant.
  - :< (optional)
    The upper type bound of the type variable. Defaults to
    Any, or the most general type of the same rank as the
    lower bound.
  - :> (optional)
    The lower type bound of the type variable. Defaults to
    Nothing, or the least general type of the same rank as the
    upper bound.

  eg. ; a datatype in the current namespace
      (ann-datatype MyDatatype [a :- Number,
                                b :- Long])

      ; a datatype in another namespace
      (ann-datatype another.ns.TheirDatatype
                    [str :- String,
                     vec :- (Vec Number)])

      ; a datatype, polymorphic in a
      (ann-datatype [[a :variance :covariant]]
                    MyPolyDatatype
                    [str :- String,
                     vec :- (Vec Number)])"
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

(defn ^:skip-wiki
  ann-pdatatype* 
  "Internal use only. Use ann-pdatatype."
  [dname vbnd fields opt]
  nil)

(defmacro ^:skip-wiki ann-pdatatype 
  "REMOVED OPERATION: ann-pdatatype, use ann-datatype"
  [dname vbnd fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
  (assert nil "REMOVED OPERATION: ann-pdatatype, use ann-datatype")
  (assert (not rplc) "Replace NYI")
  (assert (symbol? dname)
          (str "Must provide local symbol: " dname))
  `(ann-pdatatype* '~dname '~vbnd '~fields '~opt))

(declare ann-record-collect-eval)

(defn ^:skip-wiki
  ann-record* 
  "Internal use only. Use ann-record"
  [dname fields opt]
  ; ensure that ann-record-collect-eval isn't called during the bootstrap
  (when *collect-on-eval*
    (ann-record-collect-eval dname fields opt))
  nil)

(defmacro 
  ^{:forms '[(ann-record dname [field :- type*] opts*)
             (ann-record binder dname [field :- type*] opts*)]}
  ann-record 
  "Annotate record Class name dname with expected fields.
  If unqualified, qualify in the current namespace.
  Takes an optional type variable binder before the name.

  Fields must be specified in the same order as presented 
  in defrecord, with exactly the same field names.

  Also annotates record factories and constructors.

  Binder is a vector of specs. Each spec is a vector
  with the variable name as the first entry, followed by
  keyword arguments:
  - :variance (mandatory)
    The declared variance of the type variable. Possible
    values are :covariant, :contravariant and :invariant.
  - :< (optional)
    The upper type bound of the type variable. Defaults to
    Any, or the most general type of the same rank as the
    lower bound.
  - :> (optional)
    The lower type bound of the type variable. Defaults to
    Nothing, or the least general type of the same rank as the
    upper bound.
  
  eg. ; a record in the current namespace
      (ann-record MyRecord [a :- Number,
                            b :- Long])

      ; a record in another namespace
      (ann-record another.ns.TheirRecord
                    [str :- String,
                     vec :- (Vec Number)])

      ; a record, polymorphic in a
      (ann-record [[a :variance :covariant]]
                    MyPolyRecord
                    [str :- String,
                     vec :- (Vec Number)])"
  [& args]
  ;[dname fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
  (let [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [dname fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
        (if bnd-provided?
          (next args)
          args)]
    (if bnd-provided?
      ;reuse ann-precord for now
      `(ann-precord ~dname ~vbnd ~fields ~@opt)
      `(ann-record* '~dname '~fields '~opt))))

(defn ^:skip-wiki
  ann-precord* 
  "Internal use only. Use ann-precord."
  [dname vbnd fields opt]
  nil)

(defmacro ann-precord 
  "Annotate record Class name dname with a polymorphic binder and expected fields.
  If unqualified, qualify in the current namespace."
  [dname vbnd fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
  `(ann-precord* '~dname '~vbnd '~fields '~opt))

(defn ^:skip-wiki
  ann-protocol* 
  "Internal use only. Use ann-protocol."
  [vbnd varsym mth]
  nil)

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
      (defprotocol> IFoo
        (bar [this])
        (baz [this]))

      ; polymorphic protocol
      ; x is scoped in the methods
      (ann-protocol [[x :variance :covariant]]
        IFooPoly
        bar
        [(IFooPoly x) -> Any]
        baz
        [(IFooPoly x) -> Number])
      (defprotocol> IFooPoly
        (bar [this])
        (baz [this]))"
  [& args]
  (let [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [varsym & mth] (if bnd-provided?
                         (next args)
                         args)
        _ (let [fs (frequencies (map first (partition 2 mth)))]
            (when-let [dups (seq (filter (fn [[_ freq]] (< 1 freq)) fs))]
              (println (str "WARNING: Duplicate method annotations in ann-protocol (" varsym 
                            "): " (str/join ", " (map first dups))))
              (flush)))
        {:as mth} mth]
    `(ann-protocol* '~vbnd '~varsym '~mth)))

(defn ^:skip-wiki
  ann-pprotocol* 
  "Internal use only. Use ann-pprotocol."
  [varsym vbnd mth]
  nil)

(defmacro ^:skip-wiki ann-pprotocol  
  "UNSUPPPORTED OPERATION: ann-pprotocol, use ann-protocol with binder 
  as first argument, ie. before protocol name"
  [varsym vbnd & {:as mth}]
  (prn "UNSUPPPORTED OPERATION: ann-pprotocol, use ann-protocol with binder as first argument, ie. before protocol name")
  `(ann-pprotocol* '~varsym '~vbnd '~mth))

(defn ^:skip-wiki
  override-constructor* 
  "Internal use only. Use override-constructor."
  [ctorsym typesyn]
  nil)

(defmacro override-constructor 
  "Override all constructors for Class ctorsym with type."
  [ctorsym typesyn]
  `(override-constructor* '~ctorsym '~typesyn))

(defn ^:skip-wiki
  override-method* 
  "Internal use only. Use override-method."
  [methodsym typesyn]
  nil)

(defmacro override-method 
  "Override type for qualified method methodsym."
  [methodsym typesyn]
  `(override-method* '~methodsym '~typesyn))

(defn ^:skip-wiki
  typed-deps* 
  "Internal use only. Use typed-deps."
  [args]
  nil)

(defmacro typed-deps 
  "Declare namespaces which should be checked before the current namespace.
  Accepts any number of symbols. Only has effect via check-ns.
  
  eg. (typed-deps clojure.core.typed.holes
                  myns.types)"
  [& args]
  `(typed-deps* '~args))

;(defn unchecked-ns*
;  "Internal use only. Use unchecked-ns."
;  [])
;
;(defmacro unchecked-ns
;  "Declare this namespace to be unchecked. 
;  
;  This disables type collection and checking for the current namespace.
;  Useful when the namespace has a dependency on clojure.core.typed,
;  and therefore a candidate for automatically inferred type dependencies,
;  but should never be type checked.
;  
;  eg. (unchecked-ns)"
;  []
;  `(unchecked-ns*))

(defmacro atom>
  "Like atom, but creates an Atom1 of type t.
  
  Same as (atom (ann-form init t) args*)
  
  eg. (atom> Number 1)
      (atom> (Vec Any) [])"
  [t init & args]
  `(atom (ann-form ~init ~t) ~@args))

(defmacro ref>
  "Like ref, but creates a Ref1 of type t.
  
  Same as (ref (ann-form init t) args*)
  
  eg. (ref> Number 1)
      (ref> (Vec Any) [])"
  [t init & args]
  `(ref (ann-form ~init ~t) ~@args))


(defn ^:skip-wiki var>* [sym]
  (impl/the-var sym))

(defmacro var>
  "Like var, but resolves at runtime like ns-resolve and is understood by
  the type checker. sym must be fully qualified (without aliases).
  
  eg. (var> clojure.core/+)"
  [sym]
  `(var>* '~sym))

(defn ^:skip-wiki
  warn-on-unannotated-vars*
  "Internal use only. Use allow-unannotated-vars"
  []
  nil)

(defmacro warn-on-unannotated-vars
  "Allow unannotated vars in the current namespace. 
  
  Emits a warning instead of a type error when checking
  a def without a corresponding expected type.
  
  eg. (warn-on-unannotated-vars)"
  []
  `(warn-on-unannotated-vars*))

(declare check-form-info print-errors! ^:dynamic *currently-checking-clj*)

(defn check-form*
  "Takes a (quoted) form and optional expected type syntax and
  type checks the form. If expected is provided, type-provided?
  must be true."
  ([form] (check-form* form nil nil))
  ([form expected] (check-form* form expected true))
  ([form expected type-provided?]
   (load-if-needed)
   (let [unparse-TCResult-in-ns (impl/v 'clojure.core.typed.parse-unparse/unparse-TCResult-in-ns)
         {:keys [delayed-errors ret]} (check-form-info form 
                                                       :expected expected 
                                                       :type-provided? type-provided?)]
     (if-let [errors (seq delayed-errors)]
       (print-errors! errors)
       (impl/with-clojure-impl
         (binding [*currently-checking-clj* true]
           (unparse-TCResult-in-ns ret *ns*)))))))

; cf can pollute current type environment to allow REPL experimentation, 
; which is ok because check-ns resets it when called.
(defmacro cf
  "Takes a form and an optional expected type and
  returns a human-readable inferred type for that form.
  Throws an exception if type checking fails.

  Do not use cf inside a typed namespace. cf is intended to be
  used at the REPL or within a unit test. Note that testing for
  truthiness is not sufficient to unit test a call to cf, as nil
  and false are valid type syntax.

  cf preserves annotations from previous calls to check-ns or cf,
  and keeps any new ones collected during a cf. This is useful for
  debugging and experimentation. cf may be less strict than check-ns
  with type checker warnings.
  
  eg. (cf 1) 
      ;=> Long

      (cf #(inc %) [Number -> Number])
      ;=> [Number -> Number]"
   ([form] `(check-form* '~form))
   ([form expected] `(check-form* '~form '~expected)))

(declare ^:dynamic *verbose-forms*)

(defn ^:skip-wiki
  print-errors! 
  "Internal use only"
  [errors]
  {:pre [(seq errors)
         (every? #(instance? clojure.lang.ExceptionInfo %) errors)]}
  (binding [*out* *err*]
    (doseq [^Exception e errors]
      (let [{:keys [env] :as data} (ex-data e)]
        (print "Type Error ")
        (print (str "(" (-> env :ns :name) ":" (:line env) 
                    (when-let [col (:column env)]
                      (str ":" col))
                    ") "))
        (print (.getMessage e))
        (println)
        (flush)
        (let [[_ form :as has-form?] (find data :form)]
          (when has-form?
            (print "in: ")
            (binding [*print-length* (when-not *verbose-forms*
                                       6)
                      *print-level* (when-not *verbose-forms*
                                      4)]
              (println form))
            (println)
            (println)
            (flush)))
        (flush))))
  (throw (ex-info (str "Type Checker: Found " (count errors) " error" (when (< 1 (count errors)) "s"))
                  {:type-error :top-level-error
                   :errors errors})))

(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *already-collected* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *already-checked* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *currently-checking-clj* nil)
(defonce ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *delayed-errors* nil)

(defonce ^:dynamic 
  ^{:doc 
  "If true, print fully qualified types in error messages
  and return values. Bind around a type checking form like 
  cf or check-ns.
  
  eg. 
  (binding [*verbose-types* true] 
    (cf 1 Number))
  ;=> java.lang.Number"}
  *verbose-types* 
  nil)

(defonce ^:dynamic 
  ^{:doc 
  "If true, print complete forms in error messages. Bind
  around a type checking form like cf or check-ns.
  
  eg.
  (binding [*verbose-forms* true]
    (cf ['deep ['deep ['deep ['deep]]]] Number))
  ;=> <full form in error>"}
  *verbose-forms* 
  nil)

(defonce ^:dynamic
  *trace-checker*
  nil)

(defn ^:skip-wiki
  -init-delayed-errors 
  "Internal use only"
  []
  (atom [] :validator #(and (vector? %)
                            (every? (fn [a] 
                                      (instance? clojure.lang.ExceptionInfo a))
                                    %))))

(def ^:skip-wiki ^:private ^:dynamic *currently-loading* false)

(defn load-if-needed 
  "Load and initialize all of core.typed if not already"
  []
  (when-not *currently-loading*
    (binding [*collect-on-eval* false
              *currently-loading* true]
      (when-not (find-ns 'clojure.core.typed.init)
        (require 'clojure.core.typed.init))
      (let [init-ns (find-ns 'clojure.core.typed.init)]
        (assert init-ns)
        (when-not (@(ns-resolve init-ns 'loaded?))
          (println "Initializing core.typed ...")
          (flush)
          (time (@(ns-resolve init-ns 'load-impl)))
          (println "core.typed initialized.")
          (flush))))))


(defn reset-caches 
  "Reset internal type caches."
  []
  (p/p :typed/reset-caches
  (load-if-needed)
  (@(ns-resolve (find-ns 'clojure.core.typed.subtype) 'reset-subtype-cache))
  (@(ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'reset-Un-cache))
  (@(ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'reset-In-cache))
  (@(ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'reset-supers-cache!))
  (@(ns-resolve (find-ns 'clojure.core.typed.type-ctors) 'reset-RClass-of-cache!))
  (@(ns-resolve (find-ns 'clojure.core.typed.cs-gen) 'reset-dotted-var-store!))
  nil))

(defn check-form-info 
  "Alpha - subject to change

  Type checks a (quoted) form and returns a map of results from type checking the
  form.
  
  Options
  - :expected        Type syntax representing the expected type for this form
                     type-provided? option must be true to utilise the type.
  - :type-provided?  If true, use the expected type to check the form"
  [form & {:keys [expected type-provided?]}]
  (load-if-needed)
  (reset-caches)
  (let [check (impl/v 'clojure.core.typed.check/check)
        expr-type (impl/v 'clojure.core.typed.check/expr-type)
        ast-for-form (impl/v 'clojure.core.typed.analyze-clj/ast-for-form)
        collect (impl/v 'clojure.core.typed.collect-phase/collect)
        ret (impl/v 'clojure.core.typed.type-rep/ret)
        parse-type (impl/v 'clojure.core.typed.parse-unparse/parse-type)]
    (if *currently-checking-clj*
      (throw (Exception. "Found inner call to check-ns or cf"))
      (impl/with-clojure-impl
        (binding [*currently-checking-clj* true
                  *delayed-errors* (-init-delayed-errors)
                  *collect-on-eval* false]
          (let [expected (when type-provided?
                           (ret (parse-type expected)))
                ast (ast-for-form form)
                _ (collect ast)
                _ (reset-caches)
                c-ast (check ast expected)
                res (expr-type c-ast)]
            {:delayed-errors @*delayed-errors*
             :ret res}))))))

(defn check-ns-info
  "Alpha - subject to change

  Same as check-ns, but returns a map of results from type checking the
  namespace."
  ([] (check-ns-info *ns*))
  ([ns-or-syms & {:keys [collect-only trace]}]
   (let [start (. System (nanoTime))]
     (load-if-needed)
     (reset-caches)
     (let [reset-envs! @(ns-resolve (find-ns 'clojure.core.typed.reset-env)
                                    'reset-envs!)
           collect-ns @(ns-resolve (find-ns 'clojure.core.typed.collect-phase)
                                   'collect-ns)
           check-ns-and-deps @(ns-resolve (find-ns 'clojure.core.typed.check)
                                          'check-ns-and-deps)
           vars-with-unchecked-defs @(ns-resolve (find-ns 'clojure.core.typed.var-env)
                                                 'vars-with-unchecked-defs)
           uri-for-ns (impl/v 'clojure.jvm.tools.analyzer/uri-for-ns)
           
           nsym-coll (map #(if (symbol? %)
                             ; namespace might not exist yet, so ns-name is not appropriate
                             ; to convert to symbol
                             %
                             (ns-name %))
                          (if ((some-fn symbol? #(instance? clojure.lang.Namespace %))
                               ns-or-syms)
                            [ns-or-syms]
                            ns-or-syms))]
       (cond
         *currently-checking-clj* (throw (Exception. "Found inner call to check-ns or cf"))

         :else
         (binding [*currently-checking-clj* true
                   *delayed-errors* (-init-delayed-errors)
                   *already-collected* (atom #{})
                   *already-checked* (atom #{})
                   *trace-checker* trace
                   *collect-on-eval* false]
           (reset-envs!)
           (impl/with-clojure-impl
             ;; collect
             (let [collect-start (. System (nanoTime))
                   _ (doseq [nsym nsym-coll]
                       (collect-ns nsym))
                   ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                   collected @*already-collected*]
               (println "Collected" (count collected) "namespaces in" ms "msecs")
               (flush))
             ;(reset-caches)
             ;
             ;; check 
             (when-not collect-only
               (doseq [nsym nsym-coll]
                 (check-ns-and-deps nsym)))
             (let [vs (vars-with-unchecked-defs)]
               (binding [*out* *err*]
                 (doseq [v vs]
                   (println "WARNING: Type Checker: Definition missing:" v 
                            "\nHint: Use :no-check metadata with ann if this is an unchecked var")
                   (flush))))
             ;             (when-let [errors (seq @*delayed-errors*)]
             ;               (print-errors! errors))
             (let [ms (/ (double (- (. System (nanoTime)) start)) 1000000.0)
                   checked @*already-checked*
                   nlines (p/p :typed/line-count
                               (apply + (for [nsym checked]
                                          (with-open [rdr (io/reader (uri-for-ns nsym))]
                                            (count (line-seq rdr))))))]
               (println "Checked" (count checked) "namespaces (approx." nlines "lines) in" ms "msecs")
               (flush))
             {:delayed-errors @*delayed-errors*})))))))

(defn check-ns
  "Type check a namespace/s (a symbol or Namespace, or collection).
  If not provided default to current namespace.
  Returns a true value if type checking is successful, otherwise
  throws an Exception.

  Do not use check-ns within a checked namespace.
  It is intended to be used at the REPL or within a unit test.
  Suggested idiom for clojure.test: (is (check-ns 'your.ns))

  check-ns resets annotations collected from 
  previous check-ns calls or cf. A successful check-ns call will
  preserve any type annotations collect during that checking run.
  
  Keyword arguments:
  - :collect-only  if true, collect type annotations but don't type check code.
                   Useful for debugging purposes.

  If providing keyword arguments, the namespace to check must be provided
  as the first argument.

  Bind *verbose-types* to true to print fully qualified types.
  Bind *verbose-forms* to print full forms in error messages.
  
  eg. (check-ns 'myns.typed)
      ;=> :ok
     
      ; implicitly check current namespace
      (check-ns)
      ;=> :ok
  
      ; collect but don't check the current namespace
      (check-ns *ns* :collect-only true)"
  ([] (check-ns (ns-name *ns*)))
  ([ns-or-syms & {:keys [collect-only trace] :as kw}]
   (let [{:keys [delayed-errors]} (apply check-ns-info ns-or-syms (apply concat kw))]
     (if-let [errors (seq delayed-errors)]
       (print-errors! errors)
       :ok))))

; (ann all-defs-in-ns [Namespace -> (Set Symbol)])
(defn ^:private ^:no-wiki 
  all-defs-in-ns
  [ns]
  {:pre [(instance? clojure.lang.Namespace ns)]}
  (set
    (map #(symbol (str (ns-name ns)) (str %))
         (clojure.set/difference 
           (set (keys (ns-map ns))) 
           (set (keys (ns-refers ns))) 
           (set (keys (ns-imports ns)))))))

;(ann statistics [(U Symbol (Coll Symbol)) -> (Map Symbol Stats)])
(defn statistics 
  "Takes a collection of namespace symbols and returns a map mapping the namespace
  symbols to a map of data"
  [nsyms]
  (assert (and (coll? nsyms) (every? symbol? nsyms))
          "Must pass a collection of symbols to statistics")
  (reduce (fn [stats nsym]
            (let [_ (check-ns nsym :collect-only true)
                  ns (find-ns nsym)
                  _ (assert ns (str "Namespace " nsym " not found"))]
              (conj stats
                    [nsym
                     {:vars {:all-vars (all-defs-in-ns ns)
                             :no-checks (let [; deref the atom
                                              all-no-checks @(impl/v 'clojure.core.typed.var-env/CLJ-NOCHECK-VAR?)]
                                          (filter (fn [s] (= (namespace s) nsym)) all-no-checks))
                             :var-annotations (let [; deref the atom
                                                    annots @(impl/v 'clojure.core.typed.var-env/CLJ-VAR-ANNOTATIONS)]
                                                (->> annots
                                                     (filter (fn [[k v]] (= (namespace k) (str nsym))))
                                                     (map (fn [[k v]] [k (binding [*verbose-types* true]
                                                                           ((impl/v 'clojure.core.typed.parse-unparse/unparse-type)
                                                                            v))]))
                                                     (into {})))}}])))
          {} nsyms))

; (ann var-coverage [(Coll Symbol) -> nil])
(defn var-coverage 
  "Summarises annotated var coverage statistics to *out*
  for namespaces nsyms, a collection of symbols or a symbol/namespace.
  Defaults to the current namespace if no argument provided."
  ([] (var-coverage *ns*))
  ([nsyms-or-nsym]
   (assert (or (instance? clojure.lang.Namespace nsyms-or-nsym)
               (symbol? nsyms-or-nsym)
               (and (coll? nsyms-or-nsym) (every? symbol? nsyms-or-nsym)))
           "Must pass a collection of symbols or a symbol/namespace to var-coverage")
   (let [nsyms (if ((some-fn symbol? #(instance? clojure.lang.Namespace %))
                    nsyms-or-nsym)
                 [(ns-name nsyms-or-nsym)]
                 nsyms-or-nsym)
         stats (statistics nsyms)
         nall-vars (->> (vals stats) 
                        (map :vars) 
                        (map :all-vars)
                        (apply set/union)
                        set
                        count)
         nannotated-vars (->> (vals stats) 
                              (map :vars) 
                              (map :var-annotations) 
                              (map count)
                              (apply +))
         perc (if (zero? nall-vars)
                0
                (long (* (/ nannotated-vars nall-vars) 100)))]
     (println (str "Found " nannotated-vars " annotated vars out of " nall-vars " vars"))
     (println (str perc "% var annotation coverage"))
     (flush))))


(defn ^:private ^:skip-wiki collect-eval-form [frm]
  (load-if-needed)
  (when *collect-on-eval*
    (impl/with-clojure-impl
      (binding [*collect-on-eval* false]
        ((impl/v 'clojure.core.typed.collect-phase/collect-form)
         frm)))))

(defn ^:skip-wiki ^:private 
  ann-collect-eval [qsym typesyn check?]
  (when-not *compile-files*
    (load-if-needed)
    (collect-eval-form 
      `(ann* '~qsym '~typesyn '~check?))))

(defn ^:skip-wiki ^:private 
  ann-record-collect-eval [dname fields opt]
  (when-not *compile-files*
    (load-if-needed)
    (collect-eval-form
      `(ann-record* '~dname '~fields '~opt))))

(defn ^:skip-wiki ^:private 
  ann-datatype-collect-eval [vbnd dname fields opts]
  (when-not *compile-files*
    (load-if-needed)
    (collect-eval-form
      `(ann-datatype* '~vbnd '~dname '~fields '~opts))))

(comment 
  (check-ns 'clojure.core.typed.test.example)

  ; very slow because of update-composite
  (check-ns 'clojure.core.typed.test.rbt)

  (check-ns 'clojure.core.typed.test.macro)
  (check-ns 'clojure.core.typed.test.conduit)
  (check-ns 'clojure.core.typed.test.person)
  (check-ns 'clojure.core.typed.test.core-logic)
  (check-ns 'clojure.core.typed.test.ckanren)
  )

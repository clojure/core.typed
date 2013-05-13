(ns clojure.core.typed
  (:require [clojure.pprint :as pprint])
  (:refer-clojure :exclude [defrecord type]))

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

;=============================================================
; # Internal Loading

; ## Lazy Loading
;
; We handle lazy loading by moving all of c.c.typed's namespace
; dependencies to c.c.typed.init. We assume that
; if c.c.typed.init exists, then core.typed is fully loaded.
; Does this work with namespaces reloading tools like tools.namespace?
;
; By default, core.typed only loads:
; - c.c.typed macros (eg. `doseq>`, `for>`)
; - c.c.typed internal marker functions
;
; Code that requires anything further than this (eg. type checking) 
; must load *all* of core.typed with `load-if-needed`.

(defn load-if-needed 
  "Load and initialize all of core.typed if not already"
  []
  (when-not (find-ns 'clojure.core.typed.init)
    (println "Initializing core.typed ...")
    (flush)
    (require 'clojure.core.typed.init)
    (println "core.typed initialized.")
    (flush)))

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

(def -base-aliases
  "Internal use only."
  '#{Option AnyInteger AnyPrimitive Atom1})

(doseq [v -base-aliases]
  (intern 'clojure.core.typed v))

;=============================================================
; Query functions

; Usually query functions need to force core.typed to fully load.
; To be as lazy as possible, we use `ns-resolve` to grab the Vars
; we need.

;(ann method-type [Symbol -> nil])
(defn method-type
  "Given a method symbol, print the core.typed types assigned to it.
  Intended for use at the REPL."
  [mname]
  (load-if-needed)
  (let [type-reflect @(ns-resolve (find-ns 'clojure.reflect) 
                                  'type-reflect)
        unparse-type @(ns-resolve (find-ns 'clojure.core.typed.parse-unparse)
                                  'unparse-type)
        Method->Type @(ns-resolve (find-ns 'clojure.core.typed.check)
                                  'Method->Type)
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
  preceeded by literal string debug-string"
  [debug-string frm]
  frm)

(defn inst-poly 
  "Internal use only. Use inst."
  [inst-of types-syn]
  inst-of)

(defn inst-poly-ctor 
  "Internal use only. Use inst-ctor"
  [inst-of types-syn]
  inst-of)

(defmacro inst 
  "Instantiate a polymorphic type with a number of types"
  [inst-of & types]
  `(inst-poly ~inst-of '~types))

(defmacro inst-ctor
  "Instantiate a call to a constructor with a number of types.
  First argument must be an immediate call to a constructor."
  [inst-of & types]
  `(inst-poly-ctor ~inst-of '~types))

(defn fn>-ann 
  "Internal use only. Use fn>."
  [fn-of param-types-syn]
  fn-of)

(defn pfn>-ann 
  "Internal use only. Use pfn>."
  [fn-of polys param-types-syn]
  fn-of)

(defn loop>-ann 
  "Internal use only. Use loop>"
  [loop-of bnding-types]
  loop-of)

(defmacro dotimes>
  "Like dotimes."
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
  
  eg.
  (for> :- Number
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
                           [(~'clojure.core.typed/Option (~'clojure.lang.Seqable ~bind-ann)) ~'-> (~'clojure.lang.LazySeq ~ret-ann)])
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
                              (~'clojure.lang.LazySeq ~ret-ann)])))))]
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
                        `(loop> [~seq- :- (~'U nil (~'clojure.lang.Seqable ~k-ann)) (seq ~v), 
                                 ~chunk- :- (~'U nil (~'clojure.lang.IChunk ~k-ann)) nil
                                 ~count- :- ~'(U Integer Long) 0,
                                 ~i- :- ~'(U Integer Long) 0]
                           (if (and (< ~i- ~count-)
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

(defmacro fn> 
  "Define a typed anonymous function.
  (fn> name? :- type? [[param :- type]* & [param :- type *]?] exprs*)
  (fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [& forms]
  (let [{:keys [fn parsed-methods]} (parse-fn> false forms)]
    `(fn>-ann ~fn '~parsed-methods)))

(defmacro letfn> 
  "Like letfn, but each binding must be annotated.

  eg.
  (letfn> [a :- [Number -> Number]
           (a [b] 2)
           c :- [Symbol -> nil]
           (c [s] nil)]
    ...)"
  [bindings & body]
  (let [; (Vector (U '[Symbol TypeSyn] LetFnInit))
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
  "Define a typed protocol. Syntax like defprotocol."
  `(tc-ignore
     (defprotocol ~@body)))

(defmacro loop>
  "Like loop, except loop variables require annotation.

  eg. (loop> [a :- Number 1
              b :- (U nil Number) nil]
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

(defn declare-datatypes* 
  "Internal use only. Use declare-datatypes."
  [nms]
  nil)

(defmacro declare-datatypes 
  "Declare datatypes, similar to declare but on the type level."
  [& syms]
  `(declare-datatypes* '~syms))

(defn declare-protocols* 
  "Internal use only. Use declare-protocols."
  [syms]
  nil)

(defmacro declare-protocols 
  "Declare protocols, similar to declare but on the type level."
  [& syms]
  `(declare-protocols* '~syms))

(defn declare-alias-kind* 
  "Internal use only. Use declare-alias-kind."
  [sym ty]
  nil)

(defmacro declare-alias-kind
  "Declare a kind for an alias, similar to declare but on the kind level."
  [sym ty]
  `(do
     (declare ~sym)
     (declare-alias-kind* '~sym '~ty)))

(defn declare-names* 
  "Internal use only. Use declare-names."
  [syms]
  nil)

(defmacro declare-names 
  "Declare names, similar to declare but on the type level."
  [& syms]
  `(declare-names* '~syms))

(defn def-alias* 
  "Internal use only. Use def-alias."
  [sym type]
  nil)

(defmacro def-alias 
  "Define a type alias. Takes an optional doc-string as a second
  argument."
  ([sym doc-str type]
   (assert (string? doc-str) "Doc-string passed to def-alias must be a string")
   `(def-alias ~(vary-meta sym assoc :doc doc-str) ~type))
  ([sym type]
   (assert (symbol? sym) (str "First argument to def-alias must be a symbol: " sym))
   (let [qsym (if (namespace sym)
                sym
                (symbol (-> *ns* ns-name str) (str sym)))
         m (-> (meta sym)
             (update-in [:doc] #(str % "\n\n" (with-out-str (pprint/pprint type)))))]
     `(do
        (intern '~(symbol (namespace qsym)) '~(symbol (name qsym)))
        (tc-ignore (alter-meta! (resolve '~qsym) merge '~m))
        (def-alias* '~qsym '~type)))))

;(ann into-array>* [Any Any -> Any])
(defn into-array>*
  "Internal use only. Use into-array>."
  ([cljt coll]
   (load-if-needed)
   (let [pt @(ns-resolve (find-ns 'clojure.core.typed.parse-unparse)
                         'parse-type)
         amc @(ns-resolve (find-ns 'clojure.core.typed.array-ops)
                          'Type->array-member-Class)]
     (into-array (-> cljt pt amc) coll)))
  ([javat cljt coll]
   (load-if-needed)
   (let [pt @(ns-resolve (find-ns 'clojure.core.typed.parse-unparse)
                         'parse-type)
         amc @(ns-resolve (find-ns 'clojure.core.typed.array-ops)
                          'Type->array-member-Class)]
     (into-array (-> javat pt amc) coll)))
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

(defn ann-form* 
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
(defn tc-ignore-forms* 
  "Internal use only. Use tc-ignore"
  [r]
  r)

;; `do` is special at the top level
(defmacro tc-ignore 
  "Ignore forms in body during type checking"
  [& body]
  `(do ~@(map (fn [b] `(tc-ignore-forms* ~b)) body)))

(defn non-nil-return* 
  "Internal use only. Use non-nil-return."
  [msym arities]
  nil)

(defmacro non-nil-return 
  "Override the return type of qualified method msym to be non-nil.
  Takes a set of relevant arities,
  represented by the number of parameters it takes (rest parameter counts as one),
  or :all which overrides all arities.
  
  eg.  (non-nil-return java.lang.Class/getDeclaredMethod :all)"
  [msym arities]
  `(non-nil-return* '~msym '~arities))

(defn nilable-param* 
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

(defn ann* 
  "Internal use only. Use ann."
  [varsym typesyn check?]
  nil)

(defmacro ann 
  "Annotate varsym with type. If unqualified, qualify in the current namespace.
  If varsym has metadata {:nocheck true}, ignore definitions of varsym while type checking."
  [varsym typesyn]
  (let [qsym (if (namespace varsym)
               varsym
               (symbol (-> *ns* ns-name str) (str varsym)))]
    `(ann* '~qsym '~typesyn '~(-> varsym meta :nocheck not))))

(defn ann-datatype*
  "Internal use only. Use ann-datatype."
  [dname fields opts]
  nil)

(defmacro ann-datatype 
  "Annotate datatype Class name dname with expected fields.
  If unqualified, qualify in the current namespace."
  [dname fields & {ancests :unchecked-ancestors rplc :replace :as opts}]
  (assert (not rplc) "Replace NYI")
  (assert (symbol? dname)
          (str "Must provide name symbol: " dname))
  `(ann-datatype* '~dname '~fields '~opts))

(defn ann-pdatatype* 
  "Internal use only. Use ann-pdatatype."
  [dname vbnd fields opt]
  nil)

(defmacro ann-pdatatype 
  "Annotate datatype Class name dname with a polymorphic binder and expected fields.
  If unqualified, qualify in the current namespace."
  [dname vbnd fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
  (assert (not rplc) "Replace NYI")
  (assert (symbol? dname)
          (str "Must provide local symbol: " dname))
  `(ann-pdatatype* '~dname '~vbnd '~fields '~opt))

(defn ann-record* 
  "Internal use only. Use ann-record"
  [dname fields opt]
  nil)

(defmacro ann-record 
  "Annotate record Class name dname with expected fields.
  If unqualified, qualify in the current namespace."
  [dname fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
  `(ann-record* '~dname '~fields '~opt))

(defn ann-precord* 
  "Internal use only. Use ann-precord."
  [dname vbnd fields opt]
  nil)

(defmacro ann-precord 
  "Annotate record Class name dname with a polymorphic binder and expected fields.
  If unqualified, qualify in the current namespace."
  [dname vbnd fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
  `(ann-precord* '~dname '~vbnd '~fields '~opt))

(defn ann-protocol* 
  "Internal use only. Use ann-protocol."
  [varsym mth]
  nil)

(defmacro ann-protocol 
  "Annotate protocol var with method types.
  
  eg. (ann-protocol IFoo
        bar
        [IFoo -> Any]
        baz
        [IFoo -> Number])"
  [varsym & {:as mth}]
  `(ann-protocol* '~varsym '~mth))

(defn ann-pprotocol* 
  "Internal use only. Use ann-pprotocol."
  [varsym vbnd mth]
  nil)

(defmacro ann-pprotocol 
  "Annotate polymorphic protocol with a polymorphic binder and method types."
  [varsym vbnd & {:as mth}]
  `(ann-pprotocol* '~varsym '~vbnd '~mth))

(defn override-constructor* 
  "Internal use only. Use override-constructor."
  [ctorsym typesyn]
  nil)

(defmacro override-constructor 
  "Override all constructors for Class ctorsym with type."
  [ctorsym typesyn]
  `(override-constructor* '~ctorsym '~typesyn))

(defn override-method* 
  "Internal use only. Use override-method."
  [methodsym typesyn]
  nil)

(defmacro override-method 
  "Override type for qualified method methodsym."
  [methodsym typesyn]
  `(override-method* '~methodsym '~typesyn))

(defn typed-deps* 
  "Internal use only. Use typed-deps."
  [args]
  nil)

(defmacro typed-deps 
  "Declare namespaces which should be checked before the current namespace.
  Accepts any number of symbols."
  [& args]
  `(typed-deps* '~args))


; cf can pollute current type environment to allow REPL experimentation, 
; which is ok because check-ns resets it when called.
(defmacro cf
  "Type check a Clojure form and return its type"
  ([form]
   `(do
      (load-if-needed)
      (let [check# @(ns-resolve (find-ns '~'clojure.core.typed.check)
                                '~'check)
            expr-type# @(ns-resolve (find-ns '~'clojure.core.typed.check)
                                    '~'expr-type )
            unparse-TCResult-in-ns# @(ns-resolve (find-ns '~'clojure.core.typed.parse-unparse)
                                           '~'unparse-TCResult-in-ns)
            ensure-clojure# @(ns-resolve (find-ns '~'clojure.core.typed.current-impl)
                                         '~'ensure-clojure)
            ast-for-form# @(ns-resolve (find-ns '~'clojure.core.typed.analyze-clj)
                                       '~'ast-for-form)
            collect# @(ns-resolve (find-ns '~'clojure.core.typed.collect-phase)
                                  '~'collect)]
        (if *currently-checking-clj*
          (throw (Exception. "Found inner call to check-ns or cf"))
          (do (ensure-clojure#)
              (binding [*currently-checking-clj* true
                        *delayed-errors* (-init-delayed-errors)]
                (let [ast# (ast-for-form# '~form)
                      _# (collect# ast#)
                      cexpr# (check# ast#)]
                  (if-let [errors# (seq @*delayed-errors*)]
                    (print-errors! errors#)
                    (-> cexpr#
                        expr-type#
                        (unparse-TCResult-in-ns# *ns*))))))))))
   ([form expected]
   `(do
      (load-if-needed)
      (let [check# @(ns-resolve (find-ns '~'clojure.core.typed.check)
                                '~'check)
            expr-type# @(ns-resolve (find-ns '~'clojure.core.typed.check)
                                    '~'expr-type )
            unparse-TCResult-in-ns# @(ns-resolve (find-ns '~'clojure.core.typed.parse-unparse)
                                                 '~'unparse-TCResult-in-ns)
            ensure-clojure# @(ns-resolve (find-ns '~'clojure.core.typed.current-impl)
                                         '~'ensure-clojure)
            ast-for-form# @(ns-resolve (find-ns '~'clojure.core.typed.analyze-clj)
                                       '~'ast-for-form)
            collect# @(ns-resolve (find-ns '~'clojure.core.typed.collect-phase)
                                  '~'collect)
            ret# @(ns-resolve (find-ns '~'clojure.core.typed.type-rep)
                              '~'ret)
            parse-type# @(ns-resolve (find-ns 'clojure.core.typed.parse-unparse)
                                     '~'parse-type)]
      (if *currently-checking-clj*
        (throw (Exception. "Found inner call to check-ns or cf"))
        (do (ensure-clojure#)
            (binding [*currently-checking-clj* true
                      *delayed-errors* (-init-delayed-errors)]
              (let [ast# (ast-for-form# '(ann-form ~form ~expected))
                    _# (collect# ast#)
                    c-ast# (check# ast# 
                                   (ret#
                                     (parse-type# '~expected)))]
                (if-let [errors# (seq @*delayed-errors*)]
                  (print-errors! errors#)
                  (-> c-ast# 
                      expr-type# 
                      (unparse-TCResult-in-ns# *ns*)))))))))))

(declare ^:dynamic *verbose-forms*)

(defn print-errors! [errors]
  {:pre [(seq errors)
         (every? #(instance? clojure.lang.ExceptionInfo %) errors)]}
  (binding [*out* *err*]
    (doseq [^Exception e errors]
      (let [{:keys [env] :as data} (ex-data e)]
        (print "Type Error ")
        (print (str "(" (-> env :ns :name) ":" (:line env) 
                    (when-let [col (:column env)]
                      (str ":"col))
                    ") "))
        (print (.getMessage e))
        (println)
        (flush)
        (let [[_ form :as has-form?] (find data :form)]
          (when has-form?
            (binding [*print-length* (when-not *verbose-forms*
                                       4)
                      *print-level* (when-not *verbose-forms*
                                      2)]
              (print "in: ")
              (println form)
              (println)
              (println)
              (flush))))
        (flush))))
  (throw (ex-info (str "Type Checker: Found " (count errors) " errors")
                  {:type-error :top-level-error
                   :errors errors})))

(def ^:dynamic *already-collected*)
(def ^:dynamic *already-checked*)
(def ^:dynamic *currently-checking-clj* nil)
(def ^:dynamic *delayed-errors*)

(def ^:dynamic *verbose-types* 
  "If true, print fully qualified types in error messages
  and return values."
  nil)
(def ^:dynamic *verbose-forms* 
  "If true, print complete forms in error messages."
  nil)

(defn -init-delayed-errors 
  "Internal use only"
  []
  (atom [] :validator #(and (vector? %)
                            (every? (fn [a] 
                                      (instance? clojure.lang.ExceptionInfo a))
                                    %))))

(defn check-ns
  "Type check a namespace. If not provided default to current namespace.
  
  Bind *verbose-types* to true to print fully qualified types.
  Bind *verbose-forms* to print full forms in error messages."
  ([] (check-ns (ns-name *ns*)))
  ([nsym]
   (load-if-needed)
   (let [reset-envs! @(ns-resolve (find-ns 'clojure.core.typed.init)
                                  'reset-envs!)
         ensure-clojure @(ns-resolve (find-ns 'clojure.core.typed.current-impl)
                                     'ensure-clojure)
         collect-ns @(ns-resolve (find-ns 'clojure.core.typed.collect-phase)
                                 'collect-ns)
         check-ns-and-deps @(ns-resolve (find-ns 'clojure.core.typed.check)
                                        'check-ns-and-deps)
         vars-with-unchecked-defs @(ns-resolve (find-ns 'clojure.core.typed.var-env)
                                               'vars-with-unchecked-defs)]
   (reset-envs!)
   (cond
     *currently-checking-clj* (throw (Exception. "Found inner call to check-ns or cf"))

     :else
     (binding [*currently-checking-clj* true
               *delayed-errors* (-init-delayed-errors)
               *already-collected* (atom #{})
               *already-checked* (atom #{})]
       (ensure-clojure)
       (collect-ns nsym)
       (check-ns-and-deps nsym)
       (let [vs (vars-with-unchecked-defs)]
         (binding [*out* *err*]
           (doseq [v vs]
             (println "WARNING: Type Checker: Definition missing:" v)
             (flush))))
       (when-let [errors (seq @*delayed-errors*)]
         (print-errors! errors))
       :ok)))))

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

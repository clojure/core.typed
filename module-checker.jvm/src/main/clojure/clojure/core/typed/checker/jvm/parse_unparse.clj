;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.jvm.parse-unparse
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.name-env :as nme-env]
            [clojure.core.typed.checker.object-rep :as orep]
            [clojure.core.typed.checker.path-rep :as pthrep]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.dvar-env :as dvar]
            [clojure.core.typed.checker.filter-rep :as f]
            [clojure.core.typed.checker.filter-ops :as fl]
            [clojure.core.typed.checker.jvm.constant-type :as const]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.checker.indirect-utils :as indu]
            [clojure.core.typed.checker.indirect-ops :as ind]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.hset-utils :as hset]
            [clojure.set :as set]
            [clojure.math.combinatorics :as comb])
  (:import (clojure.core.typed.checker.type_rep NotType DifferenceType Intersection Union FnIntersection
                                        DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top TypeOf Unchecked TopFunction B F Result AnyValue
                                        KwArgsSeq TCError Extends JSNumber JSBoolean SymbolicClosure
                                        CLJSInteger ArrayCLJS JSNominal JSString TCResult AssocType
                                        GetType HSequential HSet JSUndefined JSNull JSSymbol JSObject
                                        JSObj)
           (clojure.core.typed.checker.filter_rep TopFilter BotFilter TypeFilter NotTypeFilter AndFilter OrFilter
                                          ImpFilter NoFilter)
           (clojure.core.typed.checker.object_rep NoObject EmptyObject Path)
           (clojure.core.typed.checker.path_rep KeyPE CountPE ClassPE KeysPE ValsPE NthPE KeywordPE)
           (clojure.lang Cons IPersistentList Symbol IPersistentVector)))

(defonce ^:dynamic *parse-type-in-ns* nil)
(set-validator! #'*parse-type-in-ns* (some-fn nil? symbol? con/namespace?))

(declare unparse-type unparse-filter unparse-filter-set unparse-flow-set unparse-object
         unparse-path-elem)

; Types print by unparsing them
(do (defmethod print-method clojure.core.typed.checker.impl_protocols.TCType [s writer]
      (print-method (unparse-type s) writer))
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.TCType clojure.lang.IRecord)
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.TCType java.util.Map)
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.TCType clojure.lang.IPersistentMap)

    (defmethod print-method clojure.core.typed.checker.impl_protocols.TCAnyType [s writer]
      (print-method (unparse-type s) writer))
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.TCAnyType clojure.lang.IRecord)
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.TCAnyType java.util.Map)
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.TCAnyType clojure.lang.IPersistentMap)

    (defmethod print-method clojure.core.typed.checker.impl_protocols.IFilter [s writer]
      (cond 
        (f/FilterSet? s) (print-method (unparse-filter-set s) writer)
        (r/FlowSet? s) (print-method (unparse-flow-set s) writer)
        :else (print-method (unparse-filter s) writer)))
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.IFilter clojure.lang.IRecord)
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.IFilter java.util.Map)
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.IFilter clojure.lang.IPersistentMap)

    (defmethod print-method clojure.core.typed.checker.impl_protocols.IRObject [s writer]
      (print-method (unparse-object s) writer))
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.IRObject clojure.lang.IRecord)
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.IRObject java.util.Map)
    (prefer-method print-method clojure.core.typed.checker.impl_protocols.IRObject clojure.lang.IPersistentMap)

    (defmethod print-method clojure.core.typed.checker.path_rep.IPathElem [s writer]
      (print-method (unparse-path-elem s) writer))
    (prefer-method print-method clojure.core.typed.checker.path_rep.IPathElem clojure.lang.IRecord)
    (prefer-method print-method clojure.core.typed.checker.path_rep.IPathElem java.util.Map)
    (prefer-method print-method clojure.core.typed.checker.path_rep.IPathElem clojure.lang.IPersistentMap)
    )

(defmacro with-parse-ns [sym & body]
  `(binding [*parse-type-in-ns* ~sym]
     ~@body))

(defn with-parse-ns* [sym f]
  {:pre [(symbol? sym)]}
  (binding [*parse-type-in-ns* sym]
    (f)))

(declare parse-type parse-type* resolve-type-clj resolve-type-cljs)

(defn parse-clj [s]
  (impl/with-clojure-impl
    (parse-type s)))

(defn parse-cljs [s]
  (impl/with-cljs-impl
    (parse-type s)))

(defn parse-type [s]
  (parse-type* s))

(defmulti parse-type* class)
(defmulti parse-type-list 
  (fn [[n]]
    {:post [((some-fn nil? symbol?) %)]}
    (when (symbol? n)
      (or (impl/impl-case
            :clojure (cond
                       (special-symbol? n) n
                       :else (let [r (resolve-type-clj n)]
                               (when (var? r)
                                 (coerce/var->symbol r))))
            :cljs (resolve-type-cljs n))
          n))))

(def parsed-free-map? (con/hmap-c? :fname symbol?
                                   :bnd r/Bounds?
                                   :variance r/variance?))

; parsing TFn, protocol, RClass binders
(defn ^:private parse-free-with-variance [f]
  {:post [(parsed-free-map? %)]}
  (if (symbol? f)
    {:fname f
     :bnd r/no-bounds
     :variance :invariant}
    (let [[n & {:keys [< > variance] :as opts}] f]
      (when (contains? opts :kind)
        (err/deprecated-warn "Kind annotation for TFn parameters"))
      (when-not (r/variance? variance)
        (err/int-error (str "Invalid variance " (pr-str variance) " in free binder: " f)))
      {:fname n 
       :bnd (let [upper-or-nil (when (contains? opts :<)
                                 (parse-type <))
                  lower-or-nil (when (contains? opts :>)
                                 (parse-type >))]
              (c/infer-bounds upper-or-nil lower-or-nil))
       :variance variance})))

(defn parse-free-binder-with-variance [binder]
  {:post [(every? parsed-free-map? %)]}
  (reduce (fn [fs fsyn]
            {:pre [(every? parsed-free-map? fs)]
             :post [(every? parsed-free-map? %)]}
            ;(prn "parse-free-binder-with-variance" (map :fname fs))
            (conj fs
                  (free-ops/with-bounded-frees 
                    (zipmap (map (comp r/make-F :fname) fs)
                            (map :bnd fs))
                    (parse-free-with-variance fsyn))))
          [] binder))

; parsing All binders
;return a vector of [name bnds]
(defn parse-free [f]
  {:post [((con/hvector-c? symbol? r/Bounds?) %)]}
  (let [validate-sym (fn [s]
                       (when-not (symbol? s)
                         (err/int-error (str "Type variable must be a symbol, given: " (pr-str s))))
                       (when (namespace s)
                         (err/int-error (str "Type variable must not be namespace qualified: " (pr-str s))))
                       (when (.contains (name s) ".")
                         (err/int-error (str "Type variable must not contain dots (.): " (pr-str s))))
                       (when (#{"true" "nil" "false"} (name s))
                         (err/int-error (str "Type variable must not be named true, false, or nil: " (pr-str s)))))]
    (if (symbol? f)
      (do (validate-sym f)
          [f r/no-bounds])
      (let [[n & {:keys [< >] :as opts}] f]
        (validate-sym n)
        (when (contains? opts :kind)
          (err/deprecated-warn "Kind annotation for TFn parameters"))
        (when (:variance opts) 
          (err/int-error "Variance not supported for variables introduced with All"))
        [n (let [upper-or-nil (when (contains? opts :<)
                                (parse-type <))
                 lower-or-nil (when (contains? opts :>)
                                (parse-type >))]
             (c/infer-bounds upper-or-nil lower-or-nil))]))))

(defn check-forbidden-rec [rec tbody]
  (letfn [(well-formed? [t]
            (and (not= rec t)
                 (if ((some-fn r/Intersection? r/Union?) t)
                   (every? well-formed? (:types t))
                   true)))]
    (when-not (well-formed? tbody)
      (err/int-error (str "Recursive type not allowed here")))))

(defn- Mu*-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.checker.type-ctors) 'Mu*)]
    (assert (var? v) "Mu* unbound")
    v))

(defn parse-rec-type [[rec & [[free-symbol :as bnder] type 
                              :as args]]]
  (when-not (== 1 (count bnder))
    (err/int-error "Rec type requires exactly one entry in binder"))
  (when-not (== 2 (count args))
    (err/int-error "Wrong arguments to Rec"))
  (let [Mu* @(Mu*-var)
        _ (when-not (= 1 (count bnder)) 
            (err/int-error "Only one variable allowed: Rec"))
        f (r/make-F free-symbol)
        body (free-ops/with-frees [f]
               (parse-type type))
        
        _ (check-forbidden-rec f body)]
    (Mu* (:name f) body)))

;(defmethod parse-type-list 'DottedPretype
;  [[_ psyn bsyn]]
;  (let [df (dvar/*dotted-scope* bsyn)]
;    (assert df bsyn)
;    (r/DottedPretype1-maker (free-ops/with-frees [df]
;                         (parse-type psyn))
;                       (:name (dvar/*dotted-scope* bsyn)))))

(defn parse-CountRange [[_ & [n u :as args]]]
  (when-not (#{1 2} (count args))
    (err/int-error "Wrong arguments to CountRange"))
  (when-not (integer? n)
    (err/int-error "First argument to CountRange must be an integer"))
  (when-not (or (#{1} (count args))
                (integer? u))
    (err/int-error "Second argument to CountRange must be an integer"))
  (r/make-CountRange n u))

(defmethod parse-type-list 'CountRange [t] (parse-CountRange t))
(defmethod parse-type-list 'clojure.core.typed/CountRange [t] (parse-CountRange t))
(defmethod parse-type-list 'cljs.core.typed/CountRange [t] (parse-CountRange t))

(declare resolve-type-clj)

(defn uniquify-local [sym]
  (get-in vs/*current-expr* [:env :clojure.core.typed.analyzer.common.passes.uniquify/locals-frame-val sym]))

(defmethod parse-type-list 'clojure.core.typed/TypeOf [[_ sym :as t]]
  (impl/assert-clojure)
  (when-not (= 2 (count t))
    (err/int-error (str "Wrong number of arguments to TypeOf (" (count t) ")")))
  (when-not (symbol? sym)
    (err/int-error "Argument to TypeOf must be a symbol."))
  (let [uniquified-local (uniquify-local sym)
        vsym (let [r (resolve-type-clj sym)]
               (when (var? r)
                 (coerce/var->symbol r)))]
    (if uniquified-local
      (let [t (ind/type-of-nofail uniquified-local)]
        (when-not t
          (err/int-error (str "Could not resolve TypeOf for local " sym)))
        t)
      (do
        (when-not vsym
          (err/int-error (str "Could not resolve TypeOf for var " sym)))
        (r/-type-of vsym)))))

(defn parse-ExactCount [[_ & [n :as args]]]
  (when-not (#{1} (count args))
    (err/int-error "Wrong arguments to ExactCount"))
  (when-not (integer? n)
    (err/int-error "First argument to ExactCount must be an integer"))
  (r/make-ExactCountRange n))

(defmethod parse-type-list 'ExactCount [t] (parse-ExactCount t))
(defmethod parse-type-list 'clojure.core.typed/ExactCount [t] (parse-ExactCount t))
(defmethod parse-type-list 'cljs.core.typed/ExactCount [t] (parse-ExactCount t))

(defn- RClass-of-var []
  (let [v (ns-resolve (find-ns 'clojure.core.typed.checker.type-ctors) 'RClass-of)]
    (assert (var? v) "RClass-of unbound")
    v))

(defn predicate-for [on-type]
  (let [RClass-of @(RClass-of-var)]
    (r/make-FnIntersection
      (r/make-Function [r/-any] (impl/impl-case
                                  :clojure (RClass-of Boolean)
                                  :cljs    (r/JSBoolean-maker))
                       :filter (fl/-FS (fl/-filter on-type 0)
                                       (fl/-not-filter on-type 0))))))

(defn parse-Pred [[_ & [t-syn :as args]]]
  (when-not (== 1 (count args))
    (err/int-error "Wrong arguments to predicate"))
  (predicate-for (parse-type t-syn)))

; possibly should be called Pred
(defmethod parse-type-list 'predicate [t] 
  (err/deprecated-plain-op 'predicate 'Pred)
  (parse-Pred t))
(defmethod parse-type-list 'clojure.core.typed/Pred [t] (parse-Pred t))
(defmethod parse-type-list 'cljs.core.typed/Pred [t] (parse-Pred t))

; FIXME deprecate
(defmethod parse-type-list 'Not
  [[_ tsyn :as all]]
  (when-not (= (count all) 2) 
    (err/int-error (str "Wrong arguments to Not (expected 1): " all)))
  (r/NotType-maker (parse-type tsyn)))

(defn parse-Difference [[_ tsyn & dsyns :as all]]
  (when-not (<= 3 (count all))
    (err/int-error (str "Wrong arguments to Difference (expected at least 2): " all)))
  (apply r/-difference (parse-type tsyn) (mapv parse-type dsyns)))

(defmethod parse-type-list 'Difference [t] 
  (err/deprecated-plain-op 'Difference)
  (parse-Difference t))
(defmethod parse-type-list 'clojure.core.typed/Difference [t] (parse-Difference t))
(defmethod parse-type-list 'cljs.core.typed/Difference [t] (parse-Difference t))

(defmethod parse-type-list 'Rec [syn] 
  (err/deprecated-plain-op 'Rec)
  (parse-rec-type syn))
(defmethod parse-type-list 'clojure.core.typed/Rec [syn] (parse-rec-type syn))
(defmethod parse-type-list 'cljs.core.typed/Rec [syn] (parse-rec-type syn))

(defn parse-Assoc [[_ tsyn & entries :as all]]
  (when-not (<= 1 (count (next all)))
    (err/int-error (str "Wrong arguments to Assoc: " all)))
  (let [{ellipsis-pos '...}
        (zipmap entries (range))

        [entries dentries] (split-at (if ellipsis-pos
                                       (dec ellipsis-pos)
                                       (count entries))
                                     entries)

        [drest-type _ drest-bnd] (when ellipsis-pos
                                   dentries)

        _ (when-not (-> entries count even?)
            (err/int-error (str "Incorrect Assoc syntax: " all " , must have even number of key/val pair.")))

        _ (when-not (or (not ellipsis-pos)
                        (= (count dentries) 3))
            (err/int-error (str "Incorrect Assoc syntax: " all " , Dotted rest entry must be 3 entries")))

        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (err/int-error "Dotted bound must be symbol"))]
  (r/AssocType-maker (parse-type tsyn)
                     (doall (->> entries
                                 (map parse-type)
                                 (partition 2)
                                 (map vec)))
                     (when ellipsis-pos
                       (let [bnd (dvar/*dotted-scope* drest-bnd)
                             _ (when-not bnd
                                 (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))]
                         (r/DottedPretype1-maker
                           (free-ops/with-frees [bnd] ;with dotted bound in scope as free
                             (parse-type drest-type))
                           (:name bnd)))))))

(defmethod parse-type-list 'Assoc [t] (parse-Assoc t))
(defmethod parse-type-list 'clojure.core.typed/Assoc [t] (parse-Assoc t))
(defmethod parse-type-list 'cljs.core.typed/Assoc [t] (parse-Assoc t))

(defn parse-Get [[_ tsyn keysyn & not-foundsyn :as all]]
  (when-not (#{2 3} (count (next all)))
    (err/int-error (str "Wrong arguments to Get: " all)))
  (r/-get (parse-type tsyn)
          (parse-type keysyn)
          :not-found
          (when (#{3} (count (next all)))
            (parse-type not-foundsyn))))

(defmethod parse-type-list 'Get [t] (parse-Get t))
(defmethod parse-type-list 'clojure.core.typed/Get [t] (parse-Get t))
(defmethod parse-type-list 'cljs.core.typed/Get [t] (parse-Get t))

; convert flattened kw arguments to vectors
(defn normalise-binder [bnds]
  (loop [bnds bnds
         out []]
    (cond
      (empty? bnds) out

      (vector? (first bnds)) (let [[s & rst] bnds]
                               (recur rst
                                      (conj out s)))
      :else
      (let [[sym & rst] bnds
            [group rst] (loop [bnds rst
                               out [sym]]
                          (if (keyword? (second bnds))
                            (let [_ (when-not (#{2} (count (take 2 bnds)))
                                      (err/int-error (str "Keyword option " (second bnds)
                                                        " has no associated value")))
                                  [k v & rst] bnds]
                              (recur rst
                                     (conj out k v)))
                            [bnds out]))]
        (recur rst
               (conj out group))))))

(defn parse-dotted-binder [bnds]
  {:pre [(vector? bnds)]}
  (let [frees-with-bnds (reduce (fn [fs fsyn]
                                  {:pre [(vector? fs)]
                                   :post [(every? (con/hvector-c? symbol? r/Bounds?) %)]}
                                  (conj fs
                                        (free-ops/with-bounded-frees (into {} (map (fn [[n bnd]] [(r/make-F n) bnd]) fs))
                                          (parse-free fsyn))))
                                [] (-> bnds pop pop))
        dvar (parse-free (-> bnds pop peek))]
    [frees-with-bnds dvar]))

(defn parse-normal-binder [bnds]
  (let [frees-with-bnds
        (reduce (fn [fs fsyn]
                  {:pre [(vector? fs)]
                   :post [(every? (con/hvector-c? symbol? r/Bounds?) %)]}
                  (conj fs
                        (free-ops/with-bounded-frees (into {} (map (fn [[n bnd]] [(r/make-F n) bnd]) fs))
                          (parse-free fsyn))))
                [] bnds)]
    [frees-with-bnds nil]))

(defn parse-unknown-binder [bnds]
  {:pre [((some-fn nil? vector?) bnds)]}
  (when bnds
    ((if (#{'...} (peek bnds))
       parse-dotted-binder
       parse-normal-binder)
     bnds)))

(defn parse-All-binder [bnds]
  {:pre [(vector? bnds)]}
  (let [[positional kwargs] (split-with (complement keyword?) bnds)
        positional (vec positional)
        _ (when-not (even? (count kwargs))
            (err/int-error (str "Expected an even number of keyword options to All, given: " (vec kwargs))))
        _ (when (seq kwargs)
            (when-not (apply distinct? (map first (partition 2 kwargs)))
              (err/int-error (str "Gave repeated keyword args to All: " (vec kwargs)))))
        {:keys [named] :as kwargs} kwargs
        _ (let [unsupported (set/difference (set (keys kwargs)) #{:named})]
            (when (seq unsupported)
              (err/int-error (str "Unsupported keyword argument(s) to All: " unsupported))))
        _ (when (contains? kwargs :named)
            (when-not (and (vector? named)
                           (every? symbol? named))
              (err/int-error (str ":named keyword argument to All must be a vector of symbols, given: " (pr-str named)))))
        dotted? (= '... (peek positional))
        bnds* (if named
                (let [positional-no-dotted (if dotted?
                                             (pop (pop positional))
                                             positional)
                      ;; fit :named variables between positional and dotted variable, because 
                      ;; PolyDots expects the dotted variable last.
                      bnds* (vec (concat positional-no-dotted named
                                         (when dotted?
                                           (subvec positional (- (count positional) 2)))))]
                  bnds*)
                positional)
        no-dots (if dotted?
                  (pop bnds*)
                  bnds*)
        _ (when (seq no-dots)
            (when-not (apply distinct? no-dots)
              (err/int-error (str "Variables bound by All must be unique, given: " no-dots))))
        named-map (let [sym-to-pos (into {} (map-indexed #(vector %2 %1)) no-dots)]
                    (select-keys sym-to-pos named))
        ;; TODO 
        ;; update usages of parse-unknown-binder to use parse-All-binder
        [frees-with-bnds dvar] ((if dotted?
                                  parse-dotted-binder
                                  parse-normal-binder)
                                bnds*)]
    {:frees-with-bnds frees-with-bnds
     :dvar dvar
     :named named-map}))

;dispatch on last element of syntax in binder
(defn parse-all-type [bnds type]
  (let [_ (assert (vector? bnds))
        {:keys [frees-with-bnds dvar named]} (parse-All-binder bnds)
        bfs (into {} (map (fn [[n bnd]] [(r/make-F n) bnd]) frees-with-bnds))]
    (if dvar
      (free-ops/with-bounded-frees bfs
        (c/PolyDots* (map first (concat frees-with-bnds [dvar]))
                     (map second (concat frees-with-bnds [dvar]))
                     (dvar/with-dotted [(r/make-F (first dvar))]
                       (parse-type type))
                     :named named))
      (free-ops/with-bounded-frees bfs
        (c/Poly* (map first frees-with-bnds)
                 (map second frees-with-bnds)
                 (parse-type type)
                 :named named)))))

(defmethod parse-type-list 'Extends
  [[_ extends & {:keys [without] :as opts} :as syn]]
  (when-not (empty? (set/difference (set (keys opts)) #{:without}))
    (err/int-error (str "Invalid options to Extends:" (keys opts))))
  (when-not (vector? extends) 
    (err/int-error (str "Extends takes a vector of types: " (pr-str syn))))
  (c/-extends (doall (map parse-type extends))
              :without (doall (map parse-type without))))

(defn parse-All [[_All_ bnds syn & more :as all]]
  ;(prn "All syntax" all)
  (when more
    (err/int-error (str "Bad All syntax: " all)))
  (parse-all-type bnds syn))

(defmethod parse-type-list 'All [t] 
  (err/deprecated-plain-op 'All)
  (parse-All t))
(defmethod parse-type-list 'clojure.core.typed/All [t] (parse-All t))
(defmethod parse-type-list 'cljs.core.typed/All [t] (parse-All t))

(defn parse-union-type [[u & types]]
  (c/make-Union (doall (map parse-type types))))

(defmethod parse-type-list 'U [syn] 
  (err/deprecated-plain-op 'U)
  (parse-union-type syn))
(defmethod parse-type-list 'clojure.core.typed/U [syn] (parse-union-type syn))
(defmethod parse-type-list 'cljs.core.typed/U [syn] (parse-union-type syn))

; don't do any simplification of the intersection because some types might
; not be resolved
(defn parse-intersection-type [[i & types]]
  (c/make-Intersection (doall (map parse-type types))))

(defmethod parse-type-list 'I [syn] 
  (err/deprecated-plain-op 'I)
  (parse-intersection-type syn))
(defmethod parse-type-list 'clojure.core.typed/I [syn] (parse-intersection-type syn))
(defmethod parse-type-list 'cljs.core.typed/I [syn] (parse-intersection-type syn))

(defn parse-Array 
  [[_ syn & none]]
  (when-not (empty? none)
    (err/int-error "Expected 1 argument to Array"))
  (let [t (parse-type syn)]
    (impl/impl-case
      :clojure (let [jtype (if (r/RClass? t)
                             (r/RClass->Class t)
                             Object)]
                 (r/PrimitiveArray-maker jtype t t))
      :cljs (r/ArrayCLJS-maker t t))))

(defmethod parse-type-list 'Array [syn] (parse-Array syn))
(defmethod parse-type-list 'cljs.core.typed/Array [syn] (parse-Array syn))

(defn parse-ReadOnlyArray
  [[_ osyn & none]]
  (when-not (empty? none) 
    (err/int-error "Expected 1 argument to ReadOnlyArray"))
  (let [o (parse-type osyn)]
    (impl/impl-case
      :clojure (r/PrimitiveArray-maker Object (r/Bottom) o)
      :cljs (r/ArrayCLJS-maker (r/Bottom) o))))

(defmethod parse-type-list 'ReadOnlyArray [syn] (parse-ReadOnlyArray syn))
(defmethod parse-type-list 'cljs.core.typed/ReadOnlyArray [syn] (parse-ReadOnlyArray syn))

(defmethod parse-type-list 'Array2
  [[_ isyn osyn & none]]
  (when-not (empty? none) 
    (err/int-error "Expected 2 arguments to Array2"))
  (let [i (parse-type isyn)
        o (parse-type osyn)]
    (impl/impl-case
      :clojure (r/PrimitiveArray-maker Object i o)
      :cljs (r/ArrayCLJS-maker i o))))

(defmethod parse-type-list 'Array3
  [[_ jsyn isyn osyn & none]]
  (impl/assert-clojure)
  (when-not (empty? none) 
    (err/int-error "Expected 3 arguments to Array3"))
  (let [jrclass (c/fully-resolve-type (parse-type jsyn))
        _ (when-not (r/RClass? jrclass) 
            (err/int-error "First argument to Array3 must be a Class"))]
    (r/PrimitiveArray-maker (r/RClass->Class jrclass) (parse-type isyn) (parse-type osyn))))

(declare parse-function)

(defn parse-fn-intersection-type [[Fn & types]]
  (apply r/make-FnIntersection (mapv parse-function types)))

(defn parse-Fn [[_ & types :as syn]]
  (when-not (seq types) 
    (err/int-error (str "Must pass at least one arity to Fn: " (pr-str syn))))
  (when-not (every? vector? types) 
    (err/int-error (str "Fn accepts vectors, given: " (pr-str syn))))
  (parse-fn-intersection-type syn))

(defmethod parse-type-list 'Fn [t] 
  (err/deprecated-plain-op 'Fn 'IFn)
  (parse-Fn t))
(defmethod parse-type-list 'clojure.core.typed/IFn [t] (parse-Fn t))
(defmethod parse-type-list 'cljs.core.typed/IFn [t] (parse-Fn t))

(defn parse-free-binder [[nme & {:keys [variance < > kind] :as opts}]]
  (when-not (symbol? nme)
    (err/int-error "First entry in free binder should be a name symbol"))
  {:nme nme :variance (or variance :invariant)
   :bound (r/Bounds-maker
            ;upper
            (when-not kind
              (if (contains? opts :<)
                (parse-type <)
                r/-any))
            ;lower
            (when-not kind
              (if (contains? opts :>)
                (parse-type >)
                r/-nothing))
            ;kind
            (when kind
              (parse-type kind)))})

(defn parse-tfn-binder [[nme & opts-flat :as all]]
  {:pre [(vector? all)]
   :post [((con/hmap-c? :nme symbol? :variance r/variance?
                        :bound r/Bounds?)
           %)]}
  (let [_ (when-not (even? (count opts-flat))
            (err/int-error (str "Uneven arguments passed to TFn binder: "
                              (pr-str all))))
        {:keys [variance < >] 
         :or {variance :inferred}
         :as opts} 
        (apply hash-map opts-flat)]
    (when-not (symbol? nme)
      (err/int-error "Must provide a name symbol to TFn"))
    (when (contains? opts :kind)
      (err/deprecated-warn "Kind annotation for TFn parameters"))
    (when-not (r/variance? variance)
      (err/int-error (str "Invalid variance: " (pr-str variance))))
    {:nme nme :variance variance
     :bound (let [upper-or-nil (when (contains? opts :<)
                                 (parse-type <))
                  lower-or-nil (when (contains? opts :>)
                                 (parse-type >))]
              (c/infer-bounds upper-or-nil lower-or-nil))}))

(defn parse-type-fn 
  [[_ binder bodysyn :as tfn]]
  (when-not (= 3 (count tfn))
    (err/int-error (str "Wrong number of arguments to TFn: " (pr-str tfn))))
  (when-not (every? vector? binder)
    (err/int-error (str "TFn binder should be vector of vectors: " (pr-str tfn))))
  (let [; don't scope a free in its own bounds. Should review this decision
        free-maps (free-ops/with-free-symbols (map (fn [s]
                                                     {:pre [(vector? s)]
                                                      :post [(symbol? %)]}
                                                     (first s))
                                                   binder)
                    (mapv parse-tfn-binder binder))
        bodyt (free-ops/with-bounded-frees (into {}
                                                 (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound])
                                                      free-maps))
                (parse-type bodysyn))
        ; We check variances lazily in TypeFn-body*. This avoids any weird issues with calculating
        ; variances with potentially partially defined types.
        ;vs (free-ops/with-bounded-frees (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound])
        ;                                     free-maps)
        ;     (frees/fv-variances bodyt))
        ;_ (doseq [{:keys [nme variance]} free-maps]
        ;    (when-let [actual-v (vs nme)]
        ;      (when-not (= (vs nme) variance)
        ;        (err/int-error (str "Type variable " nme " appears in " (name actual-v) " position "
        ;                          "when declared " (name variance))))))
        ]
    (c/TypeFn* (map :nme free-maps) (map :variance free-maps)
               (map :bound free-maps) bodyt
               {:meta {:env vs/*current-env*}})))

(defmethod parse-type-list 'TFn [syn] 
  (err/deprecated-plain-op 'TFn)
  (parse-type-fn syn))
(defmethod parse-type-list 'clojure.core.typed/TFn [syn] (parse-type-fn syn))
(defmethod parse-type-list 'cljs.core.typed/TFn [syn] (parse-type-fn syn))

(declare parse-quoted-hvec)

(defmethod parse-type-list 'Seq* [syn] 
  (err/deprecated-plain-op 'Seq* 'HSeq)
  (r/-hseq (mapv parse-type (rest syn))))
(defmethod parse-type-list 'List* [syn]
  (err/deprecated-plain-op 'List* 'HList)
  (r/HeterogeneousList-maker (mapv parse-type (rest syn))))
(defmethod parse-type-list 'Vector* [syn] 
  (err/deprecated-plain-op 'Vector* 'HVec)
  (parse-quoted-hvec (rest syn)))

;; parse-HVec, parse-HSequential and parse-HSeq have many common patterns
;; so we reuse them
(defn parse-types-with-rest-drest [err-msg]
  (fn [syns]
    (let [rest? (#{'*} (last syns))
          dotted? (#{'...} (-> syns butlast last))
          _ (when (and rest? dotted?)
              (err/int-error (str err-msg syns)))
          {:keys [fixed rest drest]}
          (cond
            rest?
            (let [fixed (mapv parse-type (drop-last 2 syns))
                  rest (parse-type (-> syns butlast last))]
              {:fixed fixed
               :rest rest})
            dotted?
            (let [fixed (mapv parse-type (drop-last 3 syns))
                  [drest-type _dots_ drest-bnd :as dot-syntax] (take-last 3 syns)
                  ; should never fail, if the logic changes above it's probably
                  ; useful to keep around.
                  _ (when-not (#{3} (count dot-syntax))
                      (err/int-error (str "Bad vector syntax: " dot-syntax)))
                  bnd (dvar/*dotted-scope* drest-bnd)
                  _ (when-not bnd
                      (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))]
              {:fixed fixed
               :drest (r/DottedPretype1-maker
                        (free-ops/with-frees [bnd] ;with dotted bound in scope as free
                                             (parse-type drest-type))
                        (:name bnd))})
            :else {:fixed (mapv parse-type syns)})]
      {:fixed fixed
       :rest rest
       :drest drest})))

(def parse-hvec-types (parse-types-with-rest-drest
                        "Invalid heterogeneous vector syntax:"))
(def parse-hsequential-types (parse-types-with-rest-drest
                               "Invalid heterogeneous sequential syntax:"))
(def parse-hseq-types (parse-types-with-rest-drest
                        "Invalid heterogeneous seq syntax:"))
(def parse-hlist-types (parse-types-with-rest-drest
                         "Invalid heterogeneous list syntax:"))

(declare parse-object parse-filter-set)

(defn parse-heterogeneous* [parse-h*-types constructor]
  (fn [[_ syn & {:keys [filter-sets objects repeat]}]]
    (let [{:keys [fixed drest rest]} (parse-h*-types syn)]
      (constructor fixed
                   :filters (when filter-sets
                              (mapv parse-filter-set filter-sets))
                   :objects (when objects
                              (mapv parse-object objects))
                   :drest drest
                   :rest rest
                   :repeat (when (true? repeat)
                             true)))))

(def parse-HVec (parse-heterogeneous* parse-hvec-types r/-hvec))
(def parse-HSequential (parse-heterogeneous* parse-hsequential-types r/-hsequential))
(def parse-HSeq (parse-heterogeneous* parse-hseq-types r/-hseq))
(def parse-HList (parse-heterogeneous* parse-hseq-types (comp #(assoc % :kind :list)
                                                              r/-hsequential)))

(defmethod parse-type-list 'HVec [t] (parse-HVec t))
(defmethod parse-type-list 'clojure.core.typed/HVec [t] (parse-HVec t))
(defmethod parse-type-list 'cljs.core.typed/HVec [t] (parse-HVec t))

(defmethod parse-type-list 'HSequential [t] (parse-HSequential t))
(defmethod parse-type-list 'clojure.core.typed/HSequential [t] (parse-HSequential t))
(defmethod parse-type-list 'cljs.core.typed/HSequential [t] (parse-HSequential t))

(defmethod parse-type-list 'HSeq [t] 
  (err/deprecated-plain-op 'HSeq)
  (parse-HSeq t))
(defmethod parse-type-list 'clojure.core.typed/HSeq [t] (parse-HSeq t))
(defmethod parse-type-list 'cljs.core.typed/HSeq [t] (parse-HSeq t))

;; TODO CLJS HList
(defmethod parse-type-list 'clojure.core.typed/HList [t] (parse-HList t))

(defn parse-HSet [[_ ts & {:keys [complete?] :or {complete? true}} :as args]]
  (let [bad (seq (remove hset/valid-fixed? ts))]
    (when bad
      (err/int-error (str "Bad arguments to HSet: " (pr-str bad))))
    (r/-hset (set (map r/-val ts)) :complete? complete?)))

(defmethod parse-type-list 'clojure.core.typed/HSet [t] (parse-HSet t))
(defmethod parse-type-list 'cljs.core.typed/HSet [t] (parse-HSet t))

(defn- syn-to-hmap [mandatory optional absent-keys complete?]
  (when mandatory
    (when-not (map? mandatory)
      (err/int-error (str "Mandatory entries to HMap must be a map: " mandatory))))
  (when optional
    (when-not (map? optional)
      (err/int-error (str "Optional entries to HMap must be a map: " optional))))
  (letfn [(mapt [m]
            (into {} (for [[k v] m]
                       [(r/-val k)
                        (parse-type v)])))]
    (let [_ (when-not (every? empty? [(set/intersection (set (keys mandatory))
                                                        (set (keys optional)))
                                      (set/intersection (set (keys mandatory))
                                                        (set absent-keys))
                                      (set/intersection (set (keys optional))
                                                        (set absent-keys))])
              (err/int-error (str "HMap options contain duplicate key entries: "
                                "Mandatory: " (into {} mandatory) ", Optional: " (into {} optional) 
                                ", Absent: " (set absent-keys))))
          _ (when-not (every? keyword? (keys mandatory)) (err/int-error "HMap's mandatory keys must be keywords"))
          mandatory (mapt mandatory)
          _ (when-not (every? keyword? (keys optional)) (err/int-error "HMap's optional keys must be keywords"))
          optional (mapt optional)
          _ (when-not (every? keyword? absent-keys) (err/int-error "HMap's absent keys must be keywords"))
          absent-keys (set (map r/-val absent-keys))]
      (c/make-HMap :mandatory mandatory :optional optional 
                   :complete? complete? :absent-keys absent-keys))))

(defn parse-quoted-hvec [syn]
  (let [{:keys [fixed drest rest]} (parse-hvec-types syn)]
    (r/-hvec fixed
             :drest drest
             :rest rest)))

(defmethod parse-type-list 'quote 
  [[_ syn]]
  (cond
    ((some-fn number? keyword? symbol? string?) syn) (r/-val syn)
    (vector? syn) (parse-quoted-hvec syn)
    ; quoted map is a partial map with mandatory keys
    (map? syn) (syn-to-hmap syn nil nil false)
    :else (err/int-error (str "Invalid use of quote: " (pr-str syn)))))

(declare parse-in-ns)

(defn multi-frequencies 
  "Like frequencies, but only returns frequencies greater
  than one"
  [coll]
  (->> coll
       frequencies
       (filter (fn [[k freq]]
                 (when (< 1 freq)
                   true)))
       (into {})))

(defn parse-HMap [[_HMap_ & flat-opts :as all]]
  (let [supported-options #{:optional :mandatory :absent-keys :complete?}
        ; support deprecated syntax (HMap {}), which is now (HMap :mandatory {})
        deprecated-mandatory (when (map? (first flat-opts))
                               (err/deprecated-warn
                                 "(HMap {}) syntax has changed, use (HMap :mandatory {})")
                               (first flat-opts))
        flat-opts (if deprecated-mandatory
                    (next flat-opts)
                    flat-opts)
        _ (when-not (even? (count flat-opts))
            (err/int-error (str "Uneven keyword arguments to HMap: " (pr-str all))))
        flat-keys (->> flat-opts
                       (partition 2)
                       (map first))
        _ (when-not (every? keyword? flat-keys)
            (err/int-error (str "HMap requires keyword arguments, given " (pr-str (first flat-keys))
                              #_#_" in: " (pr-str all))))
        _ (let [kf (->> flat-keys
                        multi-frequencies
                        (map first)
                        seq)]
            (when-let [[k] kf]
              (err/int-error (str "Repeated keyword argument to HMap: " (pr-str k)))))

        {:keys [optional mandatory absent-keys complete?]
         :or {complete? false}
         :as others} (apply hash-map flat-opts)
        _ (when-let [[k] (seq (set/difference (set (keys others)) supported-options))]
            (err/int-error (str "Unsupported HMap keyword argument: " (pr-str k))))
        _ (when (and deprecated-mandatory mandatory)
            (err/int-error (str "Cannot provide both deprecated initial map syntax and :mandatory option to HMap")))
        mandatory (or deprecated-mandatory mandatory)]
    (syn-to-hmap mandatory optional absent-keys complete?)))

(defmethod parse-type-list 'HMap [t] (parse-HMap t))
(defmethod parse-type-list 'clojure.core.typed/HMap [t] (parse-HMap t))
(defmethod parse-type-list 'cljs.core.typed/HMap [t] (parse-HMap t))

(defn parse-JSObj [[_JSObj_ types :as all]]
  (let [_ (when-not (= 2 (count all))
            (err/int-error (str "Bad syntax to JSObj: " (pr-str all))))
        _ (when-not (every? keyword? (keys types))
            (err/int-error (str "JSObj requires keyword keys, given " (pr-str (class (first (remove keyword? (keys types))))))))
        parsed-types (zipmap (keys types)
                             (map parse-type (vals types)))]
    (r/JSObj-maker parsed-types)))

(defmethod parse-type-list 'cljs.core.typed/JSObj [t] (parse-JSObj t))

(def ^:private cljs-ns (delay (impl/dynaload 'clojure.core.typed.util-cljs/cljs-ns)))

(defn- parse-in-ns []
  {:post [(symbol? %)]}
  (or *parse-type-in-ns*
      (impl/impl-case
        :clojure (ns-name *ns*)
        :cljs (@cljs-ns))))

(defn- resolve-type-clj 
  "Returns a var, class or nil"
  [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn var? class? nil?) %)]}
  (impl/assert-clojure)
  (let [nsym (parse-in-ns)]
    (if-let [ns (find-ns nsym)]
      (ns-resolve ns sym)
      (err/int-error (str "Cannot find namespace: " sym)))))

(defn- resolve-alias-clj 
  "Returns a symbol if sym maps to a type alias, otherwise nil"
  [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn symbol? nil?) %)]}
  (impl/assert-clojure)
  (let [nsym (parse-in-ns)
        nsp (some-> (namespace sym) symbol)]
    (if-let [ns (find-ns nsym)]
      (when-let [qual (if nsp
                        (some-> (or ((ns-aliases ns) nsp)
                                    (find-ns nsp))
                                ns-name)
                        (ns-name ns))]
        (let [_ (assert (and (symbol? qual)
                             (not (namespace qual))))
              qsym (symbol (name qual) (name sym))]
          (when (contains? (nme-env/name-env) qsym)
            qsym)))
      (err/int-error (str "Cannot find namespace: " sym)))))

(let [cljs-resolve-var (delay (impl/dynaload 'clojure.core.typed.util-cljs/resolve-var))]
  (defn- resolve-type-cljs 
    "Returns a qualified symbol or nil"
    [sym]
    {:pre [(symbol? sym)]
     :post [((some-fn symbol?
                      nil?)
             %)]}
    (impl/assert-cljs)
    (let [nsym (parse-in-ns)
          res (@cljs-resolve-var nsym sym)]
      res)))

(defn parse-RClass [cls-sym params-syn]
  (impl/assert-clojure)
  (let [RClass-of @(RClass-of-var)
        cls (resolve-type-clj cls-sym)
        _ (when-not (class? cls) (err/int-error (str (pr-str cls-sym) " cannot be resolved")))
        tparams (doall (map parse-type params-syn))]
    (RClass-of cls tparams)))

(defn parse-Value [[_Value_ syn :as all]]
  (when-not (#{2} (count all))
    (err/int-error (str "Incorrect number of arguments to Value, " (count all)
                      ", expected 2: " all)))
  (impl/impl-case
    :clojure (const/constant-type syn)
    :cljs (cond
            ((some-fn symbol? keyword?) syn)
              (r/-val syn)
            :else (assert nil "FIXME CLJS parse Value"))))

(defmethod parse-type-list 'Value [t] (parse-Value t))
(defmethod parse-type-list 'clojure.core.typed/Val [t] (parse-Value t))
(defmethod parse-type-list 'clojure.core.typed/Value [t] (parse-Value t))
(defmethod parse-type-list 'cljs.core.typed/Val [t] (parse-Value t))
(defmethod parse-type-list 'cljs.core.typed/Value [t] (parse-Value t))

(defmethod parse-type-list 'KeywordArgs
  [[_KeywordArgs_ & {:keys [optional mandatory]}]]
  (when-not (= #{}
               (set/intersection (set (keys optional))
                                 (set (keys mandatory))))
    (err/int-error (str "Optional and mandatory keyword arguments should be disjoint: "
                      (set/intersection (set (keys optional))
                                        (set (keys mandatory))))))
  (let [optional (into {} (for [[k v] optional]
                            (do (when-not (keyword? k) (err/int-error (str "Keyword argument keys must be keywords: " (pr-str k))))
                              [(r/-val k) (parse-type v)])))
        mandatory (into {} (for [[k v] mandatory]
                             (do (when-not (keyword? k) (err/int-error (str "Keyword argument keys must be keywords: " (pr-str k))))
                               [(r/-val k) (parse-type v)])))]
    (apply c/Un (apply concat
                     (for [opts (map #(into {} %) (comb/subsets optional))]
                       (let [m (merge mandatory opts)
                             kss (comb/permutations (keys m))]
                         (for [ks kss]
                           (r/-hseq (mapcat #(find m %) ks)))))))))

(declare unparse-type deprecated-list)

(defn parse-type-list-default 
  [[n & args :as syn]]
  (if-let [d (deprecated-list syn)]
    d
    (let [op (parse-type n)]
      ;(prn "tapp op" op)
      (when-not ((some-fn r/Name? r/TypeFn? r/F? r/B? r/Poly?) op)
        (err/int-error (str "Invalid operator to type application: " syn)))
      (with-meta (r/TApp-maker op (mapv parse-type args))
                 {:syn syn
                  :env vs/*current-env*}))))

(defmethod parse-type-list :default 
  [[n & args :as syn]]
  (parse-type-list-default syn))

(defmethod parse-type* Cons [l] (parse-type-list l))
(defmethod parse-type* IPersistentList [l] 
  (parse-type-list l))

(defmulti parse-type-symbol
  (fn [n] 
    {:pre [(symbol? n)]}
    (or (impl/impl-case
          :clojure (let [r (resolve-type-clj n)]
                     (when (var? r)
                       (coerce/var->symbol r)))
          ;TODO
          :cljs (resolve-type-cljs n))
        n)))

(defn parse-Any [sym]
  (if (-> sym meta ::t/infer)
    r/-infer-any
    r/-any))

(defmethod parse-type-symbol 'Any [_] 
  (err/deprecated-plain-op 'Any)
  r/-any)
(defmethod parse-type-symbol 'clojure.core.typed/Any [s] (parse-Any s))
(defmethod parse-type-symbol 'cljs.core.typed/Any [s] (parse-Any s))

(defmethod parse-type-symbol 'clojure.core.typed/TCError [t] (r/TCError-maker))

(defmethod parse-type-symbol 'Nothing [_] 
  (err/deprecated-plain-op 'Nothing)
  (r/Bottom))
(defmethod parse-type-symbol 'clojure.core.typed/Nothing [_] (r/Bottom))
(defmethod parse-type-symbol 'cljs.core.typed/Nothing [_] (r/Bottom))

(defmethod parse-type-symbol 'AnyFunction [_] (r/TopFunction-maker))

(defmethod parse-type-symbol 'cljs.core.typed/CLJSInteger [_] (r/CLJSInteger-maker))
(defmethod parse-type-symbol 'cljs.core.typed/JSNumber [_] (r/JSNumber-maker))
(defmethod parse-type-symbol 'cljs.core.typed/JSBoolean [_] (r/JSBoolean-maker))
(defmethod parse-type-symbol 'cljs.core.typed/JSObject [_] (r/JSObject-maker))
(defmethod parse-type-symbol 'cljs.core.typed/JSString [_] (r/JSString-maker))
(defmethod parse-type-symbol 'cljs.core.typed/JSUndefined [_] (r/JSUndefined-maker))
(defmethod parse-type-symbol 'cljs.core.typed/JSNull [_] (r/JSNull-maker))
(defmethod parse-type-symbol 'cljs.core.typed/JSSymbol [_] (r/JSSymbol-maker))

(defn clj-primitives-fn []
  (let [RClass-of @(RClass-of-var)]
    {'byte (RClass-of 'byte)
     'short (RClass-of 'short)
     'int (RClass-of 'int)
     'long (RClass-of 'long)
     'float (RClass-of 'float)
     'double (RClass-of 'double)
     'boolean (RClass-of 'boolean)
     'char (RClass-of 'char)
     'void r/-nil}))

;[Any -> (U nil Type)]
(defmulti deprecated-clj-symbol identity)

(defmethod deprecated-clj-symbol :default [_] nil)

;[Any -> (U nil Type)]
(defn deprecated-symbol [sym]
  {:post [((some-fn nil? r/Type?) %)]}
  (impl/impl-case
    :clojure (deprecated-clj-symbol sym)
    :cljs nil))

;[Any -> (U nil Type)]
(defmulti deprecated-clj-list 
  (fn [[op]]
    (when (symbol? op)
      ((some-fn
         (every-pred
           class? coerce/Class->symbol)
         (every-pred
           var? coerce/var->symbol))
       (resolve-type-clj op)))))

(defmethod deprecated-clj-list :default [_] nil)

;[Any -> (U nil Type)]
(defn deprecated-list [lst]
  {:post [((some-fn nil? r/Type?) %)]}
  (impl/impl-case
    :clojure (deprecated-clj-list lst)
    :cljs nil))

(defn parse-type-symbol-default
  [sym]
  (let [primitives (impl/impl-case
                     :clojure (clj-primitives-fn)
                     :cljs {})
        free (when (symbol? sym) 
               (free-ops/free-in-scope sym))
        rsym (when-not free
               (impl/impl-case
                 :clojure (let [res (when (symbol? sym)
                                      (resolve-type-clj sym))]
                            (cond 
                              (class? res) (coerce/Class->symbol res)
                              (var? res)   (coerce/var->symbol res)
                              ;; name doesn't resolve, try declared protocol or datatype
                              ;; in the current namespace
                              :else (let [ns (parse-in-ns)
                                          dprotocol (if (namespace sym)
                                                      sym
                                                      (symbol (str ns) (str sym)))
                                          ddatatype (if (some #{\.} (str sym))
                                                      sym
                                                      (symbol (str (munge ns)) (str sym)))
                                          nmesym (resolve-alias-clj sym)]
                                      (cond
                                        nmesym nmesym
                                        (nme-env/declared-protocol? dprotocol) dprotocol
                                        (nme-env/declared-datatype? ddatatype) ddatatype))))
                 :cljs (when (symbol? sym)
                         (resolve-type-cljs sym))))
        _ (assert ((some-fn symbol? nil?) rsym))]
    (cond
      free free
      (primitives sym) (primitives sym)
      rsym ((some-fn deprecated-symbol r/Name-maker) rsym)
      :else (let [menv (let [m (meta sym)]
                         (when ((every-pred :line :column :file) m)
                           m))]
              (binding [vs/*current-env* (or menv vs/*current-env*)]
                (err/int-error (str "Cannot resolve type: " (pr-str sym)
                                    "\nHint: Is " (pr-str sym) " in scope?")
                               {:use-current-env true}))))))

(defmethod parse-type-symbol :default
  [sym]
  (parse-type-symbol-default sym))

(defmethod parse-type* Symbol [l] (parse-type-symbol l))
(defmethod parse-type* Boolean [v] (if v r/-true r/-false)) 
(defmethod parse-type* nil [_] r/-nil)

(declare parse-path-elem parse-filter*)

(defn parse-filter [f]
  (cond
    (= 'tt f) f/-top
    (= 'ff f) f/-bot
    (= 'no-filter f) f/-no-filter
    (not ((some-fn seq? list?) f)) (err/int-error (str "Malformed filter expression: " (pr-str f)))
    :else (parse-filter* f)))

(defn parse-object-path [{:keys [id path]}]
  (when-not (f/name-ref? id)
    (err/int-error (str "Must pass natural number or symbol as id: " (pr-str id))))
  (orep/-path (when path (mapv parse-path-elem path)) id))

(defn parse-object [obj]
  (case obj
    empty orep/-empty
    no-object orep/-no-object
    (parse-object-path obj)))

(defn parse-filter-set [{:keys [then else] :as fsyn}]
  (when-not (map? fsyn)
    (err/int-error "Filter set must be a map"))
  (let [extra (set/difference (set (keys fsyn)) #{:then :else})]
    (when-not (empty? extra)
      (err/int-error (str "Invalid filter set option: " (first extra)))))
  (fl/-FS (if (contains? fsyn :then)
            (parse-filter then)
            f/-top)
          (if (contains? fsyn :else)
            (parse-filter else)
            f/-top)))

(defmulti parse-filter* 
  #(when (coll? %)
     (first %)))

(defmethod parse-filter* :default
  [syn]
  (err/int-error (str "Malformed filter expression: " (pr-str syn))))

(defmethod parse-filter* 'is
  [[_ & [tsyn nme psyns :as all]]]
  (when-not (#{2 3} (count all))
    (err/int-error (str "Wrong number of arguments to is")))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-filter t nme p)))

(defmethod parse-filter* '!
  [[_ & [tsyn nme psyns :as all]]]
  (when-not (#{2 3} (count all))
    (err/int-error (str "Wrong number of arguments to !")))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-not-filter t nme p)))

(defmethod parse-filter* '|
  [[_ & fsyns]]
  (apply fl/-or (mapv parse-filter fsyns)))

(defmethod parse-filter* '&
  [[_ & fsyns]]
  (apply fl/-and (mapv parse-filter fsyns)))

(defmethod parse-filter* 'when
  [[_ & [a c :as args] :as all]]
  (when-not (#{2} (count args))
    (err/int-error (str "Wrong number of arguments to when: " all)))
  (fl/-imp (parse-filter a) (parse-filter c)))

;FIXME clean up the magic. eg. handle (Class foo bar) as an error
(defmulti parse-path-elem 
  #(cond
     (symbol? %) %
     (coll? %) (first %)
     :else 
       (err/int-error (str "Malformed path element: " (pr-str %)))))

(defmethod parse-path-elem :default [syn]
  (err/int-error (str "Malformed path element: " (pr-str syn))))

(defmethod parse-path-elem 'Class [_] (pthrep/ClassPE-maker))
(defmethod parse-path-elem 'Count [_] (pthrep/CountPE-maker))

(defmethod parse-path-elem 'Keys [_] (pthrep/KeysPE-maker))
(defmethod parse-path-elem 'Vals [_] (pthrep/ValsPE-maker))

(defmethod parse-path-elem 'Key
  [[_ & [ksyn :as all]]]
  (when-not (= 1 (count all))
    (err/int-error "Wrong arguments to Key"))
  (pthrep/-kpe ksyn))

(defmethod parse-path-elem 'Nth
  [[_ & [idx :as all]]]
  (when-not (= 1 (count all))
    (err/int-error "Wrong arguments to Nth"))
  (pthrep/NthPE-maker idx))

(defmethod parse-path-elem 'Keyword [_] (pthrep/KeywordPE-maker))

(defn- parse-kw-map [m]
  {:post [((con/hash-c? r/Value? r/Type?) %)]}
  (into {} (for [[k v] m]
             [(r/-val k) (parse-type v)])))

(defn parse-function [f]
  {:post [(r/Function? %)]}
  (let [is-arrow '#{-> :->}
        all-dom (take-while (complement is-arrow) f)
        [the-arrow rng & opts-flat :as chk] (drop-while (complement is-arrow) f) ;opts aren't used yet
        _ (when ('#{->} the-arrow)
            ;TODO deprecate
            )
        _ (when-not (<= 2 (count chk))
            (err/int-error (str "Incorrect function syntax: " f)))

        _ (when-not (even? (count opts-flat))
            (err/int-error (str "Incorrect function syntax, must have even number of keyword parameters: " f)))

        opts (apply hash-map opts-flat)

        {ellipsis-pos '...
         asterix-pos '*
         kw-asterix-pos :*
         ampersand-pos '&
         push-rest-pos '<*
         push-dot-pos '<...}
        (zipmap all-dom (range))

        _ (when-not (#{0 1} (count (filter identity [asterix-pos ellipsis-pos ampersand-pos 
                                                     kw-asterix-pos push-rest-pos])))
            (err/int-error "Can only provide one rest argument option: & ... * or <*"))

        asterix-pos (or asterix-pos kw-asterix-pos)

        _ (when-let [ks (seq (remove #{:filters :object :flow} (keys opts)))]
            (err/int-error (str "Invalid function keyword option/s: " ks)))

        filters (when-let [[_ fsyn] (find opts :filters)]
                  (parse-filter-set fsyn))

        object (when-let [[_ obj] (find opts :object)]
                 (parse-object obj))

        flow (when-let [[_ obj] (find opts :flow)]
               (r/-flow (parse-filter obj)))

        fixed-dom (cond
                    asterix-pos (take (dec asterix-pos) all-dom)
                    ellipsis-pos (take (dec ellipsis-pos) all-dom)
                    ampersand-pos (take ampersand-pos all-dom)
                    push-rest-pos (take (dec push-rest-pos) all-dom)
                    push-dot-pos (take (dec push-dot-pos) all-dom)
                    :else all-dom)

        rest-type (when asterix-pos
                    (nth all-dom (dec asterix-pos)))
        _ (when-not (or (not asterix-pos)
                        (= (count all-dom) (inc asterix-pos)))
            (err/int-error (str "Trailing syntax after rest parameter: " (pr-str (drop (inc asterix-pos) all-dom)))))
        [drest-type _ drest-bnd :as drest-seq] (when ellipsis-pos
                                                 (drop (dec ellipsis-pos) all-dom))
        _ (when-not (or (not ellipsis-pos) (= 3 (count drest-seq)))
            (err/int-error "Dotted rest entry must be 3 entries"))
        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (err/int-error "Dotted bound must be symbol"))
        [pdot-type _ pdot-bnd :as pdot-seq] (when push-dot-pos
                                              (drop (dec push-dot-pos) all-dom))
        _ (when-not (or (not push-dot-pos) (= 3 (count pdot-seq)))
            (err/int-error "push dotted rest entry must be 3 entries"))
        _ (when-not (or (not push-dot-pos) (symbol? pdot-bnd))
            (err/int-error "push dotted bound must be symbol"))
        [& {optional-kws :optional mandatory-kws :mandatory} :as kws-seq]
        (let [kwsyn (when ampersand-pos
                      (drop (inc ampersand-pos) all-dom))]
          ; support deprecated syntax [& {} -> ] to be equivalent to [& :optional {} -> ]
          (if (and kwsyn
                   (map? (first kwsyn)))
            (do (err/deprecated-warn "[& {} -> ] function syntax is deprecated. Use [& :optional {} -> ]")
                (cons :optional kwsyn))
            kwsyn))

        _ (when-not (or (not ampersand-pos) (seq kws-seq))
            (err/int-error "Must provide syntax after &"))

        prest-type (when push-rest-pos
                     (nth all-dom (dec push-rest-pos)))
        _ (when-not (or (not push-rest-pos)
                        (= (count all-dom) (inc push-rest-pos)))
            (err/int-error (str "Trailing syntax after pust-rest parameter: " (pr-str (drop (inc push-rest-pos) all-dom)))))]
    (r/make-Function (mapv parse-type fixed-dom)
                     (parse-type rng)
                     :rest
                     (when asterix-pos
                       (parse-type rest-type))
                     :drest
                     (when ellipsis-pos
                       (let [bnd (dvar/*dotted-scope* drest-bnd)
                             _ (when-not bnd 
                                 (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))]
                         (r/DottedPretype1-maker
                           (free-ops/with-frees [bnd] ;with dotted bound in scope as free
                             (parse-type drest-type))
                           (:name bnd))))
                     :prest
                     (when push-rest-pos
                       (parse-type prest-type))
                     :pdot
                     (when push-dot-pos
                       (let [bnd (dvar/*dotted-scope* pdot-bnd)
                             _ (when-not bnd
                                 (err/int-error (str (pr-str pdot-bnd) " is not in scope as a dotted variable")))]
                         (r/DottedPretype1-maker
                           (free-ops/with-frees [bnd] ;with dotted bound in scope as free
                             (parse-type pdot-type))
                           (:name bnd))))
                     :filter filters
                     :object object
                     :flow flow
                     :optional-kws (when optional-kws
                                     (parse-kw-map optional-kws))
                     :mandatory-kws (when mandatory-kws
                                      (parse-kw-map mandatory-kws)))))

(defmethod parse-type* IPersistentVector
  [f]
  (apply r/make-FnIntersection [(parse-function f)]))

(defmethod parse-type* :default
  [k]
  (err/int-error (str "Bad type syntax: " (pr-str k)
                      (when ((some-fn symbol? keyword?) k)
                        (str "\n\nHint: Value types should be preceded by a quote or wrapped in the Value constructor." 
                             " eg. '" (pr-str k) " or (Value " (pr-str k)")")))))

(indu/add-indirection ind/parse-type parse-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparse

;; Unparsing types are generally agnostic to the current implementation.
;; Special types are unparsed under clojure.core.typed in the :unknown
;; implementation. All other types are verbosely printed under :unknown.

(defonce ^:dynamic *unparse-type-in-ns* nil)
(set-validator! #'*unparse-type-in-ns* (some-fn nil? symbol?))

(defn unparse-in-ns []
  {:post [((some-fn nil? symbol?) %)]}
  (or *unparse-type-in-ns*
      (impl/impl-case
        :clojure (ns-name *ns*)
        :cljs (@cljs-ns)
        :unknown nil)))

(defmacro with-unparse-ns [sym & body]
  `(binding [*unparse-type-in-ns* ~sym]
     ~@body))

(defn alias-in-ns
  "Returns an alias for namespace sym in ns, or nil if none."
  [nsym ns]
  {:pre [(string? nsym)
         (con/namespace? ns)]
   :post [((some-fn nil? symbol?) %)]}
  (impl/assert-clojure)
  (some (fn [[alias ans]]
          (when (= (str nsym) (str (ns-name ans)))
            alias))
        (ns-aliases ns)))

(defn core-lang-Class-sym [clsym]
  {:pre [(symbol? clsym)]
   :post [((some-fn nil? symbol?) %)]}
  (when (.startsWith (str clsym) "clojure.lang.")
    (symbol (.getSimpleName (Class/forName (str clsym))))))

(defn Class-symbol-intern [clsym ns]
  {:pre [(con/namespace? ns)]
   :post [((some-fn nil? symbol?) %)]}
  (some (fn [[isym cls]]
          (when (= (str clsym) (str (coerce/Class->symbol cls)))
            isym))
        (ns-imports ns)))

(defn var-symbol-intern 
  "Returns a symbol interned in ns for var symbol, or nil if none.

  (var-symbol-intern 'symbol (find-ns 'clojure.core))
  ;=> 'symbol
  (var-symbol-intern 'bar (find-ns 'clojure.core))
  ;=> nil"
  [sym ns]
  {:pre [(symbol? sym)
         (con/namespace? ns)]
   :post [((some-fn nil? symbol?) %)]}
  (some (fn [[isym var]]
          (when (= (str sym) (str (coerce/var->symbol var)))
            isym))
        (merge (ns-interns ns)
               (ns-refers ns))))

(defn unparse-Name-symbol-in-ns [sym]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  ;(prn "unparse-Name-symbol-in-ns" sym)
  (if-let [ns (and (not vs/*verbose-types*)
                   (some-> (unparse-in-ns) find-ns))]
    (impl/impl-case
      :clojure
      (or ; use an import name
          (Class-symbol-intern sym ns)
          ; core.lang classes are special
          (core-lang-Class-sym sym)
          ; use unqualified name if interned
          (when (namespace sym)
            (or (var-symbol-intern sym ns)
                ; use aliased ns if not interned, but ns is aliased
                (when-let [alias (alias-in-ns (namespace sym) ns)]
                  (symbol (str alias) (name sym)))))
          ; otherwise use fully qualified name
          sym)
      :cljs sym
      :unknown sym)
    sym))

(declare unparse-type*)

(defn unparse-type [t]
  ; quick way of giving a Name that the user is familiar with
  ;(prn "unparse-type" (class t))
  (if-let [nsym (-> t meta :source-Name)]
    nsym
    (unparse-type* t)))

(defmulti unparse-type* class)
(defn unp [t] (prn (unparse-type t)))

(defmethod unparse-type* Top [_] (unparse-Name-symbol-in-ns `t/Any))
;; TODO qualify vsym in current ns
(defmethod unparse-type* TypeOf [{:keys [vsym] :as t}] (list (unparse-Name-symbol-in-ns `t/TypeOf) vsym))
(defmethod unparse-type* Unchecked [{:keys [vsym] :as t}]
  (if vsym
    (list 'Unchecked vsym)
    'Unchecked))
(defmethod unparse-type* TCError [_] (unparse-Name-symbol-in-ns `t/TCError))
(defmethod unparse-type* Name [{:keys [id]}] (unparse-Name-symbol-in-ns id))
(defmethod unparse-type* AnyValue [_] (unparse-Name-symbol-in-ns `t/AnyValue))

(defmethod unparse-type* DottedPretype
  [{:keys [pre-type name]}]
  (list 'DottedPretype (unparse-type pre-type) (if (symbol? name)
                                                 (-> name r/make-F r/F-original-name)
                                                 name)))

(defmethod unparse-type* CountRange [{:keys [lower upper]}]
  (cond
    (= lower upper) (list (unparse-Name-symbol-in-ns `t/ExactCount)
                          lower)
    :else (list* (unparse-Name-symbol-in-ns `t/CountRange)
                 lower
                 (when upper [upper]))))

(defmethod unparse-type* App 
  [{:keys [rator rands]}]
  (list* (unparse-type rator) (mapv unparse-type rands)))

(defmethod unparse-type* TApp 
  [{:keys [rator rands] :as tapp}]
  (cond 
    ;perform substitution if obvious
    ;(TypeFn? rator) (unparse-type (resolve-tapp tapp))
    :else
    (list* (unparse-type rator) (mapv unparse-type rands))))

(defmethod unparse-type* Result
  [{:keys [t]}]
  (unparse-type t))

(defmethod unparse-type* F
  [{:keys [] :as f}]
  ; Note: don't print f here, results in infinite recursion
  ;(prn (-> f :name) (-> f :name meta))
  (r/F-original-name f))

(defmethod unparse-type* PrimitiveArray
  [{:keys [jtype input-type output-type]}]
  (cond 
    (and (= input-type output-type)
         (= Object jtype))
    (list 'Array (unparse-type input-type))

    (= Object jtype)
    (list 'Array2 (unparse-type input-type) (unparse-type output-type))

    :else
    (list 'Array3 (coerce/Class->symbol jtype)
          (unparse-type input-type) (unparse-type output-type))))

(defmethod unparse-type* B
  [{:keys [idx]}]
  (list 'B idx))

(defmethod unparse-type* Union
  [{types :types :as u}]
  (cond
    ; Prefer the user provided Name for this type. Needs more thinking?
    ;(-> u meta :from-name) (-> u meta :from-name)
    (seq types) (list* (unparse-Name-symbol-in-ns `t/U)
                       (doall (map unparse-type types)))
    :else (unparse-Name-symbol-in-ns `t/Nothing)))

(defmethod unparse-type* FnIntersection
  [{types :types}]
  (cond
    ; use vector sugar where appropriate
    (and (not vs/*verbose-types*)
         (== 1 (count types)))
    (unparse-type (first types))

    :else
    (list* (unparse-Name-symbol-in-ns `t/IFn)
           (doall (map unparse-type types)))))

(defmethod unparse-type* Intersection
  [{types :types}]
  (list* (unparse-Name-symbol-in-ns `t/I)
         (doall (map unparse-type types))))

(defmethod unparse-type* DifferenceType
  [{:keys [type without]}]
  (list* (unparse-Name-symbol-in-ns `t/Difference)
         (unparse-type* type)
         (doall (map unparse-type without))))

(defmethod unparse-type* NotType
  [{:keys [type]}]
  (list 'Not (unparse-type type)))

(defmethod unparse-type* TopFunction [_] 'AnyFunction)

(defn- unparse-kw-map [m]
  {:pre [((con/hash-c? r/Value? r/Type?) m)]}
  (into {} (for [[^Value k v] m] 
             [(.val k) (unparse-type v)])))

(defn unparse-result [{:keys [t fl o flow] :as rng}]
  {:pre [(r/Result? rng)]}
  (concat [(unparse-type t)]
          (when-not (every? (some-fn f/TopFilter? f/NoFilter?) [(:then fl) (:else fl)])
            [:filters (unparse-filter-set fl)])
          (when-not ((some-fn orep/NoObject? orep/EmptyObject?) o)
            [:object (unparse-object o)])
          (when-not ((some-fn f/TopFilter? f/NoFilter?) (:normal flow))
            [:flow (unparse-flow-set flow)])))

(defn unparse-bound [name]
  {:pre [((some-fn symbol? nat-int?) name)]}
  (if (symbol? name)
    (-> name r/make-F r/F-original-name)
    `(~'B ~name)))

(defmethod unparse-type* SymbolicClosure
  [{:keys [fexpr env]}]
  (list 'SymbolicClosure))

(defmethod unparse-type* Function
  [{:keys [dom rng kws rest drest prest pdot]}]
  (vec (concat (doall (map unparse-type dom))
               (when rest
                 [(unparse-type rest) '*])
               (when drest
                 (let [{:keys [pre-type name]} drest]
                   [(unparse-type pre-type)
                    '...
                    (unparse-bound name)]))
               (when kws
                 (let [{:keys [optional mandatory]} kws]
                   (list* '&
                          (concat
                            (when (seq mandatory)
                              [:mandatory (unparse-kw-map mandatory)])
                            (when (seq optional)
                              [:optional (unparse-kw-map optional)])))))
               (when prest
                 [(unparse-type prest) '<*])
               (when pdot
                 (let [{:keys [pre-type name]} pdot]
                   [(unparse-type pre-type)
                    '<...
                    (unparse-bound name)]))
               ['->]
               (unparse-result rng))))

(defn unparse-flow-set [flow]
  {:pre [(r/FlowSet? flow)]}
  (unparse-filter (r/flow-normal flow)))

(defmethod unparse-type* Protocol
  [{:keys [the-var poly?]}]
  (let [s (unparse-Name-symbol-in-ns the-var)]
    (if poly?
      (list* s (mapv unparse-type poly?))
      s)))

(defmethod unparse-type* DataType
  [{:keys [the-class poly?]}]
  (if poly?
    (list* (unparse-Name-symbol-in-ns the-class) (mapv unparse-type poly?))
    (unparse-Name-symbol-in-ns the-class)))

(defmethod unparse-type* RClass
  [{:keys [the-class poly?] :as r}]
  (if (empty? poly?)
    (unparse-Name-symbol-in-ns the-class)
    (list* (unparse-Name-symbol-in-ns the-class) (doall (map unparse-type poly?)))))

(defmethod unparse-type* Mu
  [m]
  (let [nme (-> (c/Mu-fresh-symbol* m) r/make-F r/F-original-name)
        body (c/Mu-body* nme m)]
    (list (unparse-Name-symbol-in-ns `t/Rec) [nme] (unparse-type body))))

(defn unparse-poly-bounds-entry [name {:keys [upper-bound lower-bound higher-kind] :as bnds}]
  (let [name (-> name r/make-F r/F-original-name)
        u (when upper-bound 
            (unparse-type upper-bound))
        l (when lower-bound 
            (unparse-type lower-bound))
        h (when higher-kind
            (unparse-type higher-kind))]
    (or (when higher-kind
          [name :kind h])
        (when-not (or (r/Top? upper-bound) (r/Bottom? lower-bound))
          [name :< u :> l])
        (when-not (r/Top? upper-bound) 
          [name :< u])
        (when-not (r/Bottom? lower-bound)
          [name :> l])
        name)))

(defn unparse-poly-dotted-bounds-entry [free-name bbnd]
  ; ignore dotted bound for now, not sure what it means yet.
  [(-> free-name r/make-F r/F-original-name) '...])

(defn unparse-poly-binder [dotted? free-names bbnds named]
  (let [named-remappings (apply sorted-map (interleave (vals named) (keys named)))
        {:keys [fixed-inb named-inb]} (group-by (fn [[i]]
                                                  (if (named-remappings i)
                                                    :named-inb
                                                    :fixed-inb))
                                                (map vector
                                                     (range)
                                                     free-names
                                                     bbnds))
        [fixed-inb dotted-inb] (if dotted?
                                 ((juxt pop peek) fixed-inb)
                                 [fixed-inb nil])
        unp-inb (fn [[_ free-name bbnd]]
                  (unparse-poly-bounds-entry free-name bbnd))
        binder (into (mapv unp-inb fixed-inb)
                     (concat
                       (when-let [[_ free-name bbnd] dotted-inb]
                         (unparse-poly-dotted-bounds-entry free-name bbnd))
                       (when named-inb
                         [:named (mapv unp-inb named-inb)])))]
    binder))

(defmethod unparse-type* PolyDots
  [{:keys [nbound named] :as p}]
  (let [free-names (vec (c/PolyDots-fresh-symbols* p))
        bbnds (c/PolyDots-bbnds* free-names p)
        binder (unparse-poly-binder true free-names bbnds named)
        body (c/PolyDots-body* free-names p)]
    (list (unparse-Name-symbol-in-ns `t/All) binder (unparse-type body))))

(defmethod unparse-type* Extends
  [{:keys [extends without]}]
  (list* 'Extends
         (mapv unparse-type extends)
         (when (seq without)
           [:without (mapv unparse-type without)])))

(defmethod unparse-type* Poly
  [{:keys [nbound named] :as p}]
  (let [free-names (c/Poly-fresh-symbols* p)
        ;_ (prn "Poly unparse" free-names (map meta free-names))
        bbnds (c/Poly-bbnds* free-names p)
        binder (unparse-poly-binder false free-names bbnds named)
        body (c/Poly-body* free-names p)]
    (list (unparse-Name-symbol-in-ns `t/All) binder (unparse-type body))))

;(ann unparse-typefn-bounds-entry [t/Sym Bounds Variance -> Any])
(defn unparse-typefn-bounds-entry [name {:keys [upper-bound lower-bound higher-kind]} v]
  (let [name (-> name r/make-F r/F-original-name)
        u (when upper-bound 
            (unparse-type upper-bound))
        l (when lower-bound 
            (unparse-type lower-bound))
        h (when higher-kind
            (unparse-type higher-kind))]
    (or (when higher-kind
          [name :variance v :kind h])
        (when-not (or (r/Top? upper-bound) (r/Bottom? lower-bound))
          [name :variance v :< u :> l])
        (when-not (r/Top? upper-bound) 
          [name :variance v :< u])
        (when-not (r/Bottom? lower-bound)
          [name :variance v :> l])
        [name :variance v])))

(defmethod unparse-type* TypeFn
  [{:keys [nbound] :as p}]
  (let [free-names (c/TypeFn-fresh-symbols* p)
        bbnds (c/TypeFn-bbnds* free-names p)
        binder (mapv unparse-typefn-bounds-entry free-names bbnds (:variances p))
        body (c/TypeFn-body* free-names p)]
    (list (unparse-Name-symbol-in-ns `t/TFn) binder (unparse-type body))))

(defmethod unparse-type* Value
  [v]
  (if ((some-fn r/Nil? r/True? r/False?) v)
    (:val v)
    (list (unparse-Name-symbol-in-ns `t/Val) (:val v))))

(defn- unparse-map-of-types [m]
  (into {} (map (fn [[k v]]
                  (assert (r/Value? k) k)
                  (vector (:val k) (unparse-type v)))
                m)))

(defmethod unparse-type* HeterogeneousMap
  [^HeterogeneousMap v]
  (list* (unparse-Name-symbol-in-ns `t/HMap)
         (concat
           ; only elide if other information is present
           (when (or (seq (:types v))
                     (not (or (seq (:optional v))
                              (seq (:absent-keys v))
                              (c/complete-hmap? v))))
             [:mandatory (unparse-map-of-types (.types v))])
           (when (seq (:optional v))
             [:optional (unparse-map-of-types (:optional v))])
           (when-let [ks (and (not (c/complete-hmap? v))
                              (seq (.absent-keys v)))]
             [:absent-keys (set (map :val ks))])
           (when (c/complete-hmap? v)
             [:complete? true]))))

(defn unparse-heterogeneous* [sym {:keys [types rest drest fs objects repeat] :as v}]
  (let [first-part (concat
                     (map unparse-type (:types v))
                     (when rest [(unparse-type rest) '*])
                     (when drest [(unparse-type (:pre-type drest))
                                  '...
                                  (unparse-bound (:name drest))]))]
    (list* sym
           (vec first-part)
           (concat
             (when repeat
               [:repeat true])
             (when-not (every? #{(fl/-FS f/-top f/-top)} fs)
               [:filter-sets (mapv unparse-filter-set fs)])
             (when-not (every? #{orep/-empty} objects)
               [:objects (mapv unparse-object objects)])))))

(defmethod unparse-type* HSequential [v]
  (unparse-heterogeneous*
    (case (:kind v)
      :list (unparse-Name-symbol-in-ns `t/HList)
      :vector (unparse-Name-symbol-in-ns `t/HVec)
      :seq (unparse-Name-symbol-in-ns `t/HSeq)
      :sequential (unparse-Name-symbol-in-ns `t/HSequential))
    v))

(defmethod unparse-type* HSet
  [{:keys [fixed] :as v}]
  {:pre [(every? r/Value? fixed)]}
  (list (unparse-Name-symbol-in-ns `t/HSet) (set (map :val fixed))))

(defmethod unparse-type* KwArgsSeq
  [^KwArgsSeq v]
  (list* 'KwArgsSeq 
         (concat
           (when (seq (.optional v))
             [:optional (unparse-map-of-types (.optional v))])
           (when (seq (.mandatory v))
             [:mandatory (unparse-map-of-types (.mandatory v))])
           (when (:complete? v)
             [:complete? (:complete? v)])
           (when (:nilable-non-empty? v)
             [:nilable-non-empty? (:nilable-non-empty? v)]))))

(defmethod unparse-type* AssocType
  [{:keys [target entries dentries]}]
  (list* (unparse-Name-symbol-in-ns `t/Assoc)
         (unparse-type target)
         (concat
           (doall (map unparse-type (apply concat entries)))
           (when dentries [(unparse-type (:pre-type dentries))
                           '...
                           (unparse-bound (:name dentries))]))))

(defmethod unparse-type* GetType
  [{:keys [target key not-found]}]
  (list* (unparse-Name-symbol-in-ns `t/Get)
         (unparse-type target)
         (unparse-type key)
         (when (not= r/-nil not-found)
           [(unparse-type not-found)])))

; CLJS Types

(defmethod unparse-type* JSNumber [_] (unparse-Name-symbol-in-ns 'cljs.core.typed/JSNumber))
(defmethod unparse-type* JSBoolean [_] (unparse-Name-symbol-in-ns 'cljs.core.typed/JSBoolean))
(defmethod unparse-type* JSObject [_] (unparse-Name-symbol-in-ns 'cljs.core.typed/JSObject))
(defmethod unparse-type* CLJSInteger [_] (unparse-Name-symbol-in-ns 'cljs.core.typed/CLJSInteger))
(defmethod unparse-type* JSString [_] (unparse-Name-symbol-in-ns 'cljs.core.typed/JSString))
(defmethod unparse-type* JSSymbol [_] (unparse-Name-symbol-in-ns 'cljs.core.typed/JSSymbol))
(defmethod unparse-type* JSUndefined [_] (unparse-Name-symbol-in-ns 'cljs.core.typed/JSUndefined))
(defmethod unparse-type* JSNull [_] (unparse-Name-symbol-in-ns 'cljs.core.typed/JSNull))
(defmethod unparse-type* JSObj [t] (list (unparse-Name-symbol-in-ns 'cljs.core.typed/JSObj)
                                         (zipmap (keys (:types t))
                                                 (map unparse-type (vals (:types t))))))

(defmethod unparse-type* ArrayCLJS
  [{:keys [input-type output-type]}]
  (cond 
    (= input-type output-type) (list 'Array (unparse-type input-type))
    :else (list 'Array2 (unparse-type input-type) (unparse-type output-type))))

(defmethod unparse-type* JSNominal
  [{:keys [name poly?]}]
  (let [sym (symbol name)]
    (if (seq poly?)
      (list* sym (map unparse-type poly?))
      sym)))

; Objects

(declare unparse-path-elem)

(defmulti unparse-object class)
(defmethod unparse-object EmptyObject [_] 'empty)
(defmethod unparse-object NoObject [_] 'no-object)
(defmethod unparse-object Path [{:keys [path id]}] (conj {:id id} (when (seq path) [:path (mapv unparse-path-elem path)])))

; Path elems

(defmulti unparse-path-elem class)
(defmethod unparse-path-elem KeyPE [t] (list 'Key (:val t)))
(defmethod unparse-path-elem CountPE [t] 'Count)
(defmethod unparse-path-elem ClassPE [t] 'Class)
(defmethod unparse-path-elem NthPE [t] (list 'Nth (:idx t)))
(defmethod unparse-path-elem KeysPE [t] 'Keys)
(defmethod unparse-path-elem ValsPE [t] 'Vals)
(defmethod unparse-path-elem KeywordPE [t] 'Keyword)

; Filters

(defmulti unparse-filter* class)

(declare unparse-filter)

(defn unparse-filter-set [{:keys [then else] :as fs}]
  {:pre [(f/FilterSet? fs)]}
  {:then (unparse-filter then)
   :else (unparse-filter else)})

(defn unparse-filter [f]
  (unparse-filter* f))

(defmethod unparse-filter* TopFilter [f] 'tt)
(defmethod unparse-filter* BotFilter [f] 'ff)
(defmethod unparse-filter* NoFilter [f] 'no-filter)

(declare unparse-type)

(defmethod unparse-filter* TypeFilter
  [{:keys [type path id]}]
  (concat (list 'is (unparse-type type) id)
          (when (seq path)
            [(mapv unparse-path-elem path)])))

(defmethod unparse-filter* NotTypeFilter
  [{:keys [type path id]}]
  (concat (list '! (unparse-type type) id)
          (when (seq path)
            [(mapv unparse-path-elem path)])))

(defmethod unparse-filter* AndFilter [{:keys [fs]}] (apply list '& (map unparse-filter fs)))
(defmethod unparse-filter* OrFilter [{:keys [fs]}] (apply list '| (map unparse-filter fs)))

(defmethod unparse-filter* ImpFilter
  [{:keys [a c]}]
  (list 'when (unparse-filter a) (unparse-filter c)))

;[TCResult -> Any]
(defn unparse-TCResult [r]
  (let [t (unparse-type (r/ret-t r))
        fs (unparse-filter-set (r/ret-f r))
        o (unparse-object (r/ret-o r))]
    (if (and (= (fl/-FS f/-top f/-top) (r/ret-f r))
             (= (r/ret-o r) orep/-empty))
      t
      (if (= (r/ret-o r) orep/-empty)
        [t fs]
        [t fs o]))))

(defn unparse-TCResult-in-ns [r ns]
  {:pre [((some-fn con/namespace? symbol?) ns)]}
  (binding [*unparse-type-in-ns* (if (symbol? ns)
                                   ns
                                   (ns-name ns))]
    (unparse-TCResult r)))

(defmethod unparse-type* TCResult
  [v]
  (unparse-TCResult v))

(indu/add-indirection ind/unparse-type unparse-type)

(ns clojure.core.typed.deps.clojure.core.contracts.constraints
  (:refer-clojure :exclude [== = not=])
  (:use [clojure.core.typed.deps.clojure.core.contracts.impl.utils :only (defcurry-from)])
  (:require [clojure.set  :as set]
            clojure.core.typed.deps.clojure.core.contracts
            clojure.core.typed.deps.clojure.core.contracts.impl.transformers))

;; # constraint functions and multimethods

(def all-numbers?  #(boolean (every? number? %&)))
(def all-positive? #(boolean (and (apply all-numbers? %&) (every? pos? %&))))
(def all-negative? #(boolean (and (apply all-numbers? %&) (every? (complement pos?) %&))))
(defn anything [& _] true)

(defn in
  "Takes an item and determines if it falls in the listed args.  This can be
   used most effectively for numbers since any numbers in a vector represent
   a range of values determined by the same arguments as given to `range`."
  [e & args]
  (boolean
   (some #{e}
         (mapcat #(if (vector? %)
                    (apply range %)
                    [%])
                 args))))

(def truthy #(when % true))
(def falsey #(not (truthy %)))

(defn whitelist
  "Takes a thing with keys (i.e. maps or sets) and checks if it contains only
   the keys listed in the given whitelist."
  [wl things]
  (set/subset? (set (keys things))
               (set wl)))

(defn implies
  "Logical implication"
  [p q]
  (or (not p) q))

(defn <-
  "Converse implication"
  [p q]
  (implies q p))

(defn except
  "P except Q"
  [p q]
  (not (implies p q)))

(defn <=>
  "Logical equality"
  [p q]
  (and (implies p q)
       (<- p q)))

(defn xor
  "Exclusive or"
  [p q]
    (not (<=> p q)))


(defcurry-from clojure.core
  == = not=)

(comment

  (defn sqr [n] (* n n))

  (def sqr_ (with-constraints
              sqr
              #(not= 0 %)))

  (sqr_ 100)
  (sqr_ 0)
)

(defmacro defconstrainedfn
  "Defines a function using the `contract` vector appearing after the arguments.

       (defconstrainedfn sqr
         [n] [number? (not= 0 n) => pos? number?]
         (* n n))

   Like the `contract` macro, multiple arity functions can be defined where each argument vector
   is immediately followed by the relevent arity expectations.  This macro will also detect
   if a map is in that constraints position and use that instead under the assumption that
   Clojure's `:pre`/`:post` map is used instead.
  "
  [name & body]
  (let [mdata (if (string? (first body))
                {:doc (first body)}
                {})
        body  (if (:doc mdata)
                (next body)
                body)
        body  (if (vector? (first body))
                (list body)
                body)
        body  (for [[args cnstr & bd] body]
                (list* args
                       (if (vector? cnstr)
                         (second (#'clojure.core.typed.deps.clojure.core.contracts.impl.transformers/build-constraints-description args cnstr (:doc mdata)))
                         cnstr)
                       bd))]
    `(defn ~name
       ~(str (:doc mdata))
       ~@body)))

(defn- build-positional-factory
  "Used to build a positional factory for a given type/record.  Because of the
  limitation of 20 arguments to Clojure functions, this factory needs to be
  constructed to deal with more arguments.  It does this by building a straight
  forward type/record ctor call in the <=20 case, and a call to the same
  ctor pulling the extra args out of the & overage parameter.  Finally, the
  arity is constrained to the number of expected fields and an ArityException
  will be thrown at runtime if the actual arg count does not match."
  [nom classname fields invariants chk]
  (let [fn-name (symbol (str '-> nom))
        [field-args over] (split-at 20 fields)
        field-count (count fields)
        arg-count (count field-args)
        over-count (count over)]
    `(defconstrainedfn ~fn-name
       [~@field-args ~@(if (seq over) '[& overage] [])]
       ~invariants
       (with-meta
         ~(if (seq over)
            `(if (= (count ~'overage) ~over-count)
               (new ~nom
                    ~@field-args
                    ~@(for [i (range 0 (count over))]
                        (list `nth 'overage i)))
               (throw (clojure.lang.ArityException. (+ ~arg-count (count ~'overage)) (name '~fn-name))))
            `(new ~nom ~@field-args))
         {:contract ~chk}))))

(defmacro defconstrainedrecord
  [name slots inv-description invariants & etc]
  (let [fields (vec slots)
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." name))
        ctor-name (symbol (str name \.))
        positional-factory-name (symbol (str "->" name))
        map-arrow-factory-name (symbol (str "map->" name))
        chk `(clojure.core.typed.deps.clojure.core.contracts/contract
                ~(symbol (str "chk-" name))
                ~inv-description
                [{:keys ~fields :as m#}]
                ~invariants)]
    `(do
       (let [t# (defrecord ~name ~fields ~@etc)]
         (defn ~(symbol (str name \?)) [r#]
           (= t# (type r#))))

       ~(build-positional-factory name classname fields invariants chk)

       (defconstrainedfn ~map-arrow-factory-name
         ([{:keys ~fields :as m#}]
            ~invariants
            (with-meta
              (merge (new ~name ~@(for [e fields] nil)) m#)
              {:contract ~chk})))
       ~name)))

(defn- apply-contract
  [f]
  (if (:hooked (meta f))
    f
    (with-meta
      (fn [& [m & args]]
        (if-let [contract (and m (-> m meta :contract))]
          ((partial contract identity) (apply f m args))
          (apply f m args)))
      {:hooked true
       :original f})))

(when *assert*
  (alter-var-root (var assoc) apply-contract)
  (alter-var-root (var dissoc) apply-contract)
  (alter-var-root (var merge) apply-contract)
  (alter-var-root (var merge-with) (fn [f] (let [mw (apply-contract f)] (fn [f & maps] (apply mw f maps)))))
  (alter-var-root (var into) apply-contract)
  (alter-var-root (var conj) apply-contract)
  (alter-var-root (var assoc-in) apply-contract)
  (alter-var-root (var update-in) apply-contract))

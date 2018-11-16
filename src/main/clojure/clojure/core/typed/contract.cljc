;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.contract
  "A contract system a la racket/contract.

  Main entry point is the `contract` macro."
  #?(:cljs (:require-macros [clojure.core.typed.contract :refer [contract instance-c]]))
  (:require [clojure.set :as set]))

;; A contract, the first argument to the `contract` macro
;; - name : Symbol
;;      a name for the contract, eg. 'int-c
;; - first-order : [Any -> Any]
;;      first order (flat) predicate for the current contract.
;;      Must return true for all inputs that passes the projection, but
;;      can also return true for values that fail the contract.
;;      eg. ifn? for [Int -> Int]
;; - projection : [Blame -> [Any -> Any]]
;;      A curried function that does the actual contract checking.
;;      Takes a Blame object and a value, and returns a new value that
;;      adheres to the current Contract object, otherwise blames Blame.
;;      eg. for the int-c contract:
;;          (fn [b]
;;            (fn [x]
;;              (if (integer? x)
;;                x
;;                (throw-blame b))))
;; - flat? : Boolean
;;      True if this is a flat contract, ie. first-order returns true
;;      for exactly the same values that pass the projection function.
(defrecord Contract [name first-order projection flat?])

;; A Blame object
;; - positive : (U String Symbol)
;;     Positive blame party.
;;     eg. "clojure.core.typed"
;; - negative : (U String Symbol)
;;     Negative blame party.
;;     eg. "Not clojure.core.typed"
;; - name (unused)
;; - contract (unused)
;; - file : (U nil String)
;;     File name where contract occurs.
;; - line, column : (U Integer nil)
;;     Line/column positions to blame.
(defrecord Blame [positive negative name contract file line column])

#_ (ann throw-blame [Blame -> Nothing])
(defn throw-blame 
  "Throw a blame object

  [Blame -> Nothing]"
  [{:keys [message positive negative file line column] :as b}]
  (throw
    (ex-info
      (str message "\n"
           "Positive blame: " positive "\n"
           "Negative blame: " negative "\n"
           "File: " file "\n"
           "Line: " line "\n"
           "Column: " column "\n")
      {:blame b})))

#_(ann-many [& :optional {:name (U Symbol String)
                                   :first-order [Any :-> Any]
                                   :projection [Blame :-> [Any :-> Any]]
                                   :flat? Boolean}
                      :-> Contract]
            make-contract
            make-flat-contract)
(defn make-contract 
  "Make a new contract.

  Keyword arguments: (see Contract datatype for more details)
  - :name         Name of the contract, (U Symbol String)
  - :first-order  First-order predicate for this contract, [Any -> Any]
  - :projection   Curried function taking blame and the value to check,
                  and returns a new checked value, or throws blame.
                  [Blame -> [Any -> Any]]
  - :flat?        True if this is a flat contract, Boolean"
  [& {:keys [name first-order projection flat?]
      :or {flat? false}}]
  (let [name (or name 
                 'anonymous-contract)
        first-order (or first-order 
                        (fn [x] true))
        projection (or projection
                       (fn [b]
                         (fn [x]
                           (if (first-order x)
                             x
                             (throw-blame b)))))]
    (map->Contract
      {:name name
       :first-order first-order
       :projection projection
       :flat? flat?})))


(defn make-flat-contract 
  "Calls `make-contract` but also passes `:flat? true` as the first arguments."
  [& args]
  (apply make-contract :flat? true args))

#_(ann make-blame [& :optional {:message String
                                :positive (U String Symbol)
                                :negative (U String Symbol)
                                :file (U Str nil)
                                :line (U Int nil)
                                :column (U Int nil)}
                   :-> Blame])
(defn make-blame 
  "Make a new blame object.

  Keyword arguments:
  - :message    A string message, String
  - :positive   Positive blame party, (U String Symbol)
  - :negative   Negative blame party, (U String Symbol)
  - :file       File that contains contract, (U Str nil)
  - :line       Line where contract occurs, (U Int nil)
  - :column     Column where contract occurs, (U Int nil)"
  [& {:as bls}]
  (map->Blame bls))

#?(:clj
(defmacro contract
  "Check a contract against a value, with an optional Blame object.
  
  (IFn [Contract Any -> Any]
       [Contract Any Blame -> Any])"
  ([c x] `(contract ~c ~x nil))
  ([c x b]
   `(((:projection ~c)
      (or ~b
          (make-blame :positive ~(str (ns-name *ns*))
                      :negative ~(str "Not " (ns-name *ns*))
                      :file ~*file*
                      :line ~(or (-> &form meta :line)
                                 @Compiler/LINE)
                      :column ~(or (-> &form meta :column)
                                   @Compiler/COLUMN))))
     ~x))))

#_(ann swap-blame [Blame :-> Blame])
(defn swap-blame 
  "Swap a blame object's blame parties.
  
  [Blame -> Blame]"
  [x] 
  {:pre [(instance? Blame x)]
   :post [(instance? Blame %)]}
  (-> x
      (assoc :positive (:negative x))
      (assoc :negative (:positive x))))

#_(ann int-c Contract)
(def int-c 
  "Flat contract for values that pass `integer?`."
  (make-flat-contract :name 'int-c :first-order integer?))

;; macro to allow instance? specialisation
#?(:clj
(defmacro instance-c
  "Flat contracts for instance? checks on Class's."
  [c]
  `(make-flat-contract :name (str ~c)
                       :first-order #(instance? ~c %))))

#_(ann Object-c Contract)
(def Object-c (instance-c Object))

#_(ann flat-val-c [Sym [Any -> Any] :-> Contract])
(defn flat-val-c
  "Contract generation for flat predicates."
  [name pred]
  (make-flat-contract :name name :first-order pred))

#_(ann-many Contract
            nil-c
            true-c
            false-c)
(def nil-c 
  "Contract that checks for `nil`."
  (flat-val-c 'nil-c nil?))
(def true-c 
  "Contract that checks for `true`."
  (flat-val-c 'true-c true?))
(def false-c 
  "Contract that checks for `false`."
  (flat-val-c 'false-c false?))

#_(ann any-c Contract)
(def any-c 
  "Contract that allows any value."
  (make-flat-contract :name any-c))

#_(ann count-range-c
       (IFn [Int -> Contract]
            [Int (U nil Int) -> Contract]))
(defn count-range-c 
  "Returns a flat contract that allows values with `count`
  greater-or-equal-to lower, and less-or-equal-to upper.
  Upper can be nil for positive infinity.

  (IFn [Int -> Contract]
       [Int (U nil Int) -> Contract])
  
  eg. (count-range-c 0 10)
      (count-range-c 0 nil)"
  ([lower] (count-range-c lower nil))
  ([lower upper]
   (make-flat-contract :name 'count-range-c
                       :first-order (fn [x]
                                      (and (or (nil? x)
                                               (coll? x))
                                           (if upper
                                             (<= lower (count x) upper)
                                             (<= lower (count x))))))))

#_(ann equiv-c [Any -> Contract])
(defn equiv-c
  "Returns a flat contract that returns true if a value is `=`
  to y.
  
  [Any -> Contract]"
  [y]
  (make-flat-contract :name 'equiv-c
                      :first-order (fn [x]
                                     (= x y))))

#_(ann identical-c [Any -> Contract])
(defn identical-c
  "Returns a flat contract that returns true if a value is `identical?`
  to y.
  
  [Any -> Contract]"
  [y]
  (make-flat-contract :name 'identical-c
                      :first-order (fn [x]
                                     (identical? x y))))


#_(ann ifn-c [(Vec Contract) Contract -> Contract])
(defn ifn-c 
  "Returns a function contract that checks a function has
  fixed domain that passes contracts `cs` and return value
  that passes contact `c2`.

  [(Vec Contract) Contract -> Contract]
  
  eg. (ifn-c [int-c] int-c)  ;; [Int -> Int] contract"
  [cs c2]
  {:pre [(every? #(instance? Contract %) cs)
         (instance? Contract c2)]
   :post [(instance? Contract %)]}
  (make-contract
    :name 'ifn-c
    :first-order ifn?
    :projection (fn [b]
                  (fn [f]
                    ; returning a contracted function
                    (contract (make-flat-contract
                                :name 'ifn?
                                :first-order ifn?)
                              f
                              b)
                    (with-meta
                      (fn [& xs]
                        (contract c2
                                  (apply f
                                         (map #(contract %1
                                                         %2
                                                         (swap-blame b))
                                              cs
                                              xs))
                                  b))
                      (if (fn? f)
                        (meta f)
                        nil))))))

(declare ->CheckedISeq)

(deftype CheckedISeq [s c b]
  clojure.lang.Sequential
  clojure.lang.ISeq
  (first [this]
    (contract c (first s) b))
  (next [this]
    (when-let [n (next s)]
      (->CheckedISeq n c b)))
  (cons [this x]
    (->CheckedISeq (conj s x) c b))
  (empty [this]
    (empty s))
  (seq [this]
    (when (seq s)
      this))
  (equiv [this o]
    (if (or (not (instance? clojure.lang.Sequential o))
            (not (instance? java.util.List o)))
      false
      (loop [ms this
             s (seq o)]
        (if (and s (= (first ms)
                      (first s)))
          (recur (next ms) (next s))
          (not ms))))))


#_(ann seqable-c [Contract :-> Contract])
(defn seqable-c
  "Alpha - subject to change.

  Returns a contract that checks Seqable things.
  
  [Contract -> Contract]"
  [c]
  {:pre [(instance? Contract c)]
   :post [(instance? Contract %)]}
  (make-contract
    :name 'seqable-c
    :projection (fn [b]
                  (fn [s]
                    (contract Object-c s b)
                    (reify
                      clojure.lang.Seqable
                      (seq [this]
                        (->CheckedISeq s c b)))))))

#_(ann or-c [Contract * :-> Contract])
(defn or-c
  "Returns a contract that checks a value passes at least
  one of the contracts `cs`.

  Any number of flat contracts may be passed to or-c. However,
  if more than one higher-order contract is provided, each time
  this contract is used, at most *one* may pass its first-order
  predicate.

  For example, (or-c (ifn-c [int-c] int-c) (ifn-c [] int-c))
  cannot be checked against `clojure.core/+` because
  the first-order check for both contracts (`ifn?`) passes.
  
  [Contract * -> Contract]
  
  eg. (or-c int-c nil-c) ;; (U Int nil)
      (or-c int-c (ifn-c [int-c] int-c)) ;; (U Int [Int -> Int])
  "
  [& cs]
  {:pre [(every? #(instance? Contract %) cs)]
   :post [(instance? Contract %)]}
  (let [{flat true hoc false} (group-by :flat? cs)
        ;_ (prn "flat" (mapv :name flat))
        ;_ (prn "hoc" (mapv :name hoc))
        flat-checks (apply some-fn (or (seq (map :first-order flat))
                                       ;; (U) always fails
                                       [(fn [_] false)]))
        choose-hoc
        (fn [x b]
          {:pre [(instance? Blame b)]}
          (let [hs (filter (fn [{:keys [first-order]}]
                             (first-order x))
                           hoc)]
            ;; don't realise more than needed, though chunking will
            ;; probably negate most of the benefit.
            (cond
              ;; more than one higher-order contract matched
              (second hs) (throw-blame b)
              ;; exactly one matched
              (first hs)  (contract (first hs) x b)
              ;; no contracts matched
              :else       (throw-blame b))))]
    (make-contract
      :name 'or-c
      :flat? (not (seq hoc))
      ; needed?
      :first-order (apply some-fn flat-checks (map :first-order hoc))
      :projection (fn [b]
                    (fn [x]
                      (if (flat-checks x)
                        x
                        (choose-hoc x b)))))))

#_(ann and-c [Contract * :-> Contract])
(defn and-c
  "Returns a contract that ensures a value passes each contract `cs`.

  At most *one* higher-order contract may be passed to `and-c`, and
  any number of flat contracts.

  [Contract * -> Contract]

  eg. (and-c (instance-c Boolean) true-c)  ;; (I Boolean true)"
  [& cs]
  {:pre [(every? #(instance? Contract %) cs)]
   :post [(instance? Contract %)]}
  (let [{flat true hoc false} (group-by (comp boolean :flat?) cs)
        ;_ (prn "flat" (mapv :name flat))
        ;_ (prn "hoc" (mapv :name hoc))
        ]
    (if (< (count hoc) 2)
      (let [h (first hoc)]
        (make-contract
          :name 'and-c
          :flat? (not h)
          :first-order (apply every-pred (or (seq (map :first-order cs))
                                             ;; (I) always passes
                                             (fn [_] true)))
          :projection (fn [b]
                        (fn [x]
                          (doseq [f flat]
                            (contract f x b))
                          ;; could stage this conditional
                          (if h
                            (contract h x b)
                            x)))))
      (throw (ex-info 
               "Cannot create and-c contract with more than one higher-order contract"
               {:hoc (map :name hoc)})))))

#_(ann hmap-c [& :optional {:mandatory (Map Keyword Contract)
                            :optional (Map Keyword Contract)
                            :absent-keys (Set Keyword)
                            :complete? Boolean}
               :-> Contract])
(defn hmap-c
  "Takes a map of mandatory and optional entry contracts,
  a set of absent keys, and :complete? true if this is a fully
  specified map. Intended to work with keyword keys, but should
  work with any keys looked up via =."
  [& {:keys [mandatory optional absent-keys complete?]
      :or {absent-keys #{}
           mandatory {}
           optional {}
           complete? false}}]
  (let [flat? (every? (comp :flat? val) (concat mandatory optional))
        ;_ (prn "flat?" flat?)
        mkeys (set (keys mandatory))
        okeys (set (keys optional))
        check-absent?
        (if complete?
          (fn [m] 
            {:pre [(map? m)]}
            true)
          (fn [m] 
            {:pre [(map? m)]}
            (empty? (set/intersection (set (keys m)) absent-keys))))
        check-completeness?
        (if complete?
          (fn [m]
            {:pre [(map? m)]}
            ;; only the mandatory or optional entries are allowed
            (empty? (set/difference (set (keys m))
                                    mkeys
                                    okeys)))
          (fn [m] 
            {:pre [(map? m)]}
            true))]
    (make-contract :name 'hmap-c
                   :flat? flat?
                   :first-order (fn [m]
                                  (and
                                    (map? m)
                                    (check-completeness? m)
                                    (check-absent? m)
                                    (every? (fn [[k {:keys [first-order]}]]
                                              (and (contains? m k)
                                                   (first-order (get m k))))
                                            mandatory)
                                    (every? (fn [[k {:keys [first-order]}]]
                                              (or (not (contains? m k))
                                                  (first-order (get m k))))
                                            optional)))
                   :projection (fn [b]
                                 (fn [m]
                                   (contract (make-flat-contract
                                               :name 'map?
                                               :first-order map?)
                                             m
                                             b)
                                   (contract (make-flat-contract
                                               :name 'hmap-completeness-check
                                               :first-order check-completeness?)
                                             m
                                             b)
                                   (contract (make-flat-contract
                                               :name 'hmap-absent-check 
                                               :first-order check-absent?)
                                             m
                                             b)
                                   (as-> 
                                     m ;; the expression
                                     m ;; the name to thread through

                                     ;; apply mandatory checks
                                     (reduce-kv (fn [m k c]
                                                  {:pre [(map? m)]
                                                   :post [(map? m)]}
                                                  (if (not (contains? m k))
                                                    (throw-blame 
                                                      (assoc b
                                                             :message (str k " key is missing")))
                                                    (if (:flat? c)
                                                      (do (contract c (get m k) b) ;; could be done asynchronously
                                                          m)
                                                      (update m k #(contract c % b)))))
                                                m ;; the current map
                                                mandatory)

                                     ;; apply optional checks
                                     (reduce-kv (fn [m k c]
                                                  {:pre [(map? m)]
                                                   :post [(map? m)]}
                                                  (if (not (contains? m k))
                                                    m
                                                    (if (:flat? c)
                                                      (do (contract c (get m k) b)
                                                          m)
                                                      (update m k #(contract c % b)))))
                                                m ;; the current map
                                                optional)

                                     ;; return the accumulated map
                                     m))))))

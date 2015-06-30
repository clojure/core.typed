;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "A unification library for Clojure."
      :author "Michael Fogus"}
  clojure.core.typed.deps.clojure.core.unify
  (:require [clojure.zip :as zip])
  (:use     [clojure.walk :as walk :only [prewalk]]))

(defn ignore-variable? [sym] (= '_ sym))

(def VARIABLE? #(or (ignore-variable? %)
                      (and (symbol? %) (re-matches #"^\?.*" (name %)))))

(defn- composite?
  "Taken from the old `contrib.core/seqable?`. Since the meaning of 'seqable' is
   questionable, I will work on phasing it out and using a more meaningful
   predicate.  At the moment, the only meaning of `composite?` is:
   Returns true if `(seq x)` will succeed, false otherwise." 
  [x]
  (or (seq? x)
      (instance? clojure.lang.Seqable x)
      (nil? x)
      (instance? Iterable x)
      (-> x class .isArray)
      (string? x)
      (instance? java.util.Map x)))

(declare garner-unifiers)

(defn- occurs?
  "Does v occur anywhere inside expr?"
  [variable? v expr binds]
  (loop [z (zip/zipper composite? seq #(do % %2) [expr])]
    (let [current (zip/node z)]
      (cond 
        (zip/end? z) false
        (= v current) true
        (and (variable? current)
             (find binds current))
        (recur (zip/next (zip/insert-right z (binds current))))
        (zip/end? z) false
        :else (recur (zip/next z))))))


(defn- bind-phase
  [binds variable expr]
  (if (or (nil? expr)
          (ignore-variable? variable))
    binds
    (assoc binds variable expr)))

(defn- determine-occursness
  [want-occurs? variable? v expr binds]
  (if want-occurs?
    `(if (occurs? ~variable? ~v ~expr ~binds)
       (throw (IllegalStateException. (str "Cycle found in the path " ~expr)))
       (bind-phase ~binds ~v ~expr))
    `(bind-phase ~binds ~v ~expr)))

(defmacro create-var-unification-fn
  [want-occurs?]
  (let [varp  (gensym)
        v     (gensym)
        expr  (gensym)
        binds (gensym)]
    `(fn var-unify
       [~varp ~v ~expr ~binds]
       (if-let [vb# (~binds ~v)] 
         (garner-unifiers ~varp vb# ~expr ~binds)
         (if-let [vexpr# (and (~varp ~expr) (~binds ~expr))]
           (garner-unifiers ~varp ~v vexpr# ~binds)
           ~(determine-occursness want-occurs? varp v expr binds))))))


(def ^{:doc "Unify the variable v with expr.  Uses the bindings supplied and possibly returns an extended bindings map."
       :private true}
  unify-variable (create-var-unification-fn true))

(def ^{:doc "Unify the variable v with expr.  Uses the bindings supplied and possibly returns an extended bindings map."
       :private true}
  unify-variable- (create-var-unification-fn false))

(defn wildcard? [form]
  (and (composite? form)
       (#{'&} (first form))))

(defn- garner-unifiers
  "Attempt to unify x and y with the given bindings (if any). Potentially returns a map of the 
   unifiers (bindings) found.  Will throw an `IllegalStateException` if the expressions
   contain a cycle relationship.  Will also throw an `IllegalArgumentException` if the
   sub-expressions clash."
  ([x y]           (garner-unifiers VARIABLE? x y))
  ([variable? x y] (garner-unifiers variable? x y {}))
  ([variable? x y binds] (garner-unifiers unify-variable variable? x y binds))
  ([uv-fn variable? x y binds]
     (cond
      (not binds)               nil
      (= x y)                   binds
      (variable? x)             (uv-fn variable? x y binds)
      (variable? y)             (uv-fn variable? y x binds)
      (wildcard? x)             (uv-fn variable? (second x) y binds)
      (wildcard? y)             (uv-fn variable? (second y) x binds)
      (every? composite? [x y]) (garner-unifiers variable?
                                                 (rest x) 
                                                 (rest y)
                                                 (garner-unifiers variable?
                                                                  (first x)
                                                                  (first y) 
                                                                  binds))
      :else (throw (IllegalArgumentException. (str "Clash found in " x))))))

(defn- subst-bindings
  "Flattens recursive bindings in the given map."
  ([binds] (subst-bindings VARIABLE? binds))
  ([variable? binds]
     (into {} (map (fn [[k v]]
                     [k (loop [v v]
                          (if (variable? v)
                            (recur (binds v))
                            v))])
                   binds))))

(defn- try-subst
  "Attempts to substitute the bindings in the appropriate locations in the given expression."
  [variable? x binds]
  {:pre [(map? binds) (fn? variable?)]}
  (walk/prewalk (fn [expr] 
                  (if (variable? expr)
                    (or (binds expr) expr) 
                    expr)) 
                x))

(defn- unifier*
  "Attempts the entire unification process from garnering the bindings to substituting
   the appropriate bindings."
  ([x y] (unifier* VARIABLE? x y))
  ([variable? x y]
     (unifier* variable? x y (garner-unifiers variable? x y)))
  ([variable? x y binds]
     (->> binds
          (subst-bindings variable?)
          (try-subst variable? y))))   ;; y is arbitrary

;; #  PUBLIC API

;; ## OCCURS

(defn make-occurs-unify-fn
  "Given a function to recognize unification variables, returns a function to
   return a bindings map for two expressions.  This function uses an 'occurs check'
   methodology for detecting cycles."
  [variable-fn]
  #(garner-unifiers unify-variable variable-fn % %2 {}))

(defn make-occurs-subst-fn
  "Given a function to recognize unification variables, returns a function that
   will attempt to substitute unification bindings between two expressions.
   This function uses an 'occurs check' methodology for detecting cycles."
  [variable-fn]
  (partial try-subst variable-fn))

(defn make-occurs-unifier-fn
  "Given a function to recognize unification variables, returns a function to
   perform the unification of two expressions. This function uses an 'occurs check'
   methodology for detecting cycles."
  [variable-fn]
  (partial unifier* variable-fn))


(def ^{:doc      (str (:doc (meta #'garner-unifiers))
                      "  Note: This function is implemented with an occurs-check.")
       :arglists '([expression1 expression2])}
  unify   (make-occurs-unify-fn VARIABLE?))

(def ^{:doc      (:doc (meta #'try-subst))
       :arglists '([expression bindings])}
  subst   (make-occurs-subst-fn VARIABLE?))

(def ^{:doc      (str (:doc (meta #'unifier*))
                      "  Note: This function is implemented with an occurs-check.")
       :arglists '([expression1 expression2])}
  unifier (make-occurs-unifier-fn VARIABLE?))

;; ## NO OCCURS

(defn make-unify-fn
  "Given a function to recognize unification variables, returns a function to
   return a bindings map for two expressions."
  [variable-fn]
  #(garner-unifiers unify-variable- variable-fn % %2 {}))

(defn make-subst-fn
  "Given a function to recognize unification variables, returns a function that
   will attempt to substitute unification bindings between two expressions."
  [variable-fn]
  (partial try-subst variable-fn))

(defn make-unifier-fn
  "Given a function to recognize unification variables, returns a function to
   perform the unification of two expressions."
  [variable-fn]
  (fn [x y]
    (unifier* variable-fn
              x
              y
              (garner-unifiers unify-variable- variable-fn x y {}))))


(def ^{:doc      (str (:doc (meta #'garner-unifiers))
                      "  Note: This function is implemented **without** an occurs-check.")
       :arglists '([expression1 expression2])}
  unify-   (make-unify-fn VARIABLE?))


(def ^{:doc      (str (:doc (meta #'unifier*))
                      "  Note: This function is implemented **without** an occurs-check.")
       :arglists '([expression1 expression2])}
  unifier- (make-unifier-fn VARIABLE?))

(ns clojure.core.typed.deps.clojure.core.contracts.impl.utils
  (:require [clojure.core.typed.deps.clojure.core.unify :as unify]))

(defn keys-apply [f ks m]
  (let [only (select-keys m ks)]
    (zipmap (keys only) (map f (vals only)))))


(defn manip-map [f ks m]
  (conj m (keys-apply f ks m)))


(defmacro assert-w-message
  [check message]
  `(when-not ~check
     (throw (new AssertionError (str "Assertion failure: " ~message "\n"
                                     (pr-str '~check))))))

(defn check-args!
  [name slots inv-description invariants]
  (assert-w-message (and inv-description (string? inv-description))
                    (str "Expecting an invariant description for " name))
  (assert-w-message (and invariants (or (map? invariants) (vector? invariants)))
                    (str "Expecting invariants of the form "
                         "[pre-conditions => post-conditions] or "
                         "{:pre [pre-conditions]}"
                         "for record type " name)))

;; Currying

(defn do-curried
  [name doc meta args body]
  (let [cargs (vec (butlast args))]
    `(defn ~name ~doc ~meta
       (~cargs (fn [x#] (~name ~@cargs x#)))
       (~args ~@body))))

(defmacro defcurried
  "Builds another arity of the fn that returns a fn awaiting the last
  param"
  [name doc meta args & body]
  (do-curried name doc meta args body))

(defmacro defcurry-from
  "Builds a pass-through curried fn for each name."
  [namespace & names]
  (->> (for [n names]
         (let [v (ns-resolve namespace n)]
           `(defcurried ~n
              ~(str "Curried version of " v)
              {:clojure.core.typed.deps.clojure.core.contracts/original ~v}
              [l# r#]
              (~v l# r#))))
       (cons `do)))

(defn constraint?
  "Determines if a symbol represents a
  core.contracts constraint."
  [sym]
  (-> sym
      resolve
      meta
      :clojure.core.typed.deps.clojure.core.contracts/original
      boolean))

(comment
  (macroexpand
   '(defcurry-from clojure.core
      == =))



  ((== 1) 1)

  (defcurried ===
    "test"
    {:added "1.5"}
    [l r]
    (== l r))

  ((=== 1) 2)

)

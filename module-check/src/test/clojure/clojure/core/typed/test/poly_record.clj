(ns clojure.core.typed.test.poly-record
  (:require [clojure.core.typed :as t]))

(t/ann-record [[foo :variance :invariant]] Foo [b :- (t/U nil Number)])
(t/tc-ignore
(defrecord Foo [b])
  )

(comment
(defmacro defrecord> 
  "Define a typed record.

  eg. ;monomorphic
      (defrecord> FooM [a :- (U nil Number),
                        b :- Number]
        Object
        (toString [this] \"\"))

      ;polymorphic
      (defrecord> [[x :variance :covariant]]
        FooP [a :- x,
              b :- Number]
        Object
        (toString [this] \"\"))"
  [& args]
  (let [vbnd (when (vector? (first args))
               (first args))
        args (if vbnd
               (next args)
               args)
        [nme fields & args] args]
    `(do (ann-record
           ~@(concat (when vbnd
                       [vbnd])
                     nme
                     fields))
         (defrecord ~nme ~(mapv first (partition 3 fields))
           ~@args))))
  )

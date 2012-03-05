;; Practical Variable-Arity Polymorphism

(+T clojure.core/map 
    (exist [t a & b]
      (fun (arity
             [(fun (arity [a & b :... b :> t]))
              & (poly IPersistentCollection b) :... b 
              :> (poly IPersistentCollection t)]))))

(+T clojure.core/+
    (fun (arity [& Number :> Number])))
(+T clojure.core/-
    (fun (dotted-arity [& Number :> Number])))

(+T clojure.core/reduce
    (exist [t a & b]
      (fun (arity
             [(fun (arity [t a & b :... b :> t]))
              t
              (poly IPersistentCollection a)
              & (poly IPersistentCollection b) :... b
              :> t]))))

(+T clojure.core/first
    (exist [a]
      (fun (arity [(poly IPersistentCollection a) :> a]))))

(+T clojure.core/rest
    (exist [a]
      (fun (arity [(poly IPersistentCollection a) :> (poly IPersistentCollection a)]))))

(+T clojure.core/next
    (exist [a]
      (fun (arity [(poly IPersistentCollection a) :> (poly IPersistentCollection a)]))))

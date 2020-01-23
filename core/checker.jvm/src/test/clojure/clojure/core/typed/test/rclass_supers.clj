(ns clojure.core.typed.test.rclass-supers
  (:require [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed :as t]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.type-rep :refer :all]
            [clojure.core.typed.checker.type-ctors :refer :all]
            [clojure.core.typed.checker.jvm.parse-unparse :refer [unparse-type]]
            [clojure.set :as set])
  (:import (clojure.lang Seqable IPersistentVector IPersistentCollection
                         Indexed IPersistentMap APersistentVector PersistentVector APersistentMap PersistentHashMap
                         PersistentHashSet PersistentTreeSet IPersistentSet APersistentSet
                         IPersistentList PersistentList ISeq ASeq IPersistentCollection Associative
                         IPersistentStack
                         PersistentTreeMap)))

(deftest rclass-supers-test
  (is-clj 
    (let [num (RClass-of Number)
          nnum (Un num -nil)]
        (doseq [[t r] {(RClass-of Seqable [num]) (Un -nil num)
                       (RClass-of IPersistentVector [num]) (Un -nil num)
                       (RClass-of APersistentVector [num]) (Un -nil num)
                       (RClass-of PersistentVector [num]) (Un -nil num)
                       (RClass-of IPersistentList [num]) (Un -nil num)
                       (RClass-of PersistentList [num]) (Un -nil num)
                       (RClass-of ISeq [num]) (Un -nil num)
                       (RClass-of ASeq [num]) (Un -nil num)
                       (RClass-of clojure.lang.Cons [num]) (Un -nil num)
                       (RClass-of clojure.lang.LazySeq [num]) (Un -nil num)
                       (RClass-of IPersistentCollection [num]) (Un -nil num)
                       (RClass-of IPersistentSet [num]) (Un -nil num)
                       (RClass-of APersistentSet [num]) (Un -nil num)
                       (RClass-of PersistentTreeSet [num]) (Un -nil num)
                       (RClass-of PersistentHashSet [num]) (Un -nil num)
                       (RClass-of Associative [num num]) -any
                       (RClass-of IPersistentStack [num]) (Un -nil num)
                       (RClass-of String) (Un -nil (RClass-of Character))
                       (RClass-of CharSequence) (Un -nil (RClass-of Character))
                       ; only supports key/val
                       ;(RClass-of clojure.lang.IMapEntry [num num]) num
                       (RClass-of clojure.lang.AMapEntry [num num]) num
                       (RClass-of clojure.lang.MapEntry [num num]) num
                       (RClass-of IPersistentMap [num num]) (Un -nil (-hvec [num num]))
                       (RClass-of APersistentMap [num num]) (Un -nil (-hvec [num num]))
                       (RClass-of PersistentHashMap [num num]) (Un -nil (-hvec [num num]))
                       (RClass-of PersistentTreeMap [num num]) (Un -nil (-hvec [num num]))}]
          (t/check-form* '(fn [a] (first a))
                         (binding [vs/*verbose-types* true]
                           (unparse-type
                             (make-FnIntersection
                               (make-Function [t] r))))))
        true)))

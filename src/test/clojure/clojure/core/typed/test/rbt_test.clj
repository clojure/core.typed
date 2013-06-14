(ns clojure.core.typed.test.rbt-test
  (:refer-clojure :exclude [and])
  (:require [clojure.core.typed :refer [ann inst cf fn> pfn> def-alias declare-names
                                        print-env print-filterset check-ns typed-deps
                                        ann-form]]
            [clojure.core.typed.test.rbt-types]
            [clojure.core.typed
             [type-rep :refer :all]
             [type-ctors :refer :all]
             [filter-rep :refer :all]
             [filter-ops :refer :all]
             [object-rep :refer :all]
             [path-rep :refer :all]
             [parse-unparse :refer :all]
             [check :as chk :refer [update tc-t]]]
            [clojure.repl :refer [pst]]
            [clojure.tools.analyzer :refer [ast]]
            [clojure.test :refer :all]))

(defmacro is-check-rbt [& body]
  `(is (do (check-ns '~'clojure.core.typed.test.rbt-types)
           ~@body)))

(comment
(-> (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.rbt-types/badRight]
                          (and (= :Black (-> tmap :tree))
                               (= :Red (-> tmap :left :tree))
                               (= :Red (-> tmap :right :tree))
                               (= :Red (-> tmap :right :left :tree)))))
                          ;(and (tc-pr-filters "first filter"
                          ;       (= :Black (-> tmap :tree)))
                          ;     (tc-pr-filters "second filter"
                          ;       (= :Red (-> tmap :left :tree)))
                          ;     (tc-pr-filters "third filter"
                          ;       (= :Red (-> tmap :right :tree)))
                          ;     (tc-pr-filters "fourth filter"
                          ;       (= :Red (-> tmap :right :left :tree))))
  ret-t :types first :rng :fl :else unparse-filter pprint)
)

(deftest update-nested-hmap-test
  (is-check-rbt (= (update (-hmap {(-val :left) (->Name 'clojure.core.typed.test.rbt-types/rbt)})
                           (-filter (-val :Red) 'id [(->KeyPE :left) (->KeyPE :tree)]))
                   (-hmap {(-val :left) 
                           (-hmap {(-val :tree) (-val :Red) 
                                   (-val :entry) (->Name 'clojure.core.typed.test.rbt-types/EntryT) 
                                   (-val :left) (->Name 'clojure.core.typed.test.rbt-types/bt) 
                                   (-val :right) (->Name 'clojure.core.typed.test.rbt-types/bt)})}))))
         
(deftest rbt-test
  #_(is-check-rbt (tc-t (clojure.core.typed/ann-form 
                        (fn [tmap]
                          (print-filterset "inner if"
                            (if (= :Red (-> tmap :right :tree))
                              (= :Red (-> tmap :right :left :tree))
                              false)))
                        [clojure.core.typed.test.rbt-types/badRight -> Any
                         :filters {:then (& (is ':Red 0 [(Key :right) (Key :tree)])
                                            (is ':Red 0 [(Key :right) (Key :left) (Key :tree)]))
                                   :else (| (! ':Red 0 [(Key :right) (Key :tree)])
                                            (! ':Red 0 [(Key :right) (Key :left) (Key :tree)]))}])))
  #_(is-check-rbt (tc-t (clojure.core.typed/ann-form 
                        (fn [tmap]
                          (print-filterset "entire"
                          (if (= :Black (-> tmap :tree))
                            (print-filterset "next1"
                              (if (= :Red (-> tmap :left :tree))
                                (print-filterset "innermost"
                                  (if (= :Red (-> tmap :right :tree))
                                    (= :Red (-> tmap :right :left :tree))
                                    false))
                                false))
                            false)))
                        [clojure.core.typed.test.rbt-types/badRight -> Any
                         :filters {:then (& (is ':Black tmap [(Key :tree)])
                                            (is ':Red tmap [(Key :left) (Key :tree)])
                                            (is ':Red tmap [(Key :right) (Key :tree)])
                                            (is ':Red tmap [(Key :right) (Key :left) (Key :tree)]))
                                   :else (| (! ':Black tmap [(Key :tree)])
                                            (! ':Red tmap [(Key :left) (Key :tree)])
                                            (! ':Red tmap [(Key :right) (Key :tree)])
                                            (! ':Red tmap [(Key :right) (Key :left) (Key :tree)]))}])))
  #_(is-check-rbt (tc-t (clojure.core.typed/ann-form 
                        (fn [tmap]
                          (if (= :Black (-> tmap :tree))
                            (print-filterset "next1"
                              (if (= :Red (-> tmap :left :tree))
                                (print-filterset "innermost"
                                  (if (= :Red (-> tmap :right :tree))
                                    (= :Red (-> tmap :right :left :tree))
                                    false))
                                false))
                            false))
                        [clojure.core.typed.test.rbt-types/badRight -> Any
                         :filters {:then (& (is ':Black tmap [(Key :tree)])
                                            (is ':Red tmap [(Key :left) (Key :tree)])
                                            (is ':Red tmap [(Key :right) (Key :tree)])
                                            (is ':Red tmap [(Key :right) (Key :left) (Key :tree)]))
                                   :else (| (! ':Black tmap [(Key :tree)])
                                            (! ':Red tmap [(Key :left) (Key :tree)])
                                            (! ':Red tmap [(Key :right) (Key :tree)])
                                            (! ':Red tmap [(Key :right) (Key :left) (Key :tree)]))}]))))

(let [f1 (parse-filter '(& (! (Value :Red) tmap ((Key :right) (Key :left) (Key :tree))) 
                           (is (Value :Red) tmap ((Key :left) (Key :tree)))))
      f2 (parse-filter '(& (! (Value :Red) tmap ((Key :right) (Key :tree))) 
                           (is (Value :Red) tmap ((Key :left) (Key :tree)))))]
  (prn "final")
  (unparse-filter (-or f1 f2)))

(unparse-filter 
  (parse-filter '(& (| (! (Value :Red) tmap ((Key :right) (Key :left) (Key :tree))) (is (Value :Red) tmap ((Key :left) (Key :tree)))) (| (! (Value :Red) tmap ((Key :right) (Key :left) (Key :tree))) (! (Value :Red) tmap ((Key :right) (Key :tree)))))))

(deftest update-composite
  (is-check-rbt
    (-> (clojure.core.typed.check/update-composite
          {'tmap (parse-type 'clojure.core.typed.test.rbt-types/badRight)}
          (parse-filter
            '(|
              (! (Value :Red) tmap ((Key :right) (Key :left) (Key :tree)))
              (! (Value :Red) tmap ((Key :right) (Key :tree)))
              (! (Value :Red) tmap ((Key :left) (Key :tree)))
              (! (Value :Black) tmap ((Key :tree))))))
        (get 'tmap)
        unparse-type)))


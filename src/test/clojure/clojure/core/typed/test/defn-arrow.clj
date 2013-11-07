(ns clojure.core.typed.test.defn-arrow
  (:require [clojure.test :refer :all]
            [clojure.core.typed :refer [defn>]]
            [clojure.core.typed.test.core :refer [subtype? check is-cf]]))

(def parse-typesig #'clojure.core.typed/defn>-parse-typesig)

(deftest defn>-parser-test
  (are [expected got] (= expected (parse-typesig got))
       '(Fn [-> foo] [baz -> bar])      '((:- foo []) (:- bar [a :- baz]))
       '[baz -> bar]                    '(:- bar [a :- baz])
       '[a b c d -> e]                  '(:- e [a0 :- a 
                                                b0 :- b 
                                                c0 :- c 
                                                d0 :- d])
       '(Fn [a -> b] [c -> d] [e -> f]) '((:- b [a0 :- a]) 
                                          (:- d [c0 :- c]) 
                                          (:- f [e0 :- e]))))
       
       
       

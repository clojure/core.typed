(ns clojure.core.typed.test.defn-arrow
  (:require [clojure.test :refer :all]
            [clojure.core.typed :refer [defn> AnyInteger] :as t]))

(def parse-typesig #'clojure.core.typed/defn>-parse-typesig)

(deftest defn>-parser-test
  (are [expected got] (= expected (parse-typesig got))
       '(clojure.core.typed/IFn [-> foo] [baz -> bar])      '((:- foo []) (:- bar [a :- baz]))
       '[baz -> bar]                    '(:- bar [a :- baz])
       '[a b c d -> e]                  '(:- e [a0 :- a 
                                                b0 :- b 
                                                c0 :- c 
                                                d0 :- d])
       '(clojure.core.typed/IFn 
          [a -> b] [c -> d] [e -> f]) '((:- b [a0 :- a]) 
                                          (:- d [c0 :- c]) 
                                          (:- f [e0 :- e]))))


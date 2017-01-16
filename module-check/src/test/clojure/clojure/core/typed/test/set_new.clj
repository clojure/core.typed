(ns clojure.core.typed.test.set_new
  (:require [clojure.core.typed :as t]
            [clojure.set :as set] 
            [clojure.test :refer :all]                
            [clojure.core.typed.test.test-utils :refer :all]))

(deftest subset-test
  (is-tc-e (subset? #{1} #{2}) Boolean 
           :requires [[clojure.set :refer [subset?]]])
  (is-tc-err (subset? #{1} #{2}) (t/Set t/Any) 
             :requires [[clojure.set :refer [subset?]]]))

(deftest superset-test
  (is-tc-e (superset? #{1} #{2}) Boolean 
           :requires [[clojure.set :refer [superset?]]])
  (is-tc-err (superset? #{1} #{2}) (t/Set t/Any) 
             :requires [[clojure.set :refer [superset?]]]))

(deftest join-test
  (is-tc-e (join #{{:a 1} {:a 2}} #{{:b 1} {:b 2}}) (t/Set (t/Map t/Any t/Any)) 
           :requires [[clojure.set :refer [join]]])
  (is-tc-err (join #{{:a 1} {:a 2}} #{{:b 1} {:b 2}}) (t/Vec t/Any)
             :requires [[clojure.set :refer [join]]]))

(deftest index-test
  (is-tc-e (index #{{:a 1 :b 2} {:a 3 :b 4} {:a 1 :b 5} {:a 2 :b 6}} [:a]) (t/Map (t/Map t/Any t/Any) (t/Set (t/Map t/Any t/Any))) 
           :requires [[clojure.set :refer [index]]])
  (is-tc-err (index #{{:a 1 :b 2} {:a 3 :b 4} {:a 1 :b 5} {:a 2 :b 6}} [:a]) (t/Vec t/Any) 
             :requires [[clojure.set :refer [index]]]))

(deftest MapInvert-test
  (is-tc-e (map-invert {:a 1, :b 2}) (t/Map t/Any t/Any)
           :requires [[clojure.set :refer [map-invert]]])
  (is-tc-err (map-invert {:a 1, :b 2}) (t/Vec t/Any) 
             :requires [[clojure.set :refer [map-invert]]]))

(deftest project-test
  (is-tc-e (project #{{:a "b" :c 1} {:a "d" :c 2}} [:a]) (t/Set (t/Map t/Any t/Any))
           :requires [[clojure.set :refer [project]]])
  (is-tc-err (project #{{:a "b" :c 1} {:a "d" :c 2}} [:a]) (t/Map t/Any t/Any) 
             :requires [[clojure.set :refer [project]]])
  (is-tc-err (project '(1 2 3) '(1)) (t/Set (t/Map t/Any t/Any))
             :requires [[clojure.set :refer [project]]]))

(deftest rename-test
  (is-tc-e (rename #{{:a 1, :b 1}  {:a 2, :b 2}} {:a :new-a}) (t/Set t/Any)
           :requires [[clojure.set :refer [rename]]])
  (is-tc-err (rename #{{:a 1, :b 1}  {:a 2, :b 2}} {:a :new-a}) (t/Vec t/Any) 
             :requires [[clojure.set :refer [rename]]])
  (is-tc-err (rename #{{:a 1, :b 1}  {:a 2, :b 2}} '(a new-a)) (t/Set t/Any) 
             :requires [[clojure.set :refer [rename]]]))


(deftest renameKeys-test
  (is-tc-e (rename-keys {:a 1, :b 2} {:a :new-a, :b :new-b}) (t/Map t/Any t/Any)
           :requires [[clojure.set :refer [rename-keys]]])
  (is-tc-err (rename-keys {:a 1, :b 2} {:a :new-a, :b :new-b}) (t/Vec t/Any) 
             :requires [[clojure.set :refer [rename-keys]]])
  (is-tc-err (rename-keys {:a 1, :b 2} '(a new-a)) (t/Map t/Any t/Any) 
             :requires [[clojure.set :refer [rename-keys]]]))

(deftest select-test
  (is-tc-e (select odd? #{1 2 3}) (t/Set t/Any)
           :requires [[clojure.set :refer [select]]])
  (is-tc-err (select odd? #{1 2 3}) (t/Vec t/Any) 
             :requires [[clojure.set :refer [select]]])
  (is-tc-err (select odd? '[1 2 3]) (t/Set t/Any) 
             :requires [[clojure.set :refer [select]]]))

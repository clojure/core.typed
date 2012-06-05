(ns typed.test.frees
  (:use [clojure.test]
        [typed.new]))

(deftest add-scopes-test
  (is (let [body (F 'a)]
        (= (add-scopes 0 body)
           body)))
  (is (let [body (F 'a)]
        (= (add-scopes 1 body)
           (Scope body))))
  (is (let [body (F 'a)]
        (= (add-scopes 3 body)
           (-> body Scope Scope Scope)))))

(deftest remove-scopes-test
  (is (let [scope (Scope (F 'a))]
        (= (remove-scopes 0 scope)
           scope)))
  (is (let [body (F 'a)]
        (= (remove-scopes 1 (Scope body))
           body))))

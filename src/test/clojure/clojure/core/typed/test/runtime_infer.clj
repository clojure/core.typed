(ns clojure.core.typed.test.runtime-infer
  (:require [clojure.test :refer :all]
            [com.gfredericks.test.chuck :as chuck]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [com.gfredericks.test.chuck.generators :as gen']
            [clojure.test.check.generators :as gen]
            [clojure.core.typed :as t]
            [clojure.pprint :refer [pprint]]
            [clojure.core.typed.runtime-infer :refer :all]))

(defmacro with-tmp-aliases [env as & body]
  `(binding [*envs* (atom (add-tmp-aliases ~env ~as))] 
     ~@body))

#_(defmacro with-type-and-alias-env 
  [t a & body]
  `(binding [*envs* 
             (atom {(current-ns)
                    {:type-env ~t
                     :alias-env ~a}})]
     ~@body))

(deftest join-test
  (checking
    "join maps"
    5
    [ts (gen/shuffle
          [(prs
             '{:E ':false})
           (prs
             '{:args (Vec '{:name clojure.lang.Symbol, :E ':var}),
               :fun
               (U
                '{:E ':lambda,
                  :arg clojure.lang.Symbol,
                  :body '{:name clojure.lang.Symbol, :E ':var},
                  :arg-type
                  '{:T ':intersection, :types clojure.lang.PersistentHashSet}}
                '{:name clojure.lang.Symbol, :E ':var}),
               :E ':app})])]

    (is (= (apply join ts)
           (prs
             (U
              '{:E ':false}
              '{:args (Vec '{:name clojure.lang.Symbol, :E ':var}),
                :fun
                (U
                 '{:E ':lambda,
                   :arg clojure.lang.Symbol,
                   :body '{:name clojure.lang.Symbol, :E ':var},
                   :arg-type
                   '{:T ':intersection, :types clojure.lang.PersistentHashSet}}
                 '{:name clojure.lang.Symbol, :E ':var}),
                :E ':app})))))
  (checking
    "inner vec"
    5
    [ts (gen/shuffle
          [(prs (Vec '{:a ?}))
           (prs (Vec '{:b ?}))])]
    (is 
      (= (apply join* ts)
         (prs (Vec (U '{:a ?} '{:b ?}))))))
  (checking 
    "functions"
    30
    [ts (gen/shuffle
          [(prs '{:a ?})
           (prs '{:a [? :-> ?]})
           (prs '{:a [? :-> Long]})
           (prs '{:a [Long :-> Long]})])]
    (= (apply join* ts)
       (prs '{:a [Long :-> Long]})))
  (checking
    "HOF"
    5
    [ts (gen/shuffle
          [(prs '{:f ?, :a java.lang.Long}) 
           (prs '{:f [[? :-> java.lang.Long] :-> ?], :a ?})])]
    (is
      (= (apply join* ts)
         (prs '{:f [[? :-> java.lang.Long] :-> ?], :a java.lang.Long}))))
  (checking
    "map return"
    5
    [ts (gen/shuffle
          [(prs ['{:f ?, :a java.lang.Long} :-> '{:b ?, :f clojure.lang.IFn, :a ?}])
           (prs ['{:f [[? :-> java.lang.Long] :-> ?], :a ?} :-> ?])])]
    (is (= (apply join ts)
           (prs
             ['{:f [[? :-> java.lang.Long] :-> ?], :a java.lang.Long}
              :->
              '{:b ?, :f clojure.lang.IFn, :a ?}]))))
  (checking
    "join union"
    5
    [ts (gen/shuffle
          [(prs
             (U '{:f [? :-> java.lang.Long], :a ?} 
                '{:f clojure.lang.IFn}))
           (prs 
             '{:f [? :-> java.lang.Long]})])]
     (is (= (apply join ts)
            (prs
              (U '{:f [? :-> java.lang.Long], :a ?} 
                 '{:f [? :-> java.lang.Long]})))))
  (checking
    "join fn in map"
    5
    [ts (gen/shuffle
          [(prs '{:f [[java.lang.Long :-> java.lang.Long] :-> ?]})
           (prs '{:f [[java.lang.Long :-> java.lang.Long] :-> java.lang.Long]})])]
    (is (= (apply join ts)
           (prs '{:f [[java.lang.Long :-> java.lang.Long] :-> java.lang.Long]})))))

(deftest squash-test
  (let [config (init-config)
        env (init-env)
        env (update-alias-env env merge
                              (with-tmp-aliases env '[a1 a2]
                                {'a1 (prs '{:a a2})
                                 'a2 (prs '{:a nil})}))]
    (binding [*envs* (atom env)]
      (is (= (alias-env (squash env config (prs a1)))
             {'a1 (prs '{:a (U nil a1)})
              'a2 (prs a1)}))))
#_
  (let [aenv (with-tmp-aliases '[a1 a2 a3 a4]
               {'a1 (prs a2)
                'a2 (prs '{:a a3})
                'a3 (prs a4)
                'a4 (prs
                      '{:a nil})})]
    (with-type-and-alias-env 
      (type-env @*envs*)
      aenv
      (is (= (squash-all (prs a1))
             (prs a1)))
      (is (= (alias-env)
             (with-tmp-aliases '[a1 a2]
               {'a1 (prs '{:a (U nil a1)})
                'a2 (prs a1)
                'a3 (prs a2) ;;TODO <^v
                'a4 (prs a3)
                }))))))

;; testing only
(defn update-path' [env infer-results]
  (let [config (init-config)
        env (reduce 
              (fn [env {:keys [path type]}]
                (update-path env config path type))
              env
              infer-results)]
    (type-env env)))

(deftest update-path-test
  (checking
    "update map"
    10
    [infers (gen/shuffle
              [(infer-result [(var-path 'use-map)
                              (key-path #{:a} :a)]
                             (-unknown))
               (infer-result [(var-path 'use-map)
                              (key-path #{:a} :a)]
                             (-class Long []))])]
    (is (= (update-path' (init-env) infers)
           {'use-map (prs '{:a Long})})))
  (checking
    "update nested map"
    20
    [infers (gen/shuffle
              [(infer-result [(var-path 'use-map)]
                             (-class clojure.lang.IFn []))
               (infer-result [(var-path 'use-map)
                              (fn-rng-path 1)
                              (key-path #{:b :f :a} :f)]
                             (-class clojure.lang.IFn []))
               (infer-result [(var-path 'use-map)
                              (fn-dom-path 1 0)
                              (key-path #{:f :a} :a)]
                             (-class Long []))
               (infer-result [(var-path 'use-map)
                              (fn-dom-path 1 0)
                              (key-path #{:f :a} :f)
                              (fn-dom-path 1 0)
                              (fn-rng-path 1)]
                             (-class Long []))])]
    (is (= (update-path' (init-env) infers))
        {'use-map
         (prs ['{:f [[? :-> Long] :-> ?], :a java.lang.Long} :-> '{:b ?, :f clojure.lang.IFn, :a ?}])}))
  (checking
    "function dom rng"
    10
    [infers (gen/shuffle
              [(infer-result [(var-path 'foo) (fn-rng-path 1)]
                             (parse-type 'Long))
               (infer-result [(var-path 'foo) (fn-dom-path 1 0)]
                             (parse-type 'Long))])]
    (is (= (update-path' (init-env) infers)
           {'foo (prs [Long :-> Long])})))
  (checking
    "unknown with function"
    10
    [infers (gen/shuffle
              [(infer-result [(var-path 'foo)] (prs ?))
               (infer-result [(var-path 'foo)] (prs [Long :-> ?]))])]
    (is (= (update-path' (init-env) infers)
           {'foo (prs [java.lang.Long :-> ?])})))

  (checking
    "combine functions"
    10
    [ts (gen/shuffle
          [(prs [(U '{:f [? :-> java.lang.Long], :a ?} 
                    '{:f [? :-> java.lang.Long]}) :-> 
                 '{:b ?, :f ?, :a java.lang.Long}])
           (prs ['{:f clojure.lang.IFn, :a ?} :-> ?])])]

    (is
      (= (apply join ts)
         (prs
           [(U '{:f [? :-> java.lang.Long], :a ?} '{:f [? :-> java.lang.Long]})
            :->
            '{:b ?, :f ?, :a java.lang.Long}]))))

  (checking
    "IFn with fns"
    10
    [ts (gen/shuffle 
          [(prs (U '{:f [? :-> java.lang.Long], :a ?} 
                   '{:f [? :-> java.lang.Long]}))
           (prs '{:f clojure.lang.IFn, :a ?})])]
    (is
      (= (apply join ts)
         (prs (U '{:f [? :-> java.lang.Long], :a ?} 
                 '{:f [? :-> java.lang.Long]})))))
  
  (checking
    "big"
    75
    [infers (gen/shuffle
              (mapv parse-infer-result
                    '[[[#'clojure.core.typed.runtime-infer/use-map (dom 1 0) (key #{:f} :f)]
                       :-
                       clojure.lang.IFn]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (dom 1 0)
                        (key #{:f :a} :f)
                        (rng 1)]
                       :-
                       java.lang.Long]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (rng 1)
                        (key #{:b :f :a} :a)]
                       :-
                       java.lang.Long]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (dom 1 0)
                        (key #{:f} :f)
                        (rng 1)]
                       :-
                       java.lang.Long]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (dom 1 0)
                        (key #{:f :a} :f)]
                       :-
                       clojure.lang.IFn]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (dom 1 0)
                        (key #{:f :a} :f)
                        (dom 1 0)]
                       :-
                       clojure.lang.IFn]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (dom 1 0)
                        (key #{:f} :f)
                        (dom 1 0)
                        (rng 1)]
                       :-
                       java.lang.Long]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (dom 1 0)
                        (key #{:f :a} :a)]
                       :-
                       java.lang.Long]
                      [[#'clojure.core.typed.runtime-infer/use-map] :- clojure.lang.IFn]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (dom 1 0)
                        (key #{:f} :f)
                        (dom 1 0)
                        (dom 1 0)]
                       :-
                       java.lang.Long]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (rng 1)
                        (key #{:b :f} :f)]
                       :-
                       clojure.lang.IFn]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (dom 1 0)
                        (key #{:f :a} :f)
                        (dom 1 0)
                        (dom 1 0)]
                       :-
                       java.lang.Long]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (dom 1 0)
                        (key #{:f :a} :f)
                        (dom 1 0)
                        (rng 1)]
                       :-
                       java.lang.Long]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (rng 1)
                        (key #{:b :f} :b)]
                       :-
                       java.lang.Long]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (rng 1)
                        (key #{:b :f :a} :f)]
                       :-
                       clojure.lang.IFn]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (dom 1 0)
                        (key #{:f} :f)
                        (dom 1 0)]
                       :-
                       clojure.lang.IFn]
                      [[#'clojure.core.typed.runtime-infer/use-map
                        (rng 1)
                        (key #{:b :f :a} :b)]
                       :-
                       java.lang.Long]]))]
    (is
      (= (update-path' (init-env) infers)
         {'clojure.core.typed.runtime-infer/use-map
          (prs
            [(U
              '{:f [[java.lang.Long :-> java.lang.Long] :-> java.lang.Long]}
              '{:f [[java.lang.Long :-> java.lang.Long] :-> java.lang.Long],
                :a java.lang.Long})
             :->
             (U
              '{:b java.lang.Long, :f clojure.lang.IFn, :a java.lang.Long}
              '{:b java.lang.Long, :f clojure.lang.IFn})])})))

#_
(is
(ppenv
  (update-path' {}
                (mapv parse-infer-result
'[[[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:name :E} :name)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :args)
   (index 2 0)
   (key #{:name :E} :E)]
  :-
  ':var]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:E :arg :body :arg-type} :body)
   (key #{:name :E} :name)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:E :arg :body :arg-type} :arg-type)
   (key #{:T :types} :types)]
  :-
  clojure.lang.PersistentHashSet]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:E :arg :body :arg-type} :body)
   (key #{:name :E} :E)]
  :-
  ':var]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:E :arg :body :arg-type} :arg)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :args)
   (index 1 0)
   (key #{:name :E} :name)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :E)]
  :-
  ':app]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :args)
   (index 1 0)
   (key #{:name :E} :E)]
  :-
  ':var]
 [[#'clojure.core.typed.test.mini-occ/parse-exp (dom 1 0)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp (dom 1 0)]
  :-
  clojure.lang.PersistentList]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:else :E :then :test} :then)
   (key #{:name :E} :E)]
  :-
  ':var]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :fun)
   (key #{:E :arg :body :arg-type} :body)
   (key #{:name :E} :name)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :args)
   (index 2 0)
   (key #{:name :E} :name)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:else :E :then :test} :else)
   (key #{:name :E} :E)]
  :-
  ':var]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:else :E :then :test} :test)
   (key #{:name :E} :name)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp] :- clojure.lang.IFn]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :fun)
   (key #{:name :E} :name)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :args)
   (index 2 1)
   (key #{:name :E} :E)]
  :-
  ':var]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:else :E :then :test} :test)
   (key #{:name :E} :E)]
  :-
  ':var]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :fun)
   (key #{:name :E} :E)]
  :-
  ':var]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :args)
   (index 2 1)
   (key #{:name :E} :name)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp (rng 1) (key #{:E} :E)]
  :-
  ':false]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:E :arg :body :arg-type} :arg-type)
   (key #{:T :types} :T)]
  :-
  ':intersection]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :fun)
   (key #{:E :arg :body :arg-type} :arg-type)
   (key #{:T :types} :T)]
  :-
  ':intersection]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:else :E :then :test} :E)]
  :-
  ':if]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:else :E :then :test} :else)
   (key #{:name :E} :name)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp (dom 1 0)] :- false]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :fun)
   (key #{:E :arg :body :arg-type} :E)]
  :-
  ':lambda]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :fun)
   (key #{:E :arg :body :arg-type} :body)
   (key #{:name :E} :E)]
  :-
  ':var]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:else :E :then :test} :then)
   (key #{:name :E} :name)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:name :E} :E)]
  :-
  ':var]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :fun)
   (key #{:E :arg :body :arg-type} :arg)]
  :-
  clojure.lang.Symbol]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:E :arg :body :arg-type} :E)]
  :-
  ':lambda]
 [[#'clojure.core.typed.test.mini-occ/parse-exp
   (rng 1)
   (key #{:args :fun :E} :fun)
   (key #{:E :arg :body :arg-type} :arg-type)
   (key #{:T :types} :types)]
  :-
  clojure.lang.PersistentHashSet]
]


  )))))

(deftest delete-generated-annotations-in-str-test
  (is (= (delete-generated-annotations-in-str
           (str "\n"
                generate-ann-start 
                "\n"
                "foo\n"
                generate-ann-end "\n"
                "bar\n\n"))
         "\nbar\n")))

#_(deftest group-arities-test
  (is (group-arities
        {:op :IFn, 
         :arities [{:op :IFn1, :dom [], :rng {:op :class, :class java.lang.Long, :args []}}]}
        {:op :IFn, :arities [{:op :IFn1, :dom [{:op :unknown}], :rng {:op :class, :class java.lang.Long, :args []}}]})))

(deftest squash-horizonally-test
  (is (let [config (init-config)
            env (as-> (init-env) env
                  (update-alias-env env
                                    assoc 
                                    'foo (prs '{:baz Long})
                                    'bar (prs '{:baz Boolean}))
                  (binding [*envs* (atom env)]
                    (update-type-env env
                                     assoc 
                                     `var1 (prs foo)
                                     `var2 (prs bar))))
            env (squash-horizonally env config)
            env (follow-all env (assoc config :simplify? false))]
        (prn env)
        (= (get (type-env env) `var1)
           (get (type-env env) `var2)))))

(deftest track-test
  ;; avoid NPE for list*
  (is (track (atom {}) '() [])))

(defn anns-from-tenv [tenv]
  (let [ns (create-ns (gensym))]
    (binding [*ann-for-ns* (constantly ns)
              *ns* ns]
      (let [_ (prn (current-ns))
            env (as-> (init-env) env
                  (update-type-env env merge tenv))
            config (init-config)
            env (populate-envs env config)
            anns (envs-to-annotations env config)]
        (pprint anns)))))

(let [st-type (prs
                '{:quads
                  (Vec
                    '{:klingons java.lang.Long,
                      :quadrant (Vec java.lang.Long),
                      :stars java.lang.Long,
                      :bases java.lang.Long}),
                  :stardate
                  '{:start java.lang.Long,
                    :current java.lang.Long,
                    :end java.lang.Long},
                  :current-klingons (Vec Nothing),
                  :starting-klingons java.lang.Long,
                  :lrs-history (Vec java.lang.String),
                  :current-sector (Vec java.lang.Long),
                  :enterprise
                  '{:photon_torpedoes java.lang.Long,
                    :sector (Vec java.lang.Long),
                    :quadrant (Vec (U java.lang.Integer java.lang.Long)),
                    :energy java.lang.Long,
                    :damage
                    '{:phasers java.lang.Long,
                      :warp_engines java.lang.Long,
                      :damage_control java.lang.Long,
                      :long_range_sensors java.lang.Long,
                      :short_range_sensors java.lang.Long,
                      :computer_display java.lang.Long,
                      :photon_torpedo_tubes java.lang.Long,
                      :shields java.lang.Long},
                    :is_docked false,
                    :shields java.lang.Long}})]
  (anns-from-tenv {'t1 st-type
                   't2 st-type}))

(let [t (prs
  [(U
    '{:exp '{:name Sym, :E ':var},
      :P ':is,
      :type '{:T ':intersection, :types (Set Nothing)}}
    '{:P ':not,
      :p
      '{:P ':=,
        :exps
        (Set
         (U
          '{:name Sym, :E ':var}
          '{:args (Vec '{:name Sym, :E ':var}),
            :fun '{:name Sym, :E ':var},
            :E ':app}))}}
    '{:P ':=,
      :exps
      (Set
       (U
        '{:name Sym, :E ':var}
        '{:args (Vec '{:name Sym, :E ':var}),
          :fun '{:name Sym, :E ':var},
          :E ':app}))}
    '{:P (U ':or ':and),
      :ps
      (Set
       (U
        '{:exp '{:name Sym, :E ':var},
          :P ':is,
          :type '{:T ':intersection, :types (Set Nothing)}}
        '{:P ':=,
          :exps
          (Set
           (U
            '{:name Sym, :E ':var}
            '{:args (Vec '{:name Sym, :E ':var}),
              :fun '{:name Sym, :E ':var},
              :E ':app}))}))})
   :->
   Any])]
(anns-from-tenv {'unparse-prop1 t
                 'unparse-prop2 t}))

(ns clojure.core.typed.annotator.test.runtime-infer
  (:require [clojure.test :refer :all]
            [clojure.pprint :as pp]
            [clojure.repl :as repl]
            [clojure.set :as set]
            [com.gfredericks.test.chuck :as chuck]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [com.gfredericks.test.chuck.generators :as gen']
            [clojure.test.check.generators :as gen]
            [clojure.core.typed :as t]
            [clojure.core.typed.analyzer :as ana2]
            [clojure.tools.analyzer.jvm :as taj]
            [clojure.core.typed.analyzer.jvm :as jana2]
            [clojure.core.typed.runtime-infer :refer :all :as infer]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.annotator.pprint :refer [pprint]] 
            [clojure.core.typed.annotator.parse :refer [prs parse-type]]
            [clojure.core.typed.annotator.rep :refer [infer-result
                                                      var-path
                                                      key-path
                                                      fn-rng-path
                                                      fn-dom-path]
             :as rep]
            [clojure.core.typed.annotator.track :refer [track-var] :as track]
            [clojure.core.typed.annotator.join :refer [make-Union
                                                       join*
                                                       join-HMaps
                                                       join]]
            [clojure.core.typed.annotator.env :as env]
            [clojure.core.typed.annotator.frontend.spec :refer [unparse-spec'
                                                                envs-to-specs]]
            [clojure.core.typed.annotator.insert :refer [delete-generated-annotations-in-str
                                                         generate-ann-start
                                                         generate-ann-end]
             :as insert]
            [clojure.core.typed.annotator.util :refer [*ann-for-ns*
                                                       spec-ns
                                                       current-ns
                                                       unparse-type
                                                       *debug*
                                                       update-alias-env
                                                       type-env
                                                       *envs*
                                                       update-type-env
                                                       HMap-req-keyset]]
            )
  (:import (clojure.lang IExceptionInfo)))

; ppenv : Env -> nil
(defn ppenv [env]
  (pprint (into {}
                (map (fn [[k v]]
                       [k (unparse-type v)]))
                env)))

(defn add-tmp-aliases [env as]
  (update-alias-env env merge (zipmap as (repeat nil))))

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

(deftest union-test
  (is (= (make-Union [(prs Long)
                      (prs Double)])
         (prs Number)
         (make-Union [(prs Long)
                      (prs Double)
                      (prs Number)])
         )))

(deftest join-test
  (is (= (join* (prs String))
         (make-Union [(prs String)])
         (prs String)))
  (is (= (join* (prs Sym))
         (make-Union [(prs Sym)])
         (prs Sym)))
  (is (not= (prs Any)
            (prs (U Sym String))))
  ;;FIXME
  #_
  (is (=
       (prs (U '{:f [? :-> java.lang.Long], :a ?} 
               '{:f [? :-> java.lang.Long]}))
       (prs (HMap :mandatory {:f [? :-> java.lang.Long]} 
                  :optional {:a ?}))))
  ;;FIXME
  #_
  (is (=
       (join-HMaps
         (prs '{:f ':op1, :a Any})
         (prs '{:f ':op2, :a Any}))
       (join
         (prs '{:f ':op1, :a Any})
         (prs '{:f ':op2, :a Any}))
       (prs (U 
              '{:f ':op1, :a Any}
              '{:f ':op2, :a Any}))))
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
                  '{:T ':intersection, :types (Seqable Any)}}
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
                   '{:T ':intersection, :types (Seqable Any)}}
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
  #_
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
        env (generate-tenv
              env
              config
              {:infer-results infer-results})]
    (type-env env)))

(deftest update-path-test
  (checking
    "update map"
    10
    [infers (gen/shuffle
              [(infer-result [(var-path 'use-map)
                              (key-path #{:a} :a)]
                             -unknown)
               (infer-result [(var-path 'use-map)
                              (key-path #{:a} :a)]
                             (prs Long))])]
    (is (= (update-path' (init-env) infers)
           {'use-map (prs '{:a Long})})))
  (checking
    "update nested map"
    20
    [infers (gen/shuffle
              [(infer-result [(var-path 'use-map)]
                             (prs clojure.lang.IFn))
               (infer-result [(var-path 'use-map)
                              (fn-rng-path 1)
                              (key-path #{:b :f :a} :f)]
                             (prs clojure.lang.IFn))
               (infer-result [(var-path 'use-map)
                              (fn-dom-path 1 0)
                              (key-path #{:f :a} :a)]
                             (prs Long))
               (infer-result [(var-path 'use-map)
                              (fn-dom-path 1 0)
                              (key-path #{:f :a} :f)
                              (fn-dom-path 1 0)
                              (fn-rng-path 1)]
                             (prs Long))])]
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
    (is (= (update (update-path' (init-env) infers) 'foo dissoc :top-level-def)
           {'foo (prs [Long :-> Long])})))
  (checking
    "unknown with function"
    10
    [infers (gen/shuffle
              [(infer-result [(var-path 'foo)] (prs ?))
               (infer-result [(var-path 'foo)] (prs [Long :-> ?]))])]
    (is (= (update (update-path' (init-env) infers) 'foo dissoc :top-level-def)
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
)

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
        (pprint env)
        (= (get (type-env env) `var1)
           (get (type-env env) `var2)))))

(defmacro try-prim [invoke-args & body]
  (let [name (gensym)]
    `(do (defn ~name ~@body)
         (alter-var-root 
           (var ~name)
           (constantly
             (track-var ~name)))
         (~name ~@invoke-args))))

(deftest track-prim-fn
  (is (=
       :ok
       (try-prim
         [1]
         [^long a]
         :ok)))
  (is (=
       10
       (try-prim
         [1 'a]
         ^long [^long a ^Object b]
         10)))
  (is (=
       10
       (try-prim
         []
         ^long []
         10)))
  (is (=
       10.5
       (try-prim
         []
         ^double []
         10.5)))
  (is (=
       :ok
       (try-prim
         [1]
         (^double [] 10.5)
         (^Object [^long a] :ok))))
  (is (=
       10.5
       (try-prim
         []
         (^double [] 10.5)
         (^Object [^long a] :ok))))
  )

(deftest mini-occ-test
  (is 
    (do
      (require 'clojure.core.typed.annotator.test.mini-occ :reload)
      :ok)))

(deftest optional-keys-test
  (is 
    (= 
      (join-HMaps
        (prs
          (HMap :optional {:a Any}))
        (prs
          (HMap :optional {:a Any})))
      (prs
        (HMap :optional {:a Any}))))
  (is 
    (= 
      (join-HMaps
        (prs
          (HMap :optional {:a String}))
        (prs
          (HMap :mandatory {:a Long})))
      (prs
        (HMap :optional {:a (U String Long)}))))
  (is 
    (= 
      (join-HMaps
        (prs
          (HMap :mandatory {:op ':Foo}))
        (prs
          (HMap :mandatory {:op ':Foo}
                :optional {:bar String})))
      (prs
        (HMap :mandatory {:op ':Foo}
              :optional {:bar String}))))

  (is (=
       (HMap-req-keyset
         (prs
           (HMap :mandatory {:a Long})))
       #{:a}))
  (is (=
       (HMap-req-keyset
         (prs
           (HMap :optional {:a Long})))
       #{}))
  (is (=
       (HMap-req-keyset
         (prs
           (HMap :optional {:a Long}
                 :mandatory {:b Long})))
       #{:b}))
  (is (=
       (HMap-req-opt-keysets
         (prs
           (HMap :optional {:a Long}
                 :mandatory {:b Long})))
       #{{:req-keyset #{:b}, :opt-keyset #{:a}}}))
  (is (=
       (make-Union
         [(prs
            (HMap :mandatory {:op ':Foo}))
          (prs
            (HMap :mandatory {:op ':Foo}
                  :optional  {:opt String}))])
       (prs (HMap :mandatory {:op ':Foo}
                  :optional {:opt String}))))
  (is (=
       (make-Union
         [(prs
            (HMap :mandatory {:op ':Foo}
                  :optional {:opt Long}))
          (prs
            (HMap :optional {:op ':Foo
                             :opt String}))])
       (prs (HMap :optional {:op ':Foo
                             :opt (U Long String)}))))
  (is 
    (= 
      (join-HMaps
        (prs
          '{:op ':the-foo
            :the-bar Sym
            :opt Sym})
        (prs
          '{:op ':the-foo
            :the-bar Sym}))
      (prs
        (HMap :mandatory {:op ':the-foo
                          :the-bar Sym}
              :optional {:opt Sym}))))
)

(def ^:dynamic *print-anns* true)

(defn *-from-tenv [f tenv config]
  (let [ns (create-ns (gensym))]
    (binding [*ann-for-ns* (constantly ns)
              *ns* ns
              *debug* (if-let [[_ debug] (find config :debug)]
                        debug
                        *debug*)]
      ;; set up ns refers
      (refer-clojure)
      (require '[clojure.core.typed 
                 :as t 
                 :refer [defalias ann
                         ;Any U Vec Map
                         ;Sym HMap Nothing
                         ]])
      (when spec-ns
        (require [spec-ns :as 's]))

      (let [_ (prn "Current ns:" (current-ns))
            env (as-> (init-env) env
                  (update-type-env env merge tenv))
            env (populate-envs env config)
            anns (f env config)]
        (when *print-anns*
          (pprint anns)))
      :ok)))

(defn anns-from-tenv [tenv & [config]]
  (binding [unparse-type unparse-type']
    (*-from-tenv envs-to-annotations
                 tenv
                 (or config {}))))

(defn specs-from-tenv [tenv & [config]]
  (binding [unparse-type unparse-spec']
    (*-from-tenv envs-to-specs
                 tenv
                 (merge config
                        {:spec? true}))))

(deftest gen-anns
  (is (let [st-type (prs
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
                       't2 st-type})))
  (is
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
              :type '{:T ':intersection, :types (Set Nothing)}
              :foo Sym}
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
    (anns-from-tenv {;'unparse-prop1 t
                     'unparse-prop2 t}
                    {:debug :iterations})))
  (is
    (let [t (prs
      ['{:P ':or
         :ps
         (Set
           '{:P ':and
             :ps
             (Set
               '{:P ':Top})})}
       :->
       Any])]
    (anns-from-tenv {;'unparse-prop1 t
                     'unparse-prop2 t}
                    {:debug #{:iterations :squash}})))

  (is
    (let [t (prs
      ['{:P ':or
         :ps
         (Set
           (U '{:P ':Top}
           '{:P ':and
             :ps
             (Set
               '{:P ':Top})}))}
       :->
       Any])]
    (anns-from-tenv {;'unparse-prop1 t
                     'unparse-prop2 t}
                    {:debug #{:iterations :squash}})))

  (is
    (let [t (prs
      '{:P ':or
         :ps
         (Set
           '{:P ':and
             :ps
             (Set
               (U '{:P ':Top}
                  '{:P ':Bottom}))})}
       )]
    (anns-from-tenv {;'unparse-prop1 t
                     'unparse-prop2 t}
                    {:debug #{:squash :iterations
                              :squash-horizontally}}))))

(deftest ann-combine
;; collapse maps with completely disjoint keys
  (is
    (let [t (prs
              [(U '{:entry1 String}
                  '{:entry2 Boolean}
                  '{:entry3 Boolean})
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

;; don't collapse common keys with keyword entry
  (is
    (let [t (prs
              [(U '{:op :foo
                    :entry1 String}
                  '{:op :bar
                    :entry2 Boolean}
                  '{:op :baz
                    :entry3 Boolean})
               :->
               Any])]
      (anns-from-tenv {'config-in t}
                      {:debug true})))

;; upcast Kw + HMap to Any
  (is
    (let [t (prs
              [(U ':foo
                  '{:op :bar
                    :entry2 Boolean})
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

;; simplify keywords + seqables to Any
  (is
    (let [t (prs
              [(U ':foo
                  (clojure.lang.Seqable String))
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

;; simplify Sym/Kw + seqable to Any
  (is
    (let [t (prs
              [(U Sym
                  (clojure.lang.Seqable String))
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

;; don't simplify Seqable + nil
  (is
    (let [t (prs
              [(U nil
                  (clojure.lang.Seqable String))
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

;; use optional keys
  (is
    (let [t (prs
              [(U '{:foo String}
                  '{:foo String
                    :bar Boolean})
               :->
               Any])]
      (anns-from-tenv {'config-in t}
                      {:debug true})))

;upcast union to Any
  (is
    (let [t (prs
              [(U Any
                  '{:foo String
                    :bar Boolean})
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

; Kw simplification
  (is
    (let [t (prs
              [(U ':foo ':bar)
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

; join on class arguments
  (is
    (let [t (prs
              [(U (Vec Integer)
                  (Vec Long))
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

; don't alias args implicitly
  (is
    (let [t (prs
              [[Any :->  Any]
               :->
               Any])]
      (anns-from-tenv {'config-in t}))))

(deftest ann-hmaps
; upcast HMaps to Map if they appear in a union
  (is
    (let [t (prs
              [(U '{:foo Any}
                  (clojure.lang.IPersistentMap Any Any))
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

; upcast HMaps to Map if they appear in a union
  (is
    (let [t (prs
              [(U '{:foo Any}
                  (clojure.lang.IPersistentMap Any Any))
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

; optional HMaps test
  (is
    (let [t (prs
              [(HMap :optional {:foo String})
               :->
               Any])]
      (anns-from-tenv {'config-in t})))

  (is
    (let [t (prs
              [(U '{:op ':the-foo
                    :the-bar Sym
                    :opt Sym}
                  '{:op ':the-foo
                    :the-bar Sym})
               :->
               Any])]
      (anns-from-tenv {'config-in t}
                      {:debug true})))

; namespaced entry + spec
  (is
    (let [t (prs
              ['{::op ':the-foo}
               :->
               Any])]
      (specs-from-tenv {'config-in t})))

;; TODO recursive example of this test
  (is
    (let [t (prs
              [(U '{:op ':the-bar
                    :the-foo String
                    :the-baz String}
                  '{:op ':the-foo
                    :the-foo Sym
                    :the-baz Sym})
               :->
               Any])]
      (anns-from-tenv {'config-in t}
                      {:debug true})))

; HMap alias naming test
  (is
    (let [t (prs
              [
               '{:op ':foo
                 :the-bar '{:op ':term
                            :val Sym}}
               :->
               Any])]
      ((juxt specs-from-tenv
             anns-from-tenv)
        {'config-in t})))

; recursive HMaps test
  (is
    (let [t (prs
              [(U
               '{:op ':foo
                 :the-bar '{:op ':bar
                            :the-foo '{:op ':foo
                                       :the-bar '{:op ':term
                                                  :val Sym}}}}
               '{:op ':foo
                 :opt Sym
                 :the-bar '{:op ':bar
                            :the-foo '{:op ':foo
                                       :the-bar '{:op ':term
                                                  :val Sym}}}}
               #_
                 '{:op ':bar
                   :the-foo '{:op ':foo
                              :the-bar '{:op ':bar
                                         :the-foo '{:op ':term
                                                    :val Sym}}}}
    )
               :->
               Any])]
             
      ((juxt #_specs-from-tenv ;; FIXME
             anns-from-tenv)
        {'config-in t}
        {:debug true})))

;; FIXME prefer :op over :type?
  (is
    (let [t (prs
              [
               (U
               '{:op ':foo
                 :type (U ':int ':nil ':faz)
                 :the-bar '{:op ':bar
                            :type (U ':int ':nil ':faz)
                            :the-foo '{:op ':foo
                                       :type (U ':int ':nil ':faz)
                                       :the-bar '{:op ':term
                                                  :val Sym}}}}
               '{:op ':foo
                 :type (U ':int ':nil ':faz)
                 :opt Sym
                 :the-bar '{:op ':bar
                            :type (U ':int ':nil ':faz)
                            :the-foo '{:op ':foo
                                       :type (U ':int ':nil ':faz)
                                       :the-bar '{:op ':term
                                                  :val Sym}}}}
                 '{:op ':bar
                   :type (U ':int ':nil ':faz)
                   :the-foo '{:op ':foo
                              :type (U ':int ':nil ':faz)
                              :the-bar '{:op ':bar
                                         :type (U ':int ':nil ':faz)
                                         :the-foo '{:op ':term
                                                    :val Sym}}}})
               :->
               Any])]
      (;specs-from-tenv 
       anns-from-tenv 
       {'config-in t}
       {:fuel 0})))

  (is
    (let [t (prs
              [':a
               Integer
               ':b
               Boolean
               :->
               Any])]
             
      ((juxt #_specs-from-tenv ;; FIXME
             anns-from-tenv)
        {'config-in t}
        {:debug true})))

;; combine maps that look similar
  (is
    (let [t (prs
              ['{:a Long
                 :b Long
                 :c Long
                 :d Long}
               '{:a Long
                 :b Long
                 :c Long}
               :->
               Any])]
             
      ((juxt #_specs-from-tenv ;; FIXME
             anns-from-tenv)
        {'config-in t}
        ))))

;; performance tests
(defn gen-height [n]
  {:pre [(not (neg? n))]}
  (if (zero? n)
    `'{:tag ':null}
    `'{:tag ':cons :cdr ~(gen-height (dec n))}))

(defn gen-tagged-union [n]
  {:pre [(not (neg? n))]}
  (if (zero? n)
    `'{:tag ':null}
    `'{:tag '~(keyword (str "cons" n)) :cdr ~(gen-tagged-union (dec n))}))

(defmacro bench
  "Evaluates expr and returns the time it took."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         msduration# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     [ret# msduration#]))

(defn bench-iteratively 
  ([f n] (bench-iteratively f 0 n))
  ([f start n]
   (loop [times []
          i start]
     (if (< n i)
       times
       (let [[_ t] (f i)]
         (recur (conj times t)
                (inc i)))))))

(defn write-csv [n v]
  {:pre [(vector? v)]}
  (spit n (apply str (interpose "," v))))

(comment
(def bench-height
  (write-csv "height-bench.csv"
             (bench-iteratively
               #(let [t (parse-type (gen-height %))]
                  (binding [*print-anns* false]
                    (bench
                      (anns-from-tenv {'prop t}
                                      {}))))
               120)))

(let [t (parse-type (gen-height 5))]
  (anns-from-tenv {'prop t}
                  {}))
(pprint (gen-tagged-union 5))

(let [t (parse-type (gen-tagged-union 10))]
  (anns-from-tenv {'prop t}
                  {}))

(def bench-tagged
  (write-csv
    "tagged-bench-past110.csv"
    (bench-iteratively
      #(let [t (parse-type (gen-tagged-union %))]
         (binding [*print-anns* false]
           (bench
             (anns-from-tenv {'prop t}
                             {}
                             #_{:debug #{:squash :iterations
                                         :squash-horizontally}}))))
      111 
      120)))
)


(deftest map-merge-test
; TODO
; other types don't collapse tagged maps
; should combining tagged and untagged maps upcast to (Map Any Any)?
  (is
    (let [t (prs
              (U nil
                 '{:a ':b}
                 '{:op ':foo
                   :b Long
                   :c Long
                   :d Long}
                 '{:op ':bar
                   :e Long
                   :w Long
                   :q Long}))]
             
      ((juxt #_specs-from-tenv ;; FIXME
             anns-from-tenv)
        {'config-in t}
        )))

; merging sufficiently similar maps
  (is
    (let [t (prs
              (U nil
                 (HMap
                   :mandatory {:name Sym}
                   :optional {:blah Sym})
                 (HMap
                   :mandatory {:name Sym
                               :ns Sym}
                   :optional {:tag Sym
                              :tag1 Sym
                              :tag2 Sym
                              :tag3 Sym
                              :tag4 Sym
                              :tag5 Sym
                              :tag6 Sym
                              :tag7 Sym
                              :tag8 Sym
                              :tag9 Sym
                              :tag10 Sym
                              :tag11 Sym
                              })))]
             
      ((juxt #_specs-from-tenv ;; FIXME
             anns-from-tenv)
        {'config-in t}
        ))))

(deftest instrument-top-level-form-test
  (is (.contains (with-out-str (instrument-top-level-form '(println "a")))
                 "a\n"))
  (is (.contains 
        (with-out-str (instrument-top-level-form '(do (def a "a") (println a))))
        "a\n")))

(defn code-to-gen-spec [root-ns root-key samples {:keys [spec?] :as config}]
  (binding [*ns* *ns*]
    (in-ns root-ns)
    (binding [*ns* (the-ns root-ns)
              *ann-for-ns* #(the-ns root-ns)
              unparse-type (if spec?
                             unparse-spec'
                             unparse-type')]
      (let [results-atom (atom (env/initial-results))
            ;instrument and track
            _ (run!
                 (fn [e]
                   (track/track
                     (track/gen-track-config)
                     results-atom
                     e
                     #{[(rep/var-path root-ns root-key)]}
                     #{}))
                 (concat (map eval (:eval samples))
                         (:edn samples)))
            _ (prn @results-atom)
            infer-out
            (infer/infer-anns root-ns
                              {:spec? spec?
                               :allow-top-level-non-IFn true
                               :results-atom results-atom})
            _ (prn "infer out" infer-out)
            spec-out
            (apply insert/prepare-ann
                   ((juxt :requires :top-level :local-fns) infer-out))]
        spec-out))))


(deftest manual-track-test
  (is (code-to-gen-spec 'user 'user/foo-bar {:eval '[(inc 1)] :edn '[a]}
                        {:spec? true})))

(declare *-from-infer-results)

;TODO remove # suffixes
(defn infer-test*
  [{:keys [defs tests expected-specs expected-types] :as opts}]
  (assert (vector? defs))
  (assert (vector? tests))
  (let [ns# (create-ns (gensym))]
     (binding [*ns* ns#
               *ann-for-ns* (constantly ns#)]
       (refer-clojure)
       (require '[clojure.core.typed :as t])
       (when spec-ns
         (require [spec-ns :as 's]))
       (let [result# (atom :ok)
             run-until-bad-result!# (fn [f# c#]
                                      (loop [c# c#]
                                        (when @result#
                                          (when (seq c#)
                                            (do (f# (first c#))
                                                (recur (rest c#)))))))
             defs# defs
             tests# tests
             _# (infer/refresh-runtime-infer)
             _# (run-until-bad-result!#
                  (fn [f#]
                    (try (instrument-top-level-form f#)
                         (catch Throwable e#
                           (println (str "Failed to evaluate " f# " with error " e#))
                           (reset! result# nil))))
                  (concat defs# tests#))]
         (when @result#
           (let [expected-specs# (let [s# expected-specs]
                                   (if (string? s#)
                                     (read-string s#)
                                     s#))
                 _# (assert ((some-fn nil? vector?) expected-specs#))
                 expected-types# (let [t# expected-types]
                                   (if (string? t#)
                                     (read-string t#)
                                     t#))
                 _# (assert ((some-fn nil? vector?) expected-types#))
                 specs# (*-from-infer-results *ns* {:spec? true})
                 types# (*-from-infer-results *ns* {})
                 assert-equal# (fn [actual# expected# msg#]
                                 (when-not (= actual# expected#)
                                   (println msg#)
                                   (println "Actual:")
                                   (pprint actual#)
                                   (println "Expected:")
                                   (pprint expected#)
                                   (reset! result# nil)))
                 ]
             (if expected-specs#
               (assert-equal# (:top-level specs#) expected-specs#
                              "Actual specs didn't match expected specs")
               (do (println "Here are the generated specs:")
                   (pprint (:top-level specs#))))
             (if expected-types#
               (assert-equal# (:top-level types#) expected-types#
                              "Actual types didn't match expected types")
               (do (println "Here are the generated types:")
                   (pprint (:top-level types#))))
             (when spec-ns
               (let [instrumentable-syms# (set
                                            (keep (fn [spc#]
                                                    (when (seq? spc#)
                                                      (when (= 's/fdef (first spc#))
                                                        (let [^clojure.lang.Var v# (resolve (second spc#))]
                                                          (when (var? v#)
                                                            (coerce/var->symbol v#))))))
                                                  (:top-level specs#)))
                     spec-defs# (set
                                  (keep (fn [spc#]
                                          (when (seq? spc#)
                                            (when (= 's/def (first spc#))
                                              (let [kw# (second spc#)]
                                                (when (keyword? kw#)
                                                  (when (namespace kw#)
                                                    kw#))))))
                                        (:top-level specs#)))
                     _# (require ['clojure.spec.test.alpha])
                     instrument# (resolve 'clojure.spec.test.alpha/instrument)
                     _# (assert instrument#)
                     exercise# (resolve 'clojure.spec.alpha/exercise)
                     _# (assert exercise#)
                     exercise-fn# (resolve 'clojure.spec.alpha/exercise-fn)
                     _# (assert exercise-fn#)
                     exercise-fn-or-fail# (fn [sym#]
                                            (try (doall (exercise-fn# sym#))
                                                 (catch Throwable e#
                                                   (println "Function failed to exercise:" sym#)
                                                   (println "With the following error:")
                                                   (binding [*err* *out*]
                                                     (repl/pst e#))
                                                   (when (instance? IExceptionInfo e#)
                                                     (pp/pprint (ex-data e#)))
                                                   (reset! result# nil))))
                     exercise-or-fail# (fn [spc#]
                                         (try (doall (exercise# spc#))
                                              (catch Throwable e#
                                                (println "Spec failed to exercise")
                                                (pp/pprint spc#)
                                                (println "With the following error:")
                                                (binding [*err* *out*]
                                                  (repl/pst e#))
                                                (when (instance? IExceptionInfo e#)
                                                  (pp/pprint (ex-data e#)))
                                                (reset! result# nil))))
                     eval-or-fail# (fn [form#]
                                     (try (eval form#)
                                          (catch Throwable e#
                                            (println "Expression failed to evaluate:")
                                            (pp/pprint form#)
                                            (println "With the following error:")
                                            (repl/pst e#)
                                            (reset! result# nil))))]
                 (testing "spec declarations should evaluate"
                   (run-until-bad-result!# eval-or-fail# (concat (:requires specs#) (:top-level specs#))))
                 (testing "should be able to exercise spec defs"
                   (run-until-bad-result!# exercise-or-fail# spec-defs#))
                 (testing "should be able to exercise fns"
                   (run-until-bad-result!# exercise-fn-or-fail# instrumentable-syms#))
                 (when @result#
                   (testing "specs should instrument"
                     (when-not (= instrumentable-syms#
                                  (set (instrument# instrumentable-syms#)))
                       (println "Expected to instrument "
                                (set/difference
                                  instrumentable-syms#
                                  (set (instrument# instrumentable-syms#)))
                                " but didn't")
                       (reset! result# nil))))
                 (testing "tests should evaluate under instrumentation"
                   (run-until-bad-result!# eval-or-fail# tests#))))
             @result#))))))

(defmacro infer-test
  "Given a vector of definitions :defs and a vector of tests :tests, then
  does these steps in order. Short-circuits and returns a false value if previous steps fail.
  Returns a true value on success.

  1. Generates specs and types for the definitions
  2. Ensure generated specs and types are identical to :expected-types and :expected-specs, respectively (when provided).
     These are either vectors of annotations, or a string that contains vectors
     of annotations that will be `read` in the correct namespace (useful to aid keyword namespace
     resolution in specs).
     If one of these is not provided, the respective annotations are pprint'ed so they can
     be easily copied into the test.
  3. Evaluates generated specs.
  4. Exercises spec aliases.
  5. Exercises spec'd functions.
  6. Instruments all spec'd functions.
  7. Runs :test code again under spec instrumentation."
  [& {:keys [defs tests expected-specs expected-types] :as opts}]
  `(infer-test* '~opts))

(defn *-from-infer-results [ns config]
  (binding [*ann-for-ns* (constantly *ns*)
            *debug* (if-let [[_ debug] (find config :debug)]
                      debug
                      *debug*)
            unparse-type (if (:spec? config)
                           unparse-spec'
                           unparse-type')]
    (infer/infer-anns *ns* config)))

(deftest test-infer-test
  (is (infer-test :defs [(defn blah [a] (inc a))]
                  :tests [(blah 1)]
                  :expected-specs [(s/fdef blah :args (s/cat :a int?) :ret int?)]
                  :expected-types [(declare) (t/ann blah [t/Int :-> t/Int])]))
  (testing "detects Exception always thrown in :defs"
    (is (not
          (infer-test :defs [(throw (Exception.))]
                      :tests []
                      :expected-specs [(s/fdef blah :args (s/cat :a int?) :ret int?)]
                      :expected-types [(declare) (t/ann blah [t/Int :-> t/Int])]))))
  (testing "detects Exception always thrown in :tests"
    (is (not
          (infer-test :defs [(defn blah [a] (inc a))]
                      :tests [(throw (Exception.))]
                      :expected-specs [(s/fdef blah :args (s/cat :a int?) :ret int?)]
                      :expected-types [(declare) (t/ann blah [t/Int :-> t/Int])]))))
  (testing "detects bad exercise-fn"
    (is (not
          (infer-test :defs [(defn blah [a]
                               (assert (zero? a)))]
                      :tests [(blah 0)]
                      :expected-specs [(s/fdef blah :args (s/cat :a int?) :ret nil?)]
                      :expected-types [(declare) (t/ann blah [t/Int :-> nil])]))))
  (testing "detects bad exercise-fn"
    (is (not
          (infer-test :defs [(defn blah [f]
                               (f))]
                      :tests [(blah (constantly nil))]
                      :expected-specs [(s/fdef blah :args (s/cat :f ifn?) :ret nil?)]
                      :expected-types [(declare) (t/ann blah [[:-> nil] :-> nil])]))))
  (testing "detects bad provided specs"
    (is (not
          (infer-test :defs [(defn blah [f])]
                      :tests [(blah identity)]
                      :expected-specs [(s/fdef blah :args (s/cat :asdf ifn?) :ret nil?)]
                      :expected-types [(declare) (t/ann blah [AnyFunction :-> nil])]))))
  (testing "detects bad provided specs (wrong quantity)"
    (is (not
          (infer-test :defs [(defn blah [f]
                               (f))]
                      :tests [(blah (constantly nil))]
                      :expected-specs [(s/fdef food :args (s/cat :asdf ifn?) :ret nil?)
                                       (s/fdef blah :args (s/cat :f ifn?) :ret nil?)]
                      :expected-types [(declare) (t/ann blah [[:-> nil] :-> nil])]))))
  (testing "detects bad provided types"
    (is (not
          (infer-test :defs [(defn blah [f]
                               (f))]
                      :tests [(blah (constantly nil))]
                      :expected-specs [(s/fdef blah :args (s/cat :f ifn?) :ret nil?)]
                      :expected-types [(declare) (t/ann blah [t/Int :-> nil])]))))
  (testing "detects bad provided types (wrong quantity)"
    (is (not
          (infer-test :defs [(defn blah [f]
                               (f))]
                      :tests [(blah (constantly nil))]
                      :expected-specs [(s/fdef blah :args (s/cat :f ifn?) :ret nil?)]
                      :expected-types [(declare) (t/ann blah [[:-> nil] :-> nil])
                                       (t/ann food [t/Int :-> nil])]))))
)

(deftest HMap-infer-test
  (is (infer-test :defs [(defn takes-map [m]
                           {:pre [(or (some-> m :a #{1})
                                      true)]}
                           (mapv identity m))]
                  :tests [(takes-map {})
                          (takes-map {:a 1})
                          (takes-map {:b 1})]
                  :expected-specs "[(s/def ::ABMap (s/keys :opt-un [::a ::b]))
                                    (s/def ::a int?)
                                    (s/def ::b int?)
                                    (s/fdef
                                      takes-map
                                      :args
                                      (s/cat :m ::ABMap)
                                      :ret
                                      (s/coll-of (s/tuple #{:b :a} int?) :into vector?))]"
                  :expected-types [(declare ABMap)
                                   (t/defalias ABMap (t/HMap :optional {:a t/Int, :b t/Int}))
                                   (t/ann
                                     takes-map
                                     [ABMap
                                      :->
                                      (t/Vec '[(t/U ':a ':b) t/Int])])]))
  (is (infer-test :defs [(defn gives-map []
                           {:a 1})]
                  :tests [(require '[clojure.walk :as w])
                          (w/prewalk identity (gives-map))]))
  (is (infer-test :defs [(require '[clojure.walk :as w])
                         (defn takes-map [m]
                           (w/prewalk identity m))]
                  :tests [(takes-map {})
                          (takes-map {:a {:b 1}})
                          (takes-map {:b {:a 1}})]
                  ))
  ;; clashes detected between :req-un keys and normal aliases
  (is (infer-test :defs [(require '[clojure.walk :as w])
                         (defn takes-op [a]
                           (w/prewalk identity a))]
                  :tests [(takes-op {:Op :val :val 'blah})
                          (takes-op {:Op :if
                                     :test {:Op :val :val 'blah}
                                     :then {:Op :val :val 'blah}
                                     :else {:Op :val :val 'blah}})]))
  ;; clashes detected for multi-specs
  (is (infer-test :defs [(require '[clojure.walk :as w])
                         (def Op-multi-spec nil)
                         (defn takes-op [a]
                           (w/prewalk identity a))]
                  :tests [(takes-op {:Op :val :val 'blah})
                          (takes-op {:Op :if
                                     :test {:Op :val :val 'blah}
                                     :then {:Op :val :val 'blah}
                                     :else {:Op :val :val 'blah}})]))
  ;; ensure unvisited map entries have type Any/? not (s/or)
  (is (infer-test :defs [(require '[clojure.walk :as w])
                         (def Op-multi-spec nil)
                         (defn takes-op [a]
                           (mapv identity a))]
                  :tests [(takes-op {:Op :val :val 'blah
                                     :children {:foo [:a :b :c]}
                                     })
                          (takes-op {:Op :if
                                     :children {:foo [:a :b :c]}})]))
)

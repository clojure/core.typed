(ns clojure.core.typed.test.update
  (:refer-clojure :exclude [update])
  (:require 
    ; this loads the type system, must go first
    [clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed.checker.jvm.parse-unparse :refer [parse-clj]]
            [clojure.core.typed :as t]
            [clojure.test :refer :all]
            [clojure.core.typed.checker.lex-env :refer [-PropEnv]]
            [clojure.core.typed.checker.type-rep :refer [-nil -false make-CountRange]]
            [clojure.core.typed.checker.type-ctors :refer [Un In RClass-of -name]]
            [clojure.core.typed.checker.update :as update :refer [env+ update]]
            [clojure.core.typed.checker.filter-rep :refer [-top]]
            [clojure.data :refer [diff]]
            [clojure.pprint :refer [pprint]]
            [clojure.core.typed.checker.filter-ops :refer [-imp -not-filter -filter]]))

(defmacro with-validator [v & body]
  `(let [~v (atom true :validator (constantly true))
         b# ~@body]
     b#))

(defn prop-sub? [{l1 :l props1 :props} {l2 :l props2 :props}]
  (and (apply = (map keys [l1 l2]))
       (every? identity
               (let [ks (keys l1)]
                 (map subtype?
                      (map #(get %1 %2) (repeat l1) ks)
                      (map #(get %1 %2) (repeat l2) ks))))
       (= props1 props2)))

(deftest simple-env+
  (testing "updating empty env with tt"
    (is (with-validator v
          (prop-sub? (env+ (-PropEnv {} #{}) [-top] v)
                      (-PropEnv {} #{})))))
  (testing "updating non-empty env with tt"
    (is (let [l {'x -nil}]
          (with-validator v
            (prop-sub? (env+ (-PropEnv l #{})
                             [-top] v)
                       (-PropEnv l #{}))))))
  (testing "updating a typical `and` conjunction

           (let [v (get-vector)
                 s (seq v)]
             (when s
               ...))"
    (is-clj 
      (let [l {'v (parse-clj `(t/Vec t/Num))
               's (parse-clj `(t/NilableNonEmptySeq t/Num))}]
        (with-validator v
          (prop-sub?
            (env+ (-PropEnv l #{(-imp (-not-filter (Un -nil -false) 's)
                                      (-filter (make-CountRange 1) 'v))})
                  [(-not-filter (Un -nil -false) 's)]
                  v)
            (-PropEnv {'v (parse-clj `(t/NonEmptyVec t/Num))
                       's (parse-clj `(t/NonEmptySeq t/Num))}
                      #{(-not-filter (Un -nil -false) 's)
                        (-filter (make-CountRange 1) 'v)})))))))

;(def f
;  (future
;       (do
;         (require '[clojure.tools.trace])
;         (clojure.tools.trace/trace-vars clojure.core.typed.checker.filter-ops/-and
;                       clojure.core.typed.checker.filter-ops/-or)
;         (spit "combine"
;               (with-out-str
;                 (t/check-ns 'clojure.core.typed.test.CTYP-83-performance))))))
;
;(env+ #PropEnv{:l {fv2__#0 (Option java.lang.Number), 
;                   fv1__#0 (Option java.lang.Number), 
;                   v2__#1 (NilableNonEmptySeq Num), 
;                   v1__#1 (NilableNonEmptySeq Num), 
;                   v2__#0 (U VersionVector nil), 
;                   v1__#0 (U VersionVector nil)}, 
;               :props #{(when (is (U nil false) v1__#1) (is (U EmptyCount nil) v1__#0)) 
;                        (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is NonEmptyCount v2__#0))) 
;                        (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is NonEmptyCount v1__#0))) 
;                        (when (is (U nil false) v2__#1) (is (U EmptyCount nil) v2__#0))}} 
;      [(& (when (is (U nil false) v1__#1) 
;            (is (U EmptyCount nil) v1__#0)) 
;          (is (U nil false) v1__#1) 
;          (when (! (U nil false) v2__#1) 
;            (& (! nil v2__#0) (is NonEmptyCount v2__#0))) 
;          (when (! (U nil false) v1__#1) 
;            (& (! nil v1__#0) (is NonEmptyCount v1__#0))) 
;          (is (U nil false) v2__#1) 
;          (when (is (U nil false) v2__#1) 
;            (is (U EmptyCount nil) v2__#0)))] 
;      #<Atom@15366b7d: true>)
;PropEnv{:l {fv2__#0 (Option java.lang.Number), 
;            fv1__#0 (Option java.lang.Number), 
;            v2__#1 nil, 
;            v1__#1 nil, 
;            v2__#0 nil, 
;            v1__#0 nil}, 
;        :props #{(is (U nil false) v1__#1) 
;                 (when (! (U nil false) v2__#1) 
;                   (& (! nil v2__#0) (is NonEmptyCount v2__#0))) 
;                 (is (U EmptyCount nil) v1__#0) 
;                 (is (U EmptyCount nil) v2__#0) 
;                 (when (! (U nil false) v1__#1) 
;                   (& (! nil v1__#0) (is NonEmptyCount v1__#0))) 
;                 (is (U nil false) v2__#1)}}
;
;TRACE t73988: (env+ 
;                #PropEnv{:l {v1__#1 (NilableNonEmptySeq Num), 
;                             v2__#0 (U VersionVector nil), 
;                             v1__#0 (U VersionVector nil)}, 
;                         :props #{(when (is (U nil false) v1__#1) 
;                                    (is (U EmptyCount nil) v1__#0)) 
;                                  (when (! (U nil false) v1__#1) 
;                                    (& (! nil v1__#0) 
;                                       (is NonEmptyCount v1__#0)))}} 
;                [tt] #<Atom@60f988ac: true>)
;TRACE t73988: => #PropEnv{:l {v1__#1 (NilableNonEmptySeq Num), 
;                              v2__#0 (U VersionVector nil), 
;                              v1__#0 (U VersionVector nil)}, 
;                          :props #{(when (is (U nil false) v1__#1) 
;                                     (is (U EmptyCount nil) v1__#0)) 
;                                   (when (! (U nil false) v1__#1) 
;                                     (& (! nil v1__#0) (is NonEmptyCount v1__#0)))}}
;
;(env+ #PropEnv{:l {v2__#1 (NilableNonEmptySeq Num), 
;                   v1__#1 (NilableNonEmptySeq Num), 
;                   v2__#0 (U VersionVector nil), 
;                   v1__#0 (U VersionVector nil)}, 
;               :props #{(when (is (U nil false) v1__#1) 
;                          (is (U EmptyCount nil) v1__#0)) 
;                        (when (! (U nil false) v2__#1) 
;                          (& (! nil v2__#0) 
;                             (is NonEmptyCount v2__#0))) 
;                        (when (! (U nil false) v1__#1) 
;                          (& (! nil v1__#0) 
;                             (is NonEmptyCount v1__#0))) 
;                        (when (is (U nil false) v2__#1) 
;                          (is (U EmptyCount nil) v2__#0))}} 
;      [tt] #<Atom@6e917b55: true>)
;TRACE t74338: => #clojure.core.typed.checker.lex_env.PropEnv{:l {v2__#1 (NilableNonEmptySeq Num), v1__#1 (NilableNonEmptySeq Num), v2__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil), v1__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil)}, :props #{(when (is (U nil false) v1__#1) (is (U EmptyCount nil) v1__#0)) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is NonEmptyCount v2__#0))) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is NonEmptyCount v1__#0))) (when (is (U nil false) v2__#1) (is (U EmptyCount nil) v2__#0))}}
;
;
;
;TRACE t74596: (env+ #PropEnv{:l {and__3941__auto____#0 boolean, 
;                                 fv2__#0 (Option java.lang.Number), 
;                                 fv1__#0 (Option java.lang.Number), 
;                                 v2__#1 (NilableNonEmptySeq Num), 
;                                 v1__#1 (NilableNonEmptySeq Num), 
;                                 v2__#0 (U VersionVector nil), 
;                                 v1__#0 (U VersionVector nil)}, 
;                             :props #{(when (is (U nil false) v1__#1) 
;                                        (is (U EmptyCount nil) v1__#0)) 
;                                      (when (! (U nil false) v2__#1) 
;                                        (& (! nil v2__#0) 
;                                           (is NonEmptyCount v2__#0))) 
;                                      (when (is (U nil false) and__3941__auto____#0) 
;                                        (! (U nil false) v1__#1)) 
;                                      (when (! (U nil false) and__3941__auto____#0) 
;                                        (is (U nil false) v1__#1)) 
;                                      (when (! (U nil false) v1__#1) 
;                                        (& (! nil v1__#0) 
;                                           (is NonEmptyCount v1__#0))) 
;                                      (when (is (U nil false) v2__#1) 
;                                        (is (U EmptyCount nil) v2__#0))}} 
;                    [(is (U nil false) and__3941__auto____#0)] 
;                    #<Atom@252f13e9: true>)
;TRACE t74596: => #PropEnv{:l {and__3941__auto____#0 false, fv2__#0 (Option java.lang.Number), fv1__#0 (Option java.lang.Number), v2__#1 (NilableNonEmptySeq Num), v1__#1 (I (clojure.lang.ISeq Num) (CountRange 1)), v2__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil), v1__#0 (I (clojure.lang.IPersistentVector java.lang.Number) (CountRange 1))}, 
;                          :props #{(! nil v1__#0) 
;                                   (! (U nil false) v1__#1) 
;                                   (is NonEmptyCount v1__#0) 
;                                   (when (is (U nil false) v1__#1) (is (U EmptyCount nil) v1__#0)) 
;                                   (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is NonEmptyCount v2__#0))) 
;                                   (is (U nil false) and__3941__auto____#0) 
;                                   (when (! (U nil false) and__3941__auto____#0) (is (U nil false) v1__#1)) 
;                                   (when (is (U nil false) v2__#1) (is (U EmptyCount nil) v2__#0))}}

;TRACE t74634: (env+ #PropEnv{:l {fv2__#0 (clojure.core.typed/Option java.lang.Number), 
;                                 fv1__#0 (clojure.core.typed/Option java.lang.Number), 
;                                 v2__#1 (clojure.core.typed/NilableNonEmptySeq clojure.core.typed/Num), 
;                                 v1__#1 (clojure.core.typed/NilableNonEmptySeq clojure.core.typed/Num), 
;                                 v2__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil), 
;                                 v1__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil)}, 
;                             :props #{(when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0)) 
;                                      (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) 
;                                      (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))) 
;                                      (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))}} 
;                    [(& (| (when (! (U nil false) v2__#1) 
;                             (& (! nil v2__#0) 
;                                (is clojure.core.typed/NonEmptyCount v2__#0))) 
;                           (when (! (U nil false) v1__#1) 
;                             (& (! nil v1__#0) 
;                                (is clojure.core.typed/NonEmptyCount v1__#0)))) 
;                        (| (! (U nil false) v2__#1) 
;                           (is clojure.core.typed/NonEmptyCount v1__#0)) 
;                        (| (! (U nil false) v2__#1) 
;                           (when (is (U nil false) v1__#1) 
;                             (is (U clojure.core.typed/EmptyCount nil) v1__#0))) 
;                        (| (! nil v1__#0) 
;                           (is (U nil false) v1__#1)) 
;                        (| (! nil v1__#0) 
;                           (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) 
;                        (| (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (| (! (U nil false) v2__#1) 
;                           (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) 
;                        (| (is clojure.core.typed/NonEmptyCount v1__#0) 
;                           (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) 
;                        (| (is clojure.core.typed/NonEmptyCount v1__#0) 
;                           (is (U nil false) v1__#1)) 
;                        (| (! nil v1__#0) 
;                           (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) 
;                        (| (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (| (! (U nil false) v1__#1) 
;                           (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) 
;                        (| (! nil v1__#0) 
;                           (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) 
;                        (| (! (U nil false) v1__#1) 
;                           (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) 
;                        (when (is (U nil false) v1__#1) 
;                          (is (U clojure.core.typed/EmptyCount nil) v1__#0)) 
;                        (| (! (U nil false) v1__#1) 
;                           (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) 
;                        (| (! (U nil false) v2__#1) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (| (! nil v1__#0) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (| (is clojure.core.typed/NonEmptyCount v1__#0) 
;                           (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) 
;                        (| (is (U nil false) v1__#1) 
;                           (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) 
;                        (| (is clojure.core.typed/NonEmptyCount v1__#0) 
;                           (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) 
;                        (| (! (U nil false) v1__#1) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (when (! (U nil false) v2__#1) 
;                          (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) 
;                        (| (is clojure.core.typed/NonEmptyCount v1__#0) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (| (! nil v1__#0) 
;                           (! (U nil false) v2__#1)) 
;                        (| (is (U nil false) v1__#1) 
;                           (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) 
;                        (when (is (U nil false) v2__#1) 
;                          (is (U clojure.core.typed/EmptyCount nil) v2__#0)) 
;                        (| (! (U nil false) v2__#1) 
;                           (! (U nil false) v1__#1)))] 
;                    #<Atom@1f9f75fa: true>)
;TRACE t74634: => #clojure.core.typed.checker.lex_env.PropEnv{:l {fv2__#0 (clojure.core.typed/Option java.lang.Number), fv1__#0 (clojure.core.typed/Option java.lang.Number), v2__#1 (clojure.core.typed/NilableNonEmptySeq clojure.core.typed/Num), v1__#1 (clojure.core.typed/NilableNonEmptySeq clojure.core.typed/Num), v2__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil), v1__#0 (U clojure.core.typed.test.CTYP-83-performance/VersionVector nil)}, :props #{(| (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) (| (! (U nil false) v2__#1) (is clojure.core.typed/NonEmptyCount v1__#0)) (| (! (U nil false) v2__#1) (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) (| (! nil v1__#0) (is (U nil false) v1__#1)) (| (! nil v1__#0) (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) (| (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (| (! (U nil false) v2__#1) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) (| (is clojure.core.typed/NonEmptyCount v1__#0) (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) (| (is clojure.core.typed/NonEmptyCount v1__#0) (is (U nil false) v1__#1)) (| (! nil v1__#0) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) (| (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (| (! (U nil false) v1__#1) (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0))) (| (! nil v1__#0) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) (| (! (U nil false) v1__#1) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) (when (is (U nil false) v1__#1) (is (U clojure.core.typed/EmptyCount nil) v1__#0)) (| (! (U nil false) v1__#1) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) (| (! (U nil false) v2__#1) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (| (! nil v1__#0) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (| (is clojure.core.typed/NonEmptyCount v1__#0) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) (| (is (U nil false) v1__#1) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0)))) (| (is clojure.core.typed/NonEmptyCount v1__#0) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0)))) (| (! (U nil false) v1__#1) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (when (! (U nil false) v2__#1) (& (! nil v2__#0) (is clojure.core.typed/NonEmptyCount v2__#0))) (| (is clojure.core.typed/NonEmptyCount v1__#0) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (| (! nil v1__#0) (! (U nil false) v2__#1)) (when (! (U nil false) v1__#1) (& (! nil v1__#0) (is clojure.core.typed/NonEmptyCount v1__#0))) (| (is (U nil false) v1__#1) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0))) (when (is (U nil false) v2__#1) (is (U clojure.core.typed/EmptyCount nil) v2__#0)) (| (! (U nil false) v2__#1) (! (U nil false) v1__#1))}}

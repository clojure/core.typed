;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.check.funapp-one
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.object-rep :as obj]
            [clojure.core.typed.checker.open-result :as open-result]
            [clojure.set :as set]))

;Function TCResult^n (or nil TCResult) -> TCResult
(defn check-funapp1 [fexpr arg-exprs {{optional-kw :optional mandatory-kw :mandatory :as kws} :kws
                                      :keys [dom rng rest drest prest pdot] :as ftype0}
                     argtys expected & {:keys [check?] :or {check? true}}]
  {:pre [(r/Function? ftype0)
         (every? r/TCResult? argtys)
         ((some-fn nil? r/TCResult?) expected)
         (boolean? check?)]
   :post [(r/TCResult? %)]}
  (when drest 
    (err/nyi-error "funapp with drest args NYI"))
  ;  (prn "check-funapp1")
  ;  (prn "argtys objects" (map r/ret-o argtys))
  ;checking
  (when check?
    (let [nactual (count argtys)]
      (when-not (or (when (and (not rest)
                               (empty? optional-kw)
                               (empty? mandatory-kw))
                      (= (count dom) (count argtys)))
                    (when rest
                      (<= (count dom) nactual))
                    (when prest
                      (and (<= (count dom) nactual)
                           (zero? (rem (- nactual dom) (count (-> prest :types))))))
                    (when kws
                      (let [nexpected (+ (count dom)
                                         (* 2 (count mandatory-kw)))]
                        (and (even? (- nactual (count dom)))
                             ((if (seq optional-kw) <= =)
                              nexpected
                              nactual)))))
        (err/tc-delayed-error (str "Wrong number of arguments, expected " (count dom) " fixed parameters"
                                 (cond
                                   rest " and a rest parameter "
                                   drest " and a dotted rest parameter "
                                   prest " and a push rest parameter"
                                   kws (cond
                                         (and (seq mandatory-kw) (seq optional-kw))
                                         (str ", some optional keyword arguments and " (count mandatory-kw) 
                                              " mandatory keyword arguments")

                                         (seq mandatory-kw) (str "and " (count mandatory-kw) "  mandatory keyword arguments")
                                         (seq optional-kw) " and some optional keyword arguments"
                                         :else (str " with unknow ftype " ftype0)))
                                 ", and got " nactual
                                 " for function " (pr-str (prs/unparse-type ftype0))
                                 " and arguments " (pr-str (mapv (comp prs/unparse-type r/ret-t) argtys)))))
      (cond
        ; case for regular rest argument, or no rest parameter
        (or rest (empty? (remove nil? [rest drest kws prest])))
        (doseq [[arg-t dom-t] (map vector
                                   (map r/ret-t argtys)
                                   (concat dom (when rest (repeat rest))))]
          (below/check-below arg-t dom-t))

        ; case for mandatory or optional keyword arguments
        kws
        (do
          ;check regular args
          (doseq [[arg-t dom-t] (map vector (map r/ret-t (take (count dom) argtys)) dom)]
            (below/check-below arg-t dom-t))
          ;check keyword args
          (let [flat-kw-argtys (drop (count dom) argtys)]
            (when-not (even? (count flat-kw-argtys))
              (err/tc-delayed-error  
                (str "Uneven number of arguments to function expecting keyword arguments")))
            (let [kw-args-paired-t (apply hash-map (map r/ret-t flat-kw-argtys))]
              ;make sure all mandatory keys are present
              (when-let [missing-ks (seq 
                                      (set/difference (set (keys mandatory-kw))
                                                      (set (keys kw-args-paired-t))))]
                (err/tc-delayed-error (str "Missing mandatory keyword keys: "
                                         (pr-str (interpose ", " (map prs/unparse-type missing-ks))))))
              ;check each keyword argument is correctly typed
              (doseq [[kw-key-t kw-val-t] kw-args-paired-t]
                (when-not (r/Value? kw-key-t)
                  (err/tc-delayed-error (str "Can only check keyword arguments with Value keys, found"
                                           (pr-str (prs/unparse-type kw-key-t)))))
                (let [expected-val-t ((some-fn optional-kw mandatory-kw) kw-key-t)]
                  (if expected-val-t
                    (below/check-below kw-val-t expected-val-t)
                    ; It is an error to use an undeclared keyword arg because we want to treat the rest parameter
                    ; as a complete hash-map.
                    (err/tc-delayed-error (str "Undeclared keyword parameter " 
                                             (pr-str (prs/unparse-type kw-key-t)))))))))))))
  (let [dom-count (count dom)
        arg-count (+ dom-count (if rest 1 0) (count optional-kw))
        o-a (map r/ret-o argtys)
        _ (assert (every? obj/RObject? o-a))
        t-a (map r/ret-t argtys)
        _ (assert (every? r/Type? t-a))
        [o-a t-a] (let [rs (for [[nm oa ta] (map vector 
                                                 (range arg-count) 
                                                 (concat o-a (repeatedly obj/EmptyObject-maker))
                                                 (concat t-a (repeatedly c/Un)))]
                             [(if (>= nm dom-count) (obj/EmptyObject-maker) oa)
                              ta])]
                    [(map first rs) (map second rs)])
        [t-r f-r o-r flow-r] (open-result/open-Result rng o-a t-a)]
    (below/maybe-check-below
      (r/ret t-r f-r o-r flow-r)
      expected)))

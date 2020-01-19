;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.analyzer.jvm.passes.beta-reduce
  (:require [clojure.core.typed.analyzer.jvm.passes.classify-invoke :as classify-invoke]
            [clojure.core.typed.analyzer.jvm.passes.analyze-host-expr :as analyze-host-expr]
            [clojure.tools.analyzer.passes.jvm.annotate-tag :as annotate-tag]
            [clojure.tools.analyzer.passes.jvm.emit-form :refer [emit-form]]
            [clojure.tools.analyzer.passes.source-info :as source-info]
            [clojure.core.typed.analyzer.common.ast :as ast]
            [clojure.pprint :as pprint]
            [clojure.core.typed.analyzer.common :as ana]
            [clojure.core.typed.analyzer.common.passes.uniquify :as uniquify]))

(def beta-limit 500)

(defn find-matching-method [ast nargs]
  {:pre [(= :fn (:op ast))
         (nat-int? nargs)]
   :post [((some-fn nil? (comp #{:fn-method} :op)) %)]}
  (let [{fixed-arities false variadic-arities true} (group-by (comp boolean :variadic?) (:methods ast))
        matching-method (->> fixed-arities
                             (filter (fn [a]
                                       (= (:fixed-arity a) nargs)))
                             first)
        matching-method (or matching-method
                            (when-let [[variadic-arity] variadic-arities]
                              (when (<= (:fixed-arity variadic-arity) nargs)
                                variadic-arity)))]
    matching-method))

; Ast [TailAst -> Ast] -> Ast
(defn visit-tail-pos [ast f]
  (let [rec #(visit-tail-pos % f)]
    (case (:op ast)
      :do (update ast :ret rec)
      :if (-> ast
              (update :then rec)
              (update :else rec))
      (:let :letfn) (update ast :body rec)
      (f ast))))

(defn unwrap-with-meta [ast]
  (case (:op ast)
    :with-meta (recur (:expr ast))
    :unanalyzed (recur (-> (ana/analyze-outer ast)
                           ana/run-passes))
    ast))

;; assumption: none of (keys subst) occur in (vals subst)
(defn subst-locals [ast subst]
  (ast/postwalk ast
                (fn [ast]
                  (case (:op ast)
                    :local (if-let [sast (subst (:name ast))]
                             sast
                             ast)
                    ast))))

(defn var->vsym [^clojure.lang.Var v]
  (symbol (some-> (.ns v) ns-name str) (str (.sym v))))

(defn splice-seqable-expr
  "If ast is a seqable, returns a vector describing its members. Otherwise nil.

  :ordered entry is true if calling `first` on this expr is ordered

  eg. (vector 1 2 3)
  [{:op :sequential :expr (vector 1 2 3) :min-count 3 :max-count 3}]

  eg. (cons 4 (vector 1 2 3))
  [{:op :single :expr 4}
   {:op :sequential :expr (vector 1 2 3) :min-count 3 :max-count}]
  
  eg. (concat (vector 1 2 3) (range 0))
  [{:op :sequential :expr (vector 1 2 3) :min-count 3 :max-count 3}
   {:op :sequential :expr (range 0) :min-count ##Inf :max-count ##Inf}]

  eg. (concat (vector 1 2 3) (range 0) (vector 1 2 3))
  [{:op :sequential :expr (vector 1 2 3) :min-count 3 :max-count 3}
   {:op :sequential :expr (range 0) :min-count ##Inf :max-count ##Inf}
   {:op :sequential :expr (vector 1 2 3) :min-count 3 :max-count 3}]

  eg. (range 0)
  [{:op :sequential :expr (range 0) :min-count ##Inf :max-count ##Inf}]

  eg. (range 0 39)
  [{:op :sequential :expr (range 0 39) :min-count 39 :max-count 39}]

  eg. nil
  [{:op :sequential :expr nil :min-count 0 :max-count 0}]

  eg. (take-while symbol? (read-string))
  [{:op :sequential :expr (take-while symbol? (read)) :min-count 0 :max-count ##Inf}]

  eg. {:a 1 :b 2}
  [{:op :unordered :expr {:a 1 :b 2} :min-count 2 :max-count 2}]

  eg. #{:a :b}
  [{:op :unordered :expr #{:a :b} :min-count 2 :max-count 2}]
  "
  [{:keys [op env] :as ast}]
  {:post [((some-fn nil? vector?) %)]}
  ;(prn "splice-seqable-expr" op (emit-form ast))
  (case op
    :unanalyzed (splice-seqable-expr (-> (ana/analyze-outer ast)
                                         ana/run-passes))
    :local (when (#{:let} (:local ast))
             (some-> (:init ast) splice-seqable-expr))
    :vector [{:op :sequential
              :ordered true
              :expr ast
              :min-count (count (:items ast))
              :max-count (count (:items ast))}]
    :const (when (seqable? (:val ast))
             [{:op (if (sequential? (:val ast)) :sequential :unordered)
               :ordered (sequential? (:val ast))
               :expr (ana/analyze-const (:val ast) env)
               :min-count (count (:val ast))
               :max-count (count (:val ast))}])
    :do (splice-seqable-expr (:ret ast))
    (:let :let-fn) (splice-seqable-expr (:body ast))
    :new (let [cls ^Class (:class ast)
               csym (symbol (.getName cls))]
           (case csym
             ;; TODO needs testing
             ;clojure.lang.LazySeq (let [body (-> ast :args first :methods first :body)]
             ;                       (assert (map? body))
             ;                       (splice-seqable-expr body))
             nil))
    ;TODO lift `if` statements around invoke nodes so they are
    ; automatically handled (if performant)
    :invoke (let [{:keys [args]} ast
                  cargs (count args)
                  ufn (unwrap-with-meta (:fn ast))]
              (case (:op ufn)
                :var (let [vsym (var->vsym (:var ufn))]
                       (case vsym
                         clojure.core/concat
                         (loop [c []
                                args args]
                           (if (empty? args)
                             c
                             (let [[arg] args]
                               (when-let [spliced (splice-seqable-expr arg)]
                                 (recur (into c spliced) (next args))))))
                         clojure.core/list*
                         (when (<= 1 cargs)
                           (let [lspliced (splice-seqable-expr (peek args))]
                             (when lspliced
                               (into (mapv (fn [e]
                                             {:op :single
                                              :ordered true
                                              :expr e
                                              :min-count 1
                                              :max-count 1})
                                           (pop args))
                                     lspliced))))
                         (clojure.core/list clojure.core/vector)
                         [{:op :sequential
                           :ordered true
                           :expr ast
                           :min-count cargs
                           :max-count cargs}]
                         (clojure.core/vec clojure.core/seq clojure.core/sequence)
                         (when (= 1 cargs)
                           (splice-seqable-expr (first args)))
                         clojure.core/cons
                         (when (= 1 cargs)
                           (let [other (splice-seqable-expr (second args))]
                             (some->> other
                                      (into [{:op :single :expr (first args)
                                              :ordered true
                                              :min-count 1 :max-count 1}]))))
                         (clojure.core/rest clojure.core/next)
                         (when (= 1 cargs)
                           (when-let [spliced (splice-seqable-expr (first args))]
                             (let [dec-nat #(max 0 (dec %))
                                   consumed-from (atom nil)
                                   consumed-one (reduce (fn [spliced e]
                                                          ;; TODO deal with this case
                                                          (if-not (= (:min-count e)
                                                                     (:max-count e))
                                                            (reduced nil)
                                                            (conj spliced
                                                                  (if (or @consumed-from
                                                                          (zero? (:max-count e)))
                                                                    e
                                                                    (reset! consumed-from
                                                                            (-> e
                                                                                (update :consumed (fnil inc 0))
                                                                                (update :min-count dec-nat)
                                                                                (update :max-count dec-nat)))))))
                                                        []
                                                        spliced)]
                               (when consumed-one
                                 [{:op :rest
                                   :expr ast
                                   :ordered (if @consumed-from
                                              (:ordered @consumed-from)
                                              ;; must be empty here, so, ordered
                                              true)
                                   :min-count (apply + (map :min-count consumed-one))
                                   :max-count (apply + (map :max-count consumed-one))}]))))
                         nil))
                nil))
    nil))

(defn make-invoke-expr [the-fn args env]
  {:op :invoke
   :fn the-fn
   :env env
   :args args
   :form (list* (:form the-fn)
                (map :form args))
   :children [:fn :args]})

(defn make-var-expr [var env]
  {:op :var
   :var var
   :env env
   :form (var->vsym var)})

(defn fake-seq-invoke [seq-args env]
  (let [the-fn (make-var-expr #'seq env)
        args [{:op :vector
               :env env
               :items (vec seq-args)
               :form (mapv :form seq-args)
               :children [:items]}]
        invoke-expr (make-invoke-expr the-fn args env)]
    invoke-expr))

; ((fn* ([params*] body)) args*)
; ;=> body[args*/params*]
(defn maybe-beta-reduce-fn [ufn args & [{:keys [before-reduce] :as opts}]]
  {:pre [(= :fn (:op ufn))
         (vector? args)]}
  (when-not (:local ufn) ;;TODO
    (when-let [{:keys [params body variadic? fixed-arity env]} (find-matching-method ufn (count args))]
      ;; update before any recursive calls (ie. run-passes)
      (when before-reduce (before-reduce))
      (let [[fixed-params variadic-param] (if variadic?
                                            [(pop params) (peek params)]
                                            [params nil])
            [fixed-args variadic-args] (split-at fixed-arity args)
            subst (merge (zipmap (map :name fixed-params) fixed-args)
                         (when variadic-param
                           {(:name variadic-param) (fake-seq-invoke variadic-args env)}))]
        (-> body
            (subst-locals subst)
            ana/run-passes)))))

(defn record-beta-reduction [state]
  (swap! state update ::expansions inc))

(defn reached-beta-limit? [state]
  (or (::reached-beta-limit @state)
      (< beta-limit (::expansions @state))))

(defn ensure-within-beta-limit [state & [err-f]]
  (when (reached-beta-limit? state)
    (do (swap! state assoc ::reached-beta-limit true)
        (when err-f
          (err-f (::expansions @state))))))

; (apply f args* collarg)
; ;=> (f args* ~@collarg)
(defn maybe-beta-reduce-apply [{:keys [env] :as ufn} args & [{:keys [before-reduce] :as opts}]]
  {:pre [(= 'clojure.core/apply (var->vsym (:var ufn)))
         (vector? args)]}
  (when (<= 1 (count args))
    (let [[single-args collarg] ((juxt pop peek) args)]
      (let [{:keys [fixed rest-form] :as spliced} (splice-seqable-expr collarg)]
        (when (and spliced (seq fixed))
          (let [;; move as many fixed arguments out of the collection argument as possible
                form (if (contains? spliced :rest-form)
                       (cons (emit-form ufn)
                             (concat (map emit-form (concat single-args fixed)) [rest-form]))
                       (map emit-form (concat single-args fixed)))]
            (when before-reduce (before-reduce))
            (ana/run-passes (ana/analyze-form form env))))))))

(defn push-invoke
  "Push arguments into the function position of an :invoke
  so the function and arguments are both in the
  same :invoke node, then reanalyze the resulting :invoke node.
  
  eg. ((let [a 1] identity) 2)
      ;=> (let [a 1] (identity 2))
  eg. ((if c identity first) [1])
      ;=> (if c (identity [1]) (first [1]))
  "
  {:pass-info {:walk :post
               :before #{#'annotate-tag/annotate-tag
                         #'analyze-host-expr/analyze-host-expr
                         #'classify-invoke/classify-invoke}
               :state (fn [] (atom {::expansions 0}))}}
  [state {:keys [op] :as ast}]
  {:post [(:op %)]}
  ;(prn "expansions" (::expansions @state))
  (if (reached-beta-limit? state)
    (do
      (when-not (::reached-beta-limit @state)
        (prn "beta limit reached")
        (swap! state assoc ::reached-beta-limit true))
      ast)
    (case op
      :invoke (let [{the-fn :fn :keys [args]} ast]
                (visit-tail-pos
                  the-fn 
                  (fn [tail-ast]
                    (let [fn-form (emit-form tail-ast)
                          form (with-meta
                                 (list* fn-form (map emit-form args))
                                 (meta fn-form))
                          ;_ (prn "form" form)
                          env (:env tail-ast)
                          mform (ana/macroexpand-1 form env)]
                      ;(prn "mform" mform)
                      (if (= mform form)
                        (let [ufn (unwrap-with-meta tail-ast)
                              special-case
                              (case (:op ufn)
                                ;manually called by core.typed
                                ;:fn (maybe-beta-reduce-fn ufn args {:before-reduce #(swap! state update ::expansions inc)})
                                :var (case (var->vsym (:var ufn))
                                       clojure.core/apply (maybe-beta-reduce-apply ufn args)
                                       nil)
                                ;;TODO
                                :const (case (:type ast)
                                         #_:keyword #_(when (= 1 (count args))
                                                        (let [[map-arg] args]
                                                          ))
                                         #_:symbol
                                         #_:map
                                         #_:vector
                                         #_:set
                                         nil)
                                nil)]
                          (or special-case
                              (cond
                                ;; return original :invoke where possible
                                (= the-fn tail-ast) ast
                                :else {:op :invoke
                                       :form form
                                       :fn tail-ast
                                       :args args
                                       :env env
                                       :children [:fn :args]})))
                        (do (swap! state update ::expansions inc)
                            ;(prn "reparsing invoke" (first mform))
                            ;; TODO like analyze-seq, perhaps we can reuse the implemenation
                            (ana/run-passes
                              (-> (ana/analyze-form mform env)
                                  (update-in [:raw-forms] (fnil conj ())
                                             (vary-meta form assoc ::ana/resolved-op (ana/resolve-sym (first form) env)))))))))))
      ast)))


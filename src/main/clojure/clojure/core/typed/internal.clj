(ns ^:skip-wiki clojure.core.typed.internal)

(defn parse-fn*
  "(fn name? [[param :- type]* & [param :- type *]?] :- type? exprs*)
  (fn name? ([[param :- type]* & [param :- type *]?] :- type? exprs*)+)"
  [is-poly forms]
  (let [name (when (symbol? (first forms))
               (first forms))
        forms (if name (rest forms) forms)
        poly (when is-poly
               (first forms))
        forms (if poly (rest forms) forms)
        methods (if ((some-fn vector? keyword?) (first forms))
                  (list forms)
                  forms)
        parsed-methods (doall 
                         (for [method methods]
                           (merge-with merge
                             (loop [ann-params (first method)
                                    pvec []
                                    ann-info []]
                               (cond
                                 (empty? ann-params)
                                 ;TODO drest
                                 (let [[dom [amp rst]] (split-with (complement #{'&}) ann-info)]
                                   {:pvec pvec
                                    :ann (merge
                                           {:dom dom}
                                           (when (:rest rst)
                                             {:rest (:rest rst)})
                                           (when (:drest rst)
                                             {:drest (:drest rst)}))})

                                 ;rest param
                                 (#{'&} (first ann-params))
                                 (let [[amp & ann-params] ann-params]
                                   (if (#{:-} (second ann-params))
                                     (let [[p colon & rst-params] ann-params]
                                       (cond
                                         (#{'*} (second rst-params))
                                         (let [[t star & after-rst] rst-params]
                                           (recur after-rst
                                                  (conj pvec amp p)
                                                  (conj ann-info amp {:rest {:type t}})))

                                         (#{'...} (second rst-params))
                                         (let [[pretype dots bound & after-rst] rst-params]
                                           (recur after-rst
                                                  (conj pvec amp p)
                                                  (conj ann-info amp {:drest {:pretype {:type pretype}
                                                                              :bound bound}})))

                                         :else
                                         (throw (ex-info "Rest annotation must be followed with * or ..." {:form method}))))
                                     (let [[p & after-rst] ann-params]
                                       (recur after-rst
                                              (conj pvec amp p)
                                              (conj ann-info amp {:rest {:type 'Any}})))))

                                 ;fixed param
                                 :else
                                 (if (#{:-} (second ann-params))
                                   (let [[p colon t & rest-params] ann-params]
                                     (recur rest-params
                                            (conj pvec p)
                                            (conj ann-info {:type t})))
                                   (let [[p & rest-params] ann-params]
                                     (recur rest-params
                                            (conj pvec p)
                                            (conj ann-info {:type 'Any}))))))
                             (if (#{:-} (second method))
                               (let [[param colon t & body] method]
                                 {:body body
                                  :ann {:ret-type {:type t}}})
                               (let [[param & body] method]
                                 {:body body
                                  :ann {:ret-type {:type 'Any}}})))))]
    {:fn `(fn ~@(concat
                  (when name
                    [name])
                  (for [{:keys [body pvec]} parsed-methods]
                    (apply list pvec body))))
     :ann (map :ann parsed-methods)}))

;(ann parse-fn> [Any (Seqable Any) ->
;                '{:poly Any
;                  :fn Any ;Form
;                  :parsed-methods (Seqable '{:dom-syntax (Seqable Any)
;                                             :dom-lhs (Seqable Any)
;                                             :rng-syntax Any
;                                             :has-rng? Any
;                                             :body Any})}])
;for
(defn parse-fn>
  "(fn> name? poly? :- type? [[param :- type]* & [param :- type *]?] exprs*)
   (fn> name? poly? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
  [is-poly forms]
  (let [name (when (symbol? (first forms))
               (first forms))
        forms (if name (rest forms) forms)
        poly (when is-poly
               (first forms))
        forms (if poly (rest forms) forms)
        methods (if ((some-fn vector? keyword?) (first forms))
                  (list forms)
                  forms)
        ; turn [param :- type* & param :- type *?]
        ; into [[param :- type]* & [param :- type *]?]
        normalise-args
        (fn [arg-anns]
          (loop [flat-result ()
                 seq-exprs arg-anns]
            (cond
              (empty? seq-exprs) flat-result
              (and (#{'&} (first seq-exprs))
                   ; new syntax
                   (#{:-} (nth seq-exprs 2)))
              (do
                (assert (#{'*} (nth seq-exprs 4)))
                (assert (#{:-} (nth seq-exprs 2)))
                (assert (empty? (drop 5 seq-exprs)))
                (recur (concat flat-result ['& (vec (take 4 (next seq-exprs)))])
                       (drop 4 seq-exprs)))

              :else (do (assert (#{:-} (second seq-exprs))
                                "Incorrect syntax in fn>.")
                        (recur (concat flat-result [(vec (take 3 seq-exprs))])
                               (drop 3 seq-exprs))))))
        ;(fn> name? (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)"
        ; (HMap {:dom (Seqable TypeSyntax)
        ;        :rng (U nil TypeSyntax)
        ;        :body Any})
        parsed-methods (doall 
                         (for [method methods]
                           (let [[ret has-ret?] (when (not (vector? (first method)))
                                                  (assert (= :- (first method))
                                                          "Return type for fn> must be prefixed by :-")
                                                  [(second method) true])
                                 method (if ret 
                                          (nnext method)
                                          method)
                                 body (rest method)
                                 arg-anns (normalise-args (first method))
                                 [required-params _ [rest-param]] (split-with #(not= '& %) arg-anns)]
                             (assert (sequential? required-params)
                                     "Must provide a sequence of typed parameters to fn>")
                             (assert (not rest-param) "fn> doesn't support rest parameters yet")
                             {:dom-syntax (doall (map (comp second next) required-params))
                              :dom-lhs (doall (map first required-params))
                              :rng-syntax ret
                              :has-rng? has-ret?
                              :body body})))]
    {:poly poly
     :fn `(fn ~@(concat
                  (when name
                    [name])
                  (for [{:keys [body dom-lhs]} parsed-methods]
                    (apply list (vec dom-lhs) body))))
     :parsed-methods parsed-methods}))

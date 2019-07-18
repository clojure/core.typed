;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc ^:skip-wiki clojure.core.typed.internal
  (:require [clojure.set :as set]
            [clojure.core.typed.contract-utils :as con]))

(defn take-when
  "When pred is true of the head of seq, return [head tail]. Otherwise
  [nil seq]. Used as a helper for parsing optinal typed elements out
  of sequences. Say docstrings out of argument seqs."
  [pred seq]
  (if (pred (first seq))
    ((juxt first rest) seq)
    [nil seq]))

(defn parse-keyword-flat-map [forms]
  (loop [opts []
         forms forms]
    (cond 
      (keyword? (first forms))
      (let [[kv forms] (split-at 2 forms)]
        (assert (#{2} (count kv))
                (str "Missing keyword argument to: " (pr-str (first kv))))
        (recur (apply conj opts kv)
               forms))
      :else [opts forms])))

(defn parse-keyword-map [forms]
  (let [[flatopts forms] (parse-keyword-flat-map forms)]
    [(apply hash-map flatopts) forms]))

(defn parse-fn*
  "(fn name? [[param :- type]* & [param :- type *]?] :- type? exprs*)
  (fn name? ([[param :- type]* & [param :- type *]?] :- type? exprs*)+)"
  [[_fn_ & forms :as form]]
  {:pre [(symbol? _fn_)
         #_(= "fn" (name _fn_))]}
  (let [[{poly :forall :as opts} forms] (parse-keyword-map forms)
        [name forms] (take-when symbol? forms)
        _ (assert (not (keyword? (first forms))))
        single-arity-syntax? (vector? (first forms))
        methods (if single-arity-syntax?
                  (list forms)
                  forms)
        parsed-methods   (for [method methods]
                           (merge-with merge
                             (let [ann-params (first method)]
                               (assert (vector? ann-params))
                               {:ann-params ann-params
                                :original-method (vary-meta method #(merge (meta form)
                                                                           (meta ann-params)
                                                                           %))})
                             (loop [ann-params (first method)
                                    pvec (empty (first method)) ; an empty param vector with same metadata
                                    ann-info []]
                               (cond
                                 (empty? ann-params)
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
                                              (conj ann-info amp {:rest {:type 'clojure.core.typed/Any
                                                                         :default true}})))))

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
                                            (conj ann-info {:type 'clojure.core.typed/Any
                                                            :default true}))))))
                             (if (and (#{:-} (second method))
                                      (<= 3 (count method)))
                               (let [[param colon t & body] method]
                                 {:body body
                                  :ann {:rng {:type t}}})
                               (let [[param & body] method]
                                 {:body body
                                  :ann {:rng {:type 'clojure.core.typed/Any
                                              :default true}}}))))
        final-ann (mapv :ann parsed-methods)]
    #_(assert ((con/vec-c?
               (con/hmap-c?
                 :dom (con/every-c? (con/hmap-c? :type (constantly true)))
                 (con/optional :rest) (con/hmap-c? :type (constantly true))
                 :rng (some-fn (con/hmap-c? :default #{true})
                               (con/hmap-c? :type (constantly true)))))
             final-ann)
            final-ann)
    {:fn `(fn ~@(concat
                  (when name
                    [name])
                  (for [{:keys [body pvec]} parsed-methods]
                    (apply list pvec body))))
     :ann final-ann
     :poly poly
     :parsed-methods parsed-methods
     :name name
     :single-arity-syntax? single-arity-syntax?}))

(defn parse-defn* [args]
  (let [[flatopt args] (parse-keyword-flat-map args)
        [name & args] args
        _ (assert (symbol? name) "defn name should be a symbol")
        [docstring args] (take-when string? args)
        [attr-map args] (take-when map? args)]
    {:name (vary-meta name merge
                      {:arglists
                       (list 'quote
                             (if (vector? (first args)) ; arity = 1
                               (list (first args))
                               (map first args)))}
                      (when docstring {:doc docstring})
                      attr-map)
     :args (concat flatopt args)}))

(defn parse-loop*
  [forms]
  (let [parsed-loop (merge
                      (loop [ann-params (first forms)
                             pvec []
                             ann-info []]
                        (cond
                          (empty? ann-params)
                          {:pvec pvec
                           :ann {:params ann-info}}
                          
                          :else
                          (if (#{:-} (second ann-params))
                            (let [[p colon t init & rest-params] ann-params]
                              (recur rest-params
                                     (conj pvec p init)
                                     (conj ann-info {:type t})))
                            (let [[p init & rest-params] ann-params]
                              (recur rest-params
                                     (conj pvec p init)
                                     (conj ann-info {:type 'clojure.core.typed/Any
                                                     :default true}))))))
                      {:body (next forms)})]
    {:loop `(clojure.core/loop ~(:pvec parsed-loop) ~@(:body parsed-loop))
     :ann (:ann parsed-loop)}))

(defn binder-names [binder]
  {:post [(every? symbol? %)]}
  (map (fn [v]
         (if (vector? v)
           (first v)
           v))
       binder))

(defn gen-ann-protocol [{:keys [name methods binder] :as dp-ann}]
  (let [tvars (set (binder-names binder))
        this-type (if binder
                    `(~name ~@(binder-names binder))
                    name)]
  `(clojure.core.typed/ann-protocol 
     ~@(when binder
         [binder])
     ~name
     ~@(mapcat (fn [{:keys [name arities poly]}]
                 (let [localtvars (set (binder-names poly))
                       _ (assert (empty? (set/intersection localtvars
                                                           tvars))
                                 "Shadowing a protocol type variable in a method is disallowed")
                       fn-type `(clojure.core.typed/IFn
                                  ~@(map (fn [{:keys [ptypes ret]}]
                                           (let [[provided-this & argts] ptypes
                                                 ; if programmer provides the default 'this' type, use that,
                                                 ; otherwise use the current protocol.
                                                 actual-this (if (:default provided-this)
                                                               this-type
                                                               (:type provided-this))]
                                             `[~@(concat [actual-this] (map :type argts)) ~'-> ~(:type ret)]))
                                         arities))]
                   [name (if poly
                           `(clojure.core.typed/All ~poly ~fn-type)
                           fn-type)]))
               methods))))


(defn parse-defprotocol*
  [forms]
  (let [[binder forms] (take-when vector? forms)
        [pname & typed-decl-methods] forms
        [pdoc typed-decl-methods] (take-when string? typed-decl-methods)
        parse-pvec (fn [pvec] ; parse parameter vectors
                     {:pre [(vector? pvec)]
                      :post [((con/hmap-c? :actual vector?
                                           :ptypes vector?)
                              %)]}
                     (loop [pvec pvec
                            actual (empty pvec) ; empty vector with same metadata as pvec
                            ptypes []]
                       (assert (every? vector? [actual ptypes]))
                       (cond
                         (empty? pvec) {:ptypes ptypes :actual actual}
                         :else (if (#{:-} (second pvec))
                                 (let [_ (assert (#{3} (count (take 3 pvec)))
                                                 "Missing type annotation after :-")
                                       [b colon t & rst] pvec]
                                   (recur rst 
                                          (conj actual b)
                                          (conj ptypes {:type t})))
                                 (let [_ (assert (seq pvec))
                                       [b & rst] pvec]
                                   (recur rst 
                                          (conj actual b)
                                          (conj ptypes {:type 'clojure.core.typed/Any
                                                        :default true})))))))
        actual-decl-methods (for [m typed-decl-methods]
                              (let [[poly rst] (take-when vector? m)
                                    [name & dvecs] rst]
                                (assert (symbol? name) (str "defprotocol method name must be a symbol: " pname))
                                (loop [dvecs dvecs
                                       arities []]
                                  (cond 
                                    (or (empty? dvecs)
                                        (string? (first dvecs)))
                                    (merge {:poly poly
                                            :name name
                                            :arities arities}
                                           (when (string? (first dvecs))
                                             {:doc (first dvecs)}))

                                    :else (if (#{:-} (second dvecs))
                                            (let [_ (assert (#{3} (count (take 3 dvecs)))
                                                            "Missing type annotation after :-")
                                                  [v colon t & rst] dvecs
                                                  {:keys [ptypes actual]} (parse-pvec v)]
                                              (recur rst
                                                     (conj arities {:ret {:type t}
                                                                    :ptypes ptypes
                                                                    :actual actual})))
                                            (let [_ (assert (seq dvecs))
                                                  [v & rst] dvecs
                                                  {:keys [ptypes actual]} (parse-pvec v)]
                                              (recur rst
                                                     (conj arities {:ret {:type 'clojure.core.typed/Any
                                                                          :default true}
                                                                    :ptypes ptypes
                                                                    :actual actual}))))))))
        ann {:binder binder
             :name pname
             :methods (map #(dissoc % :doc) actual-decl-methods)}]
    {:defprotocol `(clojure.core/defprotocol 
                     ~pname 
                     ~@(when pdoc [pdoc])
                     ~@(map (fn [{:keys [name arities doc]}] 
                              `(~name ~@(concat ; prefer left-most arities if grouped duplicates
                                                (reduce
                                                  (fn [ret current]
                                                    (if (= (count current) (count (last ret)))
                                                      ret
                                                      (conj ret current)))
                                                  []
                                                  (map :actual arities))
                                                (when doc
                                                  [doc]))))
                            actual-decl-methods))
     :ann-protocol (gen-ann-protocol ann)}))

(defn parse-let*
  [[bvec & forms]]
  (let [actual-bvec (loop [bvec bvec
                           actual-bvec (empty bvec)] ; empty vector with same metadata as bvec
                      (assert (vector? actual-bvec))
                      (cond
                        (empty? bvec) actual-bvec
                        :else (if (#{:-} (second bvec))
                                (let [_ (assert (#{4} (count (take 4 bvec)))
                                                "Incorrect forms following :-")
                                      [v colon t init & rst] bvec]
                                  (recur rst
                                         (conj actual-bvec v `(clojure.core.typed/ann-form ~init ~t))))
                                (let [_ (assert (#{2} (count (take 2 bvec)))
                                                "No init found for local binding")
                                      [v init & rst] bvec]
                                  (recur rst
                                         (conj actual-bvec v init))))))]
    {:let `(clojure.core/let ~actual-bvec ~@forms)}))

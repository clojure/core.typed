;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.file-mapping
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.tools.analyzer.ast :as ast-ops]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]))

; (Vec '{:ftype Type :fn-expr Expr})
(def ^:private ^:dynamic *fn-stack* [])
(set-validator! #'*fn-stack* vector?)

;(t/defalias InfoEntry '{:msg t/Str :fn-stack (t/Vec {:ftype Type :fn-expr Expr})})
;(t/defalias MappingKey '{:line Int :column Int :file Str})
;(t/defalias InfoMap (Map MappingKey (Vec InfoEntry)))

;[Expr -> InfoMap]
(defn ast->info-map 
  [ast]
  (letfn [(mapping-key [{:keys [env] :as ast}]
            (let [ks [:line :column :file]]
              (when ((apply every-pred ks) env)
                (select-keys env ks))))]
    (case (:op ast)
      ; Functions can be checked any number of times. Each
      ; check is stored in the ::t/cmethods entry.
      :fn (let [method-mappings (for [method (::t/cmethods ast)]
                                  (let [ftype (::t/ftype method)
                                        _ (assert (r/Function? ftype))
                                        ;floc (mapping-key ast)
                                        ;_ (assert floc (select-keys (:env ast) [:line :file :column]))
                                        ]
                                    (binding [*fn-stack* (conj *fn-stack* {;:loc floc
                                                                           :name (cu/fn-self-name ast)
                                                                           :ftype ftype})]
                                      (ast->info-map method))))
                v [{:expr ast
                    :fn-stack *fn-stack*}]
                this-file (-> ast :env :file)
                _ (assert (string? this-file))]
            (merge
              (apply merge-with
                     (fn [a b] (vec (concat a b)))
                     method-mappings)
              (let [{:keys [line column end-line end-column]} (-> ast :form meta)]
                (if (and line column end-line end-column)
                  (merge {{:file this-file :line line :column column} v}
                         {{:file this-file :line end-line :column (dec end-column)} v})
                  (when-let [k (mapping-key ast)]
                    {k v})))))
      (apply merge 
             (concat (map ast->info-map (ast-ops/children ast))
                     (let [{:keys [line column end-line end-column]} (-> ast :form meta)
                           this-file (-> ast :env :file)
                           _ (assert (string? this-file))
                           v [{:expr ast
                               :fn-stack *fn-stack*}]]
                       (if (and line column end-line end-column)
                         (if (and (symbol? (:form ast))
                                  (== line end-line))
                           (for [c (range column end-column)]
                             {{:file this-file :line line :column c} v})
                           [{{:file this-file :line line :column column} v}
                            {{:file this-file 
                              :line end-line 
                              :column (dec end-column)}
                             v}])
                         (when-let [k (mapping-key ast)]
                           [{k v}]))))))))

;(defalias MsgMap (Map MappingKey Str))

;[InfoMap -> MsgMap]
(defn info-map->msg-map [info-map]
  (into {} (for [[k v] info-map]
             (let [msg (if (< 1 (count v))
                         (let [ms (seq
                                    (filter identity
                                            (map (fn [{:keys [expr fn-stack]}]
                                                   (let [r (u/expr-type expr)]
                                                     (when (r/TCResult? r)
                                                       (prs/with-unparse-ns (cu/expr-ns expr)
                                                         (str
                                                           "In context size " (count fn-stack) ":\n\t"
                                                           (pr-str (prs/unparse-type (r/ret-t r)))
                                                           "\n")))))
                                                 v)))]
                           (when ms
                             (apply str ms)))
                         (let [{:keys [expr]} (first v)
                               r (u/expr-type expr)]
                           (prs/with-unparse-ns (cu/expr-ns expr)
                             (when (r/TCResult? r)
                               (pr-str (prs/unparse-type (r/ret-t r)))))))]
               (when msg
                 [k msg])))))

(defn ast->file-mapping [ast]
  (-> ast ast->info-map info-map->msg-map))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; adapted from tools.analyzer
(ns clojure.core.typed.analyzer.passes
  (:require [clojure.core.typed.analyzer :as ana]
            [clojure.tools.analyzer.passes :as passes]
            [clojure.tools.analyzer.utils :as u]))

(defn compile-passes [pre-passes post-passes info]
  (let [with-state (filter (comp :state info) (concat pre-passes post-passes))
        ; (Map Var Atom) that is reinitialized once for each AST at the root
        state      (zipmap with-state (mapv #(:state (info %)) with-state))

        pfns-fn    (fn [passes]
                     (reduce (fn [f pass]
                               (let [i (info pass)
                                     pass (cond
                                            ;; passes with :state meta take 2 arguments: state and ast
                                            (:state i)
                                            (fn [ast]
                                              (let [pass-state (-> ast :env ::ana/state (get pass))]
                                                (pass pass-state ast)))
                                            ;; otherwise, a pass just takes ast
                                            :else pass)]
                                 #(pass (f %))))
                             (fn [ast] ast)
                             passes))
        pre-passes  (pfns-fn pre-passes)
        post-passes (pfns-fn post-passes)
        init-ast (fn [ast]
                   (let [; immediately when starting to analyze an AST, generate
                         ; atoms for each pass that requires state. these will
                         ; be passed around in (-> ast :env ::ana/state).
                         state-fn (fn [root-state]
                                    (or root-state
                                        ; this line assumes that ::ana/state is correctly propagated from its
                                        ; inception at the root of the AST.
                                        ; Note: if this code executes more than once per AST (which would be
                                        ; incorrect, since it should only run at the root), then it's probably
                                        ; a bug somewhere else that fails to propagate :env down the AST.
                                        (u/update-vals state #(%))))]
                     (update-in ast [:env ::ana/state] state-fn)))]
    {:init-ast init-ast
     :pre pre-passes
     :post post-passes}))

(defn schedule
  "Takes a set of Vars that represent tools.analyzer passes and returns a map
   m of two functions, such that (ast/walk ast (:pre m) (:post m)) runs all
   passes on ast.

   Each pass must have a :pass-info element in its Var's metadata and it must point
   to a map with the following parameters (:before, :after, :affects and :state are
   optional):
   * :after    a set of Vars, the passes that must be run before this pass
   * :before   a set of Vars, the passes that must be run after this pass
   * :depends  a set of Vars, the passes this pass depends on, implies :after
   * :walk     a keyword, one of:
                 - :none if the pass does its own tree walking and cannot be composed
                         with other passes
                 - :post if the pass requires a postwalk and can be composed with other
                         passes
                 - :pre  if the pass requires a prewalk and can be composed with other
                         passes
                 - :any  if the pass can be composed with other passes in both a prewalk
                         or a postwalk
   * :state    a no-arg function that should return an atom holding an init value that will be
               passed as the first argument to the pass (the pass will thus take the ast
               as the second parameter), the atom will be the same for the whole tree traversal
               and thus can be used to preserve state across the traversal
   An opts map might be provided, valid parameters:
   * :debug?   if true, returns a vector of the scheduled passes rather than the concrete
               function"
  [passes & [opts]]
  {:pre [(set? passes)
         (every? var? passes)]}
  (let [info        (@#'passes/indicize (mapv (fn [p] (merge {:name p} (:pass-info (meta p)))) passes))
        passes+deps (into passes (mapcat :depends (vals info)))]
    (if (not= passes passes+deps)
      (recur passes+deps [opts])
      (let [[{pre-passes  :passes :as pre}
             {post-passes :passes :as post}
             :as ps]
            (passes/schedule-passes info)

            _ (assert (= 2 (count ps)) ps)
            _ (assert (= :pre (:walk pre)))
            _ (assert (= :post (:walk post)))
            ]
        (if (:debug? opts)
          (mapv #(select-keys % [:passes :walk]) ps)
          (compile-passes pre-passes post-passes info))))))

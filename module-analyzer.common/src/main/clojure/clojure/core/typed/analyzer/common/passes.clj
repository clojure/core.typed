;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; adapted from clojure.core.tools.analyzer
;TODO port tests from clojure.tools.analyzer.passes-test
(ns clojure.core.typed.analyzer.common.passes
  (:require [clojure.core.typed.analyzer.common :as ana]
            [clojure.core.typed.analyzer.common.utils :as u]))

(defn ^:private has-deps?
  "Returns true if the pass has any dependencies"
  [pass]
  (seq (:dependencies pass)))

(defn ^:private indicize
  "Takes a set of pass-infos and returns a map of pass-name -> pass-info"
  [passes]
  (zipmap (map :name passes) passes))

(defn ^:private remove-pass
  "Takes a set of pass-infos and a pass, and removes the pass from the set of
   pass-infos, updating :dependencies and :dependants as well."
  [passes pass]
  (indicize (reduce (fn [m p] (conj m (-> p (update-in [:dependencies] disj pass)
                                        (update-in [:dependants] disj pass))))
                    #{} (vals (dissoc passes pass)))))

;modified from tools.analyzer
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

(defn desugar-deps
  "Takes a map of pass-name -> pass deps and puts the :after :affects and :before passes
   in the appropriate pass :depends"
  [passes]
  (reduce-kv (fn [m name {:keys [after affects before]}]
               (reduce (fn [m p] (update-in m [p :depends] (fnil conj #{}) name))
                       (update-in m [name :depends] (fnil into #{}) (into affects (filter passes after)))
                       before)) passes passes))

(defn ^:private calc-deps
  "Takes a map of pass-name -> pass deps, a pass name, the explicit pass dependencies
   and a set of available pass-infos.
   Resolves all the transitive deps of the pass and assocs them to the map, indexed by
   the pass name."
  [m k deps passes]
  (if (m k)
    m
    (reduce (fn [m dep]
              (let [m (calc-deps m dep (get-in passes [dep :depends]) passes)]
                (update-in m [k] into (conj (or (m dep) #{}) dep))))
            (assoc m k deps) deps)))

(defn calculate-deps
  "Takes a map of pass-name -> pass-info and adds to each pass-info :dependencies and
   :dependants info, which also contains the transitive dependencies"
  [passes]
  (let [passes (desugar-deps passes)
        dependencies (reduce-kv (fn [deps pname {:keys [depends]}]
                                  (calc-deps deps pname depends passes))
                                {} passes)
        dependants   (reduce-kv (fn [m k v] (reduce (fn [m v] (update-in m [v] (fnil conj #{}) k))
                                                   (update-in m [k] (fnil into #{}) nil) v))
                                {} dependencies)]
    (reduce-kv (fn [m k v] (assoc m k (merge (dissoc (passes k) :depends)
                                            {:dependencies (set v) :dependants (set (dependants k))})))
               {} dependencies)))

(defn group
  "Takes a scheduler state and returns a vector of three elements (or nil):
   * the :walk of the current group
   * a vector of consecutive passes that can be collapsed in a single pass (the current group)
   * the remaining scheduler state

   E.g. given:
   [{:walk :any .. } {:walk :pre ..} {:walk :post ..} {:walk :pre ..}]
   it will return:
   [:pre [{:walk :any ..} {:walk :pre ..}] [{:walk :post ..} {:walk :pre ..}]]"
  [state]
  (loop [w nil group [] [cur & rest :as state] state]
    (if (seq state)
      (cond
       (:affects (last group))
       [w group state]

       w
       (if (#{w :any} (:walk cur))
         (recur w (conj group cur) rest)
         [w group state])

       :else
       (case (:walk cur)
         :any
         (recur nil (conj group cur) rest)
         :none
         [w group state]
         (recur (:walk cur) (conj group cur) rest)))
      [w group state])))

(defn satisfies-affected? [{:keys [affects walk]} passes]
  (loop [passes passes]
    (let [free (vals (filter (comp empty? :dependants val) passes))]
      (if-let [available-passes (seq (filter (comp #{walk :any} :walk) free))]
        (recur (reduce remove-pass passes (mapv :name available-passes)))
        (empty? (filter (fn [{:keys [name]}] ((set affects) name)) (vals passes)))))))

(defn maybe-looping-pass [free passes]
  (if-let [looping (seq (filter :affects free))]
    (loop [[l & ls] looping]
      (if l
        (if (satisfies-affected? l (remove-pass passes (:name l)))
          ;; all deps satisfied
          l
          (recur ls))
        (if-let [p (first (remove :affects free))]
          ;; pick a random avaliable non-looping pass
          p
          (throw (ex-info (str "looping pass doesn't encompass affected passes: " (:name l))
                          {:pass l})))))
    ;; pick a random available pass
    (first free)))

(def ^:private ffilter (comp first filter))

(defn ^:private first-walk [f c]
  (ffilter (comp #{f} :walk) c))

(defn schedule* [state passes]
  (let [free             (filter (comp empty? :dependants) (vals passes))
        w                (first (group state))
        non-looping-free (remove :affects free)]
    (if (seq passes)
      (let [{:keys [name] :as pass} (or (ffilter :compiler free)
                                        (and w (or (first-walk w non-looping-free)
                                                   (first-walk :any non-looping-free)))
                                        (first-walk :none free)
                                        (maybe-looping-pass free passes))]
        (recur (cons (assoc pass :passes [name]) state)
               (remove-pass passes name)))
      state)))

(defn collapse [state]
  (loop [[cur & rest :as state] state ret []]
    (if (seq state)
      (if (= :none (:walk cur))
        (recur rest (conj ret cur))
        (let [[w g state] (group state)]
          (recur state (conj ret {:walk (or w :pre) :passes (mapv :name g)}))))
      ret)))

(defn schedule-passes
  [passes]
  (let [passes (calculate-deps passes)]

    (when (every? has-deps? (vals passes))
      (throw (ex-info "Dependency cycle detected" passes)))

    (when (next (filter :compiler (vals passes)))
      (throw (ex-info "Only one compiler pass allowed" passes)))

    (collapse (schedule* () passes))))

;modified from tools.analyzer
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
  (let [info        (indicize (mapv (fn [p] (merge {:name p} (:pass-info (meta p)))) passes))
        passes+deps (into passes (mapcat :depends (vals info)))]
    (if (not= passes passes+deps)
      (recur passes+deps [opts])
      (let [[{pre-passes  :passes :as pre}
             {post-passes :passes :as post}
             :as ps]
            (schedule-passes info)

            _ (assert (= 2 (count ps)) ps)
            _ (assert (= :pre (:walk pre)))
            _ (assert (= :post (:walk post)))
            ]
        (if (:debug? opts)
          (mapv #(select-keys % [:passes :walk]) ps)
          (compile-passes pre-passes post-passes info))))))

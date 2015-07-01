(ns clojure.core.typed.deps.clojure.tools.analyzer.passes
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.ast :refer [prewalk postwalk]]
            [clojure.core.typed.deps.clojure.tools.analyzer.utils :refer [update-vals]]))

(def ^:private ffilter (comp first filter))

(defn ^:private ffilter-walk [f c]
  (ffilter (comp f :walk) c))

(defn ^:private has-deps?
  "Returns true if the pass has some dependencies"
  [pass]
  (seq (:dependencies pass)))

(defn ^:private group-by-walk
  "Takes a set of pass-infos and returns a map grouping them by :walk.
   Possible keys are :any, :none, :pre and :post"
  [passes]
  (reduce-kv (fn [m k v] (assoc m k (set (map :name v))))
             {} (group-by :walk passes)))

(defn ^:private indicize
  "Takes a set of pass-infos and returns a map of pass-name -> pass-info"
  [passes]
  (zipmap (map :name passes) passes))

(defn ^:private remove-pass
  "Takes a set of pass-infos and a pass, and removes the pass from the set of
   pass-infos, updating :dependencies and :dependants aswell"
  [passes pass]
  (indicize (reduce (fn [m p] (conj m (-> p (update-in [:dependencies] disj pass)
                                        (update-in [:dependants] disj pass))))
                    #{} (vals (dissoc passes pass)))))

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

(defn desugar-deps [passes]
  (reduce-kv (fn [m name {:keys [after affects before]}]
               (reduce (fn [m p] (update-in m [p :depends] (fnil conj #{}) name))
                       (update-in m [name :depends] (fnil into #{}) (into affects (filter passes after)))
                       before)) passes passes))

(defn calculate-deps
  "Takes a map of pass-name -> pass-info and adds to each pass-info :dependencies and
   :dependants info, which also contain the transitive dependencies"
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

(defn schedule* [state passes]
  (let [f                         (filter (comp empty? :dependants val) passes)
        [free & frs :as free-all] (vals f)
        w                         (first (group state))
        non-looping-free          (remove :affects free-all)]
    (if (seq passes)
      (let [{:keys [name] :as pass} (or (ffilter :compiler free-all)
                                        (and w (or (ffilter-walk #{w} non-looping-free)
                                                   (ffilter-walk #{:any} non-looping-free)))
                                        (ffilter-walk #{:none} free-all)
                                        (maybe-looping-pass free-all passes))]
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
  (let [passes (calculate-deps passes)
        dependencies (set (mapcat :dependencies (vals passes)))]

    (when (every? has-deps? (vals passes))
      (throw (ex-info "Dependency cycle detected" passes)))

    (when (next (filter :compiler (vals passes)))
      (throw (ex-info "Only one compiler pass allowed" passes)))

    (collapse (schedule* () passes))))

(defn compile-passes [passes walk info]
  (let [with-state (filter (comp :state info) passes)
        state      (zipmap with-state (mapv #(:state (info %)) with-state))
        pfns       (reduce (fn [f p]
                             (let [i (info p)
                                   p (cond
                                      (:state i)
                                      (fn [_ s ast] (p (s p) ast))
                                      (:affects i)
                                      (fn [a _ ast] ((p a) ast))
                                      :else
                                      (fn [_ _ ast] (p ast)))]
                               (fn [a s ast]
                                 (p a s (f a s ast))))) (fn [_ _ ast] ast) passes)]
    (fn analyze [ast]
      (walk ast (partial pfns analyze (update-vals state #(%)))))))

(defn schedule
  "Takes a set of Vars that represent tools.analyzer passes and returns a function
   that takes an AST and applies all the passes and their dependencies to the AST,
   trying to compose together as many passes as possible to reduce the number of
   full tree traversals.

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
   * :affects  a set of Vars, this pass must be the last in the same tree traversal that all
               the specified passes must partecipate in
               This pass must take a function as argument and return the actual pass, the
               argument represents the reified tree traversal which the pass can use to
               control a recursive traversal, implies :depends
   * :state    a no-arg function that should return the init value of an atom that will be
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
      (if (:debug? opts)
        (mapv #(select-keys % [:passes :walk])
              (schedule-passes info))
        (reduce (fn [f {:keys [passes walk]}]
                  (let [pass (if (= walk :none)
                               (first passes)
                               (compile-passes passes (if (= :pre walk) prewalk postwalk) info))]
                    (comp pass f)))
                identity (schedule-passes info))))))

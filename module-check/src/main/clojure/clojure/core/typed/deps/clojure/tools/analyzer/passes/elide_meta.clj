;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.elide-meta)

(def ^:dynamic elides
  "Predicate IFn used to indicate what map keys to elide from metadata.
   Defaults to (set (:elide-meta *compiler-options*))"
  (set (:elide-meta *compiler-options*)))

(defn replace-meta [meta new-meta]
  (if (= :const (:op meta))
    (assoc meta
      ;:form new-meta
      :val  new-meta)
    (let [meta-map (mapv (fn [k v]
                       (when-not (elides (:form k))
                         [k v]))
                     (:keys meta) (:vals meta))]
      (assoc meta
        ;:form new-meta
        :keys (vec (keep first meta-map))
        :vals (vec (keep second meta-map))))))

(defn -elide-meta
  [{:keys [op meta expr env] :as ast}]
  (let [form (:form meta)
        new-meta (apply dissoc form (filter elides (keys form)))]
    (case op
      :const
        (if (or (not meta)
              (= new-meta (:form meta)))
          ast
          (if (not (empty? new-meta))
            (assoc-in ast [:meta :val] new-meta)
            (-> ast
                (update-in [:val] with-meta nil)
                ;(update-in [:form] with-meta nil)
                (dissoc :children :meta))))
      :with-meta
        (if (not (empty? new-meta))
          (if (= new-meta (:form meta))
            ast
            (assoc ast :meta (replace-meta meta new-meta)))
          (-> expr
              (assoc-in [:env :context] (:context env))
              (update-in [:form] with-meta {})))
      :def
        (if (not (empty? new-meta))
          (if (= new-meta (:form meta))
            ast
            (assoc ast :meta (replace-meta meta new-meta)))
          (assoc (dissoc ast :meta) :children [:init]))
        ast)))

(defn elide-meta
  "If elides is not empty and the AST node contains metadata,
   dissoc all the keys in elides from the metadata."
  [ast]
  (if (seq elides)
    (-elide-meta ast)
    ast))

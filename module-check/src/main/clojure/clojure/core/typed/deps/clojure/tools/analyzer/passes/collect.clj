;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.collect
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.utils :refer [protocol-node?]]
            [clojure.core.typed.deps.clojure.tools.analyzer.ast :refer [update-children]]
            [clojure.core.typed.deps.clojure.tools.analyzer.passes.cleanup :refer [cleanup]]))

(def ^:private ^:dynamic *collects*)

(defn -register-constant
  [form tag type]
  (let [key {:form form
             :meta (meta form)
             :tag  tag}
        collects @*collects*]
    (or (:id ((:constants collects) key)) ;; constant already in the constant table
        (let [id (:next-id collects)]
          (swap! *collects* #(assoc-in (update-in % [:next-id] inc)
                                       [:constants key]
                                       {:id   id
                                        :tag  tag
                                        :val  form
                                        :type type}))
          id))))

(defmulti -collect-const    :op)
(defmulti -collect-callsite :op)

(defmethod -collect-const    :default [ast] ast)
(defmethod -collect-callsite :default [ast] ast)

(defmethod -collect-const :const
  [{:keys [val tag type] :as ast}]
  (if (and (not= type :nil)        ;; nil and true/false can be emitted as literals,
           (not= type :boolean)) ;; no need to put them on the constant table
    (let [id (-register-constant val tag type)]
      (assoc ast :id id))
    ast))

(defmethod -collect-const :def
  [ast]
  (let [id (-register-constant (:var ast) clojure.lang.Var :var)]
    (assoc ast :id id)))

(defmethod -collect-const :var
  [ast]
  (let [id (-register-constant (:var ast) clojure.lang.Var :var)]
    (assoc ast :id id)))

(defmethod -collect-const :the-var
  [ast]
  (let [id (-register-constant (:var ast) clojure.lang.Var :var)]
    (assoc ast :id id)))

(defmethod -collect-callsite :keyword-invoke
  [ast]
  (swap! *collects* #(update-in % [:keyword-callsites] conj (-> ast :fn :form)))
  ast)

(defmethod -collect-callsite :protocol-invoke
  [ast]
  (swap! *collects* #(update-in % [:protocol-callsites] conj (-> ast :fn :var)))
  ast)

(defn merge-collects [ast]
  (merge ast (dissoc @*collects* :where :what :next-id :top-level?)))

;; collects constants and callsites in one pass
(defn -collect [ast collect-fn]
  (let [collects @*collects*
        collect? ((:where collects) (:op ast))

        ast (with-bindings ;; if it's a collection point, set up an empty constant/callsite frame
              (if collect? {#'*collects* (atom (merge collects
                                                      {:next-id            0
                                                       :constants          {}
                                                       :protocol-callsites #{}
                                                       :keyword-callsites  #{}}))}
                  {})
              (let [ast (-> ast (update-children #(-collect % collect-fn))
                           collect-fn)]
                (if collect?
                  (merge-collects ast)
                  ast)))]
        ast))

(declare collect-closed-overs*)
(defn -collect-closed-overs
  [ast]
  (-> (case (:op ast)
       :letfn ;; seed letfn bindings
       (let [bindings (:bindings ast)]
         (doseq [{:keys [name]} bindings]
           (swap! *collects* #(update-in % [:locals] conj name)))
         ast)
       :binding
       (let [name (:name ast)]
         (if (= :field (:local ast))
           (swap! *collects* #(assoc-in % [:closed-overs name] (cleanup ast))) ;; special-case: put directly as closed-overs
           (swap! *collects* #(update-in % [:locals] conj name)))                        ;; register the local as a frame-local locals
         ast)
       :local
       (let [name (:name ast)]
         (when-not ((:locals @*collects*) name)                                         ;; if the local is not in the frame-local locals
           (swap! *collects* #(assoc-in % [:closed-overs name] (cleanup ast)))) ;; then it's from the outer frame locals, thus a closed-over
         ast)
       ast)
    (update-children collect-closed-overs*))) ;; recursively collect closed-overs in the children nodes

(defn collect-closed-overs*
  [{:keys [op] :as ast}]
  (let [collects @*collects*
        collect? ((:where collects) op)]
    (if collect?
      (let [[ast {:keys [closed-overs locals]}]
            (binding [*collects* (atom (merge @*collects*
                                              {:closed-overs {} :locals #{}}))]
              [(update-children ast -collect-closed-overs) @*collects*])]
        (swap! *collects* #(update-in % [:closed-overs] merge ;; propagate closed-overs from the inner frame to the outer frame
                                      (into {}
                                            (remove (fn [[_ {:keys [local]}]] ;; remove deftype fields from the closed-over locals
                                                      (and (= op :deftype)
                                                           (= :field local)))
                                                    (apply dissoc closed-overs        ;; remove from the closed-overs locals that were
                                                           (:locals @*collects*)))))) ;; local to the inner frame
        (assoc ast :closed-overs closed-overs))
      (-collect-closed-overs ast))))

(defn collect-closed-overs
  "Takes an AST and an opts map that takes the same options as collect,
   but only collects closed-overs on the AST."
  [ast opts]
  (if ((:what opts) :closed-overs)
    (binding [*collects* (atom (merge opts {:closed-overs {} :locals #{}}))]
      (let [ast (collect-closed-overs* ast)]
        (if (:top-level? opts)
          (assoc ast :closed-overs (:closed-overs @*collects*))
          ast)))
    ast))

(defn collect-fns [what]
  (case what
    :constants    -collect-const
    :callsites    -collect-callsite
    nil))

(defn collect
  "Takes a map with:
   * :what        set of keywords describing what to collect, some of:
     ** :constants     constant expressions
     ** :closed-overs  closed over local bindings
     ** :callsites     keyword and protocol callsites
   * :where       set of :op nodes where to attach collected info
   * :top-level?  if true attach collected info to the top-level node

   Returns a function that does the takes an AST and returns an AST with the
   collected info."
  [{:keys [what top-level?] :as opts}]
  (fn this [ast]
    (-> (binding [*collects* (atom (merge {:constants           {}
                                          :protocol-callsites #{}
                                          :keyword-callsites  #{}
                                          :where              #{}
                                          :what               #{}
                                          :next-id             0}
                                         opts))]
         (let [ast (-collect ast (apply comp (keep collect-fns what)))]
           (if top-level?
             (merge-collects ast)
             ast)))
      ;; closed-overs collecting must be done in a separate pass than constant/callsites
      ;; since it requires a different walking order
      (collect-closed-overs opts))))

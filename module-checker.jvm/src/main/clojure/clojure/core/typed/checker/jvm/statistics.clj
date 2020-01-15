;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.jvm.statistics
  (:require [clojure.core.typed.checker.var-env :as var-env]
            [clojure.core.typed.checker.jvm.check-ns-clj :as chk-ns-clj]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.var-env :as var-env]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]
            [clojure.core.typed.util-vars :as vs]
            [clojure.set :as set]))

; (ann all-defs-in-ns [Namespace -> (Set Symbol)])
(defn ^:no-wiki 
  all-defs-in-ns
  [ns]
  {:pre [(con/namespace? ns)]}
  (set
    (map #(symbol (str (ns-name ns)) (str %))
         (set/difference 
           (set (keys (ns-map ns))) 
           (set (keys (ns-refers ns))) 
           (set (keys (ns-imports ns)))))))

(defn statistics 
  "Takes a collection of namespace symbols and returns a map mapping the namespace
  symbols to a map of data"
  [nsyms]
  (assert (and (coll? nsyms) (every? symbol? nsyms))
          "Must pass a collection of symbols to statistics")
  (reduce (fn [stats nsym]
            (let [_ (chk-ns-clj/check-ns nsym :collect-only true)
                  ns (find-ns nsym)
                  _ (assert ns (str "Namespace " nsym " not found"))]
              (conj stats
                    [nsym
                     {:vars {:all-vars (all-defs-in-ns ns)
                             :no-checks (let [all-no-checks (var-env/clj-nocheck-var?)]
                                          (filter (fn [s] (= (namespace s) nsym)) all-no-checks))
                             :var-annotations (let [annots (var-env/clj-var-annotations)]
                                                (->> annots
                                                     (filter (fn [[k v]] (= (namespace k) (str nsym))))
                                                     (map (fn [[k v]] [k (binding [vs/*verbose-types* true]
                                                                           (prs/unparse-type v))]))
                                                     (into {})))}}])))
          {} nsyms))

(defn var-coverage
  ([nsyms-or-nsym]
   (assert (or (con/namespace? nsyms-or-nsym)
               (symbol? nsyms-or-nsym)
               (and (coll? nsyms-or-nsym) (every? symbol? nsyms-or-nsym)))
           "Must pass a collection of symbols or a symbol/namespace to var-coverage")
   (let [nsyms (if ((some-fn symbol? con/namespace?)
                    nsyms-or-nsym)
                 [(ns-name nsyms-or-nsym)]
                 nsyms-or-nsym)
         stats (statistics nsyms)
         nall-vars (->> (vals stats) 
                        (map :vars) 
                        (map :all-vars)
                        (apply set/union)
                        set
                        count)
         nannotated-vars (->> (vals stats) 
                              (map :vars) 
                              (map :var-annotations) 
                              (map count)
                              (apply +))
         perc (if (zero? nall-vars)
                0
                (long (* (/ nannotated-vars nall-vars) 100)))]
     (println (str "Found " nannotated-vars " annotated vars out of " nall-vars " vars"))
     (println (str perc "% var annotation coverage"))
     (flush))))

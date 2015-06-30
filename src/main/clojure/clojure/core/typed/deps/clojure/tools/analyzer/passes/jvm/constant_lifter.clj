(ns clojure.core.typed.deps.clojure.tools.analyzer.passes.jvm.constant-lifter
  (:require [clojure.core.typed.deps.clojure.tools.analyzer.passes.constant-lifter :as orig]
            [clojure.core.typed.deps.clojure.tools.analyzer :refer [-analyze]]
            [clojure.core.typed.deps.clojure.tools.analyzer.utils :refer [constant? classify]]))

(defn constant-lift*
  [ast]
  (if (= :var (:op ast))
    (let [{:keys [var env form]} ast]
     (if (constant? var)
       (let [val @var]
         (assoc (-analyze :const val env (classify val))
           :form form))
       ast))
    (orig/constant-lift ast)))

(defn constant-lift
  "Like clojure.core.typed.deps.clojure.tools.analyzer.passes.constant-lifter/constant-lift but
   transforms also :var nodes where the var has :const in the metadata
   into :const nodes and preserves tag info"
  [ast]
  (merge (constant-lift* ast)
         (select-keys ast [:tag :o-tag :return-tag :arglists])))

(ns clojure.core.typed.analyzer.jvm.gilardi-test
  (:require [clojure.test :refer :all]
            [clojure.core.typed.analyzer.env :as env]
            [clojure.core.typed.analyzer.jvm :as jana2]
            [clojure.core.typed.analyzer :as ana2]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as taj]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]

            ))

(declare check-expr)

(defmulti -check :op)
(defmethod -check :default [expr expected]
  (ast/update-children expr #(check-expr % nil)))

(def ^:dynamic *intermediate-forms* nil)
(def ^:dynamic *found-defns* nil)

(defn check-expr [expr expected]
  (let [expr (assoc-in expr [:env :ns] (ns-name *ns*))]
    (case (:op expr)
      :unanalyzed (let [{:keys [form]} expr
                        ;_ (prn "found form" form)
                        ;_ (prn "*ns*" (ns-name *ns*))
                        _ (when *intermediate-forms*
                            (swap! *intermediate-forms* conj form))
                        sym (when (and (seq? form)
                                       (seq form)
                                       (symbol? (first form))
                                       (not (contains? jana2/specials (first form))))
                              (-> (ana2/resolve-sym (first form) (:env expr))
                                  ana2/var->sym))]
                    (case sym
                      clojure.core/defn (do (some-> *found-defns*
                                                    (swap! update (second form) (fnil inc 0)))
                                            (recur (ana2/analyze-outer expr) expected))
                      clojure.core/ns (do ;(prn "found ns")
                                          (recur (ana2/analyze-outer expr) expected))
                      (recur (ana2/analyze-outer expr) expected)))
      (-> expr
          ana2/run-pre-passes
          (-check expected)
          ana2/run-post-passes
          ana2/eval-top-level))))

(defn check-top-level
  ([form expected] (check-top-level form expected {}))
  ([form expected {:keys [env] :as opts}]
   (let [env (or env (taj/empty-env))]
     (with-bindings (jana2/default-thread-bindings env)
       (env/ensure (jana2/global-env)
         (-> form
             (ana2/unanalyzed-top-level env)
             (check-expr expected)))))))

(defn check-top-level-fresh-ns [& args]
  (binding [*ns* (create-ns (gensym 'test-ns))]
    (refer-clojure)
    (apply check-top-level args)))

(defn chk [& args]
  (apply check-top-level-fresh-ns args))

(deftest gildardi-test
  (is (= 1 (:result (chk 1 nil))))
  (is (= 2 (:result
             (chk `(do (ns ~(gensym 'foo))
                       (require '~'[clojure.core :as core])
                       ;(prn (ns-aliases *ns*))
                       ;(println "foo ADSF")
                       ;(prn (ns-name *ns*) (ns-aliases *ns*))
                       (~'core/inc 1))
                  nil))))
  (is (= 2 (:result
             (chk `(do (ns ~(gensym 'foo)
                         ~'(:require [clojure.core :as core]))
                       ;(println "foo ADSF")
                       (~'core/inc 1))
                  nil))))
  (is (= 'hello
         (:result
           (chk '(do (defmacro blah []
                       `'~'hello)
                     (blah))
                nil))))
  (binding [*intermediate-forms* (atom #{})]
    (is (= '42
           (:result
             (chk '(do (defmacro stage1 []
                         '(stage2))
                       (defmacro stage2 []
                         42)
                       (stage1))
                  nil))))
    (is (contains? @*intermediate-forms* '(stage1)))
    (is (contains? @*intermediate-forms* '(stage2))))
  (binding [*found-defns* (atom {})]
    (is (= '12
           (:result
             (chk '(do (defn ttest []
                         12)
                       (ttest))
                  nil))))
    (is (= {'ttest 1} @*found-defns*))))

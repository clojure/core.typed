(ns clojure.core.typed.analyzer2.passes.jvm.fix-case-test
  (:require [clojure.core.typed.analyzer2.passes.add-binding-atom :as add-binding-atom]
            [clojure.tools.analyzer.passes.jvm.fix-case-test :as fix-case-test]))

;;redefine passes mainly to move dependency on `uniquify-locals`
;; to `uniquify2/uniquify-locals`
(defn fix-case-test
  "If the node is a :case-test, annotates in the atom shared
  by the binding and the local node with :case-test"
  {:pass-info {:walk :pre :depends #{;;replace
                                     #'add-binding-atom/add-binding-atom}}}
  [& args]
  (apply fix-case-test/fix-case-test args))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.analyzer.jvm.passes.fix-case-test
  (:require [clojure.core.typed.analyzer.common.passes.add-binding-atom :as add-binding-atom]
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

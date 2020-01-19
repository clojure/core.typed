;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.analyzer.common.passes.add-binding-atom
  (:require [clojure.tools.analyzer.passes.add-binding-atom :as add-binding-atom]
            [clojure.core.typed.analyzer.common.passes.uniquify :as uniquify2]))

;;redefine passes mainly to move dependency on `uniquify-locals`
;; to `uniquify2/uniquify-locals`
(defn add-binding-atom
  "Adds an atom-backed-map to every local binding, the same
   atom will be shared between all occurences of that local.

   The atom is put in the :atom field of the node."
  {:pass-info {:walk :pre :depends #{#'uniquify2/uniquify-locals}
               :state (fn [] (atom {}))}}
  [state ast]
  (add-binding-atom/add-binding-atom state ast))

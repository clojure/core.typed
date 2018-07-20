(ns clojure.core.typed.analyzer2.passes.add-binding-atom
  (:require [clojure.tools.analyzer.passes.add-binding-atom :as add-binding-atom]
            [clojure.core.typed.analyzer2.passes.uniquify :as uniquify2]))

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


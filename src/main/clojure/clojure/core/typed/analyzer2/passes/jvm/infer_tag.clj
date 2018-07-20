(ns clojure.core.typed.analyzer2.passes.jvm.infer-tag
  (:require [clojure.tools.analyzer.passes.jvm.infer-tag :as infer-tag]
            [clojure.tools.analyzer.passes.jvm.annotate-tag :as annotate-tag]
            [clojure.tools.analyzer.passes.jvm.annotate-host-info :as annotate-host-info]
            [clojure.tools.analyzer.passes.jvm.analyze-host-expr :as analyze-host-expr]
            [clojure.core.typed.analyzer2.passes.jvm.fix-case-test :as fix-case-test]))

;;redefine passes mainly to move dependency on `uniquify-locals`
;; to `uniquify2/uniquify-locals`
(defn infer-tag
  "Performs local type inference on the AST adds, when possible,
   one or more of the following keys to the AST:
   * :o-tag      represents the current type of the
                 expression represented by the node
   * :tag        represents the type the expression represented by the
                 node is required to have, possibly the same as :o-tag
   * :return-tag implies that the node will return a function whose
                 invocation will result in a object of this type
   * :arglists   implies that the node will return a function with
                 this arglists
   * :ignore-tag true when the node is untyped, does not imply that
                 all untyped node will have this

  Passes opts:
  * :infer-tag/level  If :global, infer-tag will perform Var tag
                      inference"
  {:pass-info {:walk :post :depends #{#'annotate-tag/annotate-tag 
                                      #'annotate-host-info/annotate-host-info 
                                      ;;replace
                                      #'fix-case-test/fix-case-test 
                                      #'analyze-host-expr/analyze-host-expr} 
               ; trim is incompatible with core.typed
               #_#_:after #{#'trim}}}
  [& args]
  (apply infer-tag/infer-tag args))

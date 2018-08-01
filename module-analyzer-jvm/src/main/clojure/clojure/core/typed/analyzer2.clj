;; adapted from tools.analyzer
(ns clojure.core.typed.analyzer2
  (:require [clojure.tools.analyzer.ast :as ast])
  (:refer-clojure :exclude [macroexpand-1 var?]))

(def ^{:dynamic  true
       :arglists '([form env])
       :doc      "If form represents a macro form, returns its expansion,
                  else returns form."}
  macroexpand-1)

(def ^{:dynamic  true
       :arglists '([[op & args] env])
       :doc      "Multimethod that dispatches on op, should default to -parse"}
  parse)

(def ^{:dynamic  true
       :arglists '([sym env])
       :doc      "Creates a var for sym and returns it"}
  create-var)

(def ^{:dynamic  true
       :arglists '([obj])
       :doc      "Returns true if obj represent a var form as returned by create-var"}
  var?)

(def ^{:dynamic  true
       :doc      "A map of functions such that

                 (ast/walk ast (:pre scheduled-passes) (:post scheduled-passes))

                 runs the passes currently scheduled.
                 "}
  scheduled-passes)

(defn run-passes
  "Function that will be invoked on the AST tree immediately after it has been constructed,
   by default runs the passes declared in #'default-passes, should be rebound if a different
   set of passes is required (via analyze2/run-passes).

   Use #'clojure.tools.analyzer.passes/schedule to get a function from a set of passes that
   run-passes can be bound to."
  [ast]
  {:pre [(map? scheduled-passes)]}
  (ast/walk ast
            (:pre scheduled-passes)
            (:post scheduled-passes)))

(def specials
  '#{do if new quote set! try var
     catch throw finally def .
     let* letfn* loop* recur fn*})

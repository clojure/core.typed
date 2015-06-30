(ns clojure.core.typed.deps.clojure.jvm.tools.analyzer.examples.load-core
  (:import [clojure.lang Compiler RT DynamicClassLoader])
  (:require [clojure.core.typed.deps.clojure.jvm.tools.analyzer :as analyze]))

(comment

;; Reproducible problem, similar to the one we're having loading clojure.core

(defmacro deftesteval
  [name args expr]
  `(do
     (fn ~name ~args ~expr)  ;; not sure the minimum reproducible case, but having both these lines trigger it
     (fn ~name ~args ~expr)))

;; .. and evaling this first ..
(deftesteval myfn [x] `(+ ~x))

;; .. followed by analyzing the same form again, in this namespace ..
(analyze/analyze-one '{:ns {:name analyze.examples.load-core} :context :eval}
                     '(deftesteval myfn [x] `(+ ~x)))

;; results in ...

;CompilerException java.lang.LinkageError: loader (instance of  clojure/lang/DynamicClassLoader): attempted  duplicate class definition for name: "analyze/examples/load_core$myfn", compiling:(REPL:5)
;	clojure.lang.Compiler.analyzeSeq (Compiler.java:6416)
;	clojure.lang.Compiler.analyze (Compiler.java:6216)
;	clojure.lang.Compiler.analyzeSeq (Compiler.java:6397)
;	clojure.lang.Compiler.analyze (Compiler.java:6216)
;	clojure.lang.Compiler.analyze (Compiler.java:6177)
;	clojure.lang.Compiler$BodyExpr$Parser.parse (Compiler.java:5572)
;	clojure.lang.Compiler.analyzeSeq (Compiler.java:6409)
;	clojure.lang.Compiler.analyze (Compiler.java:6216)
;	clojure.lang.Compiler.analyzeSeq (Compiler.java:6397)
;	clojure.lang.Compiler.analyze (Compiler.java:6216)
;	clojure.lang.Compiler.analyze (Compiler.java:6177)
;	analyze.core/analyze*/invoke-analyze--770 (core.clj:565)
;Caused by:
;LinkageError loader (instance of  clojure/lang/DynamicClassLoader): attempted  duplicate class definition for name: "analyze/examples/load_core$myfn"
;	java.lang.ClassLoader.defineClass1 (ClassLoader.java:-2)
;	java.lang.ClassLoader.defineClassCond (ClassLoader.java:631)
;	java.lang.ClassLoader.defineClass (ClassLoader.java:615)
;	java.lang.ClassLoader.defineClass (ClassLoader.java:465)
;	clojure.lang.DynamicClassLoader.defineClass (DynamicClassLoader.java:46)
;	clojure.lang.Compiler$ObjExpr.getCompiledClass (Compiler.java:4533)
;	clojure.lang.Compiler$FnExpr.parse (Compiler.java:3697)
;	clojure.lang.Compiler.analyzeSeq (Compiler.java:6407)
;	clojure.lang.Compiler.analyze (Compiler.java:6216)

;(def a (analyze/analyze-path "clojure/core.clj" 'clojure.core))
  )

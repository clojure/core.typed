(ns clojure.core.typed.test.multimethods
  (:refer-clojure :exclude [update cast])
  (:require 
    ; this loads the type system, must go first
    [clojure.core.typed.test.test-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed.checker.jvm.analyze-clj :as ana]
            ;[clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.repl :refer [pst]]
            [clojure.pprint :refer [pprint]]
            [clojure.data :refer [diff]]
            [clojure.core.typed.unsafe]
            [clojure.core.typed.checker.init]
            [clojure.core.typed.checker.utils :as u :refer [expr-type]]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.jvm.check :as chk]
            [clojure.core.typed.checker.check.funapp :as funapp]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.update :as update :refer [env+ update]]
            [clojure.core.typed.checker.jvm.tc-equiv :refer [tc-equiv]]
            [clojure.core.typed.checker.collect-utils :as collect-u]
            [clojure.core.typed.checker.inst :as inst]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed.checker.type-ctors :refer :all]
            [clojure.core.typed.checker.type-rep :refer :all]
            [clojure.core.typed.checker.filter-rep :refer :all]
            [clojure.core.typed.checker.filter-ops :refer :all]
            [clojure.core.typed.checker.object-rep :refer :all]
            [clojure.core.typed.checker.path-rep :refer :all]
            [clojure.core.typed.checker.jvm.parse-unparse :refer :all]
            [clojure.core.typed.checker.jvm.constant-type :refer [constant-type]]
            [clojure.core.typed.checker.lex-env :refer :all]
            [clojure.core.typed.checker.promote-demote :refer :all]
            [clojure.core.typed.checker.frees :refer :all]
            [clojure.core.typed.checker.free-ops :refer :all]
            [clojure.core.typed.checker.dvar-env :refer :all]
            [clojure.core.typed.checker.cs-gen :refer :all]
            [clojure.core.typed.checker.cs-rep :refer :all]
            [clojure.core.typed.checker.subst :refer [subst-all] :as subst]
            [clojure.core.typed.test.rbt]
            [clojure.core.typed.test.person]
            [clojure.core.typed.internal]
            [clojure.core.typed.checker.jvm.path-type :refer :all]
            [clojure.core.typed.load :as load]
            [clojure.core.typed.checker.ns-deps-utils :as ndu]
            [clojure.core.typed.parse-ast :as prs-ast])
; we want clojure.lang.Seqable to be scoped here. 
; The :refer :all of clojure.core.typed adds another Seqable which
; is less useful here.
  (:use [clojure.core.typed :as tc :exclude [Seqable loop fn defprotocol let dotimes
                                             for doseq def remove filter defn atom ref]])
  (:import (clojure.lang ISeq IPersistentVector Atom IPersistentMap
                         ExceptionInfo Var Seqable)))

(deftest multimethod-test
  (is (check-ns 'clojure.core.typed.test.mm))
  (is-tc-e (do (ann f [Any -> Any])
               (defmulti f class)
               (defmethod f Number [n] (inc n))))
  (is-tc-e (do (ann f [Any -> Any])
               (defmulti f (fn [a] (class a)))
               (defmethod f Number [n] (inc n))))
  (is-tc-e (do (ann f [Any Any -> Any])
               (defmulti f (fn [a b]
                             [(class a) (class b)]))
               (defmethod f [Number Number] [n1 n2] (+ n1 n2)))))

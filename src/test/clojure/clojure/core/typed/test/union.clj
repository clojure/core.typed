(ns clojure.core.typed.test.union
  (:import (java.io LineNumberReader InputStreamReader PushbackReader)
           (clojure.lang RT LineNumberingPushbackReader Compiler$DefExpr Compiler$LocalBinding Compiler$BindingInit Compiler$LetExpr
                         Compiler$LetFnExpr Compiler$StaticMethodExpr Compiler$InstanceMethodExpr Compiler$StaticFieldExpr
                         Compiler$NewExpr Compiler$EmptyExpr Compiler$VectorExpr Compiler$MonitorEnterExpr
                         Compiler$MonitorExitExpr Compiler$ThrowExpr Compiler$InvokeExpr Compiler$TheVarExpr Compiler$VarExpr
                         Compiler$UnresolvedVarExpr Compiler$ObjExpr Compiler$NewInstanceMethod Compiler$FnMethod Compiler$FnExpr
                         Compiler$NewInstanceExpr Compiler$MetaExpr Compiler$BodyExpr Compiler$ImportExpr Compiler$AssignExpr
                         Compiler$TryExpr$CatchClause Compiler$TryExpr Compiler$C Compiler$LocalBindingExpr Compiler$RecurExpr
                         Compiler$MapExpr Compiler$IfExpr Compiler$KeywordInvokeExpr Compiler$InstanceFieldExpr Compiler$InstanceOfExpr
                         Compiler$CaseExpr Compiler$Expr Compiler$SetExpr Compiler$MethodParamExpr Compiler$KeywordExpr
                         Compiler$ConstantExpr Compiler$NumberExpr Compiler$NilExpr Compiler$BooleanExpr Compiler$StringExpr
                         Compiler$ObjMethod Compiler$Expr
                         Keyword Seqable Var Symbol IPersistentSet))
  (:require [clojure.core.typed :refer [def-alias declare-names check-ns AnyInteger ann
                                        warn-on-unannotated-vars]]
            (clojure.core.typed
              [parse-unparse :refer [parse-type]]
              [type-ctors :refer [Un]]
              [utils :refer [profile]])))

(warn-on-unannotated-vars)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(def-alias Env
  "An environment"
  (HMap :optional {:line AnyInteger,
                   :column AnyInteger,
                   :source Any}))

(def-alias Children 
  "A children API exists, but not checked yet"
  Any)

(def-alias Expr
  "An AST node"
  (Rec [Expr]
  (U ;; values
     '{:op ':keyword
       :env Env
       :val Keyword}
     '{:op ':constant
       :env Env
       :val Any}
     '{:op ':number
       :env Env
       :val Number}
     '{:op ':string
       :env Env
       :val String}
     '{:op ':nil
       :env Env
       :val nil}
     '{:op ':boolean
       :env Env
       :val Boolean}
     
     ;; def
     (HMap :mandatory
           {:op ':def
            :env Env
            :var Var
            :meta (U nil Expr)
            :init Expr
            :init-provided Any
            :is-dynamic Any}
           :optional
           {:children Children
            :Expr-obj Compiler$DefExpr})

     ;; local binding
     (HMap :mandatory
           {:op ':local-binding
            :env Env
            :sym Symbol
            :tag (U nil Symbol)
            :init Expr}
           :optional
           {:children Children
             :LocalBinding-obj Compiler$LocalBinding})

     ;binding init
     (HMap :mandatory
           {:op ':binding-init
            :env Env
            :local-binding Expr
            :init Expr}
           :optional
           {:children Children
            :BindingInit-obj Compiler$BindingInit})

     ; let
     (HMap :mandatory
           {:op ':let
            :env Env
            :binding-inits (Seqable Expr)
            :body Expr
            :is-loop Any}
           :optional
           {:children Children
             :Expr-obj Compiler$LetExpr})

     ;letfn
     (HMap :mandatory
           {:op ':letfn
            :env Env
            :body Expr
            :binding-inits (Seqable Expr)}
           :optional
           {:children Children
             :Expr-obj Compiler$LetFnExpr})

     ;local binding expr
     (HMap :mandatory
           {:op ':local-binding-expr
            :env Env
            :local-binding Expr
            :tag (U nil Symbol)}
           :optional
           {:children Children
             :Expr-obj Compiler$LocalBindingExpr})

     ;static method
     (HMap :mandatory
           {:op ':static-method
            :env Env
            :class Class
            :method-name Symbol
            ; probably need to type clojure.reflect
            :method Nothing
            :args (Seqable Expr)
            :tag (U nil Symbol)}
           :optional
           {:children Children
             :Expr-obj Compiler$StaticMethodExpr})

     ;instance method
     (HMap :mandatory
           {:op ':instance-method
            :env Env
            :target Expr
            :method-name Symbol
            :method Nothing
            :args (Seqable Expr)
            :tag (U nil Symbol)}
           :optional
           {:children Children
             :Expr-obj Compiler$InstanceMethodExpr})

    ;static field
    (HMap :mandatory
          {:op ':static-field
           :env Env
           :class Class
           :field-name Symbol
           :field Nothing
           :tag (U nil Symbol)}
          :optional
          {:Expr-obj Compiler$StaticFieldExpr})

    ;instance field
    (HMap :mandatory
          {:op ':instance-field
           :env Env
           :target Expr
           :target-class Class
           :field Nothing
           :field-name Symbol
           :tag (U nil Symbol)}
          :optional
          {:children Children
           :Expr-obj Compiler$InstanceFieldExpr})

    ;new
    (HMap :mandatory
          {:op ':new
           :env Env
           :ctor Nothing
           :class Class
           :args (Seqable Expr)}
          :optional
          {:children Children
           :Expr-obj Compiler$NewExpr})

    ;empty
    (HMap :mandatory
          {:op ':empty-expr
           :env Env
           :coll (I (Seqable Nothing) (ExactCount 0))}
          :optional
          {:Expr-obj Compiler$EmptyExpr})

    ;set
    (HMap :mandatory
          {:op ':set
           :env Env
           :keys (Seqable Expr)}
          :optional
          {:children Children
           :Expr-obj Compiler$SetExpr})

    ;vector
    (HMap :mandatory
          {:op ':vector
           :env Env
           :args (Seqable Expr)}
          :optional
          {:children Children
           :Expr-obj Compiler$VectorExpr})

    ;map
    (HMap :mandatory
          {:op ':map
           :env Env
           :keyvals (Seqable Expr)}
          :optional
          {:children Children
           :Expr-obj Compiler$MapExpr})

    ;monitor enter
    (HMap :mandatory
          {:op ':monitor-enter
           :env Env
           :target Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$MonitorEnterExpr})

    ;monitor exit
    (HMap :mandatory
          {:op ':monitor-exit
           :env Env
           :target Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$MonitorExitExpr})

    ;throw
    (HMap :mandatory
          {:op ':throw
           :env Env
           :exception Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$ThrowExpr})

    ;invoke
    (HMap :mandatory
          {:op ':invoke
           :env Env
           :fexpr Expr
           :tag (U nil Symbol)
           :args (Seqable Expr)
           :is-protocol Any
           :is-direct Any
           :site-index Any
           :protocol-on Any}
          :optional
          {:method Nothing
           :children Children
           :Expr-obj Compiler$InvokeExpr})

    ;keyword invoke
    (HMap :mandatory
          {:op ':keyword-invoke
           :env Env
           :kw Expr
           :tag (U nil Symbol)
           :target Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$KeywordInvokeExpr})

    ;the var
    (HMap :mandatory
          {:op ':the-var
           :env Env
           :var Var}
          :optional
          {:Expr-obj Compiler$TheVarExpr})

    ;var
    (HMap :mandatory
          {:op ':var
           :env Env
           :var Var
           :tag (U nil Symbol)}
          :optional
          {:Expr-obj Compiler$VarExpr})

    ;unresolved var
    (HMap :mandatory
          {:op ':unresolved-var
           :env Env
           :sym Symbol}
          :optional
          {:Expr-obj Compiler$UnresolvedVarExpr})

    ;objexpr
    (HMap :mandatory
          {:op ':obj-expr
           :env Env
           :tag (U nil Symbol)}
          :optional
          {:Expr-obj Compiler$ObjExpr})

    ;new instance method
    (HMap :mandatory
          {:op ':new-instance-method
           :env Env
           :name Symbol
           :required-params (Seqable Expr)
           :body Expr}
          :optional
          {:children Children
           :ObjMethod-obj Compiler$NewInstanceMethod})

    ;fn method
    (HMap :mandatory
          {:op ':fn-method
           :env Env
           :body Expr
           :required-params (Seqable Expr)
           :rest-param (U nil Expr)}
          :optional
          {:children Children
           :ObjMethod-obj Compiler$FnMethod})

    ;fn expr
    (HMap :mandatory
          {:op ':fn-expr
           :env Env
           :methods (Seqable Expr)
           :variadic-method (U nil Expr)
           :tag (U nil Symbol)}
          :optional
          {:name Symbol
           :children Children
           :Expr-obj Compiler$FnExpr})

    ;new instance expr
    (HMap :mandatory
          {:op ':deftype*
           :name Symbol
           :env Env
           :methods (Seqable Expr)
           :mmap Any
           :compiled-class Class
           :internal-name Symbol
           :this-name Symbol
           :fields (IPersistentSet Expr)
           :covariants Any
           :tag (U nil Symbol)}
          :optional
          {:children Children
           :Expr-obj Compiler$NewInstanceExpr})

    ;instanceof
    (HMap :mandatory
          {:op ':instance-of
           :env Env
           :class Class
           :the-expr Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$InstanceOfExpr})

    ;meta
    (HMap :mandatory
          {:op ':meta
           :env Env
           :meta Expr
           :expr Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$MetaExpr})

    ;do
    (HMap :mandatory
          {:op ':do
           :env Env
           :exprs (Seqable Expr)}
          :optional
          {:children Children
           :Expr-obj Compiler$BodyExpr})

    ;if
    (HMap :mandatory
          {:op ':if
           :env Env
           :test Expr
           :then Expr
           :else Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$IfExpr})

    ;case
    (HMap :mandatory
          {:op ':case*
           :env Env
           :the-expr Expr
           :tests (Seqable Expr)
           :thens (Seqable Expr)
           :default Expr
           :tests-hashes Any
           :shift Any
           :mask Any
           :test-type Any
           :switch-type Any
           :skip-check Any}
          :optional
          {:children Children
           :Expr-obj Compiler$CaseExpr})

    ;import
    (HMap :mandatory
          {:op ':import*
           :env Env
           :class-str String}
          :optional
          {:Expr-obj Compiler$ImportExpr})

    ;set!
    (HMap :mandatory
          {:op ':set!
           :env Env
           :target Expr
           :val Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$AssignExpr})

    ;catch
    (HMap :mandatory
          {:op ':catch
           :env Env
           :class Class
           :local-binding Expr
           :handler Expr}
          :optional
          {:children Children
           :CatchClause-obj Compiler$TryExpr$CatchClause})

    ;try
    (HMap :mandatory
          {:op ':try
           :env Env
           :try-expr Expr
           :finally-expr (U nil Expr)
           :catch-exprs (Seqable Expr)
           :ret-local Any
           :finally-local Any}
          :optional
          {:children Children
           :Expr-obj Compiler$TryExpr})

    ;recur
    (HMap :mandatory
          {:op ':recur
           :env Env
           :loop-locals (Seqable Expr)
           :args (Seqable Expr)}
          :optional
          {:children Children
           :Expr-Obj Compiler$RecurExpr})

    ;method param
    (HMap :mandatory
          {:op ':method-param
           :env Env
           :class Class
           :can-emit-primitive Any}
          :optional
          {:Expr-obj Compiler$MethodParamExpr}))))

(def-alias AnalyzeOpt
  "Options for analysis"
  '{})

;(ann test1 [Expr -> Any])
;(defn test1 [a]
;  (:op a))

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
                         Var))
  (:require [clojure.core.typed :as t]))

(t/warn-on-unannotated-vars)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(t/defalias Env
  "An environment"
  (t/HMap :optional {:line t/Int,
                     :column t/Int,
                     :source t/Any}))

(t/defalias Children 
  "A children API exists, but not checked yet"
  t/Any)

(t/defalias Expr
  "An AST node"
  (t/Rec [Expr]
  (t/U ;; values
     '{:op ':keyword
       :env Env
       :val t/Kw}
     '{:op ':constant
       :env Env
       :val t/Any}
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
     (t/HMap :mandatory
           {:op ':def
            :env Env
            :var (t/Var2 t/Nothing t/Any)
            :meta (t/U nil Expr)
            :init Expr
            :init-provided t/Any
            :is-dynamic t/Any}
           :optional
           {:children Children
            :Expr-obj Compiler$DefExpr})

     ;; local binding
     (t/HMap :mandatory
           {:op ':local-binding
            :env Env
            :sym t/Sym
            :tag (t/U nil t/Sym)
            :init Expr}
           :optional
           {:children Children
             :LocalBinding-obj Compiler$LocalBinding})

     ;binding init
     (t/HMap :mandatory
           {:op ':binding-init
            :env Env
            :local-binding Expr
            :init Expr}
           :optional
           {:children Children
            :BindingInit-obj Compiler$BindingInit})

     ; let
     (t/HMap :mandatory
           {:op ':let
            :env Env
            :binding-inits (t/Seqable Expr)
            :body Expr
            :is-loop t/Any}
           :optional
           {:children Children
             :Expr-obj Compiler$LetExpr})

     ;letfn
     (t/HMap :mandatory
           {:op ':letfn
            :env Env
            :body Expr
            :binding-inits (t/Seqable Expr)}
           :optional
           {:children Children
             :Expr-obj Compiler$LetFnExpr})

     ;local binding expr
     (t/HMap :mandatory
           {:op ':local-binding-expr
            :env Env
            :local-binding Expr
            :tag (t/U nil t/Sym)}
           :optional
           {:children Children
             :Expr-obj Compiler$LocalBindingExpr})

     ;static method
     (t/HMap :mandatory
           {:op ':static-method
            :env Env
            :class Class
            :method-name t/Sym
            ; probably need to type clojure.reflect
            :method t/Nothing
            :args (t/Seqable Expr)
            :tag (t/U nil t/Sym)}
           :optional
           {:children Children
             :Expr-obj Compiler$StaticMethodExpr})

     ;instance method
     (t/HMap :mandatory
           {:op ':instance-method
            :env Env
            :target Expr
            :method-name t/Sym
            :method t/Nothing
            :args (t/Seqable Expr)
            :tag (t/U nil t/Sym)}
           :optional
           {:children Children
             :Expr-obj Compiler$InstanceMethodExpr})

    ;static field
    (t/HMap :mandatory
          {:op ':static-field
           :env Env
           :class Class
           :field-name t/Sym
           :field t/Nothing
           :tag (t/U nil t/Sym)}
          :optional
          {:Expr-obj Compiler$StaticFieldExpr})

    ;instance field
    (t/HMap :mandatory
          {:op ':instance-field
           :env Env
           :target Expr
           :target-class Class
           :field t/Nothing
           :field-name t/Sym
           :tag (t/U nil t/Sym)}
          :optional
          {:children Children
           :Expr-obj Compiler$InstanceFieldExpr})

    ;new
    (t/HMap :mandatory
          {:op ':new
           :env Env
           :ctor t/Nothing
           :class Class
           :args (t/Seqable Expr)}
          :optional
          {:children Children
           :Expr-obj Compiler$NewExpr})

    ;empty
    (t/HMap :mandatory
          {:op ':empty-expr
           :env Env
           :coll (t/I (t/Seqable t/Nothing) (t/ExactCount 0))}
          :optional
          {:Expr-obj Compiler$EmptyExpr})

    ;set
    (t/HMap :mandatory
          {:op ':set
           :env Env
           :keys (t/Seqable Expr)}
          :optional
          {:children Children
           :Expr-obj Compiler$SetExpr})

    ;vector
    (t/HMap :mandatory
          {:op ':vector
           :env Env
           :args (t/Seqable Expr)}
          :optional
          {:children Children
           :Expr-obj Compiler$VectorExpr})

    ;map
    (t/HMap :mandatory
          {:op ':map
           :env Env
           :keyvals (t/Seqable Expr)}
          :optional
          {:children Children
           :Expr-obj Compiler$MapExpr})

    ;monitor enter
    (t/HMap :mandatory
          {:op ':monitor-enter
           :env Env
           :target Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$MonitorEnterExpr})

    ;monitor exit
    (t/HMap :mandatory
          {:op ':monitor-exit
           :env Env
           :target Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$MonitorExitExpr})

    ;throw
    (t/HMap :mandatory
          {:op ':throw
           :env Env
           :exception Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$ThrowExpr})

    ;invoke
    (t/HMap :mandatory
          {:op ':invoke
           :env Env
           :fexpr Expr
           :tag (t/U nil t/Sym)
           :args (t/Seqable Expr)
           :is-protocol t/Any
           :is-direct t/Any
           :site-index t/Any
           :protocol-on t/Any}
          :optional
          {:method t/Nothing
           :children Children
           :Expr-obj Compiler$InvokeExpr})

    ;keyword invoke
    (t/HMap :mandatory
          {:op ':keyword-invoke
           :env Env
           :kw Expr
           :tag (t/U nil t/Sym)
           :target Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$KeywordInvokeExpr})

    ;the var
    (t/HMap :mandatory
          {:op ':the-var
           :env Env
           :var Var}
          :optional
          {:Expr-obj Compiler$TheVarExpr})

    ;var
    (t/HMap :mandatory
          {:op ':var
           :env Env
           :var Var
           :tag (t/U nil t/Sym)}
          :optional
          {:Expr-obj Compiler$VarExpr})

    ;unresolved var
    (t/HMap :mandatory
          {:op ':unresolved-var
           :env Env
           :sym t/Sym}
          :optional
          {:Expr-obj Compiler$UnresolvedVarExpr})

    ;objexpr
    (t/HMap :mandatory
          {:op ':obj-expr
           :env Env
           :tag (t/U nil t/Sym)}
          :optional
          {:Expr-obj Compiler$ObjExpr})

    ;new instance method
    (t/HMap :mandatory
          {:op ':new-instance-method
           :env Env
           :name t/Sym
           :required-params (t/Seqable Expr)
           :body Expr}
          :optional
          {:children Children
           :ObjMethod-obj Compiler$NewInstanceMethod})

    ;fn method
    (t/HMap :mandatory
          {:op ':fn-method
           :env Env
           :body Expr
           :required-params (t/Seqable Expr)
           :rest-param (t/U nil Expr)}
          :optional
          {:children Children
           :ObjMethod-obj Compiler$FnMethod})

    ;fn expr
    (t/HMap :mandatory
          {:op ':fn-expr
           :env Env
           :methods (t/Seqable Expr)
           :variadic-method (t/U nil Expr)
           :tag (t/U nil t/Sym)}
          :optional
          {:name t/Sym
           :children Children
           :Expr-obj Compiler$FnExpr})

    ;new instance expr
    (t/HMap :mandatory
          {:op ':deftype*
           :name t/Sym
           :env Env
           :methods (t/Seqable Expr)
           :mmap t/Any
           :compiled-class Class
           :internal-name t/Sym
           :this-name t/Sym
           :fields (t/Set Expr)
           :covariants t/Any
           :tag (t/U nil t/Sym)}
          :optional
          {:children Children
           :Expr-obj Compiler$NewInstanceExpr})

    ;instanceof
    (t/HMap :mandatory
          {:op ':instance-of
           :env Env
           :class Class
           :the-expr Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$InstanceOfExpr})

    ;meta
    (t/HMap :mandatory
          {:op ':meta
           :env Env
           :meta Expr
           :expr Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$MetaExpr})

    ;do
    (t/HMap :mandatory
          {:op ':do
           :env Env
           :exprs (t/Seqable Expr)}
          :optional
          {:children Children
           :Expr-obj Compiler$BodyExpr})

    ;if
    (t/HMap :mandatory
          {:op ':if
           :env Env
           :test Expr
           :then Expr
           :else Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$IfExpr})

    ;case
    (t/HMap :mandatory
          {:op ':case*
           :env Env
           :the-expr Expr
           :tests (t/Seqable Expr)
           :thens (t/Seqable Expr)
           :default Expr
           :tests-hashes t/Any
           :shift t/Any
           :mask t/Any
           :test-type t/Any
           :switch-type t/Any
           :skip-check t/Any}
          :optional
          {:children Children
           :Expr-obj Compiler$CaseExpr})

    ;import
    (t/HMap :mandatory
          {:op ':import*
           :env Env
           :class-str String}
          :optional
          {:Expr-obj Compiler$ImportExpr})

    ;set!
    (t/HMap :mandatory
          {:op ':set!
           :env Env
           :target Expr
           :val Expr}
          :optional
          {:children Children
           :Expr-obj Compiler$AssignExpr})

    ;catch
    (t/HMap :mandatory
          {:op ':catch
           :env Env
           :class Class
           :local-binding Expr
           :handler Expr}
          :optional
          {:children Children
           :CatchClause-obj Compiler$TryExpr$CatchClause})

    ;try
    (t/HMap :mandatory
          {:op ':try
           :env Env
           :try-expr Expr
           :finally-expr (t/U nil Expr)
           :catch-exprs (t/Seqable Expr)
           :ret-local t/Any
           :finally-local t/Any}
          :optional
          {:children Children
           :Expr-obj Compiler$TryExpr})

    ;recur
    (t/HMap :mandatory
          {:op ':recur
           :env Env
           :loop-locals (t/Seqable Expr)
           :args (t/Seqable Expr)}
          :optional
          {:children Children
           :Expr-Obj Compiler$RecurExpr})

    ;method param
    (t/HMap :mandatory
          {:op ':method-param
           :env Env
           :class Class
           :can-emit-primitive t/Any}
          :optional
          {:Expr-obj Compiler$MethodParamExpr}))))

(t/defalias AnalyzeOpt
  "Options for analysis"
  '{})

(t/ann test1 [Expr -> t/Any])
(defn test1 [a]
  (:op a))

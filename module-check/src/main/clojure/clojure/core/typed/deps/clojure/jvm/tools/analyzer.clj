(ns clojure.core.typed.deps.clojure.jvm.tools.analyzer
  "Interface to Compiler's analyze.
  Entry point `analyze-path` and `analyze-one`"
  (:refer-clojure :exclude [macroexpand])
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
                         Compiler$ObjMethod Compiler$Expr))
  (:require [clojure.reflect :as reflect]
            [clojure.java.io :as io]
            [clojure.repl :as repl]
            [clojure.string :as string]
            [clojure.core.typed.deps.clojure.jvm.tools.analyzer.util :as util]
            [clojure.core.typed.deps.clojure.jvm.tools.analyzer.emit-form :as emit-form]))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(def ^:dynamic *eval-ast* 
  "If true, evaluate the output AST before returning.
  Otherwise, AST is unevaluated. Defaults to true."
  true)

(declare analyze-one)

(defn analyze-form-in-ns 
  ([nsym form] (analyze-form-in-ns nsym form {}))
  ([nsym form opt]
   (analyze-one {:ns {:name nsym} :context :eval}
                form opt)))

(defn analyze-form 
  ([form] (analyze-form form {}))
  ([form opt] (analyze-form-in-ns (ns-name *ns*) form opt)))

(defmacro ast-in-ns
  "Returns the abstract syntax tree representation of the given form,
  evaluated in the given namespace"
  ([nsym form] `(ast-in-ns ~nsym ~form {}))
  ([nsym form opt]
   `(analyze-form-in-ns '~nsym '~form ~opt)))

(defmacro ast 
  "Returns the abstract syntax tree representation of the given form,
  evaluated in the current namespace"
  ([form] `(ast ~form {}))
  ([form opt]
   `(analyze-form '~form ~opt)))

(defn macroexpand 
  "Fully macroexpand a form."
  [f]
  (-> f
      analyze-form
      emit-form/emit-form))

;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defmacro field 
  "Call a private field, must be known at compile time. Throws an error
  if field is already publicly accessible."
  ([class-obj field] `(field ~class-obj ~field nil))
  ([class-obj field obj]
   (let [{class-flags :flags :keys [members]} (reflect/reflect (resolve class-obj))
         {field-flags :flags} (some #(and (= (:name %) field) %) members)]
     (assert field-flags
             (str "Class " (resolve class-obj) " does not have field " field))
     (assert (not (and (:public class-flags)
                       (:public field-flags)))
             (str "Class " (resolve class-obj) " and field " field " is already public")))
   `(field-accessor ~class-obj '~field ~obj)))

(defn- field-accessor [^Class class-obj field obj]
  (let [^java.lang.reflect.Field 
        field (.getDeclaredField class-obj (name field))]
    (.setAccessible field true)
    (let [ret (.get field obj)]
      (if (instance? Boolean ret)
        (boolean ret)
        ret))))

(defn- method-accessor [^Class class-obj method obj types & args]
  (let [^java.lang.reflect.Method 
        method (.getMethod class-obj (name method) (into-array Class types))]
    (.setAccessible method true)
    (try 
      (.invoke method obj (object-array args))
      (catch java.lang.reflect.InvocationTargetException e
        (throw (repl/root-cause e))))))

(defn- when-column-map [expr]
  (let [field (try (.getDeclaredField (class expr) "column")
                (catch Exception e))]
    (when field
      {:column (field-accessor (class expr) 'column expr)})))

(defn- when-line-map [expr]
  (let [^java.lang.reflect.Method
        method (try (.getMethod (class expr) "line" (into-array Class []))
                 (catch Exception e))
        field (try (.getDeclaredField (class expr) "line")
                (catch Exception e))]
    (cond 
      method {:line (method-accessor (class expr) 'line expr [])}
      field {:line (field-accessor (class expr) 'line expr)})))

(defn- when-source-map [expr]
  (let [field (try (.getDeclaredField (class expr) "source")
                (catch Exception e))]
    (when field
      {:source (field-accessor (class expr) 'source expr)})))

(defn- env-location [env expr]
  (merge env
         (when-line-map expr)
         (when-column-map expr)
         (when-source-map expr)))

(defn- inherit-env [expr env]
  (merge env
         (when-let [line (-> expr :env :line)]
           {:line line})
         (when-let [column (-> expr :env :column)]
           {:column column})
         (when-let [source (-> expr :env :source)]
           {:source source})))

(defprotocol AnalysisToMap
  (analysis->map [aobj env opt]
    "Recursively converts the output of the Compiler's analysis to a map. Takes
    a map of options:
    - :children
      when true, include a :children key with all child expressions of each node
    - :java-obj
      when true, include a :java-obj key with the node's corresponding Java object"))

;; Literals extending abstract class Compiler$LiteralExpr and have public value fields

(defmacro literal-dispatch [disp-class op-keyword]
  `(extend-protocol AnalysisToMap
     ~disp-class
     (~'analysis->map
       [expr# env# opt#]
       (let []
         (merge
           {:op ~op-keyword
            :env env#
            :val (.eval expr#)}
           (when (:java-obj opt#)
             {:Expr-obj expr#}))))))

(literal-dispatch Compiler$KeywordExpr :keyword)
(literal-dispatch Compiler$ConstantExpr :constant)
(literal-dispatch Compiler$NumberExpr :number)
(literal-dispatch Compiler$StringExpr :string)
(literal-dispatch Compiler$NilExpr :nil)
(literal-dispatch Compiler$BooleanExpr :boolean)

(extend-protocol AnalysisToMap

  ;; def
  Compiler$DefExpr
  (analysis->map
    [expr env opt]
    (let [init (analysis->map (field Compiler$DefExpr init expr) env opt)
          meta (when-let [meta (field Compiler$DefExpr meta expr)]
                 (analysis->map meta env opt))]
      (merge 
        {:op :def
         :env (env-location env expr)
         :var (field Compiler$DefExpr var expr)
         :meta meta
         :init init
         :init-provided (field Compiler$DefExpr initProvided expr)
         :is-dynamic (field Compiler$DefExpr isDynamic expr)}
        (when (:children opt)
          {:children [[[:meta] {}] 
                      [[:init] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; let
  Compiler$LocalBinding
  (analysis->map
    [lb env opt]
    (let [init (when-let [init (.init lb)]
                 (analysis->map init env opt))]
      (merge
        {:op :local-binding
         :env (inherit-env init env)
         :sym (.sym lb)
         :tag (.tag lb)
         :init init}
        (when (:children opt)
          {:children [[[:init] {}] ;optional
                      ]})
        (when (:java-obj opt)
          {:LocalBinding-obj lb}))))

  Compiler$BindingInit
  (analysis->map
    [bi env opt]
    (let [local-binding (analysis->map (.binding bi) env opt)
          init (analysis->map (.init bi) env opt)]
      (merge
        {:op :binding-init
         :env (inherit-env init env)
         :local-binding local-binding
         :init init}
        (when (:children opt)
          {:children [[[:local-binding] {}]
                      [[:init] {}]]})
        (when (:java-obj opt)
          {:BindingInit-obj bi}))))

  Compiler$LetExpr
  (analysis->map
    [expr env opt]
    (let [body (analysis->map (.body expr) env opt)
          binding-inits (map analysis->map (.bindingInits expr) (repeat env) (repeat opt))]
      (merge
        {:op :let
         :env (inherit-env body env)
         :binding-inits binding-inits
         :body body
         :is-loop (.isLoop expr)}
        (when (:children opt)
          {:children [[[:binding-inits] {:exprs? true}]
                      [[:body] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; letfn
  Compiler$LetFnExpr
  (analysis->map
    [expr env opt]
    (let [body (analysis->map (.body expr) env opt)
          binding-inits (map analysis->map (.bindingInits expr) (repeat env) (repeat opt))]
      (merge
        {:op :letfn
         :env (inherit-env body env)
         :body body
         :binding-inits binding-inits}
        (when (:children opt)
          {:children [[[:binding-inits] {:exprs? true}]
                      [[:body] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; LocalBindingExpr
  Compiler$LocalBindingExpr
  (analysis->map
    [expr env opt]
    (let [local-binding (analysis->map (.b expr) env opt)]
      (merge
        {:op :local-binding-expr
         :env (inherit-env local-binding env)
         :local-binding local-binding
         :tag (.tag expr)}
        (when (:children opt)
          {:children [[[:local-binding] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; Methods
  Compiler$StaticMethodExpr
  (analysis->map
    [expr env opt]
    (let [args (map analysis->map (field Compiler$StaticMethodExpr args expr) (repeat env) (repeat opt))]
      (merge
        {:op :static-method
         :env (env-location env expr)
         :class (field Compiler$StaticMethodExpr c expr)
         :method-name (field Compiler$StaticMethodExpr methodName expr)
         :method (when-let [method (field Compiler$StaticMethodExpr method expr)]
                   (@#'reflect/method->map method))
         :args args
         :tag (field Compiler$StaticMethodExpr tag expr)}
        (when (:children opt)
          {:children [[[:args] {:exprs? true}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  Compiler$InstanceMethodExpr
  (analysis->map
    [expr env opt]
    (let [target (analysis->map (field Compiler$InstanceMethodExpr target expr) env opt)
          args (map analysis->map (field Compiler$InstanceMethodExpr args expr) (repeat env) (repeat opt))]
      (merge
        {:op :instance-method
         :env (env-location env expr)
         :target target
         :method-name (field Compiler$InstanceMethodExpr methodName expr)
         :method (when-let [method (field Compiler$InstanceMethodExpr method expr)]
                   (@#'reflect/method->map method))
         :args args
         :tag (field Compiler$InstanceMethodExpr tag expr)}
        (when (:children opt)
          {:children [[[:target] {}] 
                      [[:args] {:exprs? true}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; Fields
  Compiler$StaticFieldExpr
  (analysis->map
    [expr env opt]
    (let []
      (merge
        {:op :static-field
         :env (env-location env expr)
         :class (field Compiler$StaticFieldExpr c expr)
         :field-name (field Compiler$StaticFieldExpr fieldName expr)
         :field (when-let [field (field Compiler$StaticFieldExpr field expr)]
                  (@#'reflect/field->map field))
         :tag (field Compiler$StaticFieldExpr tag expr)}
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  Compiler$InstanceFieldExpr
  (analysis->map
    [expr env opt]
    (let [target (analysis->map (field Compiler$InstanceFieldExpr target expr) env opt)]
      (merge
        {:op :instance-field
         :env (env-location env expr)
         :target target
         :target-class (field Compiler$InstanceFieldExpr targetClass expr)
         :field (when-let [field (field Compiler$InstanceFieldExpr field expr)]
                  (@#'reflect/field->map field))
         :field-name (field Compiler$InstanceFieldExpr fieldName expr)
         :tag (field Compiler$InstanceFieldExpr tag expr)}
        (when (:children opt)
          {:children [[[:target] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  Compiler$NewExpr
  (analysis->map
    [expr env opt]
    (let [args (map analysis->map (.args expr) (repeat env) (repeat opt))]
      (merge
        {:op :new
         :env 
         ; borrow line numbers from arguments
         (if-let [iexpr (first (filter :line (map :env args)))]
           (inherit-env iexpr env)
           env)
         :ctor (when-let [ctor (.ctor expr)]
                 (@#'reflect/constructor->map ctor))
         :class (.c expr)
         :args args}
        (when (:children opt)
          {:children [[[:args] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  Compiler$EmptyExpr
  (analysis->map
    [expr env opt]
    (merge
      {:op :empty-expr
       :env env
       :coll (.coll expr)}
      (when (:java-obj opt)
        {:Expr-obj expr})))

  ;; set literal
  Compiler$SetExpr
  (analysis->map
    [expr env opt]
    (let [keys (map analysis->map (.keys expr) (repeat env) (repeat opt))]
      (merge
        {:op :set
         :env env
         :keys keys}
        (when (:children opt)
          {:children [[[:keys] {:exprs? true}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; vector literal
  Compiler$VectorExpr
  (analysis->map
    [expr env opt]
    (let [args (map analysis->map (.args expr) (repeat env) (repeat opt))]
      (merge
        {:op :vector
         :env env
         :args args}
        (when (:children opt)
          {:children [[[:args] {:exprs? true}]]})
        (when (:java-obj opt) 
          {:Expr-obj expr}))))

  ;; map literal
  Compiler$MapExpr
  (analysis->map
    [expr env opt]
    (let [keyvals (map analysis->map (.keyvals expr) (repeat env) (repeat opt))]
      (merge
        {:op :map
         :env env
         :keyvals keyvals}
        (when (:children opt)
          {:children [[[:keyvals] {:exprs? true}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; Untyped
  Compiler$MonitorEnterExpr
  (analysis->map
    [expr env opt]
    (let [target (analysis->map (field Compiler$MonitorEnterExpr target expr) env opt)]
      (merge
        {:op :monitor-enter
         :env env
         :target target}
        (when (:children opt)
          {:children [[[:target] {:exprs? true}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  Compiler$MonitorExitExpr
  (analysis->map
    [expr env opt]
    (let [target (analysis->map (field Compiler$MonitorExitExpr target expr) env opt)]
      (merge
        {:op :monitor-exit
         :env env
         :target target}
        (when (:children opt)
          {:children [[[:target] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  Compiler$ThrowExpr
  (analysis->map
    [expr env opt]
    (let [exception (analysis->map (field Compiler$ThrowExpr excExpr expr) env opt)]
      (merge
        {:op :throw
         :env env
         :exception exception}
        (when (:children opt)
          {:children [[[:exception] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; Invokes
  Compiler$InvokeExpr
  (analysis->map
    [expr env opt]
    (let [fexpr (analysis->map (field Compiler$InvokeExpr fexpr expr) env opt)
          args (map analysis->map (field Compiler$InvokeExpr args expr) (repeat env) (repeat opt))]
      (merge
        {:op :invoke
         :env (env-location env expr)
         :fexpr fexpr
         :tag (field Compiler$InvokeExpr tag expr)
         :args args
         :is-protocol (field Compiler$InvokeExpr isProtocol expr)
         :is-direct (field Compiler$InvokeExpr isDirect expr)
         :site-index (field Compiler$InvokeExpr siteIndex expr)
         :protocol-on (field Compiler$InvokeExpr protocolOn expr)}
        (when-let [m (field Compiler$InvokeExpr onMethod expr)]
          {:method (@#'reflect/method->map m)})
        (when (:children opt)
          {:children [[[:fexpr] {}]
                      [[:args] {:exprs? true}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  Compiler$KeywordInvokeExpr
  (analysis->map
    [expr env opt]
    (let [target (analysis->map (field Compiler$KeywordInvokeExpr target expr) env opt)
          kw (analysis->map (field Compiler$KeywordInvokeExpr kw expr) env opt)]
      (merge
        {:op :keyword-invoke
         :env (env-location env expr)
         :kw kw
         :tag (field Compiler$KeywordInvokeExpr tag expr)
         :target target}
        (when (:children opt)
          {:children [[[:target] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; TheVarExpr
  Compiler$TheVarExpr
  (analysis->map
    [expr env opt]
    (merge
      {:op :the-var
       :env env
       :var (.var expr)}
      (when (:java-obj opt)
        {:Expr-obj expr})))

  ;; VarExpr
  Compiler$VarExpr
  (analysis->map
    [expr env opt]
    (merge
      {:op :var
       :env env
       :var (.var expr)
       :tag (.tag expr)}
      (when (:java-obj opt)
        {:Expr-obj expr})))

  ;; UnresolvedVarExpr
  Compiler$UnresolvedVarExpr
  (analysis->map
    [expr env opt]
    (let []
      (merge
        {:op :unresolved-var
         :env env
         :sym (.symbol expr)}
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; ObjExprs
  Compiler$ObjExpr
  (analysis->map
    [expr env opt]
    (merge
      {:op :obj-expr
       :env env
       :tag (.tag expr)}
      (when (:java-obj opt)
        {:Expr-obj expr})))

  ;; FnExpr (extends ObjExpr)
  Compiler$NewInstanceMethod
  (analysis->map
    [obm env opt]
    (let [body (analysis->map (.body obm) env opt)]
      (merge
        {:op :new-instance-method
         :env (env-location env obm)
         :name (symbol (field Compiler$NewInstanceMethod name obm))
         :required-params (map analysis->map 
                               (concat [((field Compiler$ObjMethod indexlocals obm) 0)]
                                       (field Compiler$ObjMethod argLocals obm))
                               (repeat env)
                               (repeat opt))
         :body body}
        (when (:children opt)
          {:children [[[:body] {}]]})
        (when (:java-obj opt)
          {:ObjMethod-obj obm}))))

  Compiler$FnMethod
  (analysis->map
    [obm env opt]
    (let [body (analysis->map (.body obm) env opt)
          required-params (map analysis->map (.reqParms obm) (repeat env) (repeat opt))]
      (merge
        {:op :fn-method
         :env env
         :body body
         ;; Map LocalExpr@xx -> LocalExpr@xx
         ;;:locals (map analysis->map (keys (.locals obm)) (repeat env) (repeat opt))
         :required-params required-params
         :rest-param (let [rest-param (.restParm obm)]
                       (if rest-param
                         (analysis->map rest-param env opt)
                         rest-param))}
        (when (:children opt)
          {:children [[[:body] {}]]})
        (when (:java-obj opt)
          {:ObjMethod-obj obm}))))

  Compiler$FnExpr
  (analysis->map
    [expr env opt]
    (let [methods (map analysis->map (.methods expr) (repeat env) (repeat opt))]
      (merge
        {:op :fn-expr
         :env (env-location env expr)
         :methods methods
         :variadic-method (when-let [variadic-method (.variadicMethod expr)]
                            (analysis->map variadic-method env opt))
         :tag (.tag expr)}
        (when-let [nme (.thisName expr)]
          {:name (symbol nme)})
        (when (:children opt)
          {:children [[[:methods] {:exprs? true}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; NewInstanceExpr
;FIXME find vector of interfaces this implements (I think it's in mmap + IType)
  Compiler$NewInstanceExpr
  (analysis->map
    [expr env opt]
    (let [methods (map analysis->map (field Compiler$NewInstanceExpr methods expr) (repeat env) (repeat opt))]
      (merge
        {:op :deftype*
         :name (symbol (.name expr))
         :env (env-location env expr)
         :methods methods
         :mmap (field Compiler$NewInstanceExpr mmap expr)

         :compiled-class (.compiledClass expr)
         :internal-name (.internalName expr)
         :this-name (.thisName expr)

         ;(Set LocalBinding)
         :fields (set
                   (for [[k v] (field Compiler$ObjExpr fields expr)]
                     (analysis->map v env opt)))

         ;(Vec Symbol)
         :hinted-fields (field Compiler$ObjExpr hintedFields expr)
         :covariants (field Compiler$NewInstanceExpr covariants expr)
         :tag (.tag expr)}
        (when (:children opt)
          {:children [[[:methods] {:exprs? true}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; InstanceOfExpr
  Compiler$InstanceOfExpr
  (analysis->map
    [expr env opt]
    (let [exp (analysis->map (field Compiler$InstanceOfExpr expr expr) env opt)]
      (merge
        {:op :instance-of
         :env env
         :class (field Compiler$InstanceOfExpr c expr)
         :the-expr exp}
        (when (:children opt)
          {:children [[[:exp] {}]]})
        (when (:java-obj opt) 
          {:Expr-obj expr}))))

  ;; MetaExpr
  Compiler$MetaExpr
  (analysis->map
    [expr env opt]
    (let [meta (analysis->map (.meta expr) env opt)
          the-expr (analysis->map (.expr expr) env opt)]
      (merge
        {:op :meta
         :env env
         :meta meta
         :expr the-expr}
        (when (:children opt)
          {:children [[[:meta] {}]
                      [[:the-expr] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; do
  Compiler$BodyExpr
  (analysis->map
    [expr env opt]
    (let [exprs (map analysis->map (.exprs expr) (repeat env) (repeat opt))]
      (merge
        {:op :do
         :env (inherit-env (last exprs) env)
         :exprs exprs}
        (when (:children opt)
          {:children [[[:exprs] {:exprs? true}]]})
        (when (:java-obj opt) 
          {:Expr-obj expr}))))

  ;; if
  Compiler$IfExpr
  (analysis->map
    [expr env opt]
    (let [test (analysis->map (.testExpr expr) env opt)
          then (analysis->map (.thenExpr expr) env opt)
          else (analysis->map (.elseExpr expr) env opt)]
      (merge
        {:op :if
         :env (env-location env expr)
         :test test
         :then then
         :else else}
        (when (:children opt)
          {:children [[[:test] {}] 
                      [[:then] {}] 
                      [[:else] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; case
  ;; (from Compiler.java)
  ;;  //(case* expr shift mask default map<minhash, [test then]> table-type test-type skip-check?)
  Compiler$CaseExpr
  (analysis->map
    [expr env opt]
    (let [the-expr (analysis->map (.expr expr) env opt)
          tests (map analysis->map (vals (.tests expr)) (repeat env) (repeat opt))
          thens (map analysis->map (vals (.thens expr)) (repeat env) (repeat opt))
          default (analysis->map (.defaultExpr expr) env opt)]
      (merge
        {:op :case*
         :env (env-location env expr)
         :the-expr the-expr
         :tests tests
         :thens thens
         :default default
         :tests-hashes (keys (.tests expr))
         :shift (.shift expr)
         :mask (.mask expr)
         :test-type (.testType expr)
         :switch-type (.switchType expr)
         :skip-check (.skipCheck expr)}
        (when (:children opt)
          {:children [[[:the-expr]  {}]
                      [[:tests] {:exprs? true}] 
                      [[:thens] {:exprs? true}] 
                      [[:default] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))


  ;; ImportExpr
  Compiler$ImportExpr
  (analysis->map
    [expr env opt]
    (merge
      {:op :import*
       :env env
       :class-str (.c expr)}
       (when (:java-obj opt)
         {:Expr-obj expr})))

  ;; AssignExpr (set!)
  Compiler$AssignExpr
  (analysis->map
    [expr env opt]
    (let [target (analysis->map (.target expr) env opt)
          val (analysis->map (.val expr) env opt)]
      (merge
        {:op :set!
         :env env
         :target target
         :val val}
        (when (:children opt)
          {:children [[[:target] {}] 
                      [[:val] {}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;;TryExpr
  Compiler$TryExpr$CatchClause
  (analysis->map
    [ctch env opt]
    (let [local-binding (analysis->map (.lb ctch) env opt)
          handler (analysis->map (.handler ctch) env opt)]
      (merge
        {:op :catch
         :env env
         :class (.c ctch)
         :local-binding local-binding
         :handler handler}
        (when (:children opt)
          {:children [[[:local-binding] {}]
                      [[:handler] {}]]})
        (when (:java-obj opt)
          {:CatchClause-obj ctch}))))

  Compiler$TryExpr
  (analysis->map
    [expr env opt]
    (let [try-expr (analysis->map (.tryExpr expr) env opt)
          finally-expr (when-let [finally-expr (.finallyExpr expr)]
                         (analysis->map finally-expr env opt))
          catch-exprs (map analysis->map (.catchExprs expr) (repeat env) (repeat opt))]
      (merge
        {:op :try
         :env env
         :try-expr try-expr
         :finally-expr finally-expr
         :catch-exprs catch-exprs
         :ret-local (.retLocal expr)
         :finally-local (.finallyLocal expr)}
        (when (:children opt)
          {:children [[[:try-expr] {}]
                      [[:finally-expr] {}]
                      [[:catch-exprs] {:exprs? true}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  ;; RecurExpr
  Compiler$RecurExpr
  (analysis->map
    [expr env opt]
    (let [loop-locals (map analysis->map (.loopLocals expr) (repeat env) (repeat opt))
          args (map analysis->map (.args expr) (repeat env) (repeat opt))]
      (merge
        {:op :recur
         :env (env-location env expr)
         :loop-locals loop-locals
         :args args}
        (when (:children opt)
          {:children [[[:loop-locals] {:exprs? true}]
                      [[:args] {:exprs? true}]]})
        (when (:java-obj opt)
          {:Expr-obj expr}))))

  Compiler$MethodParamExpr
  (analysis->map
    [expr env opt]
    (let []
      (merge
        {:op :method-param
         :env env
         :class (.getJavaClass expr)
         :can-emit-primitive (.canEmitPrimitive expr)}
        (when (:java-obj opt)
          {:Expr-obj expr})))))

(defn keyword->Context [k]
  (case k
    :statement Compiler$C/STATEMENT
    :expression Compiler$C/EXPRESSION
    :return Compiler$C/RETURN
    :eval Compiler$C/EVAL))

(defn- analyze*
  "Must be called after binding the appropriate Compiler and RT dynamic Vars."
  ([env form] (analyze* env form {}))
  ([env form opt]
   (let [context (keyword->Context (:context env))
         expr-ast (try
                    (Compiler/analyze context form)
                    (catch RuntimeException e
                      (throw (repl/root-cause e))))
         _ (method-accessor (class expr-ast) 'eval expr-ast [])]
     (analysis->map expr-ast (merge-with conj (dissoc env :context) {:locals {}}) opt))))

(defn analyze-one
  "Analyze a single form"
  ([env form] (analyze-one env form {}))
  ([env form opt] (analyze* env form opt)))

(defn forms-seq
  "Lazy seq of forms in a Clojure or ClojureScript file."
  [^java.io.PushbackReader rdr]
  (let [eof (reify)]
    (lazy-seq
      (let [form (read rdr nil eof)]
        (when-not (identical? form eof)
          (lazy-seq (cons form (forms-seq rdr))))))))

(defn ^:private munge-ns [ns-sym]
  (-> (name ns-sym)
      (string/replace "." "/")
      (string/replace "-" "_")
      (str ".clj")))
       
(defn uri-for-ns 
  "Returns a URI representing the namespace. Throws an
  exception if URI not found."
  [ns-sym]
  (let [source-path (munge-ns ns-sym) 
        uri (io/resource source-path)]
    (when-not uri
      (throw (Exception. (str "No file found for namespace " ns-sym))))
    uri))

(defn ^LineNumberingPushbackReader
  pb-reader-for-ns
  "Returns a LineNumberingPushbackReader for namespace ns-sym"
  [ns-sym]
  (let [uri (uri-for-ns ns-sym)]
    (LineNumberingPushbackReader. (io/reader uri))))

(defonce ^:private Compiler-members (set (map :name (:members (reflect/type-reflect RT)))))
(defonce ^:private RT-members (set (map :name (:members (reflect/type-reflect RT)))))

(defmacro ^:private analyzer-bindings [source-path pushback-reader]
  `(merge
     {Compiler/LOADER (RT/makeClassLoader)
      Compiler/SOURCE_PATH (str ~source-path)
      Compiler/SOURCE (str ~source-path)
      Compiler/METHOD nil
      Compiler/LOCAL_ENV nil
      Compiler/LOOP_LOCALS nil
      Compiler/NEXT_LOCAL_NUM 0
      RT/CURRENT_NS @RT/CURRENT_NS
      Compiler/LINE_BEFORE (.getLineNumber ~pushback-reader)
      Compiler/LINE_AFTER (.getLineNumber ~pushback-reader)
      RT/UNCHECKED_MATH @RT/UNCHECKED_MATH}
     ~(when (RT-members 'WARN_ON_REFLECTION)
        `{(field RT ~'WARN_ON_REFLECTION) @(field RT ~'WARN_ON_REFLECTION)})
     ~(when (Compiler-members 'COLUMN_BEFORE)
        `{Compiler/COLUMN_BEFORE (.getColumnNumber ~pushback-reader)})
     ~(when (Compiler-members 'COLUMN_AFTER)
        `{Compiler/COLUMN_AFTER (.getColumnNumber ~pushback-reader)})
     ~(when (RT-members 'DATA_READERS)
        `{RT/DATA_READERS @RT/DATA_READERS})))

(defn analyze-file
  "Takes a file path and optionally a pushback reader.
  Returns a vector of maps representing the ASTs of the forms
  in the target file.

  Options:
  - :reader  a pushback reader to use to read the namespace forms
  - :opt     a map of analyzer options
    - :children
      when true, include a :children key with all child expressions of each node
    - :java-obj
      when true, include a :java-obj key with the node's corresponding Java object

  eg. (analyze-file \"my/ns.clj\")"
  [source-path & {:keys [reader opt] 
                  :or {reader (LineNumberingPushbackReader. (io/reader (io/resource source-path)))}}]
  (let [eof (reify)
        ^LineNumberingPushbackReader 
        pushback-reader (if (instance? LineNumberingPushbackReader reader)
                          reader
                          (LineNumberingPushbackReader. reader))]
    (with-bindings (analyzer-bindings source-path pushback-reader)
      (loop [form (read pushback-reader nil eof)
             out []]
        (if (identical? form eof)
          out
          (let [env {:ns {:name (ns-name *ns*)}
                     :source-path source-path
                     :locals {}}
                expr-ast (Compiler/analyze (keyword->Context :eval) form)
                m (analysis->map expr-ast env opt)
                _ (when *eval-ast*
                    (method-accessor Compiler$Expr 'eval expr-ast []))]
            (recur (read pushback-reader nil eof) (conj out m))))))))

(defn analyze-ns
  "Takes a LineNumberingPushbackReader and a namespace symbol.
  Returns a vector of maps, with keys :op, :env. If expressions
  have children, will have :children entry.

  Options:
  - :reader  a pushback reader to use to read the namespace forms
  - :opt     a map of analyzer options
    - :children
      when true, include a :children key with all child expressions of each node
    - :java-obj
      when true, include a :java-obj key with the node's corresponding Java object

  eg. (analyze-ns 'my-ns :opt {:children true} :reader (pb-reader-for-ns 'my.ns))"
  [source-nsym & {:keys [reader opt] :or {reader (pb-reader-for-ns source-nsym)}}]
  (let [source-path (munge-ns source-nsym)]
    (analyze-file source-path :reader reader :opt opt)))


(defn children 
  "Returns a lazy sequence of the immediate children of the expr in
  order of evaluation, where defined."
  [expr]
  (util/children expr))

(comment
  (ast 
    (try (throw (Exception.)) 
      (catch Exception e (throw e)) 
      (finally 33)))

  (ast
    (let [b 1] 
      (fn [& a] 1)))

  (ast (Integer. (+ 1 1)))

  (ast (map io/file [1 2]))

  (ast (do 
         (require '[clojure.repl :refer [pst]])
         (pst)))
  (ast (deftype A [a b]
         Object
         (toString [this])))
  
  ;children
  ; - what about optional keys? eg. :local-binding's :init? do we need an :optional case, or
  ;   maybe a `:when child-expr` will be sufficient?
  (->
    (let [expr (ast (let [a 1] a) {:children true})]
      (for [[path {:keys [exprs?]}] (:children expr)
            :let [in (get-in expr path)]
            child-expr (if exprs?
                         in
                         [in])]
        child-expr))
    clojure.pprint/pprint)

  (def in (Compiler/analyze Compiler$C/STATEMENT '(seq 1)))
  (class in)
  (def method (doto (.getMethod (class in) "eval" (into-array Class []))
                (.setAccessible true)))
  (try (.invoke method in (object-array []))
    (catch java.lang.reflect.InvocationTargetException e
      (throw (repl/root-cause e))))
    )

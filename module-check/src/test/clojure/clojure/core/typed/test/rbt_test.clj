(ns clojure.core.typed.test.rbt-test
  (:refer-clojure :exclude [and])
  (:require ;[clojure.core.typed.test.test-utils :refer :all]
            [clojure.core.typed :refer [ann inst cf fn> pfn> defalias declare-names
                                        print-env print-filterset check-ns typed-deps
                                        ann-form]]
            [clojure.core.typed.test.rbt-types]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.current-impl :as impl]
            ;[clojure.core.typed.type-rep :refer :all]
            ;[clojure.core.typed.type-ctors :refer :all]
            ;[clojure.core.typed.filter-rep :refer :all]
            ;[clojure.core.typed.filter-ops :refer :all]
            ;[clojure.core.typed.object-rep :refer :all]
            ;[clojure.core.typed.path-rep :refer :all]
            ;[clojure.core.typed.parse-unparse :refer :all]
            [clojure.core.typed.check :as chk]
            [clojure.core.typed.update :as update]
            [clojure.repl :refer [pst]]
            ;[clojure.test :refer :all]
            ))

;(defmacro check-rbt [& body]
;  `(do (check-ns '~'clojure.core.typed.test.rbt-types)
;       (impl/with-clojure-impl
;         ~@body)))
;
;(defmacro is-check-rbt [& body]
;  `(is (check-rbt ~@body)))
;
;(comment
;(-> (tc-t (clojure.core.typed/fn> [tmap :- clojure.core.typed.test.rbt-types/badRight]
;                          (and (= :Black (-> tmap :tree))
;                               (= :Red (-> tmap :left :tree))
;                               (= :Red (-> tmap :right :tree))
;                               (= :Red (-> tmap :right :left :tree)))))
;                          ;(and (tc-pr-filters "first filter"
;                          ;       (= :Black (-> tmap :tree)))
;                          ;     (tc-pr-filters "second filter"
;                          ;       (= :Red (-> tmap :left :tree)))
;                          ;     (tc-pr-filters "third filter"
;                          ;       (= :Red (-> tmap :right :tree)))
;                          ;     (tc-pr-filters "fourth filter"
;                          ;       (= :Red (-> tmap :right :left :tree))))
;  ret-t :types first :rng :fl :else unparse-filter pprint)
;)
;
;(deftest update-nested-hmap-test
;  #_(is-check-rbt (= (update/update (-hmap {(-val :left) (Name-maker 'clojure.core.typed.test.rbt-types/rbt)})
;                             (-filter (-val :Red) 'id [(-kpe :left) (-kpe :tree)]))
;                   (-hmap {(-val :left) 
;                           (-hmap {(-val :tree) (-val :Red) 
;                                   (-val :entry) (Name-maker 'clojure.core.typed.test.rbt-types/EntryT) 
;                                   (-val :left) (Name-maker 'clojure.core.typed.test.rbt-types/bt) 
;                                   (-val :right) (Name-maker 'clojure.core.typed.test.rbt-types/bt)})}))))
;         
;(deftest rbt-test
;  #_(is-check-rbt (tc-t (clojure.core.typed/ann-form 
;                        (fn [tmap]
;                          (print-filterset "inner if"
;                            (if (= :Red (-> tmap :right :tree))
;                              (= :Red (-> tmap :right :left :tree))
;                              false)))
;                        [clojure.core.typed.test.rbt-types/badRight -> clojure.core.typed/Any
;                         :filters {:then (& (is ':Red 0 [(Key :right) (Key :tree)])
;                                            (is ':Red 0 [(Key :right) (Key :left) (Key :tree)]))
;                                   :else (| (! ':Red 0 [(Key :right) (Key :tree)])
;                                            (! ':Red 0 [(Key :right) (Key :left) (Key :tree)]))}])))
;  #_(is-check-rbt (tc-t (clojure.core.typed/ann-form 
;                        (fn [tmap]
;                          (print-filterset "entire"
;                          (if (= :Black (-> tmap :tree))
;                            (print-filterset "next1"
;                              (if (= :Red (-> tmap :left :tree))
;                                (print-filterset "innermost"
;                                  (if (= :Red (-> tmap :right :tree))
;                                    (= :Red (-> tmap :right :left :tree))
;                                    false))
;                                false))
;                            false)))
;                        [clojure.core.typed.test.rbt-types/badRight -> clojure.core.typed/Any
;                         :filters {:then (& (is ':Black tmap [(Key :tree)])
;                                            (is ':Red tmap [(Key :left) (Key :tree)])
;                                            (is ':Red tmap [(Key :right) (Key :tree)])
;                                            (is ':Red tmap [(Key :right) (Key :left) (Key :tree)]))
;                                   :else (| (! ':Black tmap [(Key :tree)])
;                                            (! ':Red tmap [(Key :left) (Key :tree)])
;                                            (! ':Red tmap [(Key :right) (Key :tree)])
;                                            (! ':Red tmap [(Key :right) (Key :left) (Key :tree)]))}])))
;  #_(is-check-rbt (tc-t (clojure.core.typed/ann-form 
;                        (fn [tmap]
;                          (if (= :Black (-> tmap :tree))
;                            (print-filterset "next1"
;                              (if (= :Red (-> tmap :left :tree))
;                                (print-filterset "innermost"
;                                  (if (= :Red (-> tmap :right :tree))
;                                    (= :Red (-> tmap :right :left :tree))
;                                    false))
;                                false))
;                            false))
;                        [clojure.core.typed.test.rbt-types/badRight -> clojure.core.typed/Any
;                         :filters {:then (& (is ':Black tmap [(Key :tree)])
;                                            (is ':Red tmap [(Key :left) (Key :tree)])
;                                            (is ':Red tmap [(Key :right) (Key :tree)])
;                                            (is ':Red tmap [(Key :right) (Key :left) (Key :tree)]))
;                                   :else (| (! ':Black tmap [(Key :tree)])
;                                            (! ':Red tmap [(Key :left) (Key :tree)])
;                                            (! ':Red tmap [(Key :right) (Key :tree)])
;                                            (! ':Red tmap [(Key :right) (Key :left) (Key :tree)]))}]))))
;
;#_(let [f1 (parse-filter '(& (! (Value :Red) tmap ((Key :right) (Key :left) (Key :tree))) 
;                           (is (Value :Red) tmap ((Key :left) (Key :tree)))))
;      f2 (parse-filter '(& (! (Value :Red) tmap ((Key :right) (Key :tree))) 
;                           (is (Value :Red) tmap ((Key :left) (Key :tree)))))]
;  (prn "final")
;  (unparse-filter (-or f1 f2)))
;
;#_(unparse-filter 
;  (parse-filter '(& (| (! (Value :Red) tmap ((Key :right) (Key :left) (Key :tree))) (is (Value :Red) tmap ((Key :left) (Key :tree)))) (| (! (Value :Red) tmap ((Key :right) (Key :left) (Key :tree))) (! (Value :Red) tmap ((Key :right) (Key :tree)))))))
;
;(defmacro update-badRight-tmap [fl]
;  `(check-rbt
;     (-> (update/update-composite
;           {'~'tmap (parse-type '~'clojure.core.typed.test.rbt-types/badRight)}
;           (parse-filter
;             '~fl))
;         (get '~'tmap)
;         unparse-type)))
;
;(deftest update-composite
;  (update-badRight-tmap
;    (|
;     (! (Value :Red) tmap ((Key :right) (Key :left) (Key :tree)))
;     (! (Value :Red) tmap ((Key :right) (Key :tree)))
;     (! (Value :Red) tmap ((Key :left) (Key :tree)))
;     (! (Value :Black) tmap ((Key :tree)))))
;  (update-badRight-tmap
;    (& (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
;       (is (Value :Red) tmap [(Key :right) (Key :tree)]) 
;       (is (Value :Red) tmap [(Key :left) (Key :tree)]) 
;       (is (Value :Black) tmap [(Key :tree)]))))
;
;;#_(Un
;;  (parse-type 'clojure.core.typed.test.rbt-types/badRight)
;;  (parse-type '(HMap :mandatory {:tree (Value :Black), 
;;                                 :entry clojure.core.typed.test.rbt-types/EntryT, 
;;                                 :left (HMap :mandatory {:tree (Value :Red), 
;;                                                         :entry clojure.core.typed.test.rbt-types/EntryT, 
;;                                                         :left clojure.core.typed.test.rbt-types/bt, 
;;                                                         :right clojure.core.typed.test.rbt-types/bt}), 
;;                                 :right (U (HMap :mandatory {:tree (Value :Red), 
;;                                                             :entry clojure.core.typed.test.rbt-types/EntryT, 
;;                                                             :left clojure.core.typed.test.rbt-types/rbt, 
;;                                                             :right clojure.core.typed.test.rbt-types/bt}) 
;;                                           (HMap :mandatory {:tree (Value :Red), 
;;                                                             :entry clojure.core.typed.test.rbt-types/EntryT, 
;;                                                             :left clojure.core.typed.test.rbt-types/bt, 
;;                                                             :right clojure.core.typed.test.rbt-types/rbt}))})))
;;
;;(update-badRight-tmap
;;  (& (is ':Black tmap [(Key :tree)])
;;     (is ':Red tmap [(Key :left) (Key :tree)])
;;     (is ':Red tmap [(Key :right) (Key :tree)])
;;     (is ':Red tmap [(Key :right) (Key :left) (Key :tree)])))
;;
;(comment
;(u/profile :info :foo
;(update-badRight-tmap
;  (| (! ':Black tmap [(Key :tree)])
;     (! ':Red tmap [(Key :left) (Key :tree)])
;     (! ':Red tmap [(Key :right) (Key :tree)])
;     (! ':Red tmap [(Key :right) (Key :left) (Key :tree)])))
;  )
;(u/profile :info :foo
;(update-badRight-tmap
;  (&
;   (|
;    (! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
;    (! (Value :Red) tmap [(Key :right) (Key :tree)])
;    (! (Value :Red) tmap [(Key :left) (Key :tree)])
;    (! (Value :Black) tmap [(Key :tree)]))
;   (|
;    (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
;    (! (Value :Red) tmap [(Key :right) (Key :tree)])
;    (! (Value :Red) tmap [(Key :left) (Key :tree)])
;    (! (Value :Black) tmap [(Key :tree)]))))
;)
;  )
;
;;FIXME why doesn't this simplify?
;#_(u/profile :info :foo
;(let [t (parse-type
;          '(I clojure.core.typed.test.rbt-types/rbt 
;              (HMap :mandatory {:tree (Value :Red), 
;                                :entry clojure.core.typed.test.rbt-types/EntryT, 
;                                :left clojure.core.typed.test.rbt-types/rbt, 
;                                :right clojure.core.typed.test.rbt-types/bt})))]
;  (dotimes [_ 2]
;    (hash t))))
;;
;;(u/profile :info :foo
;;(clojure.core.typed.subtype/subtype?
;;    (parse-type 
;;      (update-badRight-tmap
;;       (&
;;        (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
;;        (is (Value :Red) tmap [(Key :right) (Key :tree)])
;;        (is (Value :Red) tmap [(Key :left) (Key :tree)])
;;        (is (Value :Black) tmap [(Key :tree)])
;;        (is (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])),))
;;  (parse-type '(U clojure.core.typed.test.rbt-types/Empty
;;                  (clojure.core.typed.test.rbt-types/Black clojure.core.typed.test.rbt-types/rbt clojure.core.typed.test.rbt-types/rbt)
;;                  (clojure.core.typed.test.rbt-types/Red clojure.core.typed.test.rbt-types/bt clojure.core.typed.test.rbt-types/bt)
;;                  (clojure.core.typed.test.rbt-types/Black clojure.core.typed.test.rbt-types/rbt 
;;                                                           (clojure.core.typed.test.rbt-types/Red clojure.core.typed.test.rbt-types/bt clojure.core.typed.test.rbt-types/rbt))))
;;  )
;;           )
;;
;;(u/profile :info :foo
;;(update-badRight-tmap
;;  (&
;;   (|
;;    (! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
;;    (! (Value :Red) tmap [(Key :right) (Key :tree)])
;;    (! (Value :Black) tmap [(Key :tree)]))
;;   (|
;;    (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
;;    (! (Value :Red) tmap [(Key :right) (Key :tree)])
;;    (! (Value :Black) tmap [(Key :tree)]))
;;   (|
;;    (! (Value :Red) tmap [(Key :right) (Key :right) (Key :tree)])
;;    (! (Value :Red) tmap [(Key :right) (Key :tree)])
;;    (! (Value :Red) tmap [(Key :left) (Key :tree)])
;;    (! (Value :Black) tmap [(Key :tree)]))
;;   (|
;;    (! (Value :Red) tmap [(Key :right) (Key :left) (Key :tree)])
;;    (! (Value :Red) tmap [(Key :right) (Key :tree)])
;;    (! (Value :Red) tmap [(Key :left) (Key :tree)])
;;    (! (Value :Black) tmap [(Key :tree)]))))
;;  )
;;
;;(def res-last (parse-type '(U (HMap :mandatory {:tree (Value :Black), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/rbt, :right (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/bt, :right clojure.core.typed.test.rbt-types/bt}), :right clojure.core.typed.test.rbt-types/rbt})}) (HMap :mandatory {:tree (Value :Black), :entry clojure.core.typed.test.rbt-types/EntryT, :left (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/bt, :right clojure.core.typed.test.rbt-types/bt}), :right (U (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/rbt, :right clojure.core.typed.test.rbt-types/bt}) (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/bt, :right clojure.core.typed.test.rbt-types/rbt}))}) (HMap :mandatory {:tree (Value :Black), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/rbt, :right (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left (U (HMap :mandatory {:tree (Value :Empty)}) (HMap :mandatory {:tree (Value :Black), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/rbt, :right clojure.core.typed.test.rbt-types/rbt})), :right (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/bt, :right clojure.core.typed.test.rbt-types/bt})})}) (HMap :mandatory {:tree (Value :Black), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/rbt, :right (U (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/rbt, :right clojure.core.typed.test.rbt-types/bt}) (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left (U (HMap :mandatory {:tree (Value :Empty)}) (HMap :mandatory {:tree (Value :Black), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/rbt, :right clojure.core.typed.test.rbt-types/rbt})), :right clojure.core.typed.test.rbt-types/rbt}))}) clojure.core.typed.test.rbt-types/badRight (HMap :mandatory {:tree (Value :Black), :entry clojure.core.typed.test.rbt-types/EntryT, :left (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/bt, :right clojure.core.typed.test.rbt-types/bt}), :right (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/rbt, :right clojure.core.typed.test.rbt-types/rbt})}) (HMap :mandatory {:tree (Value :Black), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/rbt, :right (HMap :mandatory {:tree (Value :Red), :entry clojure.core.typed.test.rbt-types/EntryT, :left clojure.core.typed.test.rbt-types/rbt, :right clojure.core.typed.test.rbt-types/rbt})})))))
;;
;;(clojure.core.typed.subtype/subtype? res-last (parse-type 'clojure.core.typed.test.rbt-types/rbt))
;;
;
;(comment
;(cf (defalias Env
;  "An environment"
;  (HMap :optional {:line AnyInteger,
;                   :column AnyInteger,
;                   :source clojure.core.typed/Any}))
;    )
;
;  (cf
;(defalias Children 
;  "A children API exists, but not checked yet"
;  clojure.core.typed/Any)
;    )
;           (import '(clojure.lang RT LineNumberingPushbackReader Compiler$DefExpr Compiler$LocalBinding Compiler$BindingInit Compiler$LetExpr
;                         Compiler$LetFnExpr Compiler$StaticMethodExpr Compiler$InstanceMethodExpr Compiler$StaticFieldExpr
;                         Compiler$NewExpr Compiler$EmptyExpr Compiler$VectorExpr Compiler$MonitorEnterExpr
;                         Compiler$MonitorExitExpr Compiler$ThrowExpr Compiler$InvokeExpr Compiler$TheVarExpr Compiler$VarExpr
;                         Compiler$UnresolvedVarExpr Compiler$ObjExpr Compiler$NewInstanceMethod Compiler$FnMethod Compiler$FnExpr
;                         Compiler$NewInstanceExpr Compiler$MetaExpr Compiler$BodyExpr Compiler$ImportExpr Compiler$AssignExpr
;                         Compiler$TryExpr$CatchClause Compiler$TryExpr Compiler$C Compiler$LocalBindingExpr Compiler$RecurExpr
;                         Compiler$MapExpr Compiler$IfExpr Compiler$KeywordInvokeExpr Compiler$InstanceFieldExpr Compiler$InstanceOfExpr
;                         Compiler$CaseExpr Compiler$Expr Compiler$SetExpr Compiler$MethodParamExpr Compiler$KeywordExpr
;                         Compiler$ConstantExpr Compiler$NumberExpr Compiler$NilExpr Compiler$BooleanExpr Compiler$StringExpr
;                         Compiler$ObjMethod Compiler$Expr
;                         Keyword Seqable Var Symbol IPersistentSet))
;
;  (u/profile
;    :info :foo
;    (parse-type
;  '(Rec [Expr]
;  (U ;; values
;     '{:op ':keyword
;       :env Env
;       :val Keyword}
;     '{:op ':constant
;       :env Env
;       :val clojure.core.typed/Any}
;     '{:op ':number
;       :env Env
;       :val Number}
;     '{:op ':string
;       :env Env
;       :val String}
;     '{:op ':nil
;       :env Env
;       :val nil}
;     '{:op ':boolean
;       :env Env
;       :val Boolean}
;     
;     ;; def
;     (HMap :mandatory
;           {:op ':def
;            :env Env
;            :var Var
;            :meta (U nil Expr)
;            :init Expr
;            :init-provided clojure.core.typed/Any
;            :is-dynamic clojure.core.typed/Any}
;           :optional
;           {:children Children
;            :Expr-obj Compiler$DefExpr})
;
;     ;; local binding
;     (HMap :mandatory
;           {:op ':local-binding
;            :env Env
;            :sym Symbol
;            :tag (U nil Symbol)
;            :init Expr}
;           :optional
;           {:children Children
;             :LocalBinding-obj Compiler$LocalBinding})
;
;     ;binding init
;     (HMap :mandatory
;           {:op ':binding-init
;            :env Env
;            :local-binding Expr
;            :init Expr}
;           :optional
;           {:children Children
;            :BindingInit-obj Compiler$BindingInit})
;
;     ; let
;     (HMap :mandatory
;           {:op ':let
;            :env Env
;            :binding-inits (Seqable Expr)
;            :body Expr
;            :is-loop clojure.core.typed/Any}
;           :optional
;           {:children Children
;             :Expr-obj Compiler$LetExpr})
;
;     ;letfn
;     (HMap :mandatory
;           {:op ':letfn
;            :env Env
;            :body Expr
;            :binding-inits (Seqable Expr)}
;           :optional
;           {:children Children
;             :Expr-obj Compiler$LetFnExpr})
;
;     ;local binding expr
;     (HMap :mandatory
;           {:op ':local-binding-expr
;            :env Env
;            :local-binding Expr
;            :tag (U nil Symbol)}
;           :optional
;           {:children Children
;             :Expr-obj Compiler$LocalBindingExpr})
;
;     ;static method
;     (HMap :mandatory
;           {:op ':static-method
;            :env Env
;            :class Class
;            :method-name Symbol
;            ; probably need to type clojure.reflect
;            :method Nothing
;            :args (Seqable Expr)
;            :tag (U nil Symbol)}
;           :optional
;           {:children Children
;             :Expr-obj Compiler$StaticMethodExpr})
;
;     ;instance method
;     (HMap :mandatory
;           {:op ':instance-method
;            :env Env
;            :target Expr
;            :method-name Symbol
;            :method Nothing
;            :args (Seqable Expr)
;            :tag (U nil Symbol)}
;           :optional
;           {:children Children
;             :Expr-obj Compiler$InstanceMethodExpr})
;
;    ;static field
;    (HMap :mandatory
;          {:op ':static-field
;           :env Env
;           :class Class
;           :field-name Symbol
;           :field Nothing
;           :tag (U nil Symbol)}
;          :optional
;          {:Expr-obj Compiler$StaticFieldExpr})
;
;    ;instance field
;    (HMap :mandatory
;          {:op ':instance-field
;           :env Env
;           :target Expr
;           :target-class Class
;           :field Nothing
;           :field-name Symbol
;           :tag (U nil Symbol)}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$InstanceFieldExpr})
;
;    ;new
;    (HMap :mandatory
;          {:op ':new
;           :env Env
;           :ctor Nothing
;           :class Class
;           :args (Seqable Expr)}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$NewExpr})
;
;    ;empty
;    (HMap :mandatory
;          {:op ':empty-expr
;           :env Env
;           :coll (I (Seqable Nothing) (ExactCount 0))}
;          :optional
;          {:Expr-obj Compiler$EmptyExpr})
;
;    ;set
;    (HMap :mandatory
;          {:op ':set
;           :env Env
;           :keys (Seqable Expr)}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$SetExpr})
;
;    ;vector
;    (HMap :mandatory
;          {:op ':vector
;           :env Env
;           :args (Seqable Expr)}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$VectorExpr})
;
;    ;map
;    (HMap :mandatory
;          {:op ':map
;           :env Env
;           :keyvals (Seqable Expr)}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$MapExpr})
;
;    ;monitor enter
;    (HMap :mandatory
;          {:op ':monitor-enter
;           :env Env
;           :target Expr}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$MonitorEnterExpr})
;
;    ;monitor exit
;    (HMap :mandatory
;          {:op ':monitor-exit
;           :env Env
;           :target Expr}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$MonitorExitExpr})
;
;    ;throw
;    (HMap :mandatory
;          {:op ':throw
;           :env Env
;           :exception Expr}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$ThrowExpr})
;
;    ;invoke
;    (HMap :mandatory
;          {:op ':invoke
;           :env Env
;           :fexpr Expr
;           :tag (U nil Symbol)
;           :args (Seqable Expr)
;           :is-protocol clojure.core.typed/Any
;           :is-direct clojure.core.typed/Any
;           :site-index clojure.core.typed/Any
;           :protocol-on clojure.core.typed/Any}
;          :optional
;          {:method Nothing
;           :children Children
;           :Expr-obj Compiler$InvokeExpr})
;
;    ;keyword invoke
;    (HMap :mandatory
;          {:op ':keyword-invoke
;           :env Env
;           :kw Expr
;           :tag (U nil Symbol)
;           :target Expr}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$KeywordInvokeExpr})
;
;    ;the var
;    (HMap :mandatory
;          {:op ':the-var
;           :env Env
;           :var Var}
;          :optional
;          {:Expr-obj Compiler$TheVarExpr})
;
;    ;var
;    (HMap :mandatory
;          {:op ':var
;           :env Env
;           :var Var
;           :tag (U nil Symbol)}
;          :optional
;          {:Expr-obj Compiler$VarExpr})
;
;    ;unresolved var
;    (HMap :mandatory
;          {:op ':unresolved-var
;           :env Env
;           :sym Symbol}
;          :optional
;          {:Expr-obj Compiler$UnresolvedVarExpr})
;
;    ;objexpr
;    (HMap :mandatory
;          {:op ':obj-expr
;           :env Env
;           :tag (U nil Symbol)}
;          :optional
;          {:Expr-obj Compiler$ObjExpr})
;
;    ;new instance method
;    (HMap :mandatory
;          {:op ':new-instance-method
;           :env Env
;           :name Symbol
;           :required-params (Seqable Expr)
;           :body Expr}
;          :optional
;          {:children Children
;           :ObjMethod-obj Compiler$NewInstanceMethod})
;
;    ;fn method
;    (HMap :mandatory
;          {:op ':fn-method
;           :env Env
;           :body Expr
;           :required-params (Seqable Expr)
;           :rest-param (U nil Expr)}
;          :optional
;          {:children Children
;           :ObjMethod-obj Compiler$FnMethod})
;
;    ;fn expr
;    (HMap :mandatory
;          {:op ':fn-expr
;           :env Env
;           :methods (Seqable Expr)
;           :variadic-method (U nil Expr)
;           :tag (U nil Symbol)}
;          :optional
;          {:name Symbol
;           :children Children
;           :Expr-obj Compiler$FnExpr})
;
;    ;new instance expr
;    (HMap :mandatory
;          {:op ':deftype*
;           :name Symbol
;           :env Env
;           :methods (Seqable Expr)
;           :mmap clojure.core.typed/Any
;           :compiled-class Class
;           :internal-name Symbol
;           :this-name Symbol
;           :fields (IPersistentSet Expr)
;           :covariants clojure.core.typed/Any
;           :tag (U nil Symbol)}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$NewInstanceExpr})
;
;    ;instanceof
;    (HMap :mandatory
;          {:op ':instance-of
;           :env Env
;           :class Class
;           :the-expr Expr}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$InstanceOfExpr})
;
;    ;meta
;    (HMap :mandatory
;          {:op ':meta
;           :env Env
;           :meta Expr
;           :expr Expr}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$MetaExpr})
;
;    ;do
;    (HMap :mandatory
;          {:op ':do
;           :env Env
;           :exprs (Seqable Expr)}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$BodyExpr})
;
;    ;if
;    (HMap :mandatory
;          {:op ':if
;           :env Env
;           :test Expr
;           :then Expr
;           :else Expr}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$IfExpr})
;
;    ;case
;    (HMap :mandatory
;          {:op ':case*
;           :env Env
;           :the-expr Expr
;           :tests (Seqable Expr)
;           :thens (Seqable Expr)
;           :default Expr
;           :tests-hashes clojure.core.typed/Any
;           :shift clojure.core.typed/Any
;           :mask clojure.core.typed/Any
;           :test-type clojure.core.typed/Any
;           :switch-type clojure.core.typed/Any
;           :skip-check clojure.core.typed/Any}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$CaseExpr})
;
;    ;import
;    (HMap :mandatory
;          {:op ':import*
;           :env Env
;           :class-str String}
;          :optional
;          {:Expr-obj Compiler$ImportExpr})
;
;    ;set!
;    (HMap :mandatory
;          {:op ':set!
;           :env Env
;           :target Expr
;           :val Expr}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$AssignExpr})
;
;    ;catch
;    (HMap :mandatory
;          {:op ':catch
;           :env Env
;           :class Class
;           :local-binding Expr
;           :handler Expr}
;          :optional
;          {:children Children
;           :CatchClause-obj Compiler$TryExpr$CatchClause})
;
;    ;try
;    (HMap :mandatory
;          {:op ':try
;           :env Env
;           :try-expr Expr
;           :finally-expr (U nil Expr)
;           :catch-exprs (Seqable Expr)
;           :ret-local clojure.core.typed/Any
;           :finally-local clojure.core.typed/Any}
;          :optional
;          {:children Children
;           :Expr-obj Compiler$TryExpr})
;
;    ;recur
;    (HMap :mandatory
;          {:op ':recur
;           :env Env
;           :loop-locals (Seqable Expr)
;           :args (Seqable Expr)}
;          :optional
;          {:children Children
;           :Expr-Obj Compiler$RecurExpr})
;
;    ;method param
;    (HMap :mandatory
;          {:op ':method-param
;           :env Env
;           :class Class
;           :can-emit-primitive clojure.core.typed/Any}
;          :optional
;          {:Expr-obj Compiler$MethodParamExpr}))))
;      ))

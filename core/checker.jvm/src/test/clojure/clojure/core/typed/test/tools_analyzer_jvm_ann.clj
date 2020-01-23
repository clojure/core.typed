(ns clojure.core.typed.test.tools-analyzer-jvm-ann
  (:require [clojure.core.typed :as t]))


(t/defalias NsMap
  (t/HMap :mandatory
        {:mappings (t/Map t/Symbol t/Any) ;contradicts docs, which say (Map Symbol Var). What about Classes?
         :aliases (t/Map t/Symbol t/Symbol)
         :ns t/Symbol}))

(t/defalias Env
  (t/HMap :mandatory
        {:locals (t/Map t/Symbol t/Any #_Expr)
         :context (t/U ':return
                     ':statement
                     ':expr)
         :ns t/Symbol
         ;:namespaces (t/Atom1 (t/Map t/Symbol NsMap))
         }
        :optional
        {
        ; ---- start t.a.j specific -----
        ; added in :deftype
        ; not sure what :this is
         :this t/Any
        ; ---- end t.a.j specific -----
         }
        ))

(t/defalias Form t/Any)

(t/defalias Children (t/Vec t/Any))

(t/defalias Tag t/Any)
(t/defalias OTag t/Any)

; Notes
; - :case-test entry added by :case is not here

(t/defalias Expr
  (t/Rec [Expr]
  ; ----- start tools.analyzer specific ----
  (t/U (t/HMap :mandatory
           {:op ':const
            :env Env
            :type t/Keyword
            :literal Boolean
            :val Form
            :form Form}
           :optional
           {:meta Expr
            :children Children})
     (t/HMap :mandatory
           {:op ':vector
            :env Env
            :items (t/Vec Expr)
            :form Form
            :children Children})
     (t/HMap :mandatory
           {:op ':with-meta
            :env Env
            :form Form
            :meta Expr
            :expr Expr
            :children Children})
     (t/HMap :mandatory
           {:op ':map
            :env Env
            :keys (t/Vec Expr)
            :vals (t/Vec Expr)
            :form Form
            :children Children})
     (t/HMap :mandatory
           {:op ':set
            :env Env
            :items (t/Vec Expr)
            :form Form
            :children Children})
     (t/HMap :mandatory
           {:op ':local
            :form Form
            :name t/Symbol
            :assignable? Boolean
            :children Children}
           :optional
           ;env is sometimes omitted. eg. :local entry in :fn-method
           {:env Env})
     (t/HMap :mandatory
           {:op ':var
            :env Env
            :form Form
            :assignable? Boolean
            ;:var (t/Var2 Nothing t/Any)
            })
     (t/HMap :mandatory
           {:op ':maybe-host-form
            :env Env
            :form Form
            :class (t/U nil t/Symbol)
            :field t/Symbol})
     (t/HMap :mandatory
           {:op ':maybe-class
            :env Env
            :form Form
            ; is this just Symbol?
            :class (t/U nil t/Symbol)})
     (t/HMap :mandatory
           {:op ':do
            :env Env
            :form Form
            :statements (t/Vec Expr)
            :ret Expr
            :children Children})
     (t/HMap :mandatory
           {:op ':if
            :env Env
            :form Form
            :test Expr
            :then Expr
            :else Expr
            :children Children})
     (t/HMap :mandatory
           {:op ':new
            :env Env
            :form Form
            :class Class
            :args (t/Vec Expr)
            :children Children})
     (t/HMap :mandatory
           {:op ':quote
            :env Env
            :form Form
            :expr Expr
            :literal? Boolean
            :children Children})
     (t/HMap :mandatory
           {:op ':set!
            :env Env
            :form Form
            :target Expr
            :val Expr
            :children Children})
     (t/HMap :mandatory
           {:op ':try
            :env Env
            :form Form
            :body Expr
            :catches (t/Vec Expr)
            :children Children}
           :optional
           {:finally Expr})
     (t/HMap :mandatory
           {:op ':binding
            :env Env
            :form Form
            :name t/Symbol
            :local t/Any}
           :optional
           {:tag Tag
            :arg-id t/Int
            :variadic? Boolean
            ;---- start t.a.j specific ----
            ; not sure what o-tag is
            :o-tag OTag
            ; added in :deftype
            :mutable t/Any
            ;---- end t.a.j specific ----
            })
     (t/HMap :mandatory
           {:op ':catch
            :env Env
            :form Form
            :class Class
            :local Expr
            :body Expr
            :children Children})
     (t/HMap :mandatory
           {:op ':throw
            :env Env
            :form Form
            :exception Expr})
     (t/HMap :mandatory
           {:op ':letfn
            :env Env
            :form Form
            :body Expr
            :bindings (t/Vec Expr)
            :children Children})
     (t/HMap :mandatory
           {:op ':let
            :form Form
            :env Env
            :body Expr
            :bindings (t/Vec Expr)
            :children Children})
     (t/HMap :mandatory
           {:op ':loop
            :form Form
            :env Env
            :loop-id t/Symbol
            :body Expr
            :bindings (t/Vec Expr)
            :children Children})
     (t/HMap :mandatory
           {:op ':recur
            :env Env
            :form Form
            :exprs (t/Vec Expr)
            :loop-id t/Symbol
            :children Children})
     (t/HMap :mandatory
           {:op ':fn-method
            :env Env
            :form Form
            :loop-id t/Symbol
            :variadic? Boolean
            :params (t/Vec Expr)
            :fixed-arity t/Int
            :body Expr
            :children Children}
           :optional
           {:local Expr})
     (t/HMap :mandatory
           {:op ':fn
            :env Env
            :form Form
            ;unsure if nilable
            :name (t/U nil t/Symbol)
            :variadic? Boolean
            :max-fixed-arity (t/U nil t/Int)
            :methods (t/Vec Expr)
            :children Children}
           :optional
           {:local Expr})
     (t/HMap :mandatory
           {:op ':def
            :env Env
            :form Form
            :name t/Symbol
            ;:var (t/Var2 Nothing t/Any)
            }
           :optional
           {:meta Expr
            :init Expr
            :doc String
            :children Children})
     (t/HMap :mandatory
           {:op ':host-call
            :env Env
            :form Form
            :target Expr
            :method t/Symbol
            :args (t/Vec Expr)
            :children Children})
     (t/HMap :mandatory
           {:op ':host-field
            :env Env
            :form Form
            :target Expr
            :field t/Symbol
            :children Children})
     (t/HMap :mandatory
           {:op ':host-interop
            :env Env
            :form Form
            :target Expr
            :m-or-f t/Symbol
            :children Children})
     (t/HMap :mandatory
           {:op ':invoke
            :env Env
            :form Form
            :fn Expr
            :args (t/Vec Expr)
            :children Children}
           :optional
           {:meta Expr})
; ---- end tools.analyzer specific -----
           
; ---- start tools.analyzer.jvm specific ----

     (t/HMap :mandatory
           {:op ':the-var
            :env Env
            :form Form
            ;:var (t/Var2 Nothing t/Any)
            })
     (t/HMap :mandatory
           {:op ':monitor-enter
            :env Env
            :form Form
            :target Expr
            :children Children})
     (t/HMap :mandatory
           {:op ':monitor-exit
            :env Env
            :form Form
            :target Expr
            :children Children})
     (t/HMap :mandatory
           {:op ':import
            :env Env
            :form Form
            ;cannot be a Class. j.t.a calls this :class-str
            :class String})
     (t/HMap :mandatory
           {:op ':method
            :env Env
            :form Form
            :loop-id t/Symbol
            :params (t/Vec Expr)
            :fixed-arity t/Int
            :body Expr
            :children Children}
           :optional
           ; possibly inherited from :fn-method
           {:local Expr
            ; added in :reify
            :interfaces (t/Set (t/U nil Class))
            })
     (t/HMap :mandatory
           {:op ':reify
            :env Env
            :form Form
            :class-name t/Symbol
            :methods (t/Vec Expr)
            :children Children})
     (t/HMap :mandatory
           {:op ':deftype
            :env Env
            :form Form
            :name t/Symbol
            :class-name t/Symbol
            :fields (t/Vec Expr)
            :methods (t/Vec Expr)
            ; not sure what :interfaces is
            :interfaces (t/Set t/Any)
            :children Children})
     (t/HMap :mandatory
           {:op ':case-test
            ;TODO
            :hash t/Any
            :test Expr
            :children Children})
     (t/HMap :mandatory
           {:op ':case-then
            ;TODO
            :hash t/Any
            :then Expr
            :children Children})
     (t/HMap :mandatory
           {:op ':case
            :env Env
            :form Form
            :test Expr
            :default Expr
            :tests (t/Vec Expr)
            :thens (t/Vec Expr)
            ;TODO
            :shift t/Any
            :mask t/Any
            :low t/Any
            :high t/Any
            :switch-type t/Any
            :test-type t/Any
            :skip-check? Boolean
            :children Children})

;       --- start analyze-host-expr pass ---
      ;maybe-instance-method / analyze-host-call
      (t/HMap :mandatory
            {:op ':instance-call
             :form Form
             :env Env
             :tag Tag
             :o-tag OTag
             :instance Expr
             :method t/Symbol
             :class Class
             :children Children}
            :optional
            {:args (t/Vec Expr)}) ;absent in maybe-instance-method
      ;maybe-static-method / analyze-host-call
      (t/HMap :mandatory
            {:op ':static-call
             :form Form
             :env Env
             :tag Tag
             :o-tag OTag
             :class Class
             :method t/Symbol}
            :optional
            {:args (t/Vec Expr)}) ; absent in maybe-static-method
      ;maybe-static-field / analyze-host-field
      (t/HMap :mandatory
            {:op ':static-field
             :form Form
             :env Env
             :assignable? Boolean
             :class Class
             :field t/Symbol
             :o-tag OTag
             :tag Tag})
      ;maybe-instance-field / analyze-host-field
      (t/HMap :mandatory
            {:op ':instance-field
             :form Form
             :env Env
             :assignable? Boolean
             :class Class
             :instance Expr
             :field t/Symbol
             :o-tag OTag
             :tag Tag
             :children Children})
;       --- end analyze-host-expr pass ---

;       --- start classify-invoke pass ---
      (t/HMap :mandatory
            {:op ':keyword-invoke
             :env Env
             :form Form
             :fn Expr
             :args (t/Vec Expr)
             :children Children}
            :optional
            {:meta Expr})

      (t/HMap :mandatory
            {:op ':instance?
             :env Env
             :form Form
             :class Class
             :target Expr
             :o-tag OTag
             :tag Tag
             :children Children})

      (t/HMap :mandatory
            {:op ':protocol-invoke
             :env Env
             :form Form
             :fn Expr
             :args (t/Vec Expr)
             :children Children}
            :optional
            {:meta Expr})

      (t/HMap :mandatory
            {:op ':prim-invoke
             :env Env
             :form Form
             :fn Expr
             :prim-interface Class
             :args (t/Vec Expr)
             :tag Tag
             :o-tag OTag
             :children Children}
            :optional
            {:meta Expr})


; ---- end tools.analyzer.jvm specific ----

)))

(time
  ((t/pred Expr)
   1))

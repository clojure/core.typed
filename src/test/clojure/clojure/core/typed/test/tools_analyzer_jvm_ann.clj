(ns clojure.core.typed.test.tools-analyzer-jvm-ann
  (:require [clojure.core.typed :as t]))


(t/def-alias NsMap
  (HMap :mandatory
        {:mappings (t/Map t/Symbol Any) ;contradicts docs, which say (Map Symbol Var). What about Classes?
         :aliases (t/Map t/Symbol t/Symbol)
         :ns t/Symbol}))

(t/def-alias Env
  (HMap :mandatory
        {:locals (t/Map t/Symbol Any #_Expr)
         :context (U ':return
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
         :this Any
        ; ---- end t.a.j specific -----
         }
        ))

(t/def-alias Form Any)

(t/def-alias Children (t/Vec Any))

(t/def-alias Tag Any)
(t/def-alias OTag Any)

; Notes
; - :case-test entry added by :case is not here

(t/def-alias Expr
  (Rec [Expr]
  ; ----- start tools.analyzer specific ----
  (U (HMap :mandatory
           {:op ':const
            :env Env
            :type t/Keyword
            :literal Boolean
            :val Form
            :form Form}
           :optional
           {:meta Expr
            :children Children})
     (HMap :mandatory
           {:op ':vector
            :env Env
            :items (t/Vec Expr)
            :form Form
            :children Children})
     (HMap :mandatory
           {:op ':with-meta
            :env Env
            :form Form
            :meta Expr
            :expr Expr
            :children Children})
     (HMap :mandatory
           {:op ':map
            :env Env
            :keys (t/Vec Expr)
            :vals (t/Vec Expr)
            :form Form
            :children Children})
     (HMap :mandatory
           {:op ':set
            :env Env
            :items (t/Vec Expr)
            :form Form
            :children Children})
     (HMap :mandatory
           {:op ':local
            :form Form
            :name t/Symbol
            :assignable? Boolean
            :children Children}
           :optional
           ;env is sometimes omitted. eg. :local entry in :fn-method
           {:env Env})
     (HMap :mandatory
           {:op ':var
            :env Env
            :form Form
            :assignable? Boolean
            ;:var (t/Var2 Nothing Any)
            })
     (HMap :mandatory
           {:op ':maybe-host-form
            :env Env
            :form Form
            :class (U nil t/Symbol)
            :field t/Symbol})
     (HMap :mandatory
           {:op ':maybe-class
            :env Env
            :form Form
            ; is this just Symbol?
            :class (U nil t/Symbol)})
     (HMap :mandatory
           {:op ':do
            :env Env
            :form Form
            :statements (t/Vec Expr)
            :ret Expr
            :children Children})
     (HMap :mandatory
           {:op ':if
            :env Env
            :form Form
            :test Expr
            :then Expr
            :else Expr
            :children Children})
     (HMap :mandatory
           {:op ':new
            :env Env
            :form Form
            :class Class
            :args (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':quote
            :env Env
            :form Form
            :expr Expr
            :literal? Boolean
            :children Children})
     (HMap :mandatory
           {:op ':set!
            :env Env
            :form Form
            :target Expr
            :val Expr
            :children Children})
     (HMap :mandatory
           {:op ':try
            :env Env
            :form Form
            :body Expr
            :catches (t/Vec Expr)
            :children Children}
           :optional
           {:finally Expr})
     (HMap :mandatory
           {:op ':binding
            :env Env
            :form Form
            :name t/Symbol
            :local Any}
           :optional
           {:tag Tag
            :arg-id t/Int
            :variadic? Boolean
            ;---- start t.a.j specific ----
            ; not sure what o-tag is
            :o-tag OTag
            ; added in :deftype
            :mutable Any
            ;---- end t.a.j specific ----
            })
     (HMap :mandatory
           {:op ':catch
            :env Env
            :form Form
            :class Class
            :local Expr
            :body Expr
            :children Children})
     (HMap :mandatory
           {:op ':throw
            :env Env
            :form Form
            :exception Expr})
     (HMap :mandatory
           {:op ':letfn
            :env Env
            :form Form
            :body Expr
            :bindings (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':let
            :form Form
            :env Env
            :body Expr
            :bindings (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':loop
            :form Form
            :env Env
            :loop-id t/Symbol
            :body Expr
            :bindings (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':recur
            :env Env
            :form Form
            :exprs (t/Vec Expr)
            :loop-id t/Symbol
            :children Children})
     (HMap :mandatory
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
     (HMap :mandatory
           {:op ':fn
            :env Env
            :form Form
            ;unsure if nilable
            :name (U nil t/Symbol)
            :variadic? Boolean
            :max-fixed-arity (U nil t/Int)
            :methods (t/Vec Expr)
            :children Children}
           :optional
           {:local Expr})
     (HMap :mandatory
           {:op ':def
            :env Env
            :form Form
            :name t/Symbol
            ;:var (t/Var2 Nothing Any)
            }
           :optional
           {:meta Expr
            :init Expr
            :doc String
            :children Children})
     (HMap :mandatory
           {:op ':host-call
            :env Env
            :form Form
            :target Expr
            :method t/Symbol
            :args (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':host-field
            :env Env
            :form Form
            :target Expr
            :field t/Symbol
            :children Children})
     (HMap :mandatory
           {:op ':host-interop
            :env Env
            :form Form
            :target Expr
            :m-or-f t/Symbol
            :children Children})
     (HMap :mandatory
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

     (HMap :mandatory
           {:op ':the-var
            :env Env
            :form Form
            ;:var (t/Var2 Nothing Any)
            })
     (HMap :mandatory
           {:op ':monitor-enter
            :env Env
            :form Form
            :target Expr
            :children Children})
     (HMap :mandatory
           {:op ':monitor-exit
            :env Env
            :form Form
            :target Expr
            :children Children})
     (HMap :mandatory
           {:op ':import
            :env Env
            :form Form
            ;cannot be a Class. j.t.a calls this :class-str
            :class String})
     (HMap :mandatory
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
            :interfaces (t/Set (U nil Class))
            })
     (HMap :mandatory
           {:op ':reify
            :env Env
            :form Form
            :class-name t/Symbol
            :methods (t/Vec Expr)
            :children Children})
     (HMap :mandatory
           {:op ':deftype
            :env Env
            :form Form
            :name t/Symbol
            :class-name t/Symbol
            :fields (t/Vec Expr)
            :methods (t/Vec Expr)
            ; not sure what :interfaces is
            :interfaces (t/Set Any)
            :children Children})
     (HMap :mandatory
           {:op ':case-test
            ;TODO
            :hash Any
            :test Expr
            :children Children})
     (HMap :mandatory
           {:op ':case-then
            ;TODO
            :hash Any
            :then Expr
            :children Children})
     (HMap :mandatory
           {:op ':case
            :env Env
            :form Form
            :test Expr
            :default Expr
            :tests (t/Vec Expr)
            :thens (t/Vec Expr)
            ;TODO
            :shift Any
            :mask Any
            :low Any
            :high Any
            :switch-type Any
            :test-type Any
            :skip-check? Boolean
            :children Children})

;       --- start analyze-host-expr pass ---
      ;maybe-instance-method / analyze-host-call
      (HMap :mandatory
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
      (HMap :mandatory
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
      (HMap :mandatory
            {:op ':static-field
             :form Form
             :env Env
             :assignable? Boolean
             :class Class
             :field t/Symbol
             :o-tag OTag
             :tag Tag})
      ;maybe-instance-field / analyze-host-field
      (HMap :mandatory
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
      (HMap :mandatory
            {:op ':keyword-invoke
             :env Env
             :form Form
             :fn Expr
             :args (t/Vec Expr)
             :children Children}
            :optional
            {:meta Expr})

      (HMap :mandatory
            {:op ':instance?
             :env Env
             :form Form
             :class Class
             :target Expr
             :o-tag OTag
             :tag Tag
             :children Children})

      (HMap :mandatory
            {:op ':protocol-invoke
             :env Env
             :form Form
             :fn Expr
             :args (t/Vec Expr)
             :children Children}
            :optional
            {:meta Expr})

      (HMap :mandatory
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

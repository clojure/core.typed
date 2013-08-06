(ns cljs.core.typed
  "Macros for Clojurescript type checking")

; many of these macros resolve to CLJS functions in 
; the CLJS ns cljs.core.typed

(defmacro ann-form 
  "Annotate a form with an expected type."
  [form ty]
  `(ann-form* ~form '~ty))

(defmacro ann 
  "Annotate varsym with type. If unqualified, qualify in the current namespace.
  If varsym has metadata {:no-check true}, ignore definitions of varsym while type checking.
  
  eg. ; annotate the var foo in this namespace
      (ann foo [Number -> Number])
  
      ; annotate a var in another namespace
      (ann another.ns/bar [-> nil])
   
      ; don't check this var
      (ann ^:no-check foobar [Integer -> String])"
  [varsym typesyn]
  (let []
    `(ann* '~(symbol varsym) '~typesyn '~(meta varsym))))

(defmacro check-ns
  ([] (check-cljs-ns*) :ok)
  ([nsym] (check-cljs-ns* nsym) :ok))

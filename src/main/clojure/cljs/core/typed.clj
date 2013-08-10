(ns cljs.core.typed
  "Macros for Clojurescript type checking"
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl :refer [v]]
            [cljs.analyzer :as ana]
            [cljs.compiler :as comp]))

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
  `(ann* '~varsym '~typesyn))

(defmacro 
  ^{:forms '[(ann-protocol vbnd varsym & methods)
             (ann-protocol varsym & methods)]}
  ann-protocol 
  "Annotate a possibly polymorphic protocol var with method types.
  
  eg. (ann-protocol IFoo
        bar
        [IFoo -> Any]
        baz
        [IFoo -> Number])

      ; polymorphic
      (ann-protocol [[x :variance :covariant]]
        IFoo
        bar
        [IFoo -> Any]
        baz
        [IFoo -> Number])"
  [& args]
  (let [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        varsym (if bnd-provided?
                 (second args)
                 (first args))
        {:as mth} (if bnd-provided?
                    (next (next args))
                    (next args))]
    `(ann-protocol* '~vbnd '~varsym '~mth)))

(def ^:dynamic *currently-checking-cljs* nil)
(def ^{:doc "Internal use only"} ^:skip-wiki ^:dynamic *delayed-errors*)
(def ^:dynamic *already-collected*)

(defn ^:skip-wiki
  -init-delayed-errors 
  "Internal use only"
  []
  (atom [] :validator #(and (vector? %)
                            (every? (fn [a] 
                                      (instance? clojure.lang.ExceptionInfo a))
                                    %))))

(defn cf* [form expected expected-provided?]
  (t/load-if-needed)
  (t/reset-caches)
  (comp/with-core-cljs
    (if *currently-checking-cljs*
      (throw (Exception. "Found inner call to check-ns or cf"))
      (binding [*currently-checking-cljs* true
                *delayed-errors* (-init-delayed-errors)]
        (impl/with-cljs-impl
          (let [ast ((v 'clojure.core.typed.analyze-cljs/ast-for-form) form)
                ;collect
                _ ((v 'clojure.core.typed.collect-cljs/collect) ast)
                ;check
                c-ast ((v 'clojure.core.typed.check-cljs/check) ast
                       (when expected-provided?
                         ((v 'clojure.core.typed.type-rep/ret)
                          ((v 'clojure.core.typed.parse-unparse/parse-type) expected))))]
            ;handle errors
            (if-let [errors (seq @*delayed-errors*)]
              (t/print-errors! errors)
              (-> c-ast 
                  ((v 'clojure.core.typed.check/expr-type))
                  ((v 'clojure.core.typed.parse-unparse/unparse-TCResult-in-ns) ana/*cljs-ns*)))))))))

(defmacro cf
  "Check a single form with an optional expected type."
  ([form] `(cf* '~form nil nil))
  ([form expected] `(cf* '~form '~expected true)))

(defn check-ns
  "Check a Clojurescript namespace, or the current namespace."
  ([] (check-ns ana/*cljs-ns*))
  ([nsym]
   (t/load-if-needed)
   (t/reset-caches)
   ((v 'clojure.core.typed.reset-env/reset-envs!))
   (comp/with-core-cljs
     (if *currently-checking-cljs*
       (throw (Exception. "Found inner call to check-ns or cf"))
       (do
         (t/load-if-needed)
         (binding [*currently-checking-cljs* true
                   *already-collected* (atom #{})
                   *delayed-errors* (-init-delayed-errors)]
           (impl/with-cljs-impl
             (let [_ ((v 'clojure.core.typed.collect-cljs/collect-ns) nsym)
                   _ ((v 'clojure.core.typed.check-cljs/check-ns) nsym)]
               ;handle errors
               (if-let [errors (seq @*delayed-errors*)]
                 (t/print-errors! errors)
                 :ok)))))))))

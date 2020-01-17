;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.analyze-cljs
  (:refer-clojure :exclude [extenders])
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.util-cljs :as uc]
            [clojure.core.typed.analyzer-api-intercept :as api]
            [cljs.analyzer :as ana]
            [clojure.tools.reader :as reader]
            [clojure.java.io :as io]
            [cljs.env :as env]
            [cljs.util :as cljsu]))

(declare ast-for-form)

(defn ast-for-form-in-ns
  "Returns an AST node for the form 
  analyzed in the given namespace"
  [nsym form]
  (uc/with-cljs-typed-env
    (binding [ana/*cljs-ns* nsym]
      (ast-for-form form {:eval-fn false}))))

; FIXME reintroduce hygienic transformation!
(defn ast-for-form
  "Returns an AST node for the form"
  [form {:keys [expected eval-fn] :as opt}]
  ;; TODO support bindings-atom, as in c.c.t.analyze-clj
  ;; TODO propagate analyzer env from opt
  (uc/with-cljs-typed-env
    (let [ast (api/analyze (api/empty-env) form)]
      ;(prn "ast-for-form" (:op ast))
      (if eval-fn
        (eval-fn ast opt)
        ast))))

; like cljs.analyze/analyze-form-seq, but returns a vector of
; analysis results
(defn analyze-form-seq
  ([forms]
   (analyze-form-seq forms nil))
  ([forms opts]
    (uc/with-cljs-typed-env
     (let [env (assoc (api/empty-env) :build-options opts)]
       (binding [ana/*file-defs* nil
                 ;#?@(:clj [*unchecked-if* false])
                 ana/*unchecked-if* false
                 ana/*cljs-ns* 'cljs.user
                 ana/*cljs-file* nil
                 reader/*alias-map* (or reader/*alias-map* {})]
         (loop [ns nil forms forms
                out []]
           (if (some? forms)
             (let [form (first forms)
                   env  (assoc env :ns (ana/get-namespace ana/*cljs-ns*))
                   ast  (api/analyze env form nil opts)]
               (if (= (:op ast) :ns)
                 (recur (:name ast) (next forms) (conj out ast))
                 (recur ns (next forms) (conj out ast))))
             out)))))))

;; FIXME hygienic transformation
(defn ast-for-ns 
  "Returns a vector of AST nodes contained
  in the given namespace symbol nsym"
  [nsym]
  {:pre [(symbol? nsym)]}
  (analyze-form-seq (ana/forms-seq* (io/reader (cljsu/ns->source nsym)))))

(let [get-namespace (delay (impl/dynaload 'cljs.analyzer/get-namespace))]
  (defn extenders
    "Returns a set of descendants for a protocol"
    [psym]
    {:pre [(symbol? psym)
           (namespace psym)]}
    (assert nil "FIXME extenders")
    (require 'cljs.analyzer)
    (or (get-in (@get-namespace (symbol (namespace psym)))
                [:defs (symbol (name psym)) :impls])
        #{})))

(let [analyze-symbol (delay (impl/dynaload 'cljs.analyzer/analyze-symbol))
      empty-env (delay (impl/dynaload 'cljs.analyzer/empty-env))]
  (defn analyze-qualified-symbol 
    "Return a var expr that the fully qualified symbol names"
    [sym]
    {:pre [(symbol? sym)] 
     :post [(= :var (:op %))]}
    ;; is this still correct?
    (assert nil "FIXME analyze-qualified-symbol")
    (@analyze-symbol (@empty-env) sym)))

;(analyze-qualified-symbol 'cljs.core/ISeq)
;(analyze-qualified-symbol 'cljs.core.SubVec)


;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.print-env
  (:require [clojure.core.typed.checker.lex-env :as lex]
            [clojure.core.typed.checker.tvar-env :as tvar-env]
            [clojure.core.typed.checker.tvar-bnds :as tvar-bnds]
            [clojure.core.typed.checker.jvm.parse-unparse :as prs]))

(defn print-env*
  ([] (print-env* (lex/lexical-env)))
  ([e]
   {:pre [(lex/PropEnv? e)]}
   ;; DO NOT REMOVE
   (let [tvar-scope tvar-env/*current-tvars*
         tvar-bounds tvar-bnds/*current-tvar-bnds*
         scoped-names (keys tvar-scope)
         actual-names (map :name (vals tvar-scope))
         _ (every? symbol? actual-names)
         actual-bnds (map tvar-bounds actual-names)]
     (prn {:env (into {} (for [[k v] (:l e)]
                           [k (prs/unparse-type v)]))
           :props (map prs/unparse-filter (:props e))
           :aliases (:aliases e)
           ;:frees (map (t/fn
           ;              [nme :- t/Sym, bnd :- (U nil Bounds)]
           ;              {:pre [(symbol? nme)
           ;                     ((some-fn nil? r/Bounds?) bnd)]}
           ;              (if bnd
           ;                (prs/unparse-poly-bounds-entry nme bnd)
           ;                [nme 'NO-BOUNDS]))
           ;            scoped-names
           ;            actual-bnds)
           ;:tvar-scope tvar-scope
           ;:tvar-bnds tvar-bounds
           }))))

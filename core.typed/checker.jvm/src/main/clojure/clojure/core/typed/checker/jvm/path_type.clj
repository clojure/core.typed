;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.jvm.path-type
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.path-rep :as pe]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.checker.path-rep :as pr]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.jvm.subtype :as sub]
            [clojure.core.typed :as t]
            [clojure.core.typed.errors :as err])
  (:import (clojure.lang Keyword Symbol)))

; ported from Typed Racket, originally by Andrew Kent
;; returns the result of following a path into a type
;;  Type (Listof PathElem)-> Type
;; Ex. (Pair α ) β'(CarPE) -> α
;; resolved is the set of resolved types so far at a particular
;; path - it ensures we are making progress, that we do not
;; continue unfolding types infinitely while not progressing.
;; It is intentionally reset each time we decrease the
;; paths size on a recursive call, and maintained/extended
;; when the path does not decrease on a recursive call.
(defn path-type
  ([t ps] (path-type t ps #{}))
  ([t ps resolved]
   {:pre [(r/Type? t)
          (pr/path-elems? ps)
          (con/set-c? r/Type?)]
    :post [(r/Type? %)]}
   (let [t (c/fully-resolve-type t resolved)]
     (cond
       (empty? ps) t

       ((some-fn r/Union? r/Intersection?) t)
       (apply (if (r/Union? t) c/Un c/In)
              (map (fn [t*]
                     {:post [(r/Type? %)]}
                     (path-type t* ps resolved))
                   (:types t)))

       (pe/KeysPE? (first ps))
       (c/-name 
         'clojure.core.typed/ASeq
         (cond
           (r/HeterogeneousMap? t) (c/RClass-of clojure.lang.Keyword)
           (r/RClass? t) (let [_ (assert (= (:the-class t) 'clojure.lang.IPersistentMap))
                               _ (assert (= 2 (count (:poly? t))))]
                           (first (:poly? t)))
           :else (err/int-error (str "Bad call to path-type: bad KeysPE, " (pr-str t) ", " (pr-str ps)))))

       (pe/ValsPE? (first ps))
       (c/-name 
         'clojure.core.typed/ASeq
         (cond
           (r/HeterogeneousMap? t) (apply c/Un (mapcat vals [(:mandatory t) (:optional t)]))
           (r/RClass? t) (let [_ (assert (= (:the-class t) 'clojure.lang.IPersistentMap))
                               _ (assert (= 2 (count (:poly? t))))]
                           (second (:poly? t)))
           :else (err/int-error (str "Bad call to path-type: bad KeysPE, " (pr-str t) ", " (pr-str ps)))))


       (and (pe/KeyPE? (first ps))
            (r/HeterogeneousMap? t))
       (let [kpth (cu/KeyPE->Type (first ps))]
         (or (some-> ((:types t) kpth) (path-type (next ps)))
             (when-let [opt ((:optional t) kpth)]
               (path-type (c/Un r/-nil opt) (next ps)))
             (when ((:absent-keys t) kpth)
               (path-type r/-nil (next ps)))
             (path-type 
               (if (c/complete-hmap? t)
                 r/-nil 
                 r/-any)
               (next ps))))

       (and (pe/KeyPE? (first ps))
            (sub/subtype? t r/-nil))
       (path-type r/-nil (next ps))

       (and (pe/KeyPE? (first ps))
            (r/Record? t))
       (let [ksym (-> (first ps) :val name symbol)
             _ (assert (symbol? ksym))]
         (get (:fields t) ksym r/-any))

       (pe/KeyPE? (first ps))
       (path-type r/-any (next ps))

       (pe/CountPE? (first ps))
       (path-type (r/Name-maker `t/Int) (next ps))

       (pe/ClassPE? (first ps))
       (path-type (c/Un r/-nil (c/RClass-of Class)) (next ps))

       (and (pe/NthPE? (first ps))
            (r/HSequential? t))
       (let [idx (:idx (first ps))]
         (path-type
           (or (nth (:types t) idx nil)
               (:rest t)
               r/-any)
           (next ps)))

       (and (pe/NthPE? (first ps))
            (sub/subtype? t r/-nil))
       ;; we don't know the default value, so could return anything
       (path-type r/-any (next ps))

       (pe/KeywordPE? (first ps))
       (path-type
         (cond
           ;; We know exactly what `keyword` yields if given a singleton type.
           ;; Feeding back into `keyword` gives us exactly what we want.
           (and (r/Value? t)
                ((some-fn symbol? string? keyword? nil? number?) (:val t))) 
           (r/-val (keyword (:val t)))

           ;; `keyword` applied to keywords, symbols, and strings return keywords.
           (sub/subtype? t
                         (c/Un (c/RClass-of Keyword)
                               (c/RClass-of Symbol)
                               (c/RClass-of String)))
           (c/RClass-of Keyword)

           ;; Bottom out with most general return value for `keyword`.
           :else (c/Un r/-nil (c/RClass-of Keyword)))
         (next ps))

       :else (err/int-error (str "Bad call to path-type: " (pr-str t) ", " (pr-str ps) ", " (mapv class ps)))))))

(ns ^:skip-wiki clojure.core.typed.path-type
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.path-rep :as pe]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.path-rep :as pr]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed :as t]
            #_[clojure.core.typed.debug :refer [dbg]]
            [clojure.core.typed.errors :as err])
  (:import (clojure.lang Keyword)))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

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
            (r/Nil? t))
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

       (and (pe/NthPE? (first ps))
            (r/HeterogeneousList? t))
       (path-type
         (c/HList->HSequential t)
         ps
         (conj resolved t))

       (and (pe/NthPE? (first ps))
            (c/AnyHSequential? t))
       (let [idx (:idx (first ps))]
         (path-type
           (or (nth (:types t) idx nil)
               (:rest t)
               r/-any)
           (next ps)))

       (and (pe/KeywordPE? (first ps))
            (r/Value? t))
       (let [{:keys [val]} t]
         (path-type
           (cond
             ;; feeding back into `keyword` gives us exactly what we want.
             ((some-fn symbol? string? keyword? nil? number?) val) (r/-val (keyword val))
             :else (c/Un r/-nil (c/RClass-of Keyword)))
           (next ps)))

       :else (err/int-error (str "Bad call to path-type: " (pr-str t) ", " (pr-str ps)))
       ))))

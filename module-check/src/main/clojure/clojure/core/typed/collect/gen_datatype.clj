(ns clojure.core.typed.collect.gen-datatype
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.repl :as repl]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.datatype-ancestor-env :as ancest]
            [clojure.core.typed.datatype-env :as dt-env]
            [clojure.core.typed.frees :as frees]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.method-override-env :as override]))

(defn parse-field [[n _ t]]
  [n (prs/parse-type t)])

(defn gen-datatype* [current-env current-ns provided-name fields vbnd opt record?]
  {:pre [(symbol? current-ns)]}
  (impl/with-clojure-impl
    (let [{ancests :unchecked-ancestors} opt
          ancests (or ancests (:extends opt))
          parsed-binders (when vbnd
                           (binding [prs/*parse-type-in-ns* current-ns]
                             (prs/parse-free-binder-with-variance vbnd)))
          ;variances
          vs (seq (map :variance parsed-binders))
          args (seq (map :fname parsed-binders))
          bnds (seq (map :bnd parsed-binders))]
      (let [provided-name-str (str provided-name)
            ;_ (prn "provided-name-str" provided-name-str)
            munged-ns-str (if (some #(= \. %) provided-name-str)
                            (apply str (butlast (apply concat (butlast (partition-by #(= \. %) provided-name-str)))))
                            (str (munge current-ns)))
            ;_ (prn "munged-ns-str" munged-ns-str)
            demunged-ns-str (str (repl/demunge munged-ns-str))
            ;_ (prn "demunged-ns-str" demunged-ns-str)
            local-name (if (some #(= \. %) provided-name-str)
                         (symbol (apply str (last (partition-by #(= \. %) (str provided-name-str)))))
                         provided-name-str)
            ;_ (prn "local-name" local-name)
            s (symbol (str munged-ns-str \. local-name))
            fs (apply array-map (apply concat (free-ops/with-frees (mapv r/make-F args)
                                                (binding [vs/*current-env* current-env
                                                          prs/*parse-type-in-ns* current-ns]
                                                  (mapv parse-field (partition 3 fields))))))
            as (set (free-ops/with-frees (mapv r/make-F args)
                      (binding [vs/*current-env* current-env
                                prs/*parse-type-in-ns* current-ns]
                        (mapv (comp #(c/abstract-many args %) prs/parse-type) ancests))))
            ;_ (prn "collected ancestors" as)
            _ (ancest/add-datatype-ancestors s as)
            pos-ctor-name (symbol demunged-ns-str (str "->" local-name))
            map-ctor-name (symbol demunged-ns-str (str "map->" local-name))
            dt (c/DataType* args vs (map r/make-F args) s bnds fs record?)
            _ (dt-env/add-datatype s dt)
            pos-ctor (if args
                       (c/Poly* args bnds
                                (r/make-FnIntersection
                                  (r/make-Function (vec (vals fs)) (c/DataType-of s (map r/make-F args)))))
                       (r/make-FnIntersection
                         (r/make-Function (vec (vals fs)) (c/DataType-of s))))
            map-ctor (when record?
                       (let [hmap-arg ; allow omission of keys if nil is allowed and field is monomorphic
                             (let [{optional true mandatory false} 
                                   (group-by (fn [[_ t]] (and (empty? (frees/fv t))
                                                              (empty? (frees/fi t))
                                                              (sub/subtype? r/-nil t)))
                                             (zipmap (map (comp r/-val keyword) (keys fs))
                                                     (vals fs)))]
                               (c/make-HMap :optional (into {} optional)
                                            :mandatory (into {} mandatory)))]
                         (if args
                           (c/Poly* args bnds
                                    (r/make-FnIntersection
                                      (r/make-Function [hmap-arg] (c/DataType-of s (map r/make-F args)))))
                           (r/make-FnIntersection
                             (r/make-Function [hmap-arg] (c/DataType-of s))))))]
        (do 
          ;(when vs
          ;  (let [f (mapv r/make-F (repeatedly (count vs) gensym))]
          ;    ;TODO replacements and unchecked-ancestors go here
          ;    (rcls/alter-class* s (c/RClass* (map :name f) vs f s {} {} bnds))))
          (var-env/add-var-type pos-ctor-name pos-ctor)
          (var-env/add-nocheck-var pos-ctor-name)
          (when record?
            (override/add-method-override (symbol (str s) "create") map-ctor)
            (var-env/add-var-type map-ctor-name map-ctor)))))))

(ns clojure.core.typed.base-env-helper
  (:refer-clojure :exclude [type])
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.declared-kind-env :as decl-env]
            [clojure.core.typed.rclass-env :as rcls]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.name-env]
            [clojure.core.typed.var-env]
            [clojure.core.typed.ns-deps]
            [clojure.core.typed.collect-phase :as coll]
            [clojure.pprint :as pprint]))

(defn qualify-in-cct [as]
  (for [[k v] (partition 2 as)]
    [(-> (symbol "clojure.core.typed" (name k))
         (with-meta (meta k)))
     v]))

(defmacro alias-mappings [& args]
  `(impl/with-clojure-impl
     (let [ts# (qualify-in-cct '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (let [meta# (-> s# meta)
                       desc# (:doc meta#)
                       doc# (str #_"Type Alias\n\n"
                                 (when desc#
                                   (str desc# "\n\n")) 
                                 (with-out-str (pprint/pprint t#)))
                       _# (assert (and (symbol? s#)
                                       (namespace s#))
                                  "Need fully qualified symbol")
                       v# (intern (find-ns (symbol (namespace s#))) (symbol (name s#)))
                       _# (alter-meta! v# merge (assoc meta# :doc doc#))]
                   [(with-meta s# nil) (prs/parse-type t#)])))))))

(defmacro var-mappings [this-ns & args]
  `(impl/with-clojure-impl
     (let [this-ns# ~this-ns
           _# (assert (instance? clojure.lang.Namespace this-ns#))
           ts# (partition 2 '~args)
           conveyed-parse# (fn [s#]
                             (binding [prs/*parse-type-in-ns* this-ns#]
                               (prs/parse-type s#)))]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (do
                   (assert (and (symbol? s#)
                                (namespace s#))
                           "Need fully qualified symbol")
                   [s# (delay (conveyed-parse# t#))])))))))

(defmacro method-nonnilable-return-mappings [& args]
  `(impl/with-clojure-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (do
                   (assert (and (symbol? s#)
                                (namespace s#))
                           "Need fully qualified symbol")
                   [s# t#])))))))

(defmacro method-nilable-param-mappings [& args]
  `(impl/with-clojure-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (do
                   (assert (and (symbol? s#)
                                (namespace s#))
                           "Need fully qualified symbol")
                   [s# t#])))))))

(defmacro method-override-mappings [& args]
  `(impl/with-clojure-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (do
                   (assert (and (symbol? s#)
                                (namespace s#))
                           "Need fully qualified symbol")
                   [s# (prs/parse-type t#)])))))))

(defmacro ctor-override-mappings [& args]
  `(impl/with-clojure-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (do
                   (assert (and (symbol? s#)
                                (not (namespace s#)))
                           "Need unqualified symbol")
                   [s# (prs/parse-type t#)])))))))

;(defmacro protocol-mappings [& args]
;  `(impl/with-clojure-impl
;     (let [ts# (partition 2 '~args)]
;       (into {}
;             (doall
;               (for [[s# [argsyn# {msyns# :methods}]] ts#]
;                 (let [ms# (into {}
;                                 (for [[msym# t#] msyns#]
;                                   [msym# (prs/parse-type t#)]))
;                       args# ]
;                   (assert (and (symbol? s#)
;                                (namespace s#))
;                           "Need qualified symbol")
;                   [s# ])))))))

;; Alter class

(defn- build-replacement-syntax [m]
  `(impl/with-clojure-impl
     (into {} (doall
                (for [[k# v#] ~m]
                  [(if-let [c# (resolve k#)] 
                     (and (class? c#) (coerce/Class->symbol c#))
                     k#)
                   (prs/parse-type v#)])))))

(defn resolve-class-symbol [the-class]
  `(impl/with-clojure-impl
     (let [cls# (when-let [c# (resolve ~the-class)]
                  (when (class? c#)
                    c#))]
       (assert cls# (str "Cannot resolve class " ~the-class))
       (or (and cls# (coerce/Class->symbol cls#))
           ~the-class))))

(defn make-RClass-syn [the-class frees-syn opts]
  (let [replacements-syn (gensym 'replacements-syn)
        fs (gensym 'fs)]
    `(impl/with-clojure-impl
       (let [{~replacements-syn :replace
              unchecked-ancestors-syn# :unchecked-ancestors} (apply hash-map ~opts)
             {variances# :variances
              nmes# :nmes
              bnds# :bnds}
             (when-let [fs# (seq ~frees-syn)]
               ; don't bound frees because mutually dependent bounds are problematic
               (let [b# (free-ops/with-free-symbols (mapv (fn [s#]
                                                            {:pre [(vector? s#)]
                                                             :post [(symbol? ~'%)]}
                                                            (first s#))
                                                          fs#)
                          (mapv prs/parse-tfn-binder fs#))]
                 {:variances (map :variance b#)
                  :nmes (map :nme b#)
                  :bnds (map :bound b#)}))
             frees# (map r/make-F nmes#)
             csym# ~(resolve-class-symbol the-class)
             frees-and-bnds# (zipmap frees# bnds#)]
         (assert ((con/hash-c? r/F? r/Bounds?) frees-and-bnds#) frees-and-bnds#)
         (c/RClass* nmes# variances# frees# csym#
                    (free-ops/with-bounded-frees frees-and-bnds#
                      ~(build-replacement-syntax replacements-syn))
                    (free-ops/with-bounded-frees frees-and-bnds#
                      (set (map prs/parse-type unchecked-ancestors-syn#)))
                    bnds#)))))

(defn declared-kind-for-rclass [fields]
  (let [fs (map first fields)
        _ (assert (every? symbol? fs) fs)
        vs (map (fn [[v & {:keys [variance]}]] variance) fields)]
    (c/TypeFn* fs vs (repeat (count vs) r/no-bounds) r/-any)))

(defmacro alters [& args]
  (let [fields (gensym 'fields)
        opts (gensym 'opts)
        s (gensym 's)]
    `(impl/with-clojure-impl
       (let [ts# (partition 2 '~args)]
         (into {}
               (doall
                 (for [[~s [~fields & ~opts]] ts#]
                   (let [sym# ~(resolve-class-symbol s)
                         decl-kind# (declared-kind-for-rclass ~fields)
                         _# (when (r/TypeFn? decl-kind#)
                              (decl-env/add-declared-kind sym# decl-kind#))
                         rcls# ~(make-RClass-syn s fields opts)]
                     ;accumulate altered classes in initial env
                     (rcls/alter-class* sym# rcls#)
                     (decl-env/remove-declared-kind sym#)
                     [sym# rcls#]))))))))

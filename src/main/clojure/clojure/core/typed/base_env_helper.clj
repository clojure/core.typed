(ns clojure.core.typed.base-env-helper
  (:refer-clojure :exclude [type])
  (:require [clojure.core.typed
             [type-rep :as r]
             [parse-unparse :as prs]
             [utils :as u]
             [free-ops :as free-ops]
             [type-ctors :as c]
             [declared-kind-env :as decl-env]
             [rclass-env :as rcls]]
            [clojure.pprint :as pprint]))

(defmacro alias-mappings [& args]
  `(let [ts# (partition 2 '~args)]
     (into {}
           (for [[s# t#] ts#]
             (let [desc# (-> s# meta :doc)
                   doc# (str (when desc#
                               (str desc# "\n\n")) 
                             (with-out-str (pprint/pprint t#)))
                   _# (assert (and (symbol? s#)
                                   (namespace s#))
                              "Need fully qualified symbol")
                   v# (intern (find-ns (symbol (namespace s#))) (symbol (name s#)))
                   _# (alter-meta! v# merge {:doc doc#})]
               [(with-meta s# nil) (prs/parse-type t#)])))))

(defmacro var-mappings [& args]
  `(let [ts# (partition 2 '~args)]
     (into {}
           (for [[s# t#] ts#]
             (do
               (assert (and (symbol? s#)
                            (namespace s#))
                       "Need fully qualified symbol")
               [s# (prs/parse-type t#)])))))

(defmacro method-nonnilable-return-mappings [& args]
  `(let [ts# (partition 2 '~args)]
     (into {}
           (for [[s# t#] ts#]
             (do
               (assert (and (symbol? s#)
                            (namespace s#))
                       "Need fully qualified symbol")
               [s# t#])))))

(defmacro method-nilable-param-mappings [& args]
  `(let [ts# (partition 2 '~args)]
     (into {}
           (for [[s# t#] ts#]
             (do
               (assert (and (symbol? s#)
                            (namespace s#))
                       "Need fully qualified symbol")
               [s# t#])))))

(defmacro method-override-mappings [& args]
  `(let [ts# (partition 2 '~args)]
     (into {}
           (for [[s# t#] ts#]
             (do
               (assert (and (symbol? s#)
                            (namespace s#))
                       "Need fully qualified symbol")
               [s# (prs/parse-type t#)])))))

(defmacro ctor-override-mappings [& args]
  `(let [ts# (partition 2 '~args)]
     (into {}
           (for [[s# t#] ts#]
             (do
               (assert (and (symbol? s#)
                            (not (namespace s#)))
                       "Need unqualified symbol")
               [s# (prs/parse-type t#)])))))

;; Alter class

(defn- build-replacement-syntax [m]
  `(into {} (for [[k# v#] ~m]
              [(if-let [c# (resolve k#)] 
                 (and (class? c#) (u/Class->symbol c#))
                 k#)
               (prs/parse-type v#)])))

(defn resolve-class-symbol [the-class]
  `(let [cls# (when-let [c# (resolve ~the-class)]
                (when (class? c#)
                  c#))]
     (assert cls# (str "Cannot resolve class " ~the-class))
     (or (and cls# (u/Class->symbol cls#))
         ~the-class)))

(defn make-RClass-syn [the-class frees-syn opts]
  (let [replacements-syn (gensym 'replacements-syn)
        fs (gensym 'fs)]
    `(let [{~replacements-syn :replace
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
       (assert ((u/hash-c? r/F? r/Bounds?) frees-and-bnds#) frees-and-bnds#)
       (c/RClass* nmes# variances# frees# csym#
                  (free-ops/with-bounded-frees frees-and-bnds#
                    ~(build-replacement-syntax replacements-syn))
                  (free-ops/with-bounded-frees frees-and-bnds#
                    (set (map prs/parse-type unchecked-ancestors-syn#)))
                  bnds#))))

(defn declared-kind-for-rclass [fields]
  (let [fs (map first fields)
        _ (assert (every? symbol? fs) fs)
        vs (map (fn [[v & {:keys [variance]}]] variance) fields)]
    (c/TypeFn* fs vs (repeat (count vs) r/no-bounds) r/-any)))

(defmacro alters [& args]
  (let [fields (gensym 'fields)
        opts (gensym 'opts)
        s (gensym 's)]
    `(let [ts# (partition 2 '~args)]
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
                   [sym# rcls#])))))))

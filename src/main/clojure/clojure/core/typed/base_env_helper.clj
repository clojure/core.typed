(ns clojure.core.typed.base-env-helper
  (:require [clojure.core.typed
             [parse-unparse :as prs]
             [utils :as u]
             [free-ops :as free-ops]
             [rclass-env :as rcls]]))

(defmacro alias-mappings [& args]
  `(let [ts# (partition 2 '~args)]
     (into {}
           (for [[s# t#] ts#]
             (do
               (assert (and (symbol? s#)
                            (namespace s#))
                       "Need fully qualified symbol")
               (intern (find-ns (symbol (namespace s#))) (symbol (name s#)))
               [s# (prs/parse-type t#)])))))

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

(defn parse-RClass-binder [bnds]
  `(for [[nme# & {variance# :variance}] ~bnds]
     [variance# (r/make-F nme#)]))

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
           [variances# frees#] (when-let [~fs (seq ~frees-syn)]
                                 (let [b# ~(parse-RClass-binder fs)]
                                   [(map first b#) (map second b#)]))
           csym# ~(resolve-class-symbol the-class)]
       (c/RClass* (map :name frees#) variances# frees# csym#
                  (free-ops/with-frees frees#
                    ~(build-replacement-syntax replacements-syn))
                  (free-ops/with-frees frees#
                    (set (map prs/parse-type unchecked-ancestors-syn#)))))))

(defmacro alters [& args]
  (let [fields (gensym 'fields)
        opts (gensym 'opts)
        s (gensym 's)]
    `(let [ts# (partition 2 '~args)]
       (into {}
             (for [[~s [~fields & ~opts]] ts#]
               (let [sym# ~(resolve-class-symbol s)
                     rcls# ~(make-RClass-syn s fields opts)]
                 ;accumulate altered classes in initial env
                 (rcls/alter-class* sym# rcls#)
                 [sym# rcls#]))))))

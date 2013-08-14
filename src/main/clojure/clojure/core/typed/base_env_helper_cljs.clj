(ns clojure.core.typed.base-env-helper-cljs
  (:refer-clojure :exclude [type])
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.free-ops :as free-ops]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.declared-kind-env :as decl-env]
            [clojure.core.typed.rclass-env :as rcls]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.jsnominal-env :as jsnom]
            [clojure.pprint :as pprint]))

(defmacro alias-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (let [;desc# (-> s# meta :doc)
                       ;doc# (str (when desc#
                       ;            (str desc# "\n\n")) 
                       ;          (with-out-str (pprint/pprint t#)))
                       ;_# (assert (and (symbol? s#)
                       ;                (namespace s#))
                       ;           "Need fully qualified symbol")
                       ;v# (intern (find-ns (symbol (namespace s#))) (symbol (name s#)))
                       ;_# (alter-meta! v# merge {:doc doc#})
                       ]
                   [(with-meta s# nil) (prs/parse-type t#)])))))))

(defmacro var-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 (do
                   (assert (and (symbol? s#)
                                (namespace s#))
                           "Need fully qualified symbol")
                   [s# (prs/parse-type t#)])))))))

(defn declared-kind-for-protocol [binder]
  (let [fs (map first binder)
        _ (assert (every? symbol? fs) fs)
        vs (map (fn [[v & {:keys [variance]}]] variance) binder)]
    (c/TypeFn* fs vs (repeat (count vs) r/no-bounds) r/-any)))

(defmacro protocol-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[n# [fields# & {:as opts#}]] ts#]
                 (let [vs# (seq
                             (for [[_# & {variance# :variance}] fields#]
                               variance#))
                       decl-kind# (declared-kind-for-protocol fields#)
                       ;FIXME this is harder than it has to be
                       ; add a Name so the methods can be parsed
                       _# (nme-env/declare-protocol* n#)
                       _# (when (r/TypeFn? decl-kind#)
                            (decl-env/add-declared-kind n# decl-kind#))
                       names# (when (seq fields#)
                                (map first fields#))
                       ; FIXME specify bounds
                       bnds# (when (seq fields#)
                               (repeat (count fields#) r/no-bounds))
                       frees# (map r/make-F names#)
                       methods# (free-ops/with-bounded-frees (zipmap frees# bnds#)
                                  (into {}
                                        (for [[mname# mtype#] (:methods opts#)]
                                          [mname# (prs/parse-type mtype#)])))
                       the-var# n#
                       on-class# (c/Protocol-var->on-class the-var#)]
                   (decl-env/remove-declared-kind n#)
                   [n# (c/Protocol* names# vs# frees# the-var#
                                    on-class# methods# bnds#)])))))))

(defmacro jsnominal-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[n# [binder# & {:as opts#}]] ts#]
                 (let [names# (when (seq binder#)
                                (map first binder#))
                       {vs# :variances
                        names# :names
                        bnds# :bnds} 
                       (when (seq binder#)
                         ; don't bound frees because mutually dependent bounds are problematic
                         ; FIXME ... Or is this just laziness? 
                         (let [b# (free-ops/with-free-symbols names#
                                    (mapv prs/parse-tfn-binder binder#))]
                           {:variances (map :variance b#)
                            :nmes (map :nme b#)
                            :bnds (map :bound b#)}))
                       frees# (map r/make-F names#)
                       methods# (free-ops/with-bounded-frees (zipmap frees# bnds#)
                                  (into {}
                                        (for [[mname# mtype#] (:methods opts#)]
                                          [mname# (prs/parse-type mtype#)])))
                       fields# (free-ops/with-bounded-frees (zipmap frees# bnds#)
                                 (into {}
                                       (for [[mname# mtype#] (:fields opts#)]
                                         [mname# (prs/parse-type mtype#)])))]
                   (decl-env/remove-declared-kind n#)
                   [n# {:jsnominal (c/JSNominal* names# vs# frees# n# bnds#)
                        :fields fields#
                        :methods methods#}])))))))

(defmacro jsenv-mappings [& args]
  `(impl/with-cljs-impl
     (let [ts# (partition 2 '~args)]
       (into {}
             (doall
               (for [[s# t#] ts#]
                 [s# (prs/parse-type t#)]))))))

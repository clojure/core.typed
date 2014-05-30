(ns clojure.core.typed.macros
  (:refer-clojure :exclude [type defprotocol fn loop dotimes let for doseq])
  (:require [clojure.core :as core]
            [clojure.core.typed.internal :as internal]))

(defmacro
  ^{:forms '[(def name docstring? :- type? expr)]}
  def
  "Like clojure.core/def with optional type annotations

  NB: it is impossible to refer a var called `def` as it is a
  special form. Use an alias prefix (eg. `t/def`).

  If an annotation is provided, a corresponding `ann` form
  is generated, otherwise it expands identically to clojure.core/def

  eg. ;same as clojure.core/def
      (def vname 1)
      
      ;with Number `ann`
      (def vname :- Number 1)

      ;doc
      (def vname
        \"Docstring\"
        :- Long
        1)"
  [name & fdecl]
  (core/let [[docstring fdecl] (internal/take-when string? fdecl)
             [provided? t body] (if (#{:-} (first fdecl))
                                  (core/let [_ (assert (#{3} (count fdecl))
                                                       "Bad arguments to clojure.core.typed/def")
                                             [colon t body] fdecl]
                                    [true t body])
                                  (core/let [_ (assert (#{1} (count fdecl))
                                                       "Bad arguments to clojure.core.typed/def")
                                             [body] fdecl]
                                    [false nil body]))]
    `(do ~@(when provided?
             [`(ann ~name ~t)])
         ~(list* 'def name 
                 (concat
                   (when docstring [docstring])
                   [body])))))

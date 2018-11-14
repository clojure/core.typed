(ns clojure.core.typed.annotator.frontend.spec
  (:require [clojure.core.typed.annotator.util
             :refer [qualify-spec-symbol]])
  )

(defn def-spec [k s]
  (list (qualify-spec-symbol 'def)
        k
        ; handle possibly recursive specs
        ; TODO intelligently order specs to minimize this issue
        (if (or (symbol? s)
                (set? s)
                ;; already late bound
                (and (seq? s)
                     (let [fs (first s)]
                       ((into #{}
                              (map qualify-spec-symbol)
                              ; late binding ops
                              '[and keys cat alt or nilable coll-of
                                fspec map-of tuple cat multi-spec
                                *])
                        fs))))
          s
          (list (qualify-spec-symbol 'and)
                s))))


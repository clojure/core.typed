(ns ^:skip-wiki clojure.core.typed.bootstrap-cljs)

(def -base-aliases
  '#{AnyInteger Seqable})

(defmacro base-aliases 
  "Define base aliases"
  []
  `(do ~@(map #(list 'def %) -base-aliases)))

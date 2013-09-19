(ns ^:skip-wiki clojure.core.typed.bootstrap-cljs)

(alter-meta! *ns* assoc :skip-wiki true)

(def -base-aliases
  '#{AnyInteger Seqable NonEmptySeq Number})

(defmacro base-aliases 
  "Define base aliases"
  []
  `(do ~@(map #(list 'def %) -base-aliases)))

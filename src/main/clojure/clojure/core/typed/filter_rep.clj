(in-ns 'clojure.core.typed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(def name-ref? (some-fn symbol? integer?))

(def Filter ::filter)

(defn Filter? [a]
  (isa? (class a) Filter))

(defn declare-filter [c]
  (derive c Filter))

(defrecord BotFilter []
  "Always false proposition"
  [])
(defrecord TopFilter []
  "Trivially true proposition"
  [])

(def -top (->TopFilter))
(def -bot (->BotFilter))

(defrecord NoFilter []
  "Represents no info about filters, used for parsing types"
  [])

(declare PathElem?)

(defrecord TypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is of given type"
  [(Type? type)
   (every? PathElem? path)
   (name-ref? id)])

(defrecord NotTypeFilter [type path id]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(Type? type)
   (every? PathElem? path)
   (name-ref? id)])

(defrecord AndFilter [fs]
  "Logical conjunction of filters"
  [(set? fs)
   (seq fs)
   (every? Filter? fs)])

(defrecord OrFilter [fs]
  "Logical disjunction of filters"
  [(seq fs)
   (set? fs)
   (every? Filter? fs)])

(defrecord ImpFilter [a c]
  "Antecedent (filter a) implies consequent (filter c)"
  [(Filter? a)
   (Filter? c)])

(defrecord FilterSet [then else]
  "A set of filters: those true when the expression is a true value, and 
  those when it is a false value."
  [(and (or (BotFilter? then)
            (and (BotFilter? else)
               (TopFilter? then))
            (Filter? then))
        (or (BotFilter? else)
            (and (BotFilter? then)
                 (TopFilter? else))
            (Filter? else)))])

(declare-filter BotFilter)
(declare-filter TopFilter)
(declare-filter NoFilter)
(declare-filter AndFilter)
(declare-filter OrFilter)
(declare-filter TypeFilter)
(declare-filter NotTypeFilter)
(declare-filter ImpFilter)
(declare-filter FilterSet)

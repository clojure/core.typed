(in-ns 'clojure.core.logic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Runtime Objects

(def RObject ::r-object)

(defn RObject? [a]
  (isa? (class a) RObject))

(defn declare-robject [c]
  (derive c RObject))

(defrecord EmptyObject []
  "?"
  [])

(def -empty (->EmptyObject))

(defrecord Path [path id]
  "A path to a variable. Paths grow to the right, with leftmost
  pathelem being applied first (think of -> threading operator)."
  [(or (and (seq path)
            (sequential? path))
       (nil? path))
   (every? PathElem? path)
   (name-ref? id)])

(defrecord NoObject []
  "Represents no info about the object of this expression
  should only be used for parsing type annotations and expected types"
  [])

;Objects

(declare unparse-path-elem)

(declare-robject EmptyObject)
(declare-robject Path)
(declare-robject NoObject)

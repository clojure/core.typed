(in-ns 'typed.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths

(def PathElem ::path-elem)

(defn PathElem? [a]
  (isa? (class a) PathElem))

(defn declare-path-elem [c]
  (derive c PathElem))

(defrecord FirstPE []
  "A path calling clojure.core/first"
  [])
(defrecord NextPE []
  "A path calling clojure.core/next"
  [])

(defrecord ClassPE []
  "A path calling clojure.core/class"
  [])

(defrecord CountPE []
  "A path calling clojure.core/count"
  [])

(defrecord KeyPE [val]
  "A key in a hash-map"
  [(keyword? val)])

(def -kpe ->KeyPE)

(declare-path-elem FirstPE)
(declare-path-elem NextPE)
(declare-path-elem ClassPE)
(declare-path-elem CountPE)
(declare-path-elem KeyPE)


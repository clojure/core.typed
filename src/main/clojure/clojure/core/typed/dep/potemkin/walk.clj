;; copied from https://github.com/ztellman/potemkin/blob/master/src/potemkin/walk.clj
(ns clojure.core.typed.dep.potemkin.walk)

;; adapted from clojure.walk, but preserves metadata

(defn walk
  "Like `clojure.walk/walk`, but preserves metadata."
  [inner outer form]
  (let [x (cond
            (list? form) (outer (apply list (map inner form)))
            (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
            (seq? form) (outer (doall (map inner form)))
            (coll? form) (outer (into (empty form) (map inner form)))
            :else (outer form))]
    (if (instance? clojure.lang.IObj x)
      (with-meta x (merge (meta form) (meta x)))
      x)))

(defn postwalk
  "Like `clojure.walk/postwalk`, but preserves metadata."
  [f form]
  (walk (partial postwalk f) f form))

(defn prewalk
  "Like `clojure.walk/prewalk`, but preserves metadata."
  [f form]
  (walk (partial prewalk f) identity (f form)))

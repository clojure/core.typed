;; copied from https://github.com/ztellman/potemkin/blob/master/src/potemkin/macros.clj
(ns clojure.core.typed.dep.potemkin.macros
  (:require
    [clojure.core.typed.dep.potemkin.walk :refer (postwalk)]
    [clojure.core.typed.dep.riddley.walk :as r]))

(defn safe-resolve [x]
  (try
    (resolve x)
    (catch Exception _
      nil)))

(def unified-gensym-regex #"([a-zA-Z0-9\-\'\*]+)#__\d+__auto__$")

(def gensym-regex #"(_|[a-zA-Z0-9\-\'\*]+)#?_+(\d+_*#?)+(auto__)?$")

(defn unified-gensym? [s]
  (and
    (symbol? s)
    (re-find unified-gensym-regex (str s))))

(defn gensym? [s]
  (and
    (symbol? s)
    (re-find gensym-regex (str s))))

(defn un-gensym [s]
  (second (re-find gensym-regex (str s))))

(defn unify-gensyms
  "All gensyms defined using two hash symbols are unified to the same
   value, even if they were defined within different syntax-quote scopes."
  [body]
  (let [gensym* (memoize gensym)]
    (postwalk
      #(if (unified-gensym? %)
         (symbol (str (gensym* (str (un-gensym %) "__")) "__auto__"))
         %)
      body)))

(defn normalize-gensyms
  [body]
  (let [cnt (atom 0)
        gensym* #(str % "__norm__" (swap! cnt inc))]
    (postwalk
      #(if (gensym? %)
         (symbol (gensym* (un-gensym %)))
         %)
      body)))

(defn equivalent?
  [a b]
  (if-not (and a b)
    (= a b)
    (=
      (->> a r/macroexpand-all normalize-gensyms)
      (->> b r/macroexpand-all normalize-gensyms))))



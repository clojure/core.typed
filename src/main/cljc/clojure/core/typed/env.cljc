(ns clojure.core.typed.env)

(def ^:dynamic *checker* nil)

(defn checker-or-nil []
  {:post [(or #?(:clj (instance? clojure.lang.IAtom %)
                 :cljs (instance? Atom %))
              (nil? %))]}
  *checker*)

(defn checker []
  (let [c *checker*]
    (assert #?(:clj (instance? clojure.lang.IAtom c)
               :cljs (instance? Atom c))
            (str "No checker state: " (pr-str c)))
    c))

(defn empty-checker []
  {})

(defn init-checker []
  (atom (empty-checker)
        :validator map?))

(defn deref-checker []
  {:post [(map? %)]}
  @(checker))

(defn swap-checker! [& args]
  (apply swap! (checker) args))

#?(:clj
(defmacro with-checker [c & body]
  `(binding [*checker* ~c]
     ~@body)))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

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

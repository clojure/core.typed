;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.annotator.pprint
  (:require [#?(:clj clojure.pprint :cljs cljs.pprint) :as pp]
            [clojure.core.typed.annotator.util :refer [current-ns unp]]))

;; copied from cljs.pprint
#?(:cljs
(defn- pp-type-dispatcher [obj]
  (cond
    (instance? PersistentQueue obj) :queue
    (satisfies? IDeref obj) :deref
    (symbol? obj) :symbol
    (keyword? obj) :keyword
    (seq? obj) :list
    (map? obj) :map
    (vector? obj) :vector
    (set? obj) :set
    (nil? obj) nil
    :default :default)))

(defmulti wrap-dispatch
  "A wrapper for code dispatch that prints local keywords with ::"
  {:arglists '[[object]]}
  #?(:clj class
     :cljs pp-type-dispatcher))

(defmethod wrap-dispatch :default
  [o]
  (pp/code-dispatch o))

;;; (def pprint-map (formatter-out "~<{~;~@{~<~w~^ ~_~w~:>~^, ~_~}~;}~:>"))
#?(:clj
   ;FIXME is this copy-pasted? it's since been updated in clojure.pprint
(defn- pprint-map [amap]
  (pp/pprint-logical-block :prefix "{" :suffix "}"
    (pp/print-length-loop [aseq (seq amap)]
      (when aseq
        (pp/pprint-logical-block
          (pp/write-out (ffirst aseq))
          (.write ^java.io.Writer *out* " ")
          (pp/pprint-newline :linear)
          (.set #'pp/*current-length* 0) ; always print both parts of the [k v] pair
          (pp/write-out (fnext (first aseq))))
        (when (next aseq)
          (.write ^java.io.Writer *out* ", ")
          (pp/pprint-newline :linear)
          (recur (next aseq))))))))

;; deterministic printing of HMaps
;;FIXME this doesn't work in CLJS, {:a 1} pprints as:
;; :a{ 1}
#?(:clj
(defmethod wrap-dispatch #?(:clj clojure.lang.IPersistentMap
                            :cljs :map)
  [o]
  (let [{tagged true untagged false}
        (group-by (fn [[k v]]
                    (and (seq? v)
                         (= 'quote (first v))
                         (keyword? (second v))))
                  o)
        tagged   (sort-by first tagged)
        untagged (sort-by first untagged)
        ordered
        (apply array-map
               (concat
                 (mapcat identity tagged)
                 (mapcat identity untagged)))]
    #?(:clj (pprint-map ordered)
       :cljs (pp/code-dispatch ordered)))))

(defmethod wrap-dispatch #?(:clj clojure.lang.Keyword
                            :cljs :keyword)
  [kw]
  (let [aliases #?(:clj (ns-aliases (current-ns))
                   :cljs #{})
        some-alias (delay
                     (some (fn [[k v]]
                             (when (= (namespace kw)
                                      (str (ns-name v)))
                               k))
                           aliases))]
    (cond
      (= (name (current-ns)) (namespace kw))
      (print (str "::" (name kw)))

      @some-alias 
      (print (str "::" @some-alias "/" (name kw)))

      :else
      (print kw))))

(defn pprint [& args]
  (pp/with-pprint-dispatch wrap-dispatch
    (apply pp/pprint args)))

(defn pprint-str-no-line [& args]
  (binding [pp/*print-right-margin* nil]
    ;; remove trailing newline
    (let [s (with-out-str
              (apply pprint args))]
      (subs s 0 (dec (count s))))))

(defn unp-str [t]
  (let [^String s 
        (with-out-str
          (binding [pp/*print-right-margin* nil]
            (pprint (unp t))))]
    (.replaceAll s "\\n" "")))

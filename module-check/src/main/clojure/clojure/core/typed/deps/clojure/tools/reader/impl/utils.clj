;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.deps.clojure.tools.reader.impl.utils
  (:require clojure.core.typed.deps.clojure.tools.reader.impl.ExceptionInfo) ;; force loading
  (:refer-clojure :exclude [char]))

(defn char [x]
  (when x
    (clojure.core/char x)))

;; getColumnNumber and *default-data-reader-fn* are available only since clojure-1.5.0-beta1
(def >=clojure-1-5-alpha*?
  (let [{:keys [minor qualifier]} *clojure-version*]
    (or (and (= minor 5)
             (not= "alpha"
                   (when qualifier
                     (subs qualifier 0 (dec (count qualifier))))))
        (> minor 5))))

(def <=clojure-1-7-alpha5
  (let [{:keys [minor qualifier]} *clojure-version*]
    (or (< minor 7)
        (and (= minor 7)
             (= "alpha"
                (when qualifier
                  (subs qualifier 0 (dec (count qualifier)))))
             (<= (read-string (subs qualifier (dec (count qualifier))))
                5)))))

(defmacro compile-if [cond then & [else]]
  (if (eval cond)
    then
    else))

(compile-if (= 3 (:minor *clojure-version*))
  (do
    (defn ex-info
      ([msg map]
         (clojure.core.typed.deps.clojure.tools.reader.impl.ExceptionInfo. msg map))
      ([msg map cause]
         (clojure.core.typed.deps.clojure.tools.reader.impl.ExceptionInfo. msg map cause)))
    (defn ex-data
      [^clojure.core.typed.deps.clojure.tools.reader.impl.ExceptionInfo ex]
      (.getData ex))
    (defn ex-info? [ex]
      (instance? clojure.core.typed.deps.clojure.tools.reader.impl.ExceptionInfo ex)))

  (defn ex-info? [ex]
    (instance? clojure.lang.ExceptionInfo ex)))

(compile-if <=clojure-1-7-alpha5
  (do
    (defrecord TaggedLiteral [tag form])

    (defn tagged-literal?
      "Return true if the value is the data representation of a tagged literal"
      [value]
      (instance? clojure.core.typed.deps.clojure.tools.reader.impl.utils.TaggedLiteral value))

    (defn tagged-literal
      "Construct a data representation of a tagged literal from a
       tag symbol and a form."
      [tag form]
      (clojure.core.typed.deps.clojure.tools.reader.impl.utils.TaggedLiteral. tag form))

    (ns-unmap *ns* '->TaggedLiteral)
    (ns-unmap *ns* 'map->TaggedLiteral)

    (defmethod print-method clojure.core.typed.deps.clojure.tools.reader.impl.utils.TaggedLiteral [o ^java.io.Writer w]
      (.write w "#")
      (print-method (:tag o) w)
      (.write w " ")
      (print-method (:form o) w))

    (defrecord ReaderConditional [splicing? form])
    (ns-unmap *ns* '->ReaderConditional)
    (ns-unmap *ns* 'map->ReaderConditional)

    (defn reader-conditional?
      "Return true if the value is the data representation of a reader conditional"
      [value]
      (instance? clojure.core.typed.deps.clojure.tools.reader.impl.utils.ReaderConditional value))

    (defn reader-conditional
      "Construct a data representation of a reader conditional.
       If true, splicing? indicates read-cond-splicing."
      [form splicing?]
      (clojure.core.typed.deps.clojure.tools.reader.impl.utils.ReaderConditional. splicing? form))

    (defmethod print-method clojure.core.typed.deps.clojure.tools.reader.impl.utils.ReaderConditional [o ^java.io.Writer w]
      (.write w "#?")
      (when (:splicing? o) (.write w "@"))
      (print-method (:form o) w))))

(defn whitespace?
  "Checks whether a given character is whitespace"
  [ch]
  (when ch
    (or (Character/isWhitespace ^Character ch)
        (identical? \,  ch))))

(defn numeric?
  "Checks whether a given character is numeric"
  [^Character ch]
  (when ch
    (Character/isDigit ch)))

(defn newline?
  "Checks whether the character is a newline"
  [c]
  (or (identical? \newline c)
      (nil? c)))

(defn desugar-meta
  "Resolves syntactical sugar in metadata" ;; could be combined with some other desugar?
  [f]
  (cond
    (keyword? f) {f true}
    (symbol? f)  {:tag f}
    (string? f)  {:tag f}
    :else        f))

(defn make-var
  "Returns an anonymous unbound Var"
  []
  (with-local-vars [x nil] x))

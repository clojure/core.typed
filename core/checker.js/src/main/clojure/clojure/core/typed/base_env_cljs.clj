;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.base-env-cljs
  (:require [clojure.core.typed.base-env-helper-cljs :as h]
            [clojure.core.typed.checker.base-env-common :refer [delay-and-cache-env]
             :as common]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.bootstrap-cljs :as boot]
            [cljs.analyzer :as ana]
            [clojure.core.typed.util-cljs :as ucljs]
            [cljs.env :as env]
            [clojure.set :as set]))

(ucljs/with-cljs-typed-env
(ucljs/with-core-cljs-typed
(binding [ana/*cljs-ns* 'cljs.core.typed]
(delay-and-cache-env ^:private init-protocol-env 
  (h/protocol-mappings
    
cljs.core/Fn [[]]
cljs.core/IFn [[]]
cljs.core/ICloneable [[]]
cljs.core/ICounted [[]]
cljs.core/IEmptyableCollection [[]]
cljs.core/ICollection [[[x :variance :covariant]]]
cljs.core/IIndexed [[]]
cljs.core/ASeq [[[x :variance :covariant]]]
cljs.core/ISeqable [[[x :variance :covariant]]]
cljs.core/ISeq [[[x :variance :covariant]]]
cljs.core/INext [[[x :variance :covariant]]]
cljs.core/ILookup [[[maxk :variance :covariant]
                    [v :variance :covariant]]]
cljs.core/IAssociative [[[m :variance :covariant]
                         [k :variance :covariant]
                         [v :variance :covariant]]]
cljs.core/IMap [[[k :variance :covariant]
                 [v :variance :covariant]]]
cljs.core/IMapEntry [[[k :variance :covariant]
                      [v :variance :covariant]]]
cljs.core/ISet [[[x :variance :covariant]]]
cljs.core/IStack [[[x :variance :covariant]]]
cljs.core/IVector [[[x :variance :covariant]]]
cljs.core/IDeref [[[x :variance :covariant]]]
cljs.core/IDerefWithTimeout [[[x :variance :covariant]]]
cljs.core/IMeta [[]]
cljs.core/IWithMeta [[]]
    ;TODO
;cljs.core/IReduce [[]]
;cljs.core/IKVReduce [[]]
cljs.core/IList [[[x :variance :covariant]]]
cljs.core/IEquiv [[]]
cljs.core/IHash [[]]
cljs.core/ISequential [[]]
cljs.core/Record [[]]
cljs.core/IReversible [[[x :variance :covariant]]]
cljs.core/ISorted [[]]
cljs.core/IWriter [[]]
cljs.core/IPrintWithWriter [[]]
cljs.core/IPending [[]]
    ;TODO
;cljs.core/IWatchable [[]]
    ;cljs.core/IEditableCollection [[]]
    ;cljs.core/ITransientCollection [[]]
    ;cljs.core/ITransientAssociative [[]]
    ;cljs.core/ITransientMap [[]]
    ;cljs.core/ITransientVector [[]]
    ;cljs.core/ITransientSet [[]]
cljs.core/IComparable [[]]
    ;cljs.core/IChunk [[]]
    ;cljs.core/IChunkedSeq [[]]
    ;cljs.core/IChunkedNext [[]]
cljs.core/INamed [[]]
cljs.core/Reduced [[[x :variance :covariant]]]
))

(let [rst! (delay (impl/dynaload 'clojure.core.typed.checker.protocol-env/reset-protocol-env!))]
  (defn reset-protocol-env! []
    (impl/with-cljs-impl
      (@rst! (init-protocol-env)))))

#_
(ann-jsclass js/Document
  :extends
  :implements 
  :properties
  {getElementById [cljs.core.typed/JSString -> (U nil js/HTMLElement)]}
  :static-properties
  )

(delay-and-cache-env ^:private init-jsnominals 
  (reset-protocol-env!)
  (h/jsnominal-mappings

; seems like a good place to put this
;; FIXME this is actually js/String, delete
string [[]
        :fields
        {}
        :methods
        {toLowerCase [-> cljs.core.typed/JSString]}]
    
js/Document [[]
          :fields
          {}
          :methods
          {getElementById [cljs.core.typed/JSString -> (U nil js/HTMLElement)]}]

js/HTMLElement [[]
             :fields
             {innerHTML cljs.core.typed/JSString
              tagName (U nil cljs.core.typed/JSString)}]
    
    
js/Event [[]
       :methods
       {preventDefault [-> nil]}]


    ;http://dom.spec.whatwg.org/#interface-eventtarget
js/EventTarget [[]]
    
goog.events.Listenable [[]]
goog.events.EventTarget [[]]
    ))

(let [rst! (delay (impl/dynaload 'clojure.core.typed.jsnominal-env/reset-jsnominal!))]
  (defn reset-jsnominal-env! []
    (impl/with-cljs-impl
      (@rst! (init-jsnominals)))))

;;; vars specific to cljs
(delay-and-cache-env ^:private init-var-env
  (reset-protocol-env!)
  (reset-jsnominal-env!)
  (merge
   (common/parse-cljs-ann-map @common/common-var-annotations)
   (h/var-mappings

;internal vars
cljs.core.typed/ann* [Any Any -> Any]
cljs.core.typed/ann-protocol* [Any Any Any -> Any]
cljs.core.typed/ann-datatype* [Any Any Any Any -> Any]
cljs.core.typed/def-alias* [Any Any -> Any]
cljs.core.typed/typed-deps* [Any -> Any]
cljs.core.typed/ann-jsnominal* [Any Any -> Any]

cljs.core/+ (IFn [cljs.core.typed/CLJSInteger * -> cljs.core.typed/CLJSInteger]
                 [cljs.core.typed/JSNumber * -> cljs.core.typed/JSNumber])
cljs.core/- (IFn [cljs.core.typed/CLJSInteger * -> cljs.core.typed/CLJSInteger]
                 [cljs.core.typed/JSNumber * -> cljs.core.typed/JSNumber])
cljs.core/* (IFn [cljs.core.typed/CLJSInteger * -> cljs.core.typed/CLJSInteger]
                 [cljs.core.typed/JSNumber * -> cljs.core.typed/JSNumber])
cljs.core/nth (All [x y]
                (IFn [(U nil (cljs.core/ISeqable x)) cljs.core.typed/CLJSInteger -> x]
                      [(U nil (cljs.core/ISeqable x)) cljs.core.typed/CLJSInteger y -> (U y x)]))

cljs.core/*flush-on-newline* cljs.core.typed/JSBoolean
cljs.core/*print-newline* cljs.core.typed/JSBoolean
cljs.core/*print-readably* cljs.core.typed/JSBoolean
cljs.core/*print-meta* cljs.core.typed/JSBoolean
cljs.core/*print-dup* cljs.core.typed/JSBoolean
cljs.core/*print-length* (U nil cljs.core.typed/CLJSInteger)

cljs.core/enable-console-print! [-> Any]

cljs.core/truth_ [Any -> Any]

cljs.core/coercive-= [Any Any -> cljs.core.typed/JSBoolean]

cljs.core/nil? (Pred nil)
cljs.core/undefined? (Pred JSUndefined)

cljs.core/array? (ReadOnlyArray Any)

cljs.core/object? [Any -> cljs.core.typed/JSBoolean]

cljs.core/native-satisfies? [Any Any -> Any]

cljs.core/is_proto_ [Any -> Any]

cljs.core/*main-cli-fn* (U nil [Any * -> Any])

cljs.core/missing-protocol [Any Any -> Any]
cljs.core/type->str [Any -> cljs.core.typed/JSString]

cljs.core/make-array (All [r] 
                          (IFn [cljs.core.typed/CLJSInteger -> (Array r)]
                               [Any cljs.core.typed/CLJSInteger -> (Array r)]))

cljs.core/array (All [r]
                     [r * -> (Array r)])

cljs.core/alength [(ReadOnlyArray Any) -> cljs.core.typed/CLJSInteger]

cljs.core/into-array (All [x] 
                          (IFn [(U nil (cljs.core/ISeqable x)) -> (Array x)]
                              [Any (U nil (cljs.core/ISeqable x)) -> (Array x)]))

cljs.core/pr-str* [Any -> cljs.core.typed/JSString]

cljs.core/clone [Any -> Any]

cljs.core/cloneable? (Pred cljs.core/ICloneable)


cljs.core/count
      ; TODO also accepts Counted
      ; FIXME should return integer
      [(U nil (cljs.core/ISeqable Any)) -> cljs.core.typed/CLJSInteger :object {:id 0, :path [Count]}]
cljs.core/prim-seq
      (All [x]
           [(cljs.core/ISeqable x) -> (U nil (cljs.core/ISeq x))])

cljs.core/key-test [Keyword Any -> cljs.core.typed/JSBoolean]

cljs.core/fn? [Any -> cljs.core.typed/JSBoolean]
cljs.core/ifn? [Any -> cljs.core.typed/JSBoolean]

;;pop needs to be defined here because
;;definition of List differs between clj and cljs
cljs.core/pop (All [x]
                      (IFn
                        [(IList x) -> (IList x)]
                        [(Vec x) -> (Vec x)]
                        [(Stack x) -> (Stack x)]))

cljs.core/clj->js [Any -> Any]
cljs.core/js->clj [Any -> Any]
cljs.core/js-obj  [Any * -> Any]

;;pseudo-private vars
cljs.core/-conj [Any Any -> Any]
;cljs.core.List.Empty (IList Any)
)))

(delay-and-cache-env ^:private init-var-nochecks
  (set (keys (init-var-env))))

(delay-and-cache-env init-jsvar-env
  (reset-protocol-env!)
  (reset-jsnominal-env!)
  (h/js-var-mappings
;; js
    
js/document js/Document

;; goog.dom

goog.dom/setTextContent [js/Element (U cljs.core.typed/JSString cljs.core.typed/JSNumber) -> js/Window]
goog.dom/getElementsByTagNameAndClass 
      [(U nil cljs.core.typed/JSString) (U nil cljs.core.typed/JSString) (U nil js/Document js/Element) -> (cljs.core/ISeqable js/Element)]
goog.dom.classes/set [(U js/Node nil) cljs.core.typed/JSString -> Any]
goog.dom.classes/add [(U js/Node nil) (U nil cljs.core.typed/JSString) * -> cljs.core.typed/JSBoolean]
goog.dom.classes/remove [(U js/Node nil) (U nil cljs.core.typed/JSString) * -> cljs.core.typed/JSBoolean]
goog.style/getPageOffsetLeft [(U nil js/Element) -> cljs.core.typed/JSNumber]
goog.style/getPageOffsetTop [(U nil js/Element) -> cljs.core.typed/JSNumber]
goog.events/listen [(U nil js/EventTarget goog.events.EventTarget goog.events.Listenable)
                    (U nil cljs.core.typed/JSString (ReadOnlyArray cljs.core.typed/JSString)) -> cljs.core.typed/JSNumber]

goog.events.EventType.KEYUP   cljs.core.typed/JSString
goog.events.EventType.KEYDOWN cljs.core.typed/JSString
goog.events.EventType.KEYPRESS cljs.core.typed/JSString
goog.events.EventType.CLICK   cljs.core.typed/JSString
goog.events.EventType.DBLCLICK cljs.core.typed/JSString
goog.events.EventType.MOUSEOVER cljs.core.typed/JSString
goog.events.EventType.MOUSEOUT cljs.core.typed/JSString
goog.events.EventType.MOUSEMOVE cljs.core.typed/JSString
    ))

(delay-and-cache-env init-alias-env 
  (reset-protocol-env!)
  (reset-jsnominal-env!)
  (h/alias-mappings
  ^{:doc "A type that returns true for cljs.core/integer?"}
cljs.core.typed/AnyInteger cljs.core.typed/CLJSInteger

  ^{:doc "A type that returns true for cljs.core/integer?"}
cljs.core.typed/Integer cljs.core.typed/CLJSInteger

  ^{:doc "A type that returns true for cljs.core/integer?"}
cljs.core.typed/Int cljs.core.typed/CLJSInteger

  ^{:doc "A type that returns true for cljs.core/number?"}
cljs.core.typed/Number cljs.core.typed/JSNumber

  ^{:doc "A type that returns true for cljs.core/number?"}
cljs.core.typed/Num cljs.core.typed/JSNumber

  ^{:doc "A type that returns true for cljs.core/string?"}
cljs.core.typed/String cljs.core.typed/JSString

  ^{:doc "A type that returns true for cljs.core/string?"}
cljs.core.typed/Str cljs.core.typed/JSString

  ^{:doc "A type that returns true for cljs.core/boolean?"}
cljs.core.typed/Boolean cljs.core.typed/JSBoolean

  ^{:doc "vector -- alias for common anns"}
cljs.core.typed/Vec (TFn [[x :variance :covariant]]
                         (IVector x))

  ^{:doc "vector -- alias for common anns"}
cljs.core.typed/IPersistentVector (TFn [[x :variance :covariant]]
                                       (IVector x))

  ^{:doc "map -- alias for common anns"}
cljs.core.typed/Map (TFn [[k :variance :covariant]
                          [v :variance :covariant]]
                         (IMap k v))

  ^{:doc "map -- alias for common anns"}
cljs.core.typed/IPersistentMap (TFn [[k :variance :covariant]
                                     [v :variance :covariant]]
                         (IMap k v))

  ^{:doc "map -- alias for common anns"}
cljs.core.typed/APersistentMap (TFn [[k :variance :covariant]
                                     [v :variance :covariant]]
                         (IMap k v))

  ^{:doc "associative -- alias for common anns"}
cljs.core.typed/Associative IAssociative

  ^{:doc "An atom that can read and write type x."
    :forms [(Atom1 t)]}
cljs.core.typed/Atom1 (TFn [[x :variance :invariant]] 
                           (cljs.core/Atom x x))
  ^{:doc "An atom that can write type w and read type r."
    :forms [(Atom2 t)]}
cljs.core.typed/Atom2 (TFn [[w :variance :contravariant]
                            [r :variance :covariant]] 
                           (cljs.core/Atom w r))

  ^{:doc "sequential -- alias for common anns"}
cljs.core.typed/Sequential ISequential

  ^{:doc "set -- alias for common anns"}
cljs.core.typed/Set ISet

  ^{:doc "set -- alias for common anns"}
cljs.core.typed/IPersistentSet ISet


  ^{:doc "A type that can be used to create a sequence of member type x."}
cljs.core.typed/Seqable (TFn [[x :variance :covariant]]
                             (cljs.core/ISeqable x))

  ^{:doc "A persistent sequence of member type x."
    :forms [(Seq t)]}
cljs.core.typed/Seq (TFn [[x :variance :covariant]]
                         (cljs.core/ISeq x))

  ^{:doc "A persistent sequence of member type x with count greater than 0."
    :forms [(NonEmptySeq t)]}
cljs.core.typed/NonEmptySeq (TFn [[x :variance :covariant]]
                                 (I (cljs.core/ISeq x) (CountRange 1)))




   ;;copied from impl/init-aliases

   ;;Seqables
  ^{:doc "A type that can be used to create a sequence of member type x
with count 0."
    :forms [(EmptySeqable t)]}
cljs.core.typed/EmptySeqable (TFn [[x :variance :covariant]]
                                  (I (cljs.core.typed/Seqable x) (ExactCount 0)))

   ^{:doc "A type that can be used to create a sequence of member type x
with count greater than 0."
     :forms [(NonEmptySeqable t)]}
cljs.core.typed/NonEmptySeqable
    (TFn [[x :variance :covariant]]
         (I (cljs.core.typed/Seqable x) (CountRange 1)))

    ;;Option
  ^{:doc "A union of x and nil."
    :forms [(Option t)]}
cljs.core.typed/Option (TFn [[x :variance :covariant]] (U nil x))


  ^{:doc "A persistent collection with member type x."
    :forms [(Coll t)]}
cljs.core.typed/Coll (TFn [[x :variance :covariant]]
                          (cljs.core/ICollection x))

  ^{:doc "A persistent collection with member type x and count greater than 0."
    :forms [(NonEmptyColl t)]}
cljs.core.typed/NonEmptyColl (TFn [[x :variance :covariant]]
                                  (I (cljs.core/ICollection x)
                                     (CountRange 1)))

  ^{:doc "A sequential non-empty seq retured from clojure.core/seq"
    :forms [(NonEmptyASeq t)]}
cljs.core.typed/NonEmptyASeq
   (TFn [[x :variance :covariant]]
        (I (cljs.core/ASeq x)
           (cljs.core/ISeq x)
           (cljs.core/ISeqable x)
           cljs.core/ISequential
           ;(Iterable x)
           (cljs.core/ICollection x)
           (cljs.core/IList x)
           ;clojure.lang.IObj
           (CountRange 1)))


  ^{:doc "The type of all things with count 0. Use as part of an intersection.
           eg. See EmptySeqable."
    :forms [EmptyCount]}
cljs.core.typed/EmptyCount (ExactCount 0)

  ^{:doc "The type of all things with count greater than 0. Use as part of an intersection.
           eg. See NonEmptySeq"
     :forms [NonEmptyCount]}
cljs.core.typed/NonEmptyCount (CountRange 1)

  ^{:doc "A union of x and nil."
    :forms [(Nilable t)]}
cljs.core.typed/Nilable (TFn [[x :variance :covariant]] (U nil x))

  ^{:doc "A persistent vector returned from clojure.core/vector (and others)"
    :forms [(AVec t)]}
cljs.core.typed/AVec (TFn [[x :variance :covariant]]
                             (I (IPersistentVector x)
                                ;(java.lang.Iterable x)
                                (ICollection x)
                                (IList x)
                                ;clojure.lang.IObj
                                ))

  ^{:doc "A persistent vector returned from clojure.core/vector (and others) and count greater than 0."
    :forms [(NonEmptyAVec t)]}
cljs.core.typed/NonEmptyAVec (TFn [[x :variance :covariant]]
                                        (I (IPersistentVector x)
                                           ;(java.lang.Iterable x)
                                           (ICollection x)
                                           (IList x)
                                           ;clojure.lang.IObj
                                           (CountRange 1)))

  ^{:doc "The result of clojure.core/seq."
    :forms [(NilableNonEmptyASeq t)]}
cljs.core.typed/NilableNonEmptyASeq
   (TFn [[x :variance :covariant]]
        (U nil
           (I (cljs.core/ASeq x)
              (cljs.core/ISeq x)
              cljs.core/ISequential
              ;(Iterable x)
              (cljs.core/ICollection x)
              (cljs.core/IList x)
              ;clojure.lang.IObj
              (CountRange 1))))

  ^{:doc "A Clojure persistent list."
    :forms [(PersistentList t)]}
cljs.core.typed/PersistentList
   (TFn [[x :variance :covariant]]
        (cljs.core/IList x))

  ^{:doc "Collection"}
cljs.core.typed/Collection
   (TFn [[x :variance :covariant]]
        (cljs.core/ICollection x))

  ^{:doc "A Clojure stack."
    :forms [(Stack t)]}
cljs.core.typed/Stack
   (TFn [[x :variance :covariant]]
        (cljs.core/IStack x))

   ^{:doc "Reversible maps to IReversible"
     :forms [(Reversible t)]}
cljs.core.typed/Reversible
   (TFn [[x :variance :covariant]]
        (cljs.core/IReversible x))))


(let [rst! (delay (impl/dynaload 'clojure.core.typed.checker.name-env/reset-name-env!))]
  (defn reset-alias-env! []
    (let [alias-env (init-alias-env)]
      ; Ensure init-alias-env agrees with the -base-aliases
      (assert (= (set (keys alias-env))
                 (set (map #(symbol "cljs.core.typed" (str %))
                           boot/-base-aliases)))
              (str "core.typed Bug! Base aliases do not agree with base environment."
                   " Missing from cljs.core.typed ns: "
                   (set/difference (set (keys alias-env))
                                   (set (map #(symbol "cljs.core.typed" (str %))
                                             boot/-base-aliases)))
                   " Missing from base-env-cljs ns "
                   (set/difference (set (map #(symbol "cljs.core.typed" (str %))
                                             boot/-base-aliases))
                                   (set (keys alias-env)))))
      (@rst! alias-env))))

(delay-and-cache-env init-declared-kinds {})

(delay-and-cache-env init-datatype-env
  (reset-protocol-env!)
  (reset-jsnominal-env!)
  (h/datatype-mappings

cljs.core/Atom [[[w :variance :contravariant]
                 [r :variance :covariant]]]
cljs.core/Symbol [[]]
cljs.core/Keyword [[]]
cljs.core/List [[[a :variance :covariant]]]
cljs.core/Reduced [[[x :variance :covariant]]]
    ))
)

(let [reset-var-type-env! (delay (impl/dynaload 'clojure.core.typed.checker.var-env/reset-var-type-env!))
      reset-jsvar-type-env! (delay (impl/dynaload 'clojure.core.typed.checker.var-env/reset-jsvar-type-env!))
      reset-declared-kinds! (delay (impl/dynaload 'clojure.core.typed.checker.declared-kind-env/reset-declared-kinds!))
      reset-datatype-env! (delay (impl/dynaload 'clojure.core.typed.checker.datatype-env/reset-datatype-env!))]
  (defn reset-cljs-envs! []
    (ucljs/with-cljs-typed-env
      (impl/with-cljs-impl
        (reset-alias-env!)
        (@reset-var-type-env! (init-var-env) (init-var-nochecks))
        (@reset-jsvar-type-env! (init-jsvar-env))
        (reset-protocol-env!)
        (@reset-declared-kinds! (init-declared-kinds))
        (reset-jsnominal-env!)
        (@reset-datatype-env! (init-datatype-env))))
    nil))))

(ns clojure.core.typed.base-env-cljs
  (:require [clojure.core.typed.base-env-helper-cljs :as h]
            [clojure.core.typed.base-env-common :refer [delay-and-cache-env]]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.bootstrap-cljs :as boot]
            [cljs.analyzer :as ana]
            [clojure.core.typed.util-cljs :as ucljs]
            [cljs.env :as env]
            [clojure.set :as set]))

(env/ensure
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
cljs.core/Associative [[[k :variance :covariant]
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
cljs.core/Reversible [[]]
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

    ))

(defn reset-protocol-env! []
  (impl/with-cljs-impl
    ((impl/v 'clojure.core.typed.protocol-env/reset-protocol-env!)
     (init-protocol-env))))

(delay-and-cache-env ^:private init-jsnominals
  (reset-protocol-env!)
  (h/jsnominal-mappings

; seems like a good place to put this
string [[]
        :fields
        {}
        :methods
        {toLowerCase [-> string]}]

js/Document [[]
          :fields
          {}
          :methods
          {getElementById [string -> (U nil js/HTMLElement)]}]

js/HTMLElement [[]
             :fields
             {innerHTML string
              tagName (U nil string)}]


js/Event [[]
       :methods
       {preventDefault [-> nil]}]


    ;http://dom.spec.whatwg.org/#interface-eventtarget
js/EventTarget [[]]

goog.events.Listenable [[]]
goog.events.EventTarget [[]]
    ))

(defn reset-jsnominal-env! []
  (impl/with-cljs-impl
    ((impl/v 'clojure.core.typed.jsnominal-env/reset-jsnominal!)
     (init-jsnominals))))

(delay-and-cache-env ^:private init-var-env
  (reset-protocol-env!)
  (reset-jsnominal-env!)
  (merge
    (h/var-mappings

;internal vars
cljs.core.typed/ann* [Any Any -> Any]
cljs.core.typed/ann-protocol* [Any Any Any -> Any]
cljs.core.typed/ann-datatype* [Any Any Any Any -> Any]
cljs.core.typed/def-alias* [Any Any -> Any]
cljs.core.typed/typed-deps* [Any -> Any]

cljs.core/identity (All [x] [x -> x])
cljs.core/+ (IFn [int * -> int]
                 [number * -> number])
cljs.core/+ (IFn [int * -> int]
                 [number * -> number])
cljs.core/- (IFn [int * -> int]
                 [number * -> number])
cljs.core/* (IFn [int * -> int]
                 [number * -> number])
cljs.core/> [number number * -> boolean]
cljs.core/< [number number * -> boolean]
cljs.core/= [Any * -> boolean]
cljs.core/identical? [Any Any -> boolean]
cljs.core/number? (Pred number)
cljs.core/nth (All [x y]
                (IFn [(U nil (cljs.core/ISeqable x)) int -> x]
                      [(U nil (cljs.core/ISeqable x)) int y -> (U y x)]))

cljs.core/*flush-on-newline* boolean
cljs.core/*print-newline* boolean
cljs.core/*print-readably* boolean
cljs.core/*print-meta* boolean
cljs.core/*print-dup* boolean
cljs.core/*print-length* (U nil int)

cljs.core/enable-console-print! [-> Any]

cljs.core/*1 Any
cljs.core/*2 Any
cljs.core/*3 Any

cljs.core/truth_ [Any -> Any]

cljs.core/nil? (Pred nil)

cljs.core/array? (ReadOnlyArray Any)

cljs.core/not [Any -> boolean]

cljs.core/object? [Any -> boolean]

cljs.core/string? (Pred string)

cljs.core/native-satisfies? [Any Any -> Any]

cljs.core/is_proto_ [Any -> Any]

cljs.core/*main-cli-fn* (U nil [Any * -> Any])

cljs.core/type [Any -> Any]

cljs.core/missing-protocol [Any Any -> Any]
cljs.core/type->str [Any -> string]

cljs.core/make-array (All [r]
                          (IFn [int -> (Array r)]
                                [Any int -> (Array r)]))

cljs.core/aclone (All [r]
                      [(ReadOnlyArray r) -> (Array r)])

cljs.core/array (All [r]
                     [r * -> (Array r)])

cljs.core/aget (All [x] (IFn [(ReadOnlyArray x)
                               int -> x]
                              [(ReadOnlyArray (ReadOnlyArray x))
                               int int -> x]
                              [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))
                               int int int -> x]
                              [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x))))
                               int int int int -> x]
                              ; don't support unsound cases
                              [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))))
                               int int int int int -> x]))

;TODO aset

cljs.core/alength [(ReadOnlyArray Any) -> int]

cljs.core/into-array (All [x]
                          (IFn [(U nil (cljs.core/ISeqable x)) -> (Array x)]
                              [Any (U nil (cljs.core/ISeqable x)) -> (Array x)]))

cljs.core/pr-str* [Any -> string]

cljs.core/symbol (IFn [(U string cljs.core/Symbol) -> cljs.core/Symbol]
                     [(U string nil) string -> cljs.core/Symbol])


cljs.core/clone [Any -> Any]

cljs.core/cloneable? (Pred cljs.core/ICloneable)

      ;TODO aliases
;cljs.core/seq (All [x]
;                   (IFn
;                     [(NonEmptyColl x) -> (NonEmptySeq x)]
;                     [(Option (Coll x)) -> (Option (NonEmptySeq x))
;                      :filters {:then (& (is NonEmptyCount 0)
;                                         (! nil 0))
;                                :else (| (is nil 0)
;                                         (is EmptyCount 0))}]
;                     [(Option (Seqable x)) -> (Option (NonEmptySeq x))]))

;;;; sequencial fns. I think those are incomplete
;;;; since Option, Coll, etc don't seem to work yet

cljs.core/seq (All [x]
                   (IFn [(NonEmptyColl x) -> (NonEmptySeqable x)]
                        [(Option (Coll x)) -> (Option (NonEmptySeqable x))
                         #_:filters #_{:then (& (is NonEmptyCount 0)
                                            (! nil 0))
                                   :else (| (is nil 0)
                                            (is EmptyCount 0))}]
                        [(Option (Seqable x)) -> (Option (NonEmptySeqable x))]))

cljs.core/first (All [x]
                     (IFn [(HSequential [x Any *]) -> x
                           :object {:id 0 :path [(Nth 0)]}]
                          [(Option (EmptySeqable x)) -> nil]
                          [(NonEmptySeqable x) -> x]
                          [(Option (Seqable x)) -> (Option x)]))

;;(ann cljs.core/second )

cljs.core/rest (All [x]
                    [(Option (Seqable x)) -> (ASeq x)])

cljs.core/last (All [x]
                    (IFn [(NonEmptySeqable x) -> x]
                         [(Option (Seqable x)) -> (Option x)]))

cljs.core/butlast (All [x]
                       [(Option (Seqable x)) -> (ASeq x)])



cljs.core/count
      ; TODO also accepts Counted
      ; FIXME should return integer
      [(U nil (cljs.core/ISeqable Any)) -> int :object {:id 0, :path [Count]}]
cljs.core/prim-seq
      (All [x]
           [(cljs.core/ISeqable x) -> (U nil (cljs.core/ISeq x))])

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

goog.dom/setTextContent [js/Element (U string number) -> js/Window]
goog.dom/getElementsByTagNameAndClass
      [(U nil string) (U nil string) (U nil js/Document js/Element) -> (cljs.core/ISeqable js/Element)]
goog.dom.classes/set [(U js/Node nil) string -> Any]
goog.dom.classes/add [(U js/Node nil) (U nil string) * -> boolean]
goog.dom.classes/remove [(U js/Node nil) (U nil string) * -> boolean]
goog.style/getPageOffsetLeft [(U nil js/Element) -> number]
goog.style/getPageOffsetTop [(U nil js/Element) -> number]
goog.events/listen [(U nil js/EventTarget goog.events.EventTarget goog.events.Listenable)
                    (U nil string (ReadOnlyArray string)) -> number]

goog.events.EventType.KEYUP   string
goog.events.EventType.KEYDOWN string
goog.events.EventType.KEYPRESS string
goog.events.EventType.CLICK   string
goog.events.EventType.DBLCLICK string
goog.events.EventType.MOUSEOVER string
goog.events.EventType.MOUSEOUT string
goog.events.EventType.MOUSEMOVE string
    ))

(delay-and-cache-env init-alias-env
  (reset-protocol-env!)
  (reset-jsnominal-env!)
  (h/alias-mappings

   ^{:doc "A type that returns true for cljs.core/integer?"}
   cljs.core.typed/AnyInteger int

   ^{:doc "A type that returns true for cljs.core/number?"}
   cljs.core.typed/Number number

   ^{:doc "A type that can be used to create a sequence of member type x."}
   cljs.core.typed/Seqable (TFn [[x :variance :covariant]]
                                (cljs.core/ISeqable x))

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
                                        (CountRange 1)))))


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
    ((impl/v 'clojure.core.typed.name-env/reset-name-env!) alias-env)))

(delay-and-cache-env init-declared-kinds {})

(delay-and-cache-env init-datatype-env
  (reset-protocol-env!)
  (reset-jsnominal-env!)
  (h/datatype-mappings

cljs.core/Atom [[[w :variance :contravariant]
                 [r :variance :covariant]]]
cljs.core/Symbol [[]]
cljs.core/Keyword [[]]
    ))
)

(defn reset-cljs-envs! []
  (env/ensure
    (impl/with-cljs-impl
      (reset-alias-env!)
      ((impl/v 'clojure.core.typed.var-env/reset-var-type-env!)
       (init-var-env) (init-var-nochecks))
      ((impl/v 'clojure.core.typed.var-env/reset-jsvar-type-env!)
       (init-jsvar-env))
      (reset-protocol-env!)
      ((impl/v 'clojure.core.typed.declared-kind-env/reset-declared-kinds!)
       (init-declared-kinds))
      (reset-jsnominal-env!)
      ((impl/v 'clojure.core.typed.datatype-env/reset-datatype-env!)
       (init-datatype-env))))
  nil)))

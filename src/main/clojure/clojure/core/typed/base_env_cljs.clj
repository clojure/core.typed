(ns clojure.core.typed.base-env-cljs
  (:require [clojure.core.typed.base-env-helper-cljs :as h]
            [clojure.core.typed.base-env-common :refer [delay-and-cache-env]]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.bootstrap-cljs :as boot]
            [clojure.set :as set]))

(delay-and-cache-env ^:private init-protocol-env 
  (h/protocol-mappings
    
cljs.core/ISeqable [[[x :variance :covariant]]]
cljs.core/ISeq [[[x :variance :covariant]]]
cljs.core/IMap [[[k :variance :covariant]
                 [v :variance :covariant]]]
cljs.core/ISet [[[x :variance :covariant]]]
cljs.core/IVector [[[x :variance :covariant]]]
cljs.core/IList [[[x :variance :covariant]]]
cljs.core/IEquiv [[]]


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

cljs.core/+ (Fn [int * -> int]
                [number * -> number])
cljs.core/> [number number * -> boolean]
cljs.core/< [number number * -> boolean]
cljs.core/= [Any * -> boolean]
cljs.core/identical? [Any Any -> boolean]
cljs.core/number? (predicate number)
cljs.core/nth (All [x y] 
                (Fn [(U nil (cljs.core/ISeqable x)) int -> x]
                    [(U nil (cljs.core/ISeqable x)) int y -> (U y x)]))
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
    ))


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
    ))

(defn reset-cljs-envs! []
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
     (init-datatype-env)))
  nil)

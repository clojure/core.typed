;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns 
  ^{:doc 
    "This namespace contains annotations and helper macros for type
    checking core.async code. Ensure clojure.core.async is require'd
    before performing type checking.
    
    go
      use go>

    chan
      use chan>

    buffer
      use buffer> (similar for other buffer constructors)
    "}
  ^:no-doc
  clojure.core.typed.lib.cljs.core.async
  (:require-macros [cljs.core.typed :refer [ann ann-datatype def-alias ann-protocol inst
                                            tc-ignore]
                    :as t])
  (:require [cljs.core.typed :refer [AnyInteger Seqable]]
            [cljs.core.async]))

;TODO how do we encode that nil is illegal to provide to Ports/Channels?
;     Is it essential?

;;;;;;;;;;;;;;;;;;;;
;; Protocols

(ann-protocol [[w :variance :contravariant]
               [r :variance :covariant]]
              cljs.core.async.impl.protocols/Channel)

(ann-protocol [[r :variance :covariant]]
              cljs.core.async.impl.protocols/ReadPort)

(ann-protocol [[w :variance :contravariant]] 
              cljs.core.async.impl.protocols/WritePort)

(ann-protocol [[x :variance :invariant]]
              cljs.core.async.impl.protocols/Buffer)

;unchecked-ancestors NYI
#_(ann-datatype [[w :variance :contravariant]
               [r :variance :covariant]]
              cljs.core.async.impl.channels.ManyToManyChannel 
              []
              :ancestors [(cljs.core.async.impl.protocols/Channel w r)
                          (cljs.core.async.impl.protocols/ReadPort r)
                          (cljs.core.async.impl.protocols/WritePort w)])

;;;;;;;;;;;;;;;;;;;;;
;;; Aliases

(def-alias 
  ^{:forms [(ReadOnlyChan t)]}
  ReadOnlyChan
  "A core.async channel that statically disallows writes."
  (TFn [[r :variance :covariant]]
    (Extends [(cljs.core.async.impl.protocols/WritePort Nothing)
              (cljs.core.async.impl.protocols/ReadPort r)
              (cljs.core.async.impl.protocols/Channel Nothing r)])))

(def-alias 
  ^{:forms [(Chan t)]}
  Chan
  "A core.async channel"
  (TFn [[x :variance :invariant]]
    (Extends [(cljs.core.async.impl.protocols/WritePort x)
              (cljs.core.async.impl.protocols/ReadPort x)
              (cljs.core.async.impl.protocols/Channel x x)])))

(def-alias 
  ^{:forms [TimeoutChan]}
  TimeoutChan
  "A timeout channel"
  (Chan Any))

(def-alias 
  ^{:forms [(Buffer t)]}
  Buffer
  "A buffer of type x."
  (TFn [[x :variance :invariant]]
    (cljs.core.async.impl.protocols/Buffer x)))

(def-alias 
  ^{:forms [(ReadOnlyPort t)]}
  ReadOnlyPort
  "A read-only port that can read type x"
  (TFn [[r :variance :covariant]]
    (Extends [(cljs.core.async.impl.protocols/ReadPort r) 
              (cljs.core.async.impl.protocols/WritePort Nothing)])))

(def-alias 
  ^{:forms [(WriteOnlyPort t)]}
  WriteOnlyPort
  "A write-only port that can write type x"
  (TFn [[x :variance :invariant]]
    (Extends [(cljs.core.async.impl.protocols/ReadPort x) 
              (cljs.core.async.impl.protocols/WritePort x)])))

(def-alias 
  ^{:forms [(Port t)]}
  Port
  "A port that can read and write type x"
  (TFn [[x :variance :invariant]]
    (Extends [(cljs.core.async.impl.protocols/ReadPort x) 
              (cljs.core.async.impl.protocols/WritePort x)])))

;;;;;;;;;;;;;;;;;;;;;
;;; Var annotations

(ann ^:no-check cljs.core.async/buffer (All [x] [AnyInteger -> (Buffer x)]))
(ann ^:no-check cljs.core.async/dropping-buffer (All [x] [AnyInteger -> (Buffer x)]))
(ann ^:no-check cljs.core.async/sliding-buffer (All [x] [AnyInteger -> (Buffer x)]))

(ann ^:no-check cljs.core.async/thread-call (All [x] [[-> x] -> (Chan x)]))

(ann ^:no-check cljs.core.async/timeout [AnyInteger -> TimeoutChan])

(ann ^:no-check cljs.core.async/chan (All [x] 
                                            (Fn [-> (Chan x)]
                                                [(U (Buffer x) AnyInteger) -> (Chan x)])))
;(ann clojure.core.async/>! (All [x] [(Chan x) -> (Chan x)]))

;(ann ^:no-check clojure.core.async.impl.ioc-macros/aget-object [AtomicReferenceArray AnyInteger -> Any])
;(ann ^:no-check clojure.core.async.impl.ioc-macros/aset-object [AtomicReferenceArray Any -> nil])
;(ann ^:no-check clojure.core.async.impl.ioc-macros/run-state-machine [AtomicReferenceArray -> Any])

;FIXME what is 2nd arg?
(ann ^:no-check cljs.core.async.impl.ioc-macros/put! (All [x] [AnyInteger Any (Chan x) x -> Any]))
;(ann ^:no-check clojure.core.async.impl.ioc-macros/return-chan (All [x] [AtomicReferenceArray x -> (Chan x)]))

(ann ^:no-check cljs.core.async/<!! (All [x] [(ReadOnlyPort x) -> (U nil x)]))
(ann ^:no-check cljs.core.async/>!! (All [x] [(WriteOnlyPort x) x -> nil]))
(ann ^:no-check cljs.core.async/alts!! 
     (All [x d]
          (Fn [(Seqable (U (Port x) '[(Port x) x])) (Seqable (Port x)) & :mandatory {:default d} :optional {:priority (U nil true)} -> 
               (U '[d ':default] '[(U nil x) (Port x)])]
              [(Seqable (U (Port x) '[(Port x) x])) & :optional {:priority (U nil true)} -> '[(U nil x) (Port x)]])))

(ann ^:no-check cljs.core.async/close! [(ReadOnlyChan Any) -> nil])

;(ann ^:no-check clojure.core.async.impl.dispatch/run [[-> (ReadOnlyChan Any)] -> Executor])
;(ann clojure.core.async.impl.ioc-macros/async-chan-wrapper kV


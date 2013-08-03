(ns 
  ^{:doc 
    "This namespace contains annotations and helper macros for type
    checking core.async code.
    
    go
      use go>

    chan
      use chan>

    buffer
      use buffer> (similar for other buffer constructors)
    "}
    clojure.core.typed.async
  (:require [clojure.core.typed :refer [ann ann-pdatatype def-alias ann-pprotocol check-ns cf doseq> inst loop>
                                        AnyInteger tc-ignore ann-form Seqable]
             :as t]
            [clojure.core.async :as async :refer [<! >! <!! timeout chan alt! go]]
            [clojure.core.async.impl.protocols :as p :refer [Buffer]]
            [clojure.core.async.impl.channels :as c]
            [clojure.core.async.impl.dispatch :as dispatch]
            [clojure.core.async.impl.ioc-macros :as ioc])
  (:import (clojure.core.async.impl.channels ManyToManyChannel)
           (java.util.concurrent Executor)
           (java.util.concurrent.atomic AtomicReferenceArray)))

;TODO how do we encode that nil is illegal to provide to Ports/Channels?
;     Is it essential?

(ann-pprotocol clojure.core.async.impl.protocols/Channel
               [[x :invariant]])

(ann-pprotocol clojure.core.async.impl.protocols/ReadPort
               [[x :invariant]] )

(ann-pprotocol clojure.core.async.impl.protocols/WritePort
               [[x :invariant]] )

(ann-pprotocol clojure.core.async.impl.protocols/Buffer
               [[x :invariant]] )

(ann-pdatatype clojure.core.async.impl.channels.ManyToManyChannel 
               [[x :invariant]] 
               []
               :unchecked-ancestors #{(p/Channel x)
                                      (p/ReadPort x)
                                      (p/WritePort x)})

(def-alias Chan
  "A core.async channel"
  (TFn [[x :variance :invariant]]
    (Extends [(p/WritePort x)
              (p/ReadPort x)
              (p/Channel x)])))

(def-alias TimeoutChan
  "A channel that times out. Not writeable or readable."
  (Chan Nothing))

(ann ^:nocheck clojure.core.async/buffer (All [x] [AnyInteger -> (Buffer x)]))
(ann ^:nocheck clojure.core.async/dropping-buffer (All [x] [AnyInteger -> (Buffer x)]))
(ann ^:nocheck clojure.core.async/sliding-buffer (All [x] [AnyInteger -> (Buffer x)]))

(ann ^:nocheck clojure.core.async/thread-call (All [x] [[-> x] -> (Chan x)]))

; FIXME what should the result type be? Is it writeable? Do we want bivariant channels?
(ann ^:nocheck clojure.core.async/timeout [AnyInteger -> (Chan Any)])

(ann ^:nocheck clojure.core.async/chan (All [x] 
                                            (Fn [-> (Chan x)] 
                                                [(U (Buffer x) AnyInteger) -> (Chan x)])))
;(ann clojure.core.async/>! (All [x] [(Chan x) -> (Chan x)]))

(ann ^:nocheck clojure.core.async.impl.ioc-macros/aget-object [AtomicReferenceArray AnyInteger -> Any])
(ann ^:nocheck clojure.core.async.impl.ioc-macros/aset-object [AtomicReferenceArray Any -> nil])
(ann ^:nocheck clojure.core.async.impl.ioc-macros/run-state-machine [AtomicReferenceArray -> Any])

;FIXME what is 2nd arg?
(ann ^:nocheck clojure.core.async.impl.ioc-macros/put! (All [x] [AnyInteger Any (Chan x) x -> Any]))
(ann ^:nocheck clojure.core.async.impl.ioc-macros/return-chan (All [x] [AtomicReferenceArray x -> (Chan x)]))

(def-alias Port
  "A port that can read and write type x"
  (TFn [[x :variance :invariant]]
    (Extends [(p/ReadPort x) (p/WritePort x)])))

(ann ^:nocheck clojure.core.async/<!! (All [x] [(Port x) -> (U nil x)]))
(ann ^:nocheck clojure.core.async/>!! (All [x] [(Port x) x -> nil]))
(ann ^:nocheck clojure.core.async/alts!! 
     (All [x d]
          (Fn [(Seqable (U (Port x) '[(Port x) x])) (Seqable (Port x)) & :mandatory {:default d} :optional {:priority (U nil true)} -> 
               (U '[d ':default] '[x (Port x)])]
              [(Seqable (U (Port x) '[(Port x) x])) & :optional {:priority (U nil true)} -> '[x (Port x)]])))

(ann ^:nocheck clojure.core.async/close! [(Chan Any) -> nil])

(defmacro go>
  "Asynchronously executes the body, returning immediately to the
  calling thread. Additionally, any visible calls to <!, >! and alt!/alts!
  channel operations within the body will block (if necessary) by
  'parking' the calling thread rather than tying up an OS thread (or
  the only JS thread when in ClojureScript). Upon completion of the
  operation, the body will be resumed.

  Returns a channel which will receive the result of the body when
  completed"
  [& body]
  `(let [c# (chan> ~'Any 1)
         captured-bindings# (clojure.lang.Var/getThreadBindingFrame)]
     (tc-ignore
       (dispatch/run
         (fn []
           (let [f# ~(ioc/state-machine body 1 &env ioc/async-custom-terminators)
                 state# (-> (f#)
                            (ioc/aset-all! ioc/USER-START-IDX c#
                                           ioc/BINDINGS-IDX captured-bindings#))]
             (ioc/run-state-machine state#)))))
     c#))


(defmacro chan> 
  "A statically typed core.async channel. 
  
  (chan> t ...) is the same as ((inst chan t) ...)"
  [t & args]
  `((inst chan ~t) ~@args))

(defmacro buffer>
  "A statically typed core.async buffer. 
  
  (buffer> t ...) is the same as ((inst buffer t) ...)"
  [t & args]
  `((inst async/buffer ~t) ~@args))

(defmacro sliding-buffer>
  "A statically typed core.async sliding buffer. 
  
  (sliding-buffer> t ...) is the same as ((inst sliding-buffer t) ...)"
  [t & args]
  `((inst async/sliding-buffer ~t) ~@args))

(defmacro dropping-buffer>
  "A statically typed core.async dropping buffer. 
  
  (dropping-buffer> t ...) is the same as ((inst dropping-buffer t) ...)"
  [t & args]
  `((inst async/dropping-buffer ~t) ~@args))

(ann ^:nocheck clojure.core.async.impl.dispatch/run [[-> (Chan Any)] -> Executor])
;(ann clojure.core.async.impl.ioc-macros/async-chan-wrapper kV

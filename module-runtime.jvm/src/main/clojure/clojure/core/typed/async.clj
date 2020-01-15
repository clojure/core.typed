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
      use go

    chan
      use chan

    buffer
      use buffer (similar for other buffer constructors)
    "}
  clojure.core.typed.async
  (:require [clojure.core.typed :refer [ann ann-datatype defalias inst ann-protocol]
             :as t]
            [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.core.async.impl.channels :as channels]
            [clojure.core.async.impl.dispatch :as dispatch]
            [clojure.core.async.impl.ioc-macros :as ioc] 
            )
  (:import (java.util.concurrent Executor)
           (java.util.concurrent.locks Lock)
           (java.util.concurrent.atomic AtomicReferenceArray)
           (clojure.lang IDeref)))

;TODO how do we encode that nil is illegal to provide to Ports/Channels?
;     Is it essential?

;;;;;;;;;;;;;;;;;;;;
;; Protocols

(ann-protocol clojure.core.async.impl.protocols/Channel
              close! [impl/Channel -> nil])

(ann-protocol [[r :variance :covariant]]
              clojure.core.async.impl.protocols/ReadPort
              take! [(impl/ReadPort r) Lock 
                     -> (t/U nil (IDeref (t/U nil r)))])

(ann-protocol [[w :variance :contravariant]]
              clojure.core.async.impl.protocols/WritePort
              put! [(impl/WritePort w) w Lock
                    -> (t/U nil (IDeref nil))])

(ann-protocol [[w :variance :contravariant]
               [r :variance :covariant]]
               clojure.core.async.impl.protocols/Buffer
               full? [(impl/Buffer w r) :-> t/Any]
               remove! [(impl/Buffer w r) :-> nil]
               add!* [(impl/Buffer w r) w :-> (impl/Buffer w r)]
               )

(ann-protocol clojure.core.async.impl.protocols/UnblockingBuffer)

(ann-datatype [[w :variance :contravariant]
               [r :variance :covariant]]
              clojure.core.async.impl.channels.ManyToManyChannel 
              []
              :unchecked-ancestors [impl/Channel
                                    (impl/ReadPort r)
                                    (impl/WritePort w)])

;;;;;;;;;;;;;;;;;;;;
;; Aliases

(defalias 
  ^{:forms '[(Port2 t t)]}
  Port2
  "A port that can write type w and read type r"
  (t/TFn [[w :variance :contravariant]
        [r :variance :covariant]]
    (t/I (impl/WritePort w)
       (impl/ReadPort r))))

(defalias 
  ^{:forms '[(Port t)]}
  Port
  "A port that can read and write type x"
  (t/TFn [[x :variance :invariant]]
    (Port2 x x)))

(defalias 
  ^{:forms '[(Chan2 t t)]}
  Chan2
  "A core.async channel that can take type w and put type r"
  (t/TFn [[w :variance :contravariant]
        [r :variance :covariant]]
    (t/I (Port2 w r)
       impl/Channel)))

(defalias 
  ^{:forms '[(Chan t)]}
  Chan
  "A core.async channel"
  (t/TFn [[x :variance :invariant]]
    (Chan2 x x)))

(defalias 
  ^{:forms '[(ReadOnlyChan t)]}
  ReadOnlyChan
  "A core.async channel that statically disallows writes."
  (t/TFn [[r :variance :covariant]]
    (Chan2 t/Nothing r)))

(defalias 
  ^{:forms '[(ReadOnlyPort t)]}
  ReadOnlyPort
  "A read-only port that can read type x"
  (t/TFn [[t :variance :covariant]]
    (Port2 t/Nothing t)))

(defalias 
  ^{:forms '[(WriteOnlyPort t)]}
  WriteOnlyPort
  "A write-only port that can write type p"
  (t/TFn [[p :variance :contravariant]]
    (Port2 p t/Nothing)))

(defalias
  ^{:forms '[TimeoutChan]}
  TimeoutChan
  "A timeout channel"
  (Chan t/Any))

(defalias 
  ^{:forms '[(Buffer2 t t)]}
  Buffer2
  "A buffer of that can write type w and read type t."
  (t/TFn [[w :variance :contravariant]
        [r :variance :covariant]]
    (t/I (impl/Buffer w r)
         clojure.lang.Counted)))

(defalias 
  ^{:forms '[(Buffer t)]}
  Buffer
  "A buffer of type x."
  (t/TFn [[x :variance :invariant]]
    (Buffer2 x x)))

(defalias 
  ^{:forms '[(UnblockingBuffer2 t t)]}
  UnblockingBuffer2
  "An unblocking buffer that can write type w and read type t."
  (t/TFn [[w :variance :contravariant]
        [r :variance :covariant]]
    (t/I (Buffer2 w r)
         impl/UnblockingBuffer)))

(defalias 
  ^{:forms '[(UnblockingBuffer t)]}
  UnblockingBuffer
  "An unblocking buffer of type x."
  (t/TFn [[x :variance :invariant]]
    (UnblockingBuffer2 x x)))

;;;;;;;;;;;;;;;;;;;;
;; Var annotations

(ann ^:no-check clojure.core.async/buffer (t/All [w r] [t/Int :-> (Buffer2 w r)]))
(ann ^:no-check clojure.core.async/dropping-buffer (t/All [w r] [t/Int :-> (Buffer w r)]))
(ann ^:no-check clojure.core.async/sliding-buffer (t/All [w r] [t/Int :-> (Buffer w r)]))

(ann ^:no-check clojure.core.async/thread-call (t/All [x] [[:-> x] :-> (Chan x)]))

(ann ^:no-check clojure.core.async/pipe
     (t/All [t]
            (t/IFn
              [(Chan t) (Chan t) :-> (Chan t)]
              [(Chan t) (Chan t) t/Any :-> (Chan t)])))

(ann ^:no-check clojure.core.async/timeout [t/Int :-> TimeoutChan])

; TODO buffer must be supplied when xform is
(ann ^:no-check clojure.core.async/chan
     (t/All [p t]
            (t/IFn [:-> (Chan2 p t)]
                 [(t/U (Buffer2 p t) t/Int nil) :-> (Chan2 p t)]
                 [(t/U (Buffer2 p t) t/Int nil)
                  ; xform
                  (t/U nil
                       [[(Buffer2 p t) p :-> (Buffer2 p t)]
                        :->
                        [(Buffer2 p t) p :-> (Buffer2 p t)]])
                  :-> (Chan2 p t)]
                 [(t/U (Buffer2 p t) t/Int nil)
                  ; xform
                  (t/U nil
                       [[(Buffer2 p t) p :-> (Buffer2 p t)]
                        :->
                        [(Buffer2 p t) p :-> (Buffer2 p t)]])
                  ; ex-handler
                  (t/U nil
                       [Throwable :-> (t/U nil p)])
                  :-> (Chan2 p t)])))

(ann ^:no-check clojure.core.async.impl.ioc-macros/aget-object [AtomicReferenceArray t/Int :-> t/Any])
(ann ^:no-check clojure.core.async.impl.ioc-macros/aset-object [AtomicReferenceArray t/Any :-> nil])
(ann ^:no-check clojure.core.async.impl.ioc-macros/run-state-machine [AtomicReferenceArray :-> t/Any])

;FIXME what is 2nd arg?
(ann ^:no-check clojure.core.async.impl.ioc-macros/put! (t/All [x] [t/Int t/Any (Chan x) x :-> t/Any]))
(ann ^:no-check clojure.core.async.impl.ioc-macros/return-chan (t/All [x] [AtomicReferenceArray x :-> (Chan x)]))

(ann ^:no-check clojure.core.async/<!! (t/All [t] [(Port2 t/Nothing t) :-> (t/U nil t)]))
; should this use Port's?
(ann ^:no-check clojure.core.async/<! (t/All [t] [(Port2 t/Nothing t) :-> (t/U nil t)]))
(ann ^:no-check clojure.core.async/>!! (t/All [p] [(Port2 p t/Any) p :-> t/Any]))
(ann ^:no-check clojure.core.async/>! (t/All [p t] [(Port2 p t) p :-> (Port2 p t)]))
(t/ann-many 
  (t/All [x d]
         (t/IFn [(t/Seqable (t/U (Port x) '[(Port x) x])) 
               & :mandatory {:default d} 
               :optional {:priority (t/U nil true)} 
               :-> (t/U '[d ':default] '[(t/U nil x) (Port x)])]
              [(t/Seqable (t/U (Port x) '[(Port x) x])) 
               & :optional {:priority (t/U nil true)} 
               :-> '[(t/U nil x) (Port x)]]))
  ^:no-check clojure.core.async/alts!!
  ^:no-check clojure.core.async/alts!)

(ann ^:no-check clojure.core.async/close! [impl/Channel :-> nil])

(ann ^:no-check clojure.core.async.impl.dispatch/run [[:-> (ReadOnlyChan t/Any)] :-> Executor])
;(ann clojure.core.async.impl.ioc-macros/async-chan-wrapper kV

(ann ^:no-check clojure.core.async/put!
     (t/All [p]
            (t/IFn [(Port2 p t/Any) p :-> t/Any]
                 [(Port2 p t/Any) p [t/Any :-> t/Any] :-> t/Any]
                 [(Port2 p t/Any) p [t/Any :-> t/Any] t/Any :-> t/Any])))

(ann ^:no-check clojure.core.async/map<
     (t/All [t o]
            [[t -> o]
             (Chan2 t/Nothing t)
             :->
             (Chan o)]))

(ann ^:no-check clojure.core.async/map>
     (t/All [p t]
            [[t -> p]
             (Chan2 p t)
             :->
             (Chan2 p t)]))

;(ann ^:no-check clojure.core.async/filter>
;     (t/All [t t']
;            (t/IFn
;              [[t :-> t/Any :filters {:then (is t' 0)}] (Chan2 t/Nothing t) :-> (Chan t')]
;              [[t :-> t/Any] (Chan2 t/Nothing t) :-> (Chan t)])))
;
;(ann ^:no-check clojure.core.async/remove>
;     (t/All [p t]
;            (t/IFn
;              [[t :-> t/Any :filters {:then (! p 0)}] (Chan2 p t) :-> (Chan2 p t)]
;              [[t :-> t/Any] (Chan2 p t) :-> (Chan2 p t)])))
;
;(ann ^:no-check clojure.core.async/filter<
;     (t/All [p t]
;            (t/IFn
;              [[t :-> t/Any :filters {:then (is p 0)}] (Chan2 t/Nothing t) :-> (Chan2 p t)]
;              [[t :-> t/Any] (Chan2 t/Nothing t) :-> (Chan2 t t)])))

(ann ^:no-check clojure.core.async/onto-chan
     (t/All [x]
            [(Chan x)
             (t/U nil (t/Seqable x))
             :->
             (Chan t/Any)]))

(ann ^:no-check clojure.core.async/to-chan
     (t/All [x]
            [(t/U nil (t/Seqable x))
             :-> (Chan x)]))

;(ann ^:no-check clojure.core.async/map
;     (All [x]
;          [[x :-> y]
;           (t/U nil (t/Seqable (Chan x)))))


;;;;;;;;;;;;;;;;;;;;
;; Typed wrappers

(t/tc-ignore
(defn ^:skip-wiki maybe-annotation [args]
  (let [t? (#{:-} (first args))
        t (when t? (second args))
        args (if t? 
               (drop 2 args)
               args)]
    [t? t args]))
)

(defmacro go
  "Like go but with optional annotations. Channel annotation defaults to Any.

  eg.
    (let [c (chan :- Str)]
      ;; same as (go :- t/Any ...)
      (go (a/>! c \"hello\"))
      (assert (= \"hello\" (a/<!! (go :- Str (a/<! c)))))
      (a/close! c))
  
  Notes
    This macro will macroexpand the body twice: once for type checking
    and again for the actual return value."
  [& body]
  (let [[t? t body] (maybe-annotation body)
        crossing-env (zipmap (keys &env) (repeatedly gensym))]
    `(let [c# (chan :- ~(if t? t `t/Any), 1)
           captured-bindings# (clojure.lang.Var/getThreadBindingFrame)]
       ; wrap unexpanded go body in a thunk for type checking.
       ; Will result in the body expanding twice.
       (fn [] ~@body)
       ; we don't want to touch this.
       (t/tc-ignore
         (dispatch/run
           (^:once fn* []
                   (let [~@(mapcat (fn [[l sym]] [sym `(^:once fn* [] ~(vary-meta l dissoc :tag))]) crossing-env)
                         f# ~(ioc/state-machine `(do ~@body) 1 [crossing-env &env] ioc/async-custom-terminators)
                         state# (-> (f#)
                                    (ioc/aset-all! ioc/USER-START-IDX c#
                                                   ioc/BINDINGS-IDX captured-bindings#))]
                     (ioc/run-state-machine-wrapped state#)))))
       c#)))

(defmacro go-loop
  "Like (go (t/loop ...))"
  [& body]
  (let [[t? t body] (maybe-annotation body)]
    (if t?
      `(go :- ~t (t/loop ~@body))
      `(go (t/loop ~@body)))))

(comment
(t/cf
  (let [c (chan )]
    (go (a/>! c "hello"))
    (prn (a/<!! (go :- t/Str (a/<! c))))
    (a/close! c)))

(t/cf
  (let [c1 (chan :- t/Str)
        c2 (chan :- t/Str)]
    (a/thread (while true
                (let [[v ch] (a/alts!! [c1 c2])]
                  (println "Read" v "from" ch))))
    (a/>!! c1 "hi")
    (a/>!! c2 "there")))

(t/cf
  (let [c1 (chan)
        c2 (chan :- t/Str)]
    (go (while true
          (let [[v ch] (a/alts! [c1 c2])]
            (println "Read" v "from" ch))))
    (go (a/>! c1 "hi"))
    (go (a/>! c2 "there"))))

)

(defmacro chan
  "Like chan but with optional type annotations.

  (chan :- t ...) creates a buffer that can read and write type t.
  Subsequent arguments are passed directly to clojure.core.async/chan.
  
  Note: 
    (chan :- t ...) is the same as ((inst async/chan t) ...)"
  [& args]
  (let [[t? t args] (maybe-annotation args)]
    (if t?
      `((inst async/chan ~t ~t) ~@args)
      `(async/chan ~@args))))

(defmacro buffer
  "Like buffer but with optional type annotations.

  (buffer :- t ...) creates a buffer that can read and write type t.
  Subsequent arguments are passed directly to clojure.core.async/buffer.

  Note: (buffer :- t ...) is the same as ((inst buffer t) ...)"
  [& args]
  (let [[t? t args] (maybe-annotation args)]
    (if t?
      `((inst async/buffer ~t ~t) ~@args)
      `(async/buffer ~@args))))

(defmacro sliding-buffer
  "Like sliding-buffer but with optional type annotations.

  (sliding-buffer :- t ...) creates a sliding buffer that can read and write type t.
  Subsequent arguments are passed directly to clojure.core.async/sliding-buffer.
  
  Note: (sliding-buffer :- t ...) is the same as ((inst sliding-buffer t t) ...)"
  [& args]
  (let [[t? t args] (maybe-annotation args)]
    (if t?
      `((inst async/sliding-buffer ~t ~t) ~@args)
      `(async/sliding-buffer ~@args))))


(defmacro dropping-buffer
  "Like dropping-buffer but with optional type annotations.

  (dropping-buffer :- t ...) creates a dropping buffer that can read and write type t.
  Subsequent arguments are passed directly to clojure.core.async/dropping-buffer.

  Note: (dropping-buffer :- t ...) is the same as ((inst dropping-buffer t) ...)"
  [& args]
  (let [[t? t args] (maybe-annotation args)]
    (if t?
      `((inst async/dropping-buffer ~t ~t) ~@args)
      `(async/dropping-buffer ~@args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated

(defmacro chan>
  "DEPRECATED: use chan"
  [t & args]
  (prn "DEPRECATED: chan>, use chan")
  `((inst async/chan ~t) ~@args))

(defmacro buffer>
  "DEPRECATED: use buffer"
  [t & args]
  (prn "DEPRECATED: buffer>, use buffer")
  `((inst async/buffer ~t) ~@args))

(defmacro sliding-buffer>
  "DEPRECATED: use sliding-buffer"
  [t & args]
  (prn "DEPRECATED: sliding-buffer>, use sliding-buffer")
  `((inst async/sliding-buffer ~t) ~@args))

(defmacro dropping-buffer>
  "DEPRECATED: use dropping-buffer"
  [t & args]
  (prn "DEPRECATED: dropping-buffer>, use dropping-buffer")
  `((inst async/dropping-buffer ~t) ~@args))

(defmacro go>
  "DEPRECATED: use go"
  [& body]
  (prn "DEPRECATED: go>, use go")
  `(go ~@body))

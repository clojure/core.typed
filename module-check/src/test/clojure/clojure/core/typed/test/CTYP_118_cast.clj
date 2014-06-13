(ns clojure.core.typed.test.CTYP-118-cast
  (:require [clojure.core.typed :as t]))

(t/ann i1 String)
(def i1 (cast String "a"))

(t/ann i2 (t/I String (t/Value "a")))
(def i2 (let [x String] (cast x "a")))

(t/ann i3 (t/I String (t/Value "c")))
(def i3 ((t/fn [x :- Class y :- (t/Value "c")] (cast x y)) String "c"))

(t/ann foo Class)
(def foo String)

(t/ann v (t/I String (t/Value "b")))
(def v (cast foo "b"))

(t/ann v1 Class)
(def v1 (cast (t/ann-form Class Class) (class :any)))

(t/ann v2 String)
(def v2 (cast (if true String nil) "c"))

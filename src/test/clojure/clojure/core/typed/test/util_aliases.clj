(ns clojure.core.typed.test.util-aliases
  (:require [clojure.core.typed :as t]))

(t/defalias MyName (t/HMap :mandatory {:a (t/Value 1)}))
(t/defalias MapName (t/HMap :mandatory {:a MyName}))
(t/defalias MapStruct1 (t/HMap :mandatory {:type (t/Value :MapStruct1) 
                                           :a MyName}))
(t/defalias MapStruct2 (t/HMap :mandatory {:type (t/Value :MapStruct2) 
                                           :b MyName}))
(t/defalias UnionName (t/U MapStruct1 MapStruct2))

(t/defalias HMapAlias1 '{:a t/Num})
(t/defalias HMapAlias2 '{:b t/Num})
(t/defalias HMapAliasInt1 '{:foo t/Int})
(t/defalias HMapAliasStr2 '{:bar t/Str})

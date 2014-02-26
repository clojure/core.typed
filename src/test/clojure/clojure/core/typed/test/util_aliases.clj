(ns clojure.core.typed.test.util-aliases
  (:require [clojure.core.typed :as t]))

(t/def-alias MyName (HMap :mandatory {:a (Value 1)}))
(t/def-alias MapName (HMap :mandatory {:a MyName}))
(t/def-alias MapStruct1 (HMap :mandatory {:type (Value :MapStruct1) 
                                          :a MyName}))
(t/def-alias MapStruct2 (HMap :mandatory {:type (Value :MapStruct2) 
                                          :b MyName}))
(t/def-alias UnionName (U MapStruct1 MapStruct2))

(t/def-alias HMapAlias1 '{:a Number})
(t/def-alias HMapAlias2 '{:b Number})
(t/def-alias HMapAliasInt1 '{:foo t/Int})
(t/def-alias HMapAliasStr2 '{:bar String})

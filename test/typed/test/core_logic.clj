(ns typed.test.core-logic
  (:refer-clojure :exclude [==])
  (:use [clojure.walk :only [postwalk]])
  (:require [clojure.set :as set])
  (:import [java.io Writer]
           [clojure.lang IPersistentSet Symbol])
  (:require [typed.core :refer [ann-protocol ann tc-ignore
                                declare-datatypes def-alias
                                ann-datatype]]))

(ann *occurs-check* (U true false))
(ann *locals* (IPersistentSet Symbol))

(def ^{:dynamic true} *occurs-check* true)
(def ^{:dynamic true} *locals*)

(def-alias Fail false)

(declare-datatypes ISubstitution)

(ann-protocol IUnifyTerms
              :methods
              {unify-terms [IUnifyTerms IUnifyTerms ISubstitution -> (U ISubstitution Fail)]})

(tc-ignore

(defprotocol IUnifyTerms
  (unify-terms [u v s]))

(defprotocol IUnifyWithNil
  (unify-with-nil [v u s]))

(defprotocol IUnifyWithObject
  (unify-with-object [v u s]))

(defprotocol IUnifyWithLVar
  (unify-with-lvar [v u s]))

(defprotocol IUnifyWithLSeq
  (unify-with-lseq [v u s]))

(defprotocol IUnifyWithSequential
  (unify-with-seq [v u s]))

(defprotocol IUnifyWithMap
  (unify-with-map [v u s]))

(defprotocol IUnifyWithSet
  (unify-with-set [v u s]))

(defprotocol IReifyTerm
  (reify-term [v s]))

(defprotocol IWalkTerm
  (walk-term [v s]))

(defprotocol IOccursCheckTerm
  (occurs-check-term [v x s]))

(defprotocol IBuildTerm
  (build-term [u s]))

(defprotocol IBind
  (bind [this g]))

(defprotocol IMPlus
  (mplus [a f]))

(defprotocol ITake
  (take* [a]))

)

;; =============================================================================
;; Pair

(ann-protocol IPair
              :methods
              {lhs [IPair -> Any]
               rhs [IPair -> Any]})

(tc-ignore
(defprotocol IPair
  (lhs [this])
  (rhs [this]))
)

(ann-datatype Pair [[lhs :- Any]
                    [rhs :- Any]])

(deftype Pair [lhs rhs]
  clojure.lang.Counted
  (count [_] 2)
  clojure.lang.Indexed
  (nth [_ i] (case i
                   0 lhs
                   1 rhs
                   (throw (IndexOutOfBoundsException.))))
  (nth [_ i not-found] (case i
                             0 lhs
                             1 rhs
                             not-found))
  IPair
  (lhs [_] lhs)
  (rhs [_] rhs)
  java.util.Map$Entry
  (getKey [_] lhs)
  (getValue [_] rhs)
  Object
  (toString [_]
    (str "(" lhs " . " rhs ")")))

(ann pair [Any Any -> Pair])
(defn- ^Pair pair [lhs rhs]
  (Pair. lhs rhs))


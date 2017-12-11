;; adapted from tools.analyzer
(ns clojure.core.typed.analyzer2
  (:refer-clojure :exclude [macroexpand-1 macroexpand var? record? boolean?])
  (:require [clojure.tools.analyzer.utils :as u]
            [clojure.tools.analyzer.env :as env])
  (:import (clojure.lang Symbol IPersistentVector IPersistentMap IPersistentSet ISeq IType IRecord)))

(def ^{:dynamic  true
       :arglists '([form env])
       :doc      "If form represents a macro form, returns its expansion,
                  else returns form."}
  macroexpand-1)

(def ^{:dynamic  true
       :arglists '([[op & args] env])
       :doc      "Multimethod that dispatches on op, should default to -parse"}
  parse)

(def ^{:dynamic  true
       :arglists '([sym env])
       :doc      "Creates a var for sym and returns it"}
  create-var)

(def ^{:dynamic  true
       :arglists '([obj])
       :doc      "Returns true if obj represent a var form as returned by create-var"}
  var?)

(def specials
	'#{do if new quote set! try var
		 catch throw finally def .
		 let* letfn* loop* recur fn*})

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;platform independent indirection utilities
(ns clojure.core.typed.checker.indirect-utils)

(defmacro make-indirection [& vs]
  `(do 
     ~@(map (fn [v]
              (let [atm (symbol (str v "-atom"))]
                (when-not (resolve v)
                  [`(def ~atm (atom nil))
                   `(def ~v (fn [& args#]
                              (let [i# (deref ~atm)]
                                (when-not i#
                                  (throw (ex-info (str "Indirection for " 
                                                       '~(symbol (str (ns-name *ns*)) (str v))
                                                       " not set")
                                                  {})))
                                (apply i# args#))))])))
            vs)))

(defmacro add-indirection [i var]
  `(reset! ~(symbol (str i "-atom"))
           ; saves getting stung by var reloading
           ; Could just use a var, but we want to work on CLJS
           (fn [& args#] 
             (apply ~var args#))))

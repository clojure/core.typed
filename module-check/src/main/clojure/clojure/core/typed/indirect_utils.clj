;platform independent indirection utilities
(ns clojure.core.typed.indirect-utils)

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

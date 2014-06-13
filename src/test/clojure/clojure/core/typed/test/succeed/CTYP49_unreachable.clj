(ns clojure.core.typed.test.succeed.CTYP49-unreachable
  (:require [clojure.core.typed :as t]))

(t/ann ^:no-check request 
       ['{:url String, :method ':get} 
        -> (t/Atom1 '{:status Number, :body String})])
(declare request)

(t/ann get-or-throw [String -> '{:status Number :body String}])
(defn get-or-throw [url]
  (let [doc (request {:url url :method :get})
        doc @doc
        _ (if (not= (:status doc) 200) (throw (Exception. (str "Got bad status: " doc))))]
    doc))

(ns clojure.core.typed.test.pomegranate
  (:import (clojure.lang Named))
  (:require [clojure.core.typed :as t]))

#_(t/non-nil-return java.lang.Class/getDeclaredMethod :all)
#_(t/nilable-param java.lang.reflect.Method/invoke {2 #{0}})

(t/ann call-method [Class Named (t/Vec Class) (t/U nil Object) (t/U nil Object) * -> (t/U nil Object)])

;; call-method pulled from clojure.contrib.reflect, (c) 2010 Stuart Halloway & Contributors
(defn call-method
  "Calls a private or protected method.

  params is a vector of classes which correspond to the arguments to
  the method e

  obj is nil for static methods, the instance object otherwise.

  The method-name is given a symbol or a keyword (something Named)."
  [^Class klass method-name params obj & args]
  (let [method (doto (.getDeclaredMethod klass 
                                         (name method-name)
                                         (t/into-array> Class params))
                 (.setAccessible true))]
    (.invoke method obj (t/into-array> (t/U nil Object) args))))

(ns typed.test.pomegranate
  (:import (clojure.lang DynamicClassLoader Named Seqable IPersistentVector)
           (java.net URL URLClassLoader))

  (:require #_[clojure.java.io :as io]
            #_[cemerick.pomegranate.aether :as aether]
            [typed.core :refer [ann check-ns override-method tc-pr-env ann-protocol
                                tc-ignore non-nil-return nilable-param into-array>]]
            [clojure.repl :refer [pst]])
  (:refer-clojure :exclude (add-classpath)))

(non-nil-return java.lang.Class/getDeclaredMethod
                :all)
(nilable-param java.lang.reflect.Method/invoke
               {2 :all})

(ann call-method [Class Named (IPersistentVector Class) (U nil Object) (U nil Object) * -> (U nil Object)])

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
                                         (into-array> Class Class params))
                 (.setAccessible true))]
    (.invoke method obj (into-array> Object (U nil Object) args))))

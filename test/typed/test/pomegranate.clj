(ns typed.test.pomegranate
  (:import (clojure.lang DynamicClassLoader Named Seqable IPersistentVector)
           (java.net URL URLClassLoader))

  (:require #_[clojure.java.io :as io]
            #_[cemerick.pomegranate.aether :as aether]
            [typed.core :refer [ann check-ns override-method tc-pr-env ann-protocol
                                tc-ignore non-nil-return nilable-param]]
            [clojure.repl :refer [pst]])
  (:refer-clojure :exclude (add-classpath)))

(non-nil-return java.lang.Class/getDeclaredMethod
                :arities :all)
(nilable-param java.lang.reflect.Method/invoke
               {2 :all})

(ann call-method [Class Named (IPersistentVector Class) (U nil Object) (U nil Object) * -> (U nil Object)])

;; call-method pulled from clojure.contrib.reflect, (c) 2010 Stuart Halloway & Contributors
(defn- call-method
  "Calls a private or protected method.

  params is a vector of classes which correspond to the arguments to
  the method e

  obj is nil for static methods, the instance object otherwise.

  The method-name is given a symbol or a keyword (something Named)."
  [^Class klass method-name params obj & args]
  (tc-pr-env "Env")
  (-> klass (.getDeclaredMethod (name method-name)
                                (into-array Class params))
    (doto (.setAccessible true))
    (.invoke obj (into-array Object args))))

(ann-protocol URLClasspath
              :methods
              {can-modify? [URLClasspath -> (U true false)]
               add-url [URLClasspath URL -> Any]})

(tc-ignore
(defprotocol URLClasspath
  "Ability to dynamically add urls to classloaders.

This protocol is an implementation detail.  Use
`modifiable-classloader?` and `add-classpath` or `add-dependencies`
unless you are extending a type to this protocol."
  (^{:private true} can-modify? [this] "Returns true if the given classloader can be modified.")
  (^{:private true} add-url [this url] "add the url to the classpath"))
)

(extend-type DynamicClassLoader
  URLClasspath
  (can-modify? [this] true)
  (add-url [this url] (.addURL this url)))

(ann url-classloader-base (HMap {:can-modify? (Fn [URLClassLoader -> true])
                                 :add-url (Fn [URLClassLoader URL -> Any])}))
(def ^:private url-classloader-base
  {:can-modify? (constantly true)
   :add-url (fn [this url]
              (call-method URLClassLoader 'addURL [URL] this url))})

(extend URLClassLoader URLClasspath url-classloader-base)


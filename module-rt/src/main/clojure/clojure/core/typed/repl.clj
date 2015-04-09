(ns clojure.core.typed.repl
  (:require [clojure.tools.nrepl.middleware :as mid]
            [clojure.tools.nrepl.transport :as trans]
            [clojure.tools.nrepl.misc :as misc]
            [clojure.core.typed :as t]
            [clojure.core.typed.errors :as err]
            [clojure.tools.namespace.parse :as ns]))

(defn wrap-clj-repl [handler]
  (fn [{:keys [code transport] :as msg}]
    (let [current-ns (some-> msg :session deref (get #'*ns*))
          _ (assert (instance? clojure.lang.Namespace current-ns))
          typed? 
          (boolean
            (some-> current-ns meta :core.typed))
          _ (prn code)
          rcode (try (read-string code)
                     (catch Throwable e))
          ns-form? (and (coll? rcode) 
                        (= (first rcode) 'ns)
                        (= #'ns (ns-resolve current-ns 'ns))
                        (-> (second rcode)
                            meta :core.typed boolean))]
      (spit "Foo" (prn-str msg) :append true)
      (prn "ns-form?" ns-form?)
      (when (or ns-form? typed?)
        (try
          (let [{:keys [delayed-errors]} (t/check-form-info (read-string code))]
            (when (seq delayed-errors)
              (try (err/print-errors! delayed-errors)
                   (catch Exception e
                     (prn (.getMessage e))))))
          (catch Throwable e
            (prn "[core.typed] Unhandled exception while type checking\n\n"
                 (.getMessage e)))))
      (handler msg))))

(mid/set-descriptor! #'wrap-clj-repl
  {:requires #{"clone"}
   :expects #{"eval"}
   :handles {}})

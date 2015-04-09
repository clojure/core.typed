(ns clojure.core.typed.repl
  (:require [clojure.tools.nrepl.middleware :as mid]
            [clojure.tools.nrepl.transport :as transport]
            [clojure.tools.nrepl.misc :as misc]
            [clojure.core.typed :as t]
            [clojure.core.typed.errors :as err]
            [clojure.tools.namespace.parse :as ns])
  (:import java.io.Writer))

(defn wrap-clj-repl [handler]
  (fn [{:keys [code transport session op] :as msg}]
    (let [flush (fn []
                  (.flush ^Writer (@session #'*out*))
                  (.flush ^Writer (@session #'*err*)))
          current-ns (some-> session deref (get #'*ns*))
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
                            meta :core.typed boolean))
          should-check? (and (= "eval" op)
                             (or typed? ns-form?))]
      (spit "Foo" (prn-str msg) :append true)
      (prn "ns-form?" ns-form?)
      (if should-check?
        (binding [*out* (@session #'*out*)]
          (try
            (let [{:keys [delayed-errors result]} (t/check-form-info (read-string code))]
              (when (seq delayed-errors)
                (try (err/print-errors! delayed-errors)
                     (catch Exception e
                       (prn (.getMessage e)))))
              (flush)
              (prn "Value =" result)
              (transport/send transport (misc/response-for msg {:value (pr-str result)
                                                                :printed-value true
                                                                :ns (some-> session deref (get #'*ns*))})))
            (catch Throwable e
              (prn "[core.typed] Unhandled exception while type checking\n\n"
                   (.getMessage e))
              (flush)
              )))
        (handler msg))
      (prn "FINISHED"))))

(mid/set-descriptor! #'wrap-clj-repl
  {:requires #{"clone"}
   :expects #{"eval"}
   :handles {}})

(ns clojure.core.typed.repl
  (:require [clojure.tools.nrepl.middleware :as mid]
            [clojure.tools.nrepl.transport :as transport]
            [clojure.tools.nrepl.misc :as misc]
            [clojure.core.typed :as t]
            [clojure.core.typed.errors :as err]
            [clojure.tools.namespace.parse :as ns]
            [clojure.core.typed.current-impl :as impl]
            [clojure.main :as main])
  (:import java.io.Writer))

(defn wrap-clj-repl [handler]
  (fn [{:keys [code transport session op] :as msg}]
    (let [;original-ns (@session #'*ns*)
          maybe-explicit-ns (when-let [ns (some-> (:ns msg) symbol find-ns)]
                              {#'*ns* ns})
          _ (assert (if (:ns msg)
                      (get maybe-explicit-ns #'*ns*)
                      true)
                    (str "Namespace " (:ns msg)
                         " does not exist"))
          _ (when maybe-explicit-ns
              (swap! session merge maybe-explicit-ns))
          flush (fn []
                  (.flush ^Writer (@session #'*out*))
                  (.flush ^Writer (@session #'*err*)))
          current-ns (@session #'*ns*)
          _ (assert (instance? clojure.lang.Namespace current-ns))
          typed? (boolean
                   (some-> current-ns meta :core.typed))
          rfail? (atom false)
          rcode (try (read-string code)
                     (catch Throwable e
                       (reset! rfail? true)
                       nil))
          ns-form? (and (coll? rcode) 
                        (= (first rcode) 'ns)
                        (= #'ns (ns-resolve current-ns 'ns))
                        (-> (second rcode)
                            meta :core.typed boolean))
          should-check? (and (= "eval" op)
                             (or typed? ns-form?)
                             (not @rfail?))]
      ;(prn "code" code)
      ;(prn "current-ns" current-ns)
      ;(prn "ns-msg" (:ns msg))
      ;(prn "msg" msg)
      ;(prn "should-check?" should-check?)
      (if should-check?
        (binding [*out* (@session #'*out*)]
          (t/load-if-needed)
          (impl/with-clojure-impl
            (try
              (let [[{:keys [delayed-errors out-form ret]} 
                     new-ns]
                    ;; FIXME should bindings should be in scope when rcode is macroexpanded?
                    ;; probably need to pass bindings into tools.analyzer expansion
                    (binding [*ns* current-ns]
                      [(t/check-form-info rcode
                                          :no-eval true
                                          :bindings-atom session)
                       *ns*])]
                (binding [*ns* new-ns]
                  (prn :- (:t ret)))
                (flush)
                (if (seq delayed-errors)
                  ;; ** throws exception, jumps to catch clause **
                  (err/print-errors! delayed-errors)
                  (handler (assoc msg :code (binding [*print-dup* true]
                                              ;; TODO out-form should have types attached to it!
                                              (pr-str out-form))))))
              (catch Throwable e
                (let [root-ex (#'clojure.main/root-cause e)]
                  (when-not (instance? ThreadDeath root-ex)
                    (flush)
                    (swap! session assoc #'*e e)
                    (transport/send transport 
                                    (misc/response-for msg {:status :eval-error
                                                            :ex (-> e class str)
                                                            :root-ex (-> root-ex class str)}))
                    (main/repl-caught e)))))))
        (handler msg)))))

(mid/set-descriptor! #'wrap-clj-repl
  {:requires #{"clone"}
   :expects #{"eval"}
   :handles {}})

(ns clojure.core.typed.repl
  (:require [clojure.tools.nrepl.middleware :as mid]
            [clojure.tools.nrepl.transport :as trans]
            [clojure.tools.nrepl.misc :as misc]
            [clojure.core.typed :as t]
            [clojure.core.typed.errors :as err]
            [clojure.tools.namespace.parse :as ns]))

(defn wrap-clj-repl [handler]
  (fn [{:keys [code transport] :as msg}]
    (let [typed? 
          (boolean
            (some-> msg :session deref (get #'*ns*)
                    meta :core.typed))]
      (spit "Foo" (prn-str msg) :append true)
      (when typed?
        (let [{:keys [delayed-errors]} (t/check-form-info (read-string code))]
          (when (seq delayed-errors)
            (try (err/print-errors! delayed-errors)
                 (catch Exception e
                   (prn (.getMessage e)))))))
      (handler msg))))

(mid/set-descriptor! #'wrap-clj-repl
  {:requires #{"clone"}
   :expects #{"eval"}
   :handles {}})

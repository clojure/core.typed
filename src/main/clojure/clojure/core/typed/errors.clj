(ns ^:skip-wiki clojure.core.typed.errors
  (:require [clojure.core.typed.util-vars :refer [*current-env*] :as uvs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.ast-utils :as ast-u]))

(alter-meta! *ns* assoc :skip-wiki true)

;(t/ann ^:no-check deprecated-warn [String -> nil])
;(t/ann ^:no-check int-error [String -> Nothing])

(def int-error-kw ::internal-error)

;(t/ann ^:no-check env-for-error [Any -> Any])
(defn env-for-error [env]
  env)

(defn int-error
  [estr]
  (let [env *current-env*]
    (throw (ex-info (str "Internal Error "
                         "(" (or (:file env) (-> env :ns :name)) ":" 
                         (or (:line env) "<NO LINE>")
                         (when-let [col (:column env)]
                           (str ":" col))
                         ") "
                         estr)
                    {:type-error int-error-kw}))))

;[Any * -> String]
(defn ^String error-msg 
  [& msg]
  (apply str (when *current-env*
               (str (:line *current-env*) ":"
                    (:col *current-env*)
                    " "))
         (concat msg)))

;errors from check-ns or cf
(defn top-level-error? [{:keys [type-error] :as exdata}]
  (boolean (#{:top-level-error} type-error)))

(defmacro top-level-error-thrown? [& body]
  `(with-ex-info-handlers
     [top-level-error? (constantly true)]
     ~@body
     false))

(defmacro tc-error-thrown? [& body]
  `(with-ex-info-handlers
     [tc-error? (constantly true)]
     ~@body
     false))

(def tc-error-parent ::tc-error-parent)

(defn tc-error? [exdata]
  (assert (not (instance? clojure.lang.ExceptionInfo exdata)))
  (isa? (:type-error exdata) tc-error-parent))

(defn tc-delayed-error [msg & {:keys [return form] :as opt}]
  (let [e (ex-info msg (merge {:type-error tc-error-parent}
                              (when (or (contains? opt :form)
                                        (and (bound? #'uvs/*current-expr*)
                                             uvs/*current-expr*))
                                {:form (if (contains? opt :form)
                                         form
                                         (ast-u/emit-form-fn uvs/*current-expr*))})
                              {:env (or (when uvs/*current-expr*
                                          (:env uvs/*current-expr*))
                                        (env-for-error *current-env*))}))]
    (cond
      ;can't delay here
      (not (bound? (impl/the-var 'clojure.core.typed/*delayed-errors*)))
      (throw e)

      :else
      (do
        (if-let [delayed-errors (impl/v 'clojure.core.typed/*delayed-errors*)]
          (swap! delayed-errors conj e)
          (throw (Exception. (str "*delayed-errors* not rebound"))))
        (or return (impl/v 'clojure.core.typed.type-rep/-nothing))))))

(defn derive-error [kw]
  (derive kw tc-error-parent))

(def int-error-kw ::internal-error)
(def nyi-error-kw ::nyi-error)

(derive-error int-error-kw)
(derive-error nyi-error-kw)

(defn tc-error
  [estr]
  (let [env *current-env*]
    (throw (ex-info (str "Type Error "
                         "(" (:file env) ":" (or (:line env) "<NO LINE>")
                         (when-let [col (:column env)]
                           (str ":" col))
                         ") "
                         estr)
                    (merge
                      {:type-error tc-error-parent}
                      {:env (env-for-error env)})))))

(defn deprecated-warn
  [msg]
  (let [env *current-env*]
    (println "DEPRECATED SYNTAX "
             "(" (:file env) ":" (or (:line env) "<NO LINE>")
             (when-let [col (:column env)]
               (str ":" col))
             ") :"
             msg)
    (flush)))

(defn nyi-error
  [estr]
  (let [env *current-env*]
    (throw (ex-info (str "core.typed Not Yet Implemented Error:"
                           "(" (:file env) ":" (or (:line env) "<NO LINE>")
                           (when-let [col (:column env)]
                             (str ":"col))
                           ") "
                           estr)
                    (merge {:type-error nyi-error-kw}
                           {:env (env-for-error env)})))))

(defmacro with-ex-info-handlers 
  "Handle an ExceptionInfo e thrown in body. The first handler whos left hand
  side returns true, then the right hand side is called passing (ex-info e) and e."
  [handlers & body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo e#
       (let [found?# (atom false)
             result# (reduce (fn [_# [h?# hfn#]]
                               (when (h?# (ex-data e#))
                                 (reset! found?# true)
                                 (reduced (hfn# (ex-data e#) e#))))
                             nil
                             ~(mapv vec (partition 2 handlers)))]
         (if @found?#
           result#
           (throw e#))))))

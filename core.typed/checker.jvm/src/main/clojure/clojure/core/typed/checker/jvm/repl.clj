;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.jvm.repl
  (:require [clojure.tools.nrepl.middleware :as mid]
            [clojure.tools.nrepl.transport :as transport]
            [clojure.tools.nrepl.misc :as misc]
            [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.ns-deps-utils :as ns-utils]
            ;[clojure.core.typed.load :as load]
            [clojure.core.typed.checker.jvm.analyze-clj :as ana-clj]
            [clojure.tools.reader :as rd]
            [clojure.tools.reader.reader-types :as readers]
            [clojure.main :as main])
  (:import java.io.Writer))

(defn typed-ns-form? [current-ns rcode]
  (and (seq? rcode) 
       (= (first rcode) 'ns)
       (= #'ns (when current-ns
                 (ns-resolve current-ns 'ns)))
       (ns-utils/ns-has-core-typed-metadata? rcode)))

(defn ns-has-core-typed-meta? [nsym]
  {:pre [(or (symbol? nsym)
             (instance? clojure.lang.Namespace nsym))]}
  (let [ns (find-ns (if (symbol? nsym)
                      ;; don't call ns-name
                      ;; in case this namespace doesn't exist yet
                      nsym
                      (ns-name nsym)))]
    (boolean
      (some-> ns meta :core.typed))))

(defn typed-in-ns-form? [current-ns rcode]
  ;(prn "typed-in-ns-form?" rcode)
  (and (seq? rcode)
       (= 2 (count rcode))
       (let [[op quoted-sym] rcode]
         (and
           (= op 'in-ns)
           (= #'in-ns (when current-ns
                        (ns-resolve current-ns 'in-ns)))
           (seq? quoted-sym)
           (= 2 (count quoted-sym))
           (let [[q sym] quoted-sym]
             (and
               (= 'quote q)
               (symbol? sym)
               (ns-has-core-typed-meta? sym)))))))

(defn handle-eval [handler {:keys [transport session code op] :as msg}]
  (let [original-ns (@session #'*ns*)
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
        typed? (ns-has-core-typed-meta? current-ns)
        rfail? (atom nil)
        rcode (try (if (string? code)
                     (read-string code)
                     code)
                   (catch Throwable e
                     (reset! rfail? e)
                     nil))
        should-check? (and (or typed? (typed-ns-form? current-ns rcode))
                           (not @rfail?))]
    ;(prn "code" code)
    ;(prn "current-ns" current-ns)
    ;(prn "ns-msg" (:ns msg))
    ;(prn "msg" msg)
    ;(prn "should-check?" should-check?)
    ;(prn "rcode" rcode)
    (cond 
      should-check?
      (binding [*out* (@session #'*out*)
                *err* (@session #'*err*)]
        (try
          (t/load-if-needed)
          (impl/with-clojure-impl
            (let [{:keys [ret result ex]}
                  (t/check-form-info rcode
                                     :eval-out-ast #(ana-clj/eval-ast % {})
                                     :bindings-atom session)]
              (if ex
                (let [root-ex (#'clojure.main/root-cause ex)]
                  (when-not (instance? ThreadDeath root-ex)
                    (do
                      (flush)
                      (swap! session assoc #'*e ex)
                      (transport/send transport 
                                      (misc/response-for msg {:status :eval-error
                                                              :ex (-> ex class str)
                                                              :root-ex (-> root-ex class str)}))
                      (main/repl-caught ex)
                      (flush))))
                (do
                  (swap! session assoc
                         #'*3 (@session #'*2)
                         #'*2 (@session #'*1)
                         #'*1 result)
                  (prn :- (:t ret))
                  (flush)
                  (transport/send transport 
                                  (misc/response-for msg {:value (pr-str result)
                                                          :ns (-> (@session #'*ns*) ns-name str)}))))
              (when maybe-explicit-ns
                (swap! session assoc #'*ns* original-ns))
              (transport/send transport 
                              (misc/response-for msg {:status :done}))))
          (finally
            (flush))))
      :else (handler msg))))

(defn handle-load-file [handler {:keys [file file-name file-path session transport] :as msg}]
  {:pre [session transport]}
  (let [rfail? (atom false)
        rcode (try (read-string file)
                   (catch Throwable e
                     (reset! rfail? true)
                     nil))
        flush (fn []
                (.flush ^Writer (@session #'*out*))
                (.flush ^Writer (@session #'*err*)))
        should-check? (and (or (typed-ns-form? 'user rcode)
                               ;; vim-fireplace abuses :file to switch namespaces
                               (typed-in-ns-form? 'user rcode))
                           (not @rfail?))]
    ;(prn "rcode" rcode)
    ;(prn "should-check?" should-check?)
    ;(prn "file" file)
    (cond
      should-check?
      (binding [*out* (@session #'*out*)
                *err* (@session #'*err*)]
        (t/load-if-needed)
        (impl/with-clojure-impl
          (let [eof (Object.)
                rdr (readers/indexing-push-back-reader file 1 file-path)]
            (binding [*ns* *ns*
                      *file* file-path
                      *source-path* file-name]
              (loop [result nil]
                (let [rcode (rd/read rdr false eof)]
                  ;(prn "rcode" rcode)
                  (if (not= eof rcode)
                    (do
                      ;(prn "before" rcode (@session #'*ns*))
                      (let [{:keys [ret result ex]}
                            (t/check-form-info rcode
                                               ;:eval-out-ast #(ana-clj/eval-ast % {})
                                               :bindings-atom session)]
                        ;(prn "after" ex result)
                        (if ex
                          (let [root-ex (#'clojure.main/root-cause ex)]
                            (when-not (instance? ThreadDeath root-ex)
                              (swap! session assoc #'*e ex)
                              (transport/send transport 
                                              (misc/response-for msg {:status :eval-error
                                                                      :ex (-> ex class str)
                                                                      :root-ex (-> root-ex class str)}))
                              (main/repl-caught ex)
                              (flush)))
                          (do
                            (recur result)))))
                    (do
                      (swap! session assoc
                             #'*3 (@session #'*2)
                             #'*2 (@session #'*1)
                             #'*1 result)
                      (transport/send transport 
                                      (misc/response-for msg {:value (pr-str result)}))))))
              (transport/send transport 
                              (misc/response-for msg {:status :done}))))))
      :else (handler msg))))

(defn wrap-clj-repl [handler]
  ;(load/monkeypatch-typed-load)
  (fn [{:keys [op] :as msg}]
    ;(prn "wrap-clj-repl" op)
    (cond 
      (= "load-file" op) (handle-load-file handler msg)
      (= "eval" op)      (handle-eval handler msg)
      :else (handler msg))))

(mid/set-descriptor! #'wrap-clj-repl
  {:requires #{"clone"}
   :expects #{"eval" "load-file"}
   :handles {}})

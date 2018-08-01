(ns clojure.core.typed.check2
  (:require [clojure.core.typed.analyzer2.pre-analyze :as pre]
            [clojure.core.typed.analyzer2.jvm.pre-analyze :as jpre]
            [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as taj]
            [clojure.core.typed.analyze-clj :as anaclj]
            [clojure.core.typed.analyzer2.jvm :as ana]
            [clojure.tools.analyzer.env :as env]
            [clojure.tools.analyzer.passes.jvm.emit-form :as emit-form]
            [clojure.core.typed.utils :as u]
            ))

(defn check* [{:keys [op] :as ast} expected {:keys [pre post] :as opts}]
  ;{:pre [((some-fn nil? r/TCResult?) expected)]
  ; :post [(r/TCResult? (u/expr-type %))]}
  ;(prn "op" (:op ast))
  ;(prn "check* ns" (get-in ast [:env :ns]))
  (cond
    (u/expr-type ast) ast
    (= :unanalyzed op) (do
                         ;(prn ":unanalyzed")
                         (clojure.pprint/pprint (:form ast))
                         (recur (jpre/pre-analyze ast) expected opts))
    :else
    (let [ast (pre ast)
          chk (fn [ast & [expected]]
                (check* ast expected opts))]
      (case (:op ast)
        :do (let [_ (assert (every? (comp #{:unanalyzed} :op)
                                    (cons (:ret ast) (:statements ast))))
                  env-for-ret (atom nil)
                  unreachable? (atom nil)
                  ast (cond-> ast
                        (not @unreachable?)
                        (update :statements (fn [statements]
                                              (cond
                                                (and (= 2 (count statements))
                                                     (-> statements peek :form #{:THE-special-form-kw}))
                                                statements ;check special

                                                :else (let [[env statements]
                                                            (reduce (fn [[env statements] statement]
                                                                      (let [_ (assert ((comp #{:unanalyzed} :op) statement))
                                                                            statement (cond-> statement
                                                                                        (get-in ast [::pre/config :top-level])
                                                                                        (assoc-in [:env :ns] (ns-name *ns*)))]
                                                                        ;; use env to check statement
                                                                        (if @unreachable?
                                                                          [env (conj statements statement)]
                                                                          ; update env
                                                                          [env (conj statements (check* statement nil opts))])))
                                                                    [{} []]
                                                                    statements)]
                                                        (reset! env-for-ret env)
                                                        statements))))
                        (not @unreachable?)
                        (update :ret (fn [ret]
                                       (let [_ (assert ((comp #{:unanalyzed} :op) ret))
                                             ret (cond-> ret
                                                   (get-in ast [::pre/config :top-level])
                                                   (assoc-in [:env :ns] (ns-name *ns*)))]
                                         ;; use env (if any) to check ret
                                         (if @unreachable?
                                           ret
                                           (check* ret expected opts)))))
                        (not @unreachable?)
                        post)]
              ;; check-below
              (assoc ast u/expr-type (or (-> ast :ret u/expr-type)
                                        'Bottom)))
        :const (let [ast (post ast)]
                 ;; check-below
                 (assoc ast u/expr-type `(Value ~(:val ast))))
        :quote (let [ast (-> ast
                             (update :expr #(check* % nil opts))
                             post)]
                 ;; check-below
                 (assoc ast u/expr-type (u/expr-type (:expr ast))))
        :invoke (let [env-for-args (atom nil)
                      unreachable? (atom nil)
                      ast (cond-> ast
                            (not @unreachable?)
                            (update :fn #(let [cfn (check* % nil opts)]
                                           ;; check if unreachable
                                           ;; update env-for-args
                                           cfn))
                            (not @unreachable?)
                            (update :args (fn [args]
                                            (let [[env args]
                                                  (reduce (fn [[env args] i]
                                                            (if @unreachable?
                                                              (reduced [env args])
                                                              ; update env & use to check arg
                                                              [env (update args i #(let [arg (check* % nil opts)]
                                                                                     ;; check if result is bottom and update unreachable?
                                                                                     arg))]))
                                                          [(or @env-for-args {}) args]
                                                          (range (count args)))]
                                              args)))
                            (not @unreachable?)
                            post)]
                  (assoc ast u/expr-type 'RetOfFn))
        :binding (let [;; TODO check reachability
                       ast (-> (cond-> ast
                                 (:init ast)
                                 (update :init #(check* % nil opts)))
                               post)]
                   ; attach type information?
                   (if-let [init (:init ast)]
                     (assoc ast u/expr-type (u/expr-type init))
                     ast))
        :let (let [env-for-body (atom nil)
                   unreachable? (atom nil)
                   ast (cond-> ast
                         (not @unreachable?)
                         (update :bindings (fn [bindings]
                                             (let [[env bindings]
                                                   (reduce (fn [[env bindings] binding]
                                                             (if @unreachable?
                                                               [env (conj bindings binding)]
                                                               ; update env & use to check binding
                                                               (let [binding (check* binding nil opts)]
                                                                 ;; check if result is bottom and update unreachable?
                                                                 [env (conj bindings binding)])))
                                                           [{} []]
                                                           bindings)]
                                               bindings)))
                         (not @unreachable?)
                         (update :body #(let [env @env-for-body]
                                          (check* % expected opts)))
                         (not @unreachable?)
                         post)]
               (assoc ast u/expr-type (or (-> ast :body u/expr-type)
                                         'Bottom)))
        :def (let [unreachable? (atom nil)
                   ast (cond-> ast
                         (and (not @unreachable?)
                              (:init ast))
                         (update :init #(let [init (check* % nil opts)]
                                          ;; update unreachable?
                                          init))
                         (not @unreachable?)
                         post)]
               (assoc ast u/expr-type 'VarType))
        :var (let [ast (post ast)]
               (assoc ast u/expr-type 'VarType))
        :fn-method (let [ast (-> ast
                                 (update :params #(mapv (fn [param]
                                                          (check* param nil opts))
                                                        %))
                                 (update :body #(check* % nil opts))
                                 post)]
                     (assoc ast u/expr-type 'FnMethod))
        :map (let [ast (-> ast
                           (update :keys #(mapv (fn [k]
                                                  (check* k nil opts))
                                                %))
                           (update :vals #(mapv (fn [v]
                                                  (check* v nil opts))
                                                %))
                           post)]
               (assoc ast u/expr-type 'HMap))
        :with-meta (let [ast (-> ast
                                 (update :meta #(check* % nil opts))
                                 (update :expr #(check* % nil opts))
                                 post)]
                     (assoc ast u/expr-type (u/expr-type (:expr ast))))
        :the-var (let [ast (-> ast
                               post)]
                   (assoc ast u/expr-type 'TheVar))
        :if (let [ast (-> ast
                          (update :test #(check* % nil opts))
                          (update :then #(check* % nil opts))
                          (update :else #(check* % nil opts))
                          post)]
              (assoc ast u/expr-type '(U Then Else)))
        :try (let [ast (cond-> ast
                         true
                         (update :body #(check* % nil opts))
                         true
                         (update :catches #(mapv (fn [catch]
                                                   (check* catch nil opts))
                                                 %))
                         (:finally ast)
                         (update :body #(check* % nil opts))

                         true
                         post)]
               (assoc ast u/expr-type '(U Body Catches)))
        :local (let [ast (-> ast
                             post)]
                 (assoc ast u/expr-type 'local))
        :fn (let [self-name (atom nil)
                  ast (cond-> ast
                        (:local ast)
                        (update :local (fn [local]
                                         (let [local (check* local nil opts)]
                                           (reset! self-name (:name local))
                                           local)))

                        true
                        (update :methods #(mapv (fn [method]
                                                  (let [;; do something with self-name
                                                        self-name @self-name]
                                                    (check* method nil opts)))
                                                %))
                        true
                        post)]
              (assoc ast u/expr-type 'FnType))
        ;; interop
        :new (let [env-for-args (atom nil)
                   unreachable? (atom nil)
                   ast (cond-> ast
                         (not @unreachable?)
                         (update :class #(let [class (check* % nil opts)]
                                           ;; update env-for-args
                                           class))

                         (not @unreachable?)
                         (update :args (fn [args]
                                         (let [[env args]
                                               (reduce (fn [[env args] arg]
                                                         (if @unreachable?
                                                           [env (conj args arg)]
                                                           ; update env & use to check arg
                                                           (let [arg (check* arg nil opts)]
                                                             ;; check if result is bottom and update unreachable?
                                                             [env (conj args arg)])))
                                                       [{} []]
                                                       args)]
                                           args)))

                         (not @unreachable?)
                         post)]
               ;; check-below
               (assoc ast u/expr-type 'New))
        :maybe-class (let [ast (post ast)]
                       (assert (not= :maybe-class (:op ast))
                               "Reflection found")
                       (assoc ast u/expr-type 'Class))
        :host-field (let [ast (-> ast
                                  (update :target #(check* % nil opts))
                                  post)]
                      (assert (not= :host-field (:op ast))
                              "Reflection found")
                      (assoc ast u/expr-type 'FieldResult))
        :host-call (let [unreachable? (atom nil)
                         ast (cond-> ast
                               (not @unreachable?)
                               (update :target #(let [target (check* % nil opts)]
                                                  ; update reachability
                                                  target))
                               (not @unreachable?)
                               (update :args (fn [args]
                                               (let [[env args]
                                                     (reduce (fn [[env args] arg]
                                                               (if @unreachable?
                                                                 [env (conj args arg)]
                                                                 ; update env & use to check arg
                                                                 (let [arg (check* arg nil opts)]
                                                                   ;; check if result is bottom and update unreachable?
                                                                   [env (conj args arg)])))
                                                             ;; TODO env for args?
                                                             [{} []]
                                                             args)]
                                                 args)))
                               (not @unreachable?)
                               post)]
                     (assert (not= :host-call (:op ast))
                             "Reflection found")
                     (assoc ast u/expr-type 'CallResult))
        :maybe-host-form (let [ast (post ast)]
                           (assert (not= :maybe-host-form (:op ast))
                                   "Reflection found")
                           (assoc ast u/expr-type 'Host))
        :host-interop (let [ast (-> ast
                                    (update :target #(check* % nil opts))
                                    post)]
                        (assert (not= :host-interop (:op ast))
                                "Reflection found")
                        (assoc ast u/expr-type 'Host))))))

(defn pre-gilardi [ast]
  {:pre [(not (= :unanalyzed (:op ast)))]}
  (if (get-in ast [::pre/config :top-level])
    (case (:op ast)
      :do (ast/update-children ast #(assoc-in % [::pre/config :top-level] true))
      (assoc ast ::eval-gildardi? true))
    ast))

(defn post-gilardi [ast]
  (if (::eval-gildardi? ast)
    (let [form (emit-form/emit-form ast)
          ;_ (prn "before eval" *ns*)
          _ (clojure.pprint/pprint form)
          ;_ (prn "refers defmacro" ('defmacro (ns-refers *ns*)))
          result (eval form)]
      (taj/update-ns-map!)
      ;(prn "afer eval" *ns*)
      (assoc ast :result result))
    ast))

(defn check-top-level [form expected]
  (with-bindings (assoc (anaclj/thread-bindings) #'clojure.core.typed.analyzer2/macroexpand-1 #'ana/macroexpand-1)
    (env/ensure (taj/global-env)
      (taj/update-ns-map!)
      (let [passes (-> ana/scheduled-default-passes
                       (update :pre #(comp pre-gilardi %))
                       (update :post #(comp post-gilardi %)))]
        (let [nssym (gensym)
              form `(do (ns ~(gensym))
                        ~form)
              res (-> form
                      (pre/pre-analyze-child (taj/empty-env))
                      (assoc-in [::pre/config :top-level] true)
                      (check* expected passes))]
          ;(prn "form" form)
          res)))))

(comment
        (check 1 nil)
        (keys (check '(.toString 1) nil))
        (keys (check '(.compareTo (java.io.File. "a") (java.io.File. "a")) nil))
        (keys (check '(.compareTo (java.io.File. "a") (java.io.File. "a")) nil))
        (:op (check 'java.io.File/pathSeparator nil))
        (:op (check '(defmulti foo :op) nil))
        ;; gilardi
        (:op (check '(do (def foo 1)
                         (inc foo))
                    nil))
        (:op (check '(do (defmacro id [a] a)
                         (id 'a))
                    nil))
        (:op (check '(do (defmacro id [a] a)
                         (id 'a))
                    nil))
        )

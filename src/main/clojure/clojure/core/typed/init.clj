(ns clojure.core.typed.init)

(def ^:private attempted-loading? (atom false))
(def ^:private successfully-loaded? (atom false))

(defn loaded? []
  @successfully-loaded?)

(defn load-impl []
  (cond 
    (and @attempted-loading?
         (not @successfully-loaded?))
    (throw (Exception. 
             (str "There was previously an unrecoverable internal error while loading core.typed." 
                  " Please restart your process.")))

    (and @successfully-loaded? @attempted-loading?)
    nil

    :else
    (do
      (try
        (reset! attempted-loading? true)
        (require '[clojure.core.typed
                   [utils]
                   [type-rep]
                   [type-ctors]
                   [filter-rep]
                   [filter-ops]
                   [subst]
                   [path-rep]
                   [object-rep]
                   [fold-rep]
                   [fold-default]
                   [parse-unparse]
                   [lex-env]
                   [var-env]
                   [parse-unparse]
                   [current-impl]
                   [dvar-env]
                   [datatype-ancestor-env]
                   [datatype-env]
                   [protocol-env]
                   [method-override-env]
                   [ctor-override-env]
                   [method-return-nilables]
                   [method-param-nilables]
                   [declared-kind-env]
                   [name-env]
                   [rclass-env]
                   [mm-env]
                   [constant-type]
                   [parse-unparse]
                   [frees]
                   [free-ops]
                   [cs-gen]
                   [trans]
                   [inst]
                   [subtype]
                   [array-ops]
                   [check]
                   [collect-phase]
                   [base-env]
                   [ns-deps]
                   [reset-env]]
                 '[clojure.reflect])
        (catch Exception e
          (reset! successfully-loaded? false)
          (throw e)))
      (reset! successfully-loaded? true)
      (@(ns-resolve (find-ns 'clojure.core.typed.reset-env) 'reset-envs!))
      nil)))

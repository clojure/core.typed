(ns clojure.core.typed.reset-env
  (:require [clojure.core.typed
             [type-ctors :as tc]
             [lex-env :as lex]
             [var-env :as var-env]
             [parse-unparse :as prs]
             [method-override-env :as mth-override]
             [ctor-override-env :as ctor-override]
             [method-return-nilables :as ret-env]
             [method-param-nilables :as param-env]
             [name-env :as nme-env]
             [rclass-env :as rcls-env]
             [base-env :as bse]
             [ns-deps :as deps]]))

(defn reset-envs! []
  (nme-env/reset-name-env! bse/init-alias-env)
  (var-env/reset-var-type-env! bse/init-var-env bse/init-var-nochecks)
  (ret-env/reset-nonnilable-method-return-env! bse/init-method-nonnilable-return-env)
  (param-env/reset-method-nilable-param-env! bse/init-method-nilable-param-env)
  (mth-override/reset-method-override-env! bse/init-method-override-env)
  (ctor-override/reset-constructor-override-env! bse/init-ctor-override-env)
  (rcls-env/reset-rclass-env! bse/init-altered-env)
  (deps/reset-deps!)
  nil)


(ns clojure.core.typed.reset-env
  (:require [clojure.core.typed.type-ctors :as tc]
            [clojure.core.typed.lex-env :as lex]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.method-override-env :as mth-override]
            [clojure.core.typed.ctor-override-env :as ctor-override]
            [clojure.core.typed.method-return-nilables :as ret-env]
            [clojure.core.typed.method-param-nilables :as param-env]
            [clojure.core.typed.name-env :as nme-env]
            [clojure.core.typed.rclass-env :as rcls-env]
            [clojure.core.typed.base-env :as bse]
            [clojure.core.typed.ns-deps :as deps]
            [clojure.core.typed.ns-options :as ns-opts]))

(defn reset-envs! []
  (nme-env/reset-name-env! bse/init-alias-env)
  (var-env/reset-var-type-env! bse/init-var-env bse/init-var-nochecks)
  (ret-env/reset-nonnilable-method-return-env! bse/init-method-nonnilable-return-env)
  (param-env/reset-method-nilable-param-env! bse/init-method-nilable-param-env)
  (mth-override/reset-method-override-env! bse/init-method-override-env)
  (ctor-override/reset-constructor-override-env! bse/init-ctor-override-env)
  (rcls-env/reset-rclass-env! bse/init-altered-env)
  (deps/reset-deps!)
  (ns-opts/reset-ns-opts!)
  nil)


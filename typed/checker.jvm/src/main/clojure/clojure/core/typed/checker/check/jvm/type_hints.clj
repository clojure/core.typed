;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.jvm.type-hints
  (:require [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.check.utils :as cu]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.checker.jvm.reflect-utils :as reflect-u]))

(defn suggest-type-hints [m-or-f targett argtys & {:keys [constructor-call]}]
  {:pre [((some-fn nil? r/Type?) targett)
         (every? r/Type? argtys)]}
  (let [targett (when targett
                  (c/fully-resolve-type targett))
        cls (cond
              constructor-call (coerce/symbol->Class constructor-call)
              :else (cu/Type->Class targett))]
    (when cls
      (let [r (reflect-u/reflect cls)
            {methods clojure.reflect.Method
             fields clojure.reflect.Field
             ctors clojure.reflect.Constructor
             :as members}
            (group-by
              class
              (filter (fn [{:keys [name] :as m}] 
                        (if constructor-call
                          (instance? clojure.reflect.Constructor m)
                          (= m-or-f name)))
                      (:members r)))]
      (cond
        (empty? members) (str "\n\nTarget " (coerce/Class->symbol cls) " has no member " m-or-f)
        (seq members) (str "\n\nAdd type hints to resolve the host call."
                           (when (seq ctors)
                             (str "\n\nSuggested constructors:\n"
                                  (apply str
                                           (map 
                                             (fn [{ctor-name :name 
                                                   :keys [parameter-types flags] :as field}]
                                               (str "\n  "
                                                    (apply str (interpose " " (map name flags)))
                                                    (when (seq flags) " ")
                                                    (reflect-u/pprint-reflection-sym ctor-name)
                                                    "("
                                                    (apply str 
                                                           (interpose 
                                                             ", " 
                                                             (map reflect-u/pprint-reflection-sym parameter-types)))
                                                    ")"))
                                             ctors))))
                             (when (seq fields)
                               (str "\n\nSuggested fields:\n"
                                    (apply str
                                           (map 
                                             (fn [[clssym cls-fields]]
                                               (apply str
                                                      "\n " (reflect-u/pprint-reflection-sym clssym)
                                                      "\n \\"
                                                      (map
                                                        (fn [{field-name :name 
                                                              :keys [flags type] :as field}]
                                                          (str "\n  "
                                                               (apply str (interpose " " (map name flags)))
                                                               (when (seq flags) " ")
                                                               (reflect-u/pprint-reflection-sym type) " "
                                                               field-name))
                                                        cls-fields)))
                                             (group-by :declaring-class fields)))))
                             (when (seq methods)
                               (let [methods-by-class (group-by :declaring-class methods)]
                                 (str "\n\nSuggested methods:\n"
                                      (apply str
                                             (map
                                               (fn [[clsym cls-methods]]
                                                 (apply str
                                                        "\n " (reflect-u/pprint-reflection-sym clsym)
                                                        "\n \\"
                                                        (map 
                                                          (fn [{method-name :name 
                                                                :keys [return-type parameter-types flags] :as method}] 
                                                            (str 
                                                              "\n  "
                                                              (apply str (interpose " " (map name flags)))
                                                              (when (seq flags) " ")
                                                              (reflect-u/pprint-reflection-sym return-type) " "
                                                              method-name 
                                                              "(" 
                                                              (apply str 
                                                                     (interpose 
                                                                       ", " 
                                                                       (map reflect-u/pprint-reflection-sym parameter-types))) 
                                                              ")"))
                                                          cls-methods)))
                                               methods-by-class)))))))))))

;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.multi-utils
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.checker.type-rep :as r]))

(defonce ^:dynamic *current-mm* nil)
(set-validator! #'*current-mm* (some-fn nil? 
                                        (con/hmap-c? :dispatch-fn-type r/Type?
                                                     :dispatch-val-ret r/TCResult?)))

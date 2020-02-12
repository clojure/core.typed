;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.free-in
  (:require [clojure.core.typed.checker.fold-rep :as fold]
            [clojure.core.typed.checker.type-rep :as r]
            clojure.core.typed.checker.object-rep
            clojure.core.typed.checker.filter-rep)
  (:import (clojure.core.typed.checker.object_rep Path)
           (clojure.core.typed.checker.filter_rep NotTypeFilter TypeFilter)
           (clojure.core.typed.checker.type_rep Function)))

(fold/derive-default ::free-in-for-object
                     ::free-in-for-filter
                     ::free-in-for-type)

(fold/add-fold-case ::free-in-for-object
                    Path
                    (fn [{p :path i :id :as o} {{:keys [free-in? k]} :locals}]
                      (if (= i k)
                        (reset! free-in? true)
                        o)))

(fold/add-fold-case ::free-in-for-filter
                    NotTypeFilter
                    (fn [{t :type p :path i :id :as t} {{:keys [k free-in?]} :locals}]
                      (if (= i k)
                        (reset! free-in? true)
                        t)))

(fold/add-fold-case ::free-in-for-filter
                    TypeFilter
                    (fn [{t :type p :path i :id :as t} {{:keys [k free-in?]} :locals}]
                      (if (= i k)
                        (reset! free-in? true)
                        t)))

(declare index-free-in?)

(fold/add-fold-case ::free-in-for-type
                    Function
                    (fn [{:keys [dom rng rest drest kws]} {{:keys [k free-in? for-type]} :locals}]
                      ;; here we have to increment the count for the domain, where the new bindings are in scope
                      (let [arg-count (+ (count dom) (if rest 1 0) (if drest 1 0) (count (concat (:mandatory kws)
                                                                                                 (:optional kws))))
                            st* (fn [t] (index-free-in? (if (number? k) (+ arg-count k) k) t))]
                        (doseq [d dom]
                          (for-type d))
                        (st* rng)
                        (and rest (for-type rest))
                        (and rest (for-type (:pre-type drest)))
                        (doseq [[_ v] (concat (:mandatory kws)
                                              (:optional kws))]
                          (for-type v))
                        ;dummy return value
                        (r/make-Function [] r/-any))))

;[AnyInteger Type -> Boolean]
(defn index-free-in? [k type]
  (let [free-in? (atom false :validator boolean?)]
    (letfn [(for-object [o]
              (fold/fold-rhs ::free-in-for-object
                             {:type-rec for-type
                              :locals {:free-in? free-in?
                                       :k k}}
                             o))
            (for-filter [o]
              (fold/fold-rhs ::free-in-for-filter
                             {:type-rec for-type
                              :filter-rec for-filter
                              :locals {:free-in? free-in?
                                       :k k}}
                             o))
            (for-type [t]
              (fold/fold-rhs ::free-in-for-type
                             {:type-rec for-type
                              :filter-rec for-filter
                              :object-rec for-object
                              :locals {:free-in? free-in?
                                       :k k
                                       :for-type for-type}}
                             t))]
      (for-type type)
      @free-in?)))

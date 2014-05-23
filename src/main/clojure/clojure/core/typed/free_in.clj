(ns clojure.core.typed.free-in
  (:require [clojure.core.typed.fold-rep :as fold]
            clojure.core.typed.object-rep
            clojure.core.typed.filter-rep
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.contract-utils :as con])
  (:import (clojure.core.typed.object_rep Path)
           (clojure.core.typed.filter_rep NotTypeFilter TypeFilter)
           (clojure.core.typed.type_rep Function)))

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
  (let [free-in? (atom false :validator con/boolean?)]
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

(ns ^:skip-wiki clojure.core.typed.profiling
  (:refer-clojure :exclude [defn])
  (:require [clojure.core :as core]))

(alter-meta! *ns* assoc :skip-wiki true)

(defmacro
 ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def
    name (fn ([params* ] exprs*)+)) with any doc-string or attrs added
    to the var metadata. prepost-map defines a map with optional keys
    :pre and :post that contain collections of pre or post conditions."
   :arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])
   :added "1.0"}
 defn [name & fdecl]
        ;; Note: Cannot delegate this check to def because of the call to (with-meta name ..)
        (if (instance? clojure.lang.Symbol name)
          nil
          (throw (IllegalArgumentException. "First argument to defn must be a symbol")))
        (let [m (if (string? (first fdecl))
                  {:doc (first fdecl)}
                  {})
              fdecl (if (string? (first fdecl))
                      (next fdecl)
                      fdecl)
              m (if (map? (first fdecl))
                  (conj m (first fdecl))
                  m)
              fdecl (if (map? (first fdecl))
                      (next fdecl)
                      fdecl)
              fdecl (if (vector? (first fdecl))
                      (list fdecl)
                      fdecl)
              m (if (map? (last fdecl))
                  (conj m (last fdecl))
                  m)
              fdecl (if (map? (last fdecl))
                      (butlast fdecl)
                      fdecl)
              m (conj {:arglists (list 'quote (#'clojure.core/sigs fdecl))} m)
              m (let [inline (:inline m)
                      ifn (first inline)
                      iname (second inline)]
                  ;; same as: (if (and (= 'fn ifn) (not (symbol? iname))) ...)
                  (if (if (clojure.lang.Util/equiv 'fn ifn)
                        (if (instance? clojure.lang.Symbol iname) false true))
                    ;; inserts the same fn name to the inline fn if it does not have one
                    (assoc m :inline (cons ifn (cons (clojure.lang.Symbol/intern (.concat (.getName ^clojure.lang.Symbol name) "__inliner"))
                                                     (next inline))))
                    m))
              m (conj (if (meta name) (meta name) {}) m)]
          (list 'def (with-meta name m)
                ;;todo - restore propagation of fn name
                ;;must figure out how to convey primitive hints to self calls first
                (list* `fnprofile {:fake-name name} fdecl))))

(defmacro fnprofile
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol

  Defines a function"
  {:added "1.0", :special-form true,
   :forms '[(fn name? [params* ] exprs*) (fn name? ([params* ] exprs*)+)]}
  [{:keys [fake-name]} & sigs]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs)) 
               (list sigs) 
               (if (seq? (first sigs))
                 sigs
                 ;; Assume single arity syntax
                 (throw (IllegalArgumentException. 
                          (if (seq sigs)
                            (str "Parameter declaration " 
                                 (first sigs)
                                 " should be a vector")
                            (str "Parameter declaration missing"))))))
        psig (fn* [sig]
                  ;; Ensure correct type before destructuring sig
                  (when (not (seq? sig))
                    (throw (IllegalArgumentException.
                             (str "Invalid signature " sig
                                  " should be a list"))))
                  (let [[params & body] sig
                        _ (when (not (vector? params))
                            (throw (IllegalArgumentException. 
                                     (if (seq? (first sigs))
                                       (str "Parameter declaration " params
                                            " should be a vector")
                                       (str "Invalid signature " sig
                                            " should be a list")))))
                        conds (when (and (next body) (map? (first body))) 
                                (first body))
                        body (if conds (next body) body)
                        conds (or conds (meta params))
                        pre (:pre conds)
                        post (:post conds)                       
                        body (if post
                               `((let [~'% ~(if (< 1 (count body)) 
                                              `(do ~@body) 
                                              (first body))]
                                   ~@(map (fn* [c] `(assert ~c)) post)
                                   ~'%))
                               body)
                        body (if pre
                               (concat (map (fn* [c] `(assert ~c)) pre) 
                                       body)
                               body)]
                    (#'clojure.core/maybe-destructured params body)))
        new-sigs (map psig sigs)
        add-profile (fn [name sigs]
                      (map (fn [[a & body :as p]]
                             (assert (seq? p) (pr-str sigs))
                             (list a `(p ~(keyword (-> *ns* ns-name str)
                                                     (str name))
                                           ~@body)))
                           sigs))]

    (with-meta
      (if name
        (list* 'fn* name (add-profile name new-sigs))
        (cons 'fn* (add-profile fake-name new-sigs)))
      (meta &form))))

;; ========================================================================
;; From here `defn` is unusable
;; ========================================================================

;;;;;;;;;;;;;;;;;
;; Timbre stuff
;;
;; We don't package timbre as a dependency in maven, so it's
;; only available with lein (development time). Needs a few helpers
;; to achieve this.

(def loaded-timbre?
  (try 
    (require '[taoensso.timbre.profiling])
    true
    (catch Throwable e
      false)))

;; cannot be a defn
(def timbre-exists? #(find-ns 'taoensso.timbre.profiling))

; use our own version of pspy that can be type checked
(defmacro p [name & body]
  (if (timbre-exists?)
    `(pspy ~name ~@body)
    `(do ~@body)))

;; ========================================================================
;; From here can safely use defn
;; ========================================================================

(defmacro fq-keyword
  "Returns namespaced keyword for given name."
  [name]
  `(if (and (keyword? ~name) (namespace ~name))
     ~name
     (keyword (str ~*ns*) (clojure.core/name ~name))))

(defmacro pspy
  "Profile spy. When in the context of a *pdata* binding, records execution time
  of named body. Always returns the body's result."
  [name & body]
  (let [name (fq-keyword name)]
    `(if-not taoensso.timbre.profiling/*pdata*
       (do ~@body)
       (let [name#       ~name
             start-time# (System/nanoTime)
             result#     (do ~@body)
             elapsed#    (- (System/nanoTime) start-time#)]
         (clojure.core.typed/tc-ignore 
           (swap! taoensso.timbre.profiling/*pdata* #(assoc % name# (conj (% name# []) elapsed#))))
         result#))))

(defmacro when-profile 
  "Conditional profiling. Returns nil."
  [& body]
  (when (timbre-exists?)
    `(do
       (when taoensso.timbre.profiling/*pdata*
         ~@body)
       nil)))

(defmacro profile 
  "Usage: (profile :info :foo ...)"
  [a1 a2 & body]
  (if (timbre-exists?)
    `(taoensso.timbre.profiling/profile ~a1 ~a2 ~@body)
    `(do (prn "WARNING: Cannot profile, timbre must be added as a dependency") 
         nil
         ~@body)))

(defmacro profile-if
  "Usage (profile-if p? :info :foo)"
  [p? & body]
  `(if ~p?
     (profile :info :foo ~@body)
     (do ~@body)))

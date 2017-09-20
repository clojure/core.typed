;; copied from https://github.com/ztellman/potemkin/blob/master/src/potemkin/utils.clj
(ns clojure.core.typed.dep.potemkin.utils
  (:require
    [clojure.core.typed.dep.potemkin.macros :refer [unify-gensyms]])
  (:import
    [java.util.concurrent
     ConcurrentHashMap]))

(defmacro fast-bound-fn
  "Creates a variant of bound-fn which doesn't assume you want a merged
   context between the source and execution environments."
  [& fn-body]
  (let [{:keys [major minor]} *clojure-version*
        use-thread-bindings? (and (= 1 major) (< minor 3))
        use-get-binding? (and (= 1 major) (< minor 4))]
    (if use-thread-bindings?
      `(let [bindings# (get-thread-bindings)
             f# (fn ~@fn-body)]
         (fn [~'& args#]
           (with-bindings bindings#
             (apply f# args#))))
      `(let [bound-frame# ~(if use-get-binding?
                             `(clojure.lang.Var/getThreadBindingFrame)
                             `(clojure.lang.Var/cloneThreadBindingFrame))
             f# (fn ~@fn-body)]
         (fn [~'& args#]
           (let [curr-frame# (clojure.lang.Var/getThreadBindingFrame)]
             (clojure.lang.Var/resetThreadBindingFrame bound-frame#)
             (try
               (apply f# args#)
               (finally
                 (clojure.lang.Var/resetThreadBindingFrame curr-frame#)))))))))

(defn fast-bound-fn*
  "Creates a function which conveys bindings, via fast-bound-fn."
  [f]
  (fast-bound-fn [& args]
    (apply f args)))

(defn retry-exception? [x]
  (= "clojure.lang.LockingTransaction$RetryEx" (.getName ^Class (class x))))

(defmacro try*
  "A variant of try that is fully transparent to transaction retry exceptions"
  [& body+catch]
  (let [body (take-while
               #(or (not (sequential? %)) (not (= 'catch (first %))))
               body+catch)
        catch (drop (count body) body+catch)
        ignore-retry (fn [x]
                       (when x
                         (let [ex (nth x 2)]
                           `(~@(take 3 x)
                             (if (potemkin.utils/retry-exception? ~ex)
                               (throw ~ex)
                               (do ~@(drop 3 x)))))))
        class->clause (-> (zipmap (map second catch) catch)
                        (update-in ['Throwable] ignore-retry)
                        (update-in ['Error] ignore-retry))]
    `(try
       ~@body
       ~@(->> class->clause vals (remove nil?)))))

(defmacro condp-case
  "A variant of condp which has case-like syntax for options.  When comparing
   smaller numbers of keywords, this can be faster, sometimes significantly."
  [predicate value & cases]
  (unify-gensyms
    `(let [val## ~value
           pred## ~predicate]
       (cond
         ~@(->> cases
             (partition 2)
             (map
               (fn [[vals expr]]
                 `(~(if (sequential? vals)
                      `(or ~@(map (fn [x] `(pred## val## ~x)) vals))
                      `(pred## val## ~vals))
                   ~expr)))
             (apply concat))
         :else
         ~(if (even? (count cases))
            `(throw (IllegalArgumentException. (str "no matching clause for " (pr-str val##))))
            (last cases))))))

;;; fast-memoize

(definline re-nil [x]
  `(let [x# ~x]
     (if (identical? ::nil x#) nil x#)))

(definline de-nil [x]
  `(let [x# ~x]
     (if (nil? x#) ::nil x#)))

(defmacro memoize-form [m f & args]
  `(let [k# (vector ~@args)]
     (let [v# (.get ~m k#)]
       (if-not (nil? v#)
         (re-nil v#)
         (let [v# (de-nil (~f ~@args))]
           (re-nil (or (.putIfAbsent ~m k# v#) v#)))))))

(defn fast-memoize
  "A version of `memoize` which has equivalent behavior, but is faster."
  [f]
  (let [m (ConcurrentHashMap.)]
    (fn
      ([]
         (memoize-form m f))
      ([x]
         (memoize-form m f x))
      ([x y]
         (memoize-form m f x y))
      ([x y z]
         (memoize-form m f x y z))
      ([x y z w]
         (memoize-form m f x y z w))
      ([x y z w u]
         (memoize-form m f x y z w u))
      ([x y z w u v]
         (memoize-form m f x y z w u v))
      ([x y z w u v & rest]
         (let [k (list* x y z w u v rest)]
           (let [v (.get ^ConcurrentHashMap m k)]
             (if-not (nil? v)
               (re-nil v)
               (let [v (de-nil (apply f k))]
                 (or (.putIfAbsent m k v) v)))))))))

;;;

(defmacro doit
  "A version of doseq that doesn't emit all that inline-destroying chunked-seq code."
  [[x it] & body]
  (let [it-sym (gensym "iterable")]
    `(let [~it-sym ~it
           it# (.iterator ~(with-meta it-sym {:tag "Iterable"}))]
       (loop []
         (when (.hasNext it#)
           (let [~x (.next it#)]
            ~@body)
           (recur))))))

(defmacro doary
  "An array-specific version of doseq."
  [[x ary] & body]
  (let [ary-sym (gensym "ary")]
    `(let [~(with-meta ary-sym {:tag "objects"}) ~ary]
       (dotimes [idx# (alength ~ary-sym)]
         (let [~x (aget ~ary-sym idx#)]
           ~@body)))))


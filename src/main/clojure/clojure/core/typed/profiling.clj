(ns ^:skip-wiki clojure.core.typed.profiling)

;;;;;;;;;;;;;;;;;
;; Timbre stuff
;;
;; We don't package timbre as a dependency in maven, so it's
;; only available with lein (development time). Needs a few helpers
;; to achieve this.

(try 
  (require '[taoensso.timbre.profiling])
  (catch Exception e))

; use our own version of pspy that can be type checked
(defmacro p [name & body]
  (if (find-ns 'taoensso.timbre.profiling)
    `(pspy ~name ~@body)
    `(do ~@body)))

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


(defmacro profile 
  "Usage: (profile :info :foo ...)"
  [& body]
  `(taoensso.timbre.profiling/profile ~@body))

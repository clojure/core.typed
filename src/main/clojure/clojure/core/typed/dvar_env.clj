(in-ns 'clojure.core.typed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotted Variable Environment

;symbol -> F
(def ^:dynamic *dotted-scope* {})
(set-validator! #'*dotted-scope* (hash-c? symbol? F?))

(defn bound-index? [n]
  (contains? *dotted-scope* n))

(defmacro with-dotted [dvars & body]
  `(with-dotted-mappings (into {} (for [v# ~dvars]
                                    [(:name v#) v#]))
     ~@body))

(defmacro with-dotted-mappings [dvar-map & body]
  `(binding [*dotted-scope* (merge *dotted-scope* ~dvar-map)]
     ~@body))



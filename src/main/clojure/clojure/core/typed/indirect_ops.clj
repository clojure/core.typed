(ns clojure.core.typed.indirect-ops
  (:require [clojure.core.typed.indirect-utils :as in]))

(in/make-indirection unparse-type
                     parse-type
                     check-funapp
                     assoc-pairs-noret
                     subtype?
                     -FS
                     -top-fn
                     -empty-fn
                     infer
                     PropEnv?
                     -or
                     -and
                     type-of-nofail)

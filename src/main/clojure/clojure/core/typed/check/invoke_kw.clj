(ns clojure.core.typed.check.invoke-kw
  (:require [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.utils :as u]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.path-rep :as pe]
            [clojure.core.typed.subtype :as sub]
            [clojure.core.typed.check.utils :as cu]
            [clojure.core.typed.filter-rep :as fl]
            [clojure.core.typed.filter-ops :as fo]
            [clojure.core.typed.object-rep :as obj]
            [clojure.core.typed.parse-unparse :as prs]
            [clojure.core.typed.errors :as err]))

;[TCResult TCResult (Option TCResult) (Option TCResult) -> TCResult]
(defn invoke-keyword [kw-ret target-ret default-ret expected-ret]
  {:pre [(r/TCResult? kw-ret)
         (r/TCResult? target-ret)
         ((some-fn nil? r/TCResult?) default-ret)
         ((some-fn nil? r/TCResult?) expected-ret)]
   :post [(r/TCResult? %)]}
  (u/p :check/invoke-keyword
  (let [targett (c/-resolve (r/ret-t target-ret))
        kwt (r/ret-t kw-ret)
        defaultt (when default-ret
                   (r/ret-t default-ret))]
    (cond
      ;Keyword must be a singleton with no default
      (c/keyword-value? kwt)
      (let [{{path-hm :path id-hm :id :as o} :o} target-ret
            this-pelem (pe/->KeyPE (:val kwt))
            val-type (c/find-val-type targett kwt defaultt)]
        (when expected-ret
          (when-not (sub/subtype? val-type (r/ret-t expected-ret))
            (cu/expected-error val-type (r/ret-t expected-ret))))
        (if (not= (c/Un) val-type)
          (r/ret val-type
               (fo/-FS (if (obj/Path? o)
                         (fo/-filter val-type id-hm (concat path-hm [this-pelem]))
                         fl/-top)
                       (if (obj/Path? o)
                         (fo/-or (fo/-filter (c/make-HMap :absent-keys #{kwt}) id-hm path-hm) ; this map doesn't have a kwt key or...
                                 (fo/-filter (c/Un r/-nil r/-false) id-hm (concat path-hm [this-pelem]))) ; this map has a false kwt key
                         fl/-top))
               (if (obj/Path? o)
                 (update-in o [:path] #(seq (concat % [this-pelem])))
                 o))
          (do (u/tc-warning (str "Keyword lookup gave bottom type: "
                               (:val kwt) " " (prs/unparse-type targett)))
              (r/ret r/-any))))

      :else (err/int-error (str "keyword-invoke only supports keyword lookup, no default. Found " 
                              (prs/unparse-type kwt)))))))

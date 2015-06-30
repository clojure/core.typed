(ns clojure.core.typed.deps.clojure.core.contracts.impl.transformers
  (:require [clojure.core.typed.deps.clojure.core.contracts.impl.funcify :as funcification])
  (:require [clojure.core.typed.deps.clojure.core.unify :as unify]
            [clojure.core.typed.deps.clojure.core.contracts.impl.utils :as utils]))


(defn- divide-pre-post
  "'[odd? pos? => int?]
     =>
   {:pre (odd? pos?) :post (int?)}
  "
  [cnstr]
  (if (vector? cnstr)
    (let [[L M R] (partition-by #{'=>} cnstr)]
      {:pre  (when (not= L '(=>)) L)
       :post (if (= L '(=>)) M R)})
    cnstr))

;; HoC support

(declare build-constraints-description
         build-contract-body)

(defrecord Hoc [field desc])

(defmethod funcification/funcify* Hoc [e args] e)

(def hoc? #(= Hoc (type %)))

(defn- tag-hocs
  [cnstr]
  (let [ret
        (mapcat (fn [form]
                  (if (and (seq? form) (= '_ (first form)))
                    [(list 'fn? (second form))
                     (Hoc. (second form)
                           (apply build-constraints-description (-> form nnext vec (conj "foo"))))]
                    [form]))
             cnstr)]
    ret))

(defn- build-constraints-description
  "'[n] '[odd? pos? => int?] \"foo\"
     =>
    [[n] {:pre [(pos? n) (int? n)], :post [(neg? %)]} \"foo\"]"
  [args cnstr docstring]
  (let [cnstr (vec (tag-hocs cnstr))]
    [args
     (->> (divide-pre-post cnstr)
          (utils/manip-map (partial funcification/funcify '[%]) [:post])
          (utils/manip-map (partial funcification/funcify args) [:pre]))
     docstring]))

(defn- build-condition-body
  [constraint-map body prefix-msg]
  (unify/subst
   '(try
      ((fn []
         ?CNSTR
         ?BODY))
      (catch AssertionError ae
        (throw (AssertionError. (str ?PREFIX ?MSG \newline (.getMessage ae))))))

   {'?CNSTR  constraint-map
    '?PREFIX prefix-msg
    '?BODY   body}))

(comment

  (build-contract-body
   (build-constraints-description '[f n] '[number? (_ f [n] [odd?]) => pos?] "foo"))
  
  (build-contract-body
   (build-constraints-description '[n] '[number? => odd?] "foo"))


  (prepare-args '[f b c] '{f {:desc [[n] {:pre [(odd? n)], :post []} "bar"]}})
)

(defn prepare-args [args hocs]
  (let [vargs? #{'&}
        has-vargs (boolean (some vargs? args))]
    (with-meta 
      (vec
       (map (fn [arg]
              (if-let [hoc (get hocs arg)]
                (list `partial (list* `fn (build-contract-body (:desc hoc))) arg)
                (if (map? arg)
                  (:as arg)
                  arg)))
            (->> args (remove vargs?))))
      {::vargs has-vargs})))

(defn- build-contract-body
  [[args cnstr descr :as V]]
  (let [vargs? #{'&}
        fun-name (gensym "fun")
        hocs (apply merge (map #(hash-map (:field %) %)
                               (filter hoc? (concat (:pre cnstr) (:post cnstr)))))
        cnstr {:pre  (vec (filter (complement hoc?) (:pre cnstr)))
               :post (vec (filter (complement hoc?) (:post cnstr)))}
        prep-args (prepare-args args hocs)
        callsite (if (::vargs (meta prep-args))
                   (list* `apply '?F prep-args)
                   '(apply ?F ?ARGS))]
    (unify/subst
     '(?PARMS
       (let [ret ?PRE-CHECK]
         ?POST-CHECK))

     {'?ARGS       prep-args
      '?F          fun-name
      '?PARMS      (vec (list* fun-name args))
      '?MSG        descr
      '?PRE-CHECK  (build-condition-body
                    {:pre (:pre cnstr)}
                    callsite
                    "Pre-condition failure: ")
      '?POST-CHECK (build-condition-body
                    {:post (:post cnstr)}
                    'ret
                    "Post-condition failure: ")})))

(defn- build-contract-bodies
  [constraint-descriptions]
  (for [cnstr constraint-descriptions]
    (build-contract-body cnstr)))

;; # Public API

(defn build-contract-fn-body
  [name docstring raw-constraints]
  (let [raw-cnstr   (partition 2 raw-constraints)
        cnstr-descrs (for [[a c] raw-cnstr]
                       (build-constraints-description a c docstring))] ;; needs work
    (->> cnstr-descrs
         build-contract-bodies
         (list* `fn name))))

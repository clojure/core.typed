(in-ns 'clojure.core.typed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restricted Class

;Class -> RClass
(defonce RESTRICTED-CLASS (atom {}))
(set-validator! RESTRICTED-CLASS (hash-c? symbol? Type?))

(declare with-frees)

(defn- build-replacement-syntax [m]
  `(into {} (for [[k# v#] '~m]
              [(if-let [c# (resolve k#)] 
                 (and (class? c#) (Class->symbol c#))
                 k#)
               (parse-type v#)])))

(defn parse-RClass-binder [bnds]
  (for [[nme & {:keys [variance]}] bnds]
    [variance (make-F nme)]))

(defn alter-class* [csym type]
  (swap! RESTRICTED-CLASS assoc csym type))

(defmacro alter-class [the-class frees-syn & opts]
  (let [{replacements-syn :replace
         unchecked-ancestors-syn :unchecked-ancestors} (apply hash-map opts)]
     `(let [[variances# frees#] (when-let [fs# (seq '~frees-syn)]
                                  (let [b# (parse-RClass-binder fs#)]
                                    [(map first b#) (map second b#)]))
            csym# (let [cls# (when-let [c# (resolve '~the-class)]
                               (when (class? c#)
                                 c#))]
                    (or (and cls# (Class->symbol cls#))
                        '~the-class))]
        (alter-class* csym# 
                      (RClass* (map :name frees#) variances# frees# csym#
                               (with-frees frees#
                                 ~(build-replacement-syntax replacements-syn))
                               (with-frees frees#
                                 (set (map parse-type '~unchecked-ancestors-syn)))))
        ~the-class)))

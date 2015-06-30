(ns clojure.core.typed.deps.clojure.jvm.tools.analyzer.emit-form
  (:import (clojure.lang Var)))

(def emit-default ::emit-default)

(defn derive-emit-default [tag]
  (derive tag emit-default))

(declare map->form)

(defn emit-form 
  "Return the form represented by the given AST."
  [expr]
  (map->form expr ::emit-default))

(defmulti map->form (fn [expr mode]
                      [(:op expr) mode]))

(defmethod map->form [:nil emit-default] [{:keys [val]} _] val)
(defmethod map->form [:number emit-default] [{:keys [val]} _] val)
(defmethod map->form [:constant emit-default] [{:keys [val]} _] (list 'quote val))
(defmethod map->form [:string emit-default] [{:keys [val]} _] val)
(defmethod map->form [:boolean emit-default] [{:keys [val]} _] val)
(defmethod map->form [:keyword emit-default] [{:keys [val]} _] val)

(defmethod map->form [:static-method emit-default]
  [{:keys [^Class class method-name args]} mode] 
 `(~(symbol (.getName class) (str method-name))
       ~@(map #(map->form % mode) args)))

(defmethod map->form [:static-field emit-default]
  [{:keys [^Class class field-name]} _]
  (symbol (.getName class) (str field-name)))

(defmethod map->form [:instance-field emit-default]
  [{:keys [target field-name]} mode]
  (list '. (map->form target mode) (symbol field-name)))

(defmethod map->form [:invoke emit-default]
  [{:keys [fexpr args]} mode]
  `(~(map->form fexpr mode)
       ~@(map #(map->form % mode) args)))

(defn- var->symbol [^Var var]
  (symbol (str (ns-name (.ns var))) (str (.sym var))))

(defmethod map->form [:the-var emit-default]
  [{:keys [var]} _]
  (list 'var (var->symbol var)))

(defmethod map->form [:var emit-default]
  [{:keys [var]} _]
  (var->symbol var))

(defmethod map->form [:instance-method emit-default]
  [{:keys [target method-name args]} mode]
  `(~(symbol (str "." method-name))
       ~(map->form target mode)
       ~@(map #(map->form % mode) args)))

(defmethod map->form [:new emit-default]
  [{:keys [^Class class args]} mode]
  `(new ~(symbol (.getName class))
        ~@(map #(map->form % mode) args)))

(defmethod map->form [:empty-expr emit-default] [{:keys [coll]} _] coll)
(defmethod map->form [:vector emit-default] [{:keys [args]} mode] (vec (map #(map->form % mode) args)))
(defmethod map->form [:map emit-default] [{:keys [keyvals]} mode] (apply hash-map (map #(map->form % mode) keyvals)))
(defmethod map->form [:set emit-default] [{:keys [keys]} mode] (set (map #(map->form % mode) keys)))

(defmethod map->form [:set! emit-default]
  [{:keys [target val]} mode]
  `(set! ~(map->form target mode) ~(map->form val mode)))

(defmethod map->form [:fn-expr emit-default]
  [{:keys [name methods]} mode]
  (list* 'fn* 
         (concat
           (when name
             [name])
           (map #(map->form % mode) methods))))

(defmethod map->form [:fn-method emit-default]
  [{:keys [body required-params rest-param]} mode]
  `(~(vec (concat (map #(map->form % mode) required-params)
                  (when rest-param
                    ['& (map->form rest-param mode)])))
       ~(map->form body mode)))

(defmethod map->form [:do emit-default]
  [{:keys [exprs]} mode]
  (cond
    (empty? exprs) nil
    (= 1 (count exprs)) (map->form (first exprs) mode)
    :else `(do ~@(map #(map->form % mode) exprs))))

(defmethod map->form [:let emit-default]
  [{:keys [is-loop binding-inits body]} mode]
  `(~(if is-loop
       'loop*
       'let*)
       ~(vec (apply concat (map #(map->form % mode) binding-inits)))
       ~(map->form body mode)))

(defmethod map->form [:letfn emit-default]
  [{:keys [binding-inits body]} mode]
  `(~'letfn*
       ~(vec (apply concat (map #(map->form % mode) binding-inits)))
       ~(map->form body mode)))

(defmethod map->form [:recur emit-default]
  [{:keys [args]} mode]
  `(recur ~@(map #(map->form % mode) args)))
          
;to be spliced
(defmethod map->form [:binding-init emit-default]
  [{:keys [local-binding init]} mode]
  (map #(map->form % mode) [local-binding init]))

(defmethod map->form [:local-binding emit-default] [{:keys [sym]} _] sym)
(defmethod map->form [:local-binding-expr emit-default] [{:keys [local-binding]} mode] (map->form local-binding mode))

(defmethod map->form [:if emit-default]
  [{:keys [test then else]} mode] 
  `(if ~@(map #(map->form % mode) [test then else])))

(defmethod map->form [:instance-of emit-default]
  [{:keys [^Class class the-expr]} mode] 
  `(clojure.core/instance? ~(symbol (.getName class))
                           ~(map->form the-expr mode)))

(defmethod map->form [:def emit-default]
  [{:keys [^Var var init init-provided]} mode] 
  `(def ~(.sym var) ~(when init-provided
                       (map->form init mode))))

;FIXME: methods don't print protocol/interface name
(defmethod map->form [:deftype* emit-default]
  [{:keys [name methods fields covariants ^Class compiled-class]} mode]
  (list* 'deftype* 
         (symbol (apply str (last (partition-by #{\.} (str name)))))
         name
         ;FIXME these should be hinted fields
         (vec (map #(map->form % mode) fields))
         :implements
         ;FIXME interfaces implemented
         []
         (map #(map->form % mode) methods)))

(defmethod map->form [:new-instance-method emit-default]
  [{:keys [name required-params body]} mode] 
  (list name (vec (map #(map->form % mode) required-params))
        (map->form body mode)))

(defmethod map->form [:import* emit-default]
  [{:keys [class-str]} _] 
  (list 'import* class-str))

(defmethod map->form [:keyword-invoke emit-default]
  [{:keys [kw target]} mode] 
  (list (map->form kw mode) (map->form target mode)))

(defmethod map->form [:throw emit-default]
  [{:keys [exception]} mode] 
  (list 'throw (map->form exception mode)))

(defmethod map->form [:try emit-default]
  [{:keys [try-expr catch-exprs finally-expr]} mode] 
  (list* 'try (map->form try-expr mode)
         (concat
           (map #(map->form % mode) catch-exprs)
           (when finally-expr [(list 'finally (map->form finally-expr mode))]))))

(defmethod map->form [:catch emit-default]
  [{:keys [^Class class local-binding handler]} mode]
  (list 'catch (symbol (.getName class))
        (map->form local-binding mode) 
        (map->form handler mode)))

;; (from Compiler.java)
;;  //(case* expr shift mask default map<minhash, [test then]> table-type test-type skip-check?)
(defmethod map->form [:case* emit-default]
  [{:keys [the-expr tests thens default tests-hashes shift mask low high switch-type test-type skip-check]} mode]
  (list 'case*
        (map->form the-expr mode)
        shift
        mask
        (map->form default mode)
        (zipmap tests-hashes
                (map vector
                     (map #(map->form % mode) tests)
                     (map #(map->form % mode) thens)))
        switch-type
        test-type
        skip-check))

(defmethod map->form [:meta emit-default]
  [{:keys [meta expr]} mode]
  (with-meta (map->form expr mode)
             (let [m (map->form meta mode)]
               (if (list? m)
                 ;sometimes metadata is quoted. No idea why/where.
                 (second m)
                 m))))

(comment
  (defmacro frm [f]
    `(-> (ast ~f) emit-form))
  (require '[clojure.core.typed.deps.clojure.jvm.tools.analyzer :refer [ast analyze-form]])

  (frm 1)
  (frm :a)
  
  (frm (+ 1 2))
  (frm (- 1 2))

  (frm (apply - 1 2))

  (frm (.var - 1 2))

  (frm (Integer. 1))

  (frm ())

  (frm [1])
  (frm [(- 1)])
  (frm {(- 1) 1})
  (frm #{(- 1) 1})

  (frm (let [a '(1 2)]
         (first 1)))
  (frm (loop [a '(1 2)]
         (first 1)))

  (frm (fn [{:keys [a]} b] 1))
  (frm (instance? Class 1))

  (frm nil)
  (frm (def a 1))
  (frm (defn ab [a] a))

  (frm (loop [a 1] (recur 1)))

  ; FIXME
  (frm (deftype A []
         clojure.lang.ISeq
         (first [this] this)))

  (frm (:a {}))
  (frm (throw (Exception. "a")))
  (frm (try 1 2 
         (catch Exception e 
           4)
         (catch Error e
           5)
         (finally 3.2)))
  (frm (Integer/toHexString 1))
  (frm (Integer/TYPE))
  (frm #'conj)
  
  (frm 'a)
  (frm (let [b 1] 
         [b 'a 1]))

  (frm #{1 2 3})
  (frm (case 1 2 3 4))
  (frm (case 1 :a 3 4))

  (frm (deftype A [a b]
         Object
         (toString [this])))
  (macroexpand
    '(deftype A [a b]
       Object
       (toString [this])))

   (frm (letfn [(a [b] b)
                (b [a] a)
                (c [c] a)]
          (a b c)))

  ;instance field
   (frm (.a 1))
  ;instance method
   (frm (.cancel (java.util.concurrent.FutureTask. #()) 1))
  (frm (fn [] (set! *warn-on-reflection* true)))

  (-> (analyze-form (with-meta {} {:foo 1}))
      emit-form)
)

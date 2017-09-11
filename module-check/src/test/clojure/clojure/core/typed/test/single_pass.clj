(ns clojure.core.typed.test.single-pass
  (:require [clojure.core.typed.single-pass :as si :refer [ast]]
            [clojure.tools.analyzer.jvm :as ana.jvm]
            [clojure.tools.analyzer.passes.jvm.emit-form :refer [emit-form]]
            [clojure.tools.analyzer.passes :refer [schedule]]
            [clojure.tools.analyzer.passes.trim :refer [trim]]
            [clojure.data :refer [diff]]
            [clojure.test :refer [deftest is]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as set]))

(defn leaf-keys [m]
  (cond
    (map? m)
    (reduce (fn [ks [k v]]
              (cond
                (map? v) (set/union ks (leaf-keys v))

                (and (vector? v)
                     (every? map? v))
                (apply set/union ks (map leaf-keys v))

                :else (conj ks k)))
            #{}
            m)
    (coll? m) (apply set/union (map leaf-keys m))
    :else #{}))

(defn leaf-diff [x y]
  (apply set/union
         (map leaf-keys
              (take 2
                    (diff x y)))))

(def passes (schedule (disj ana.jvm/default-passes #'trim)))

(defmacro taj [form]
  `(binding [ana.jvm/run-passes passes]
     (ana.jvm/analyze+eval '~form
                           (ana.jvm/empty-env)
                           {:handle-evaluation-exception (constantly nil)})))

(defn ppdiff [x y]
  (pprint (diff x y)))

(deftest KeywordExpr-test
  (is (= (ast :abc)
         (taj :abc)))
  (is (= (ast ':abc)
         (taj ':abc)))
  (is (= #{:tag :o-tag}
         (leaf-diff
           (ast '':abc)
           (taj '':abc)))))

(deftest NumberExpr-test
  (is (= (ast 1.2)
         (taj 1.2)))
  (is (= (ast 1)
         (taj 1)))
  (is (= (ast '1)
         (taj '1)))
  (is (= #{:tag :o-tag}
         (leaf-diff
           (ast ''1)
           (taj ''1))))
  )

(deftest StringExpr-test
  (is (= (ast "abc")
         (taj "abc")))
  (is (= (ast '"abc")
         (taj '"abc")))
  (is (= 
        #{:tag :o-tag}
        (leaf-diff
          (ast ''"abc")
          (taj ''"abc")))))

(deftest NilExpr-test
  (is (= (ast nil)
         (taj nil)))
  (is (= (ast 'nil)
         (taj 'nil)))
  (is (= #{:tag :o-tag}
         (leaf-diff
           (ast ''nil)
           (taj ''nil))))
  )

(deftest BooleanExpr-test
  (is (= 
        #{:tag :o-tag}
        (leaf-diff
          (ast true)
          (taj true))))
  (is (= 
        #{:tag :o-tag}
        (leaf-diff
          (ast false)
          (taj false))))
  (is (= 
        #{:tag :o-tag}
        (leaf-diff
          (ast 'false)
          (taj 'false))))
  (is (= 
        #{:o-tag :tag}
        (leaf-diff
          (ast ''false)
          (taj ''false))))
  )


(deftest ConstantExpr-test
  (is
    (= (ast 'nil)
       (taj 'nil)))
  ;; but they evaluate to the same thing anyway
  (is (= (:result (ast 'nil))
         (eval (emit-form (ast 'nil)))
         (eval (emit-form (taj 'nil)))))
  (is (= (emit-form (ast ''nil))
         (emit-form (taj ''nil))))
  (is (= (:result (ast ''nil))
         (eval (emit-form (ast ''nil)))
         (eval (emit-form (taj ''nil)))))
  ;; (not= #"" #""), so :val and :form will not match
  (is (let [[l] (diff (ast '#"")
                      (taj '#""))]
        (= #{:val :form :result}
           (leaf-keys l))))
  (is (=
       #{:o-tag :tag}
       (leaf-diff
         (ast '{:a 1})
         (taj '{:a 1}))))
  (is (= (ast 'refer)
         (taj 'refer)))
  (is (= (:result (ast 'refer))
         (eval (emit-form (ast 'refer)))
         (eval (emit-form (taj 'refer)))))
  (is (= 
        #{:o-tag :tag}
        (leaf-diff
          (ast ''{})
          (taj ''{}))))
  (is (= 
        (leaf-diff
          (ast '{})
          (taj '{}))))
  (is (= (:result (ast '{}))
         (eval (emit-form (ast '{})))
         (eval (emit-form (taj '{})))))
  (is (= (:result (ast {}))
         (eval (emit-form (ast {})))
         (eval (emit-form (taj {})))))
  (is (= (:result (ast {:a 1}))
         (eval (emit-form (ast {:a 1})))
         (eval (emit-form (taj {:a 1})))))
  (is (= (:result (ast {:a '1}))
         (eval (emit-form (ast {:a '1})))
         (eval (emit-form (taj {:a '1})))))
  (is (= (:result (ast ''1))
         (eval (emit-form (ast ''1)))
         (eval (emit-form (taj ''1)))))
  (is (= #{:o-tag :tag}
         (leaf-diff
           (ast ''1)
           (taj ''1))))
  (is (= (:result (ast {:a ''1}))
         (eval (emit-form (ast {:a ''1})))
         (eval (emit-form (taj {:a ''1})))))
  (is (= (:result (ast {:a ''1}))
         (eval (emit-form (ast {:a ''1})))
         (eval (emit-form (taj {:a ''1})))))
  (is (= (:result (ast {:a '''1}))
         (eval (emit-form (ast {:a '''1})))
         (eval (emit-form (taj {:a '''1})))))
  (is (= (:result (ast {:a 1}))
         (eval (emit-form (ast {:a 1})))
         (eval (emit-form (taj {:a '1})))))
  (is (= (:result (ast {:a '1}))
         (eval (emit-form (ast {:a '1})))
         (eval (emit-form (taj {:a 1})))))
  (is (=
       (ast 'refer)
       (taj 'refer)))
  (is (=
       (ast ':refer)
       (taj ':refer)))
  (is (= 
        ;FIXME :val differs
        #{:tag :o-tag :a}
        (leaf-diff
          (ast {:a 'refer})
          (taj {:a 'refer}))))
  (is (= 
        #{:tag :o-tag}
        (leaf-diff
          (ast '{:a 1})
          (taj '{:a 1}))))
  (is (= :quote
         (:op (ast ''{:a 1}))))
  (is (= 
        #{:o-tag :line :tag}
        (leaf-diff
          (ast '(ns fooblah))
          (taj '(ns fooblah)))))
  ;; FIXME :val is evaluated further in Compiler.java
  (is (= 
        #{:o-tag :tag :a}
        (leaf-diff
          (ast {:a '(ns fooblah)})
          (taj {:a '(ns fooblah)}))))
  (is (= (ast '"")
         (taj '"")))
  (is (= (:result (ast '""))
         (eval (emit-form (ast '"")))
         (eval (emit-form (taj '"")))))
  (is (= (ast 1N)
         (taj 1N)))
  (is (= (:result (ast '1N))
         (eval (emit-form (ast '1N)))
         (eval (emit-form (taj '1N)))))
  )


(deftest DefExpr-test
  ;; FIXME :tag is different
  (is (= #{:line :tag :o-tag}
         (leaf-diff (taj (def a 1))
                    (ast (def a 1)))))
  (is (= #{:o-tag :line :tag}
         (leaf-diff
           (ast (def a "foo" 1))
           (taj (def a "foo" 1))))))

(deftest BodyExpr-test
  ;; Compiler prints (do nil) instead of (do).
  (is (= #{:line}
         (leaf-diff
           (ast (do))
           (taj (do)))))
  (is (= #{:line}
         (leaf-diff
           (ast (do 1))
           (taj (do 1)))))
  ;; inner column is wrong since Compiler's BodyExpr does not remember it
  (is (= #{:line :column}
         (leaf-diff
           (ast (do (do 1)))
           (taj (do (do 1)))))))

(deftest FnExpr-test
  (is (=
       #{:loop-id :o-tag :line :column :form :tag :arglists :top-level
         :raw-forms :result}
       (leaf-diff
         ;; taj is wrapped in implicit do?
         (ast (fn []))
         (:ret (taj (fn []))))))
  (is (=
       #{:loop-id :arglist :o-tag :column :line :top-level :form :tag :arglists :atom
         :result :raw-forms}
       (leaf-diff
         (ast (fn [a]))
         (:ret (taj (fn [a]))))
       ))
  (is (=
       #{:loop-id :o-tag :variadic? :line :arg-id :tag :atom :assignable?}
       (leaf-diff
         (-> (ast (fn [a] a)) :methods first :body :ret)
         (-> (taj (fn [a] a)) :ret :methods first :body :ret))))
  (is (=
       (meta 
         (first
           (second (emit-form (ast (fn ^:a []))))))
       {:a true}))
  (is (= (emit-form (ast (fn [& body])))
         (emit-form (taj (fn [& body])))))
  (is (= (-> (ast (fn [foo] foo)) :methods first :params first :name)
         (-> (ast (fn [foo] foo)) :methods first :body :ret :name)
         'foo__#0)))

(deftest InvokeExpr-test
  (is (=
       #{:body? :loop-id :o-tag :column :line :form :tag :arglists :raw-forms}
       (leaf-diff
         (ast ((do (fn []))))
         (taj ((fn []))))))
  ;; TAJ is more agressive with :keyword-invoke
  (is (and (= (:op (ast (:a nil)))
              :invoke)
           (= (:op (taj (:a nil)))
              :keyword-invoke)))
  (is (=
       #{:o-tag :line :tag}
       (leaf-diff
         (ast (:a nil 1))
         (taj (:a nil 1))))))

(deftest LetExpr-test
  (is (= #{:loop-locals :loop-id :o-tag :column :line :once :context :tag :atom :assignable?
           }
         (leaf-diff
           (-> (ast (let [a 1] a)) :fn :methods first :body :ret :body :ret)
           (-> (taj (let [a 1] a)) :body :ret))))
  (is (= #{:loop-locals :loop-id :o-tag :column :line :once :top-level :context :form 
           :tag :atom :assignable? :raw-forms
           :result}
         (leaf-diff
           (-> (ast (let [a 1] a)) :fn :methods first :body :ret)
           (-> (taj (let [a 1] a))))))
  (is (= (-> (ast (let [a 1] a)) :fn :methods first :body :ret :bindings first :name)
         (-> (ast (let [a 1] a)) :fn :methods first :body :ret :body :ret :name)
         'a__#0)))

(deftest NewExpr-test
  (is 
    (= #{:name :declaring-class :line :form :flags
         :column :result}
       (leaf-diff
         (ast (Exception.))
         (taj (Exception.)))))
  (is (= (emit-form (ast (Exception.)))
         '(new java.lang.Exception)))
  (is 
    (= #{:name :declaring-class :parameter-types :line :flags :column}
       (leaf-diff
         ;; fully qualified :form isn't different
         (ast (java.io.File. "a"))
         (taj (java.io.File. "a"))))))

(deftest VarExpr-test
  #_
  (is (= #{:tag :o-tag :form}
         (leaf-diff
           (ast +)
           (taj +))))
  (is (=
       (:op (ast +))
       (:op (taj +))))
  (is (=
       (:form (ast +))
       (emit-form (ast +))
       'clojure.core/+))
  (is (=
       (meta (emit-form (ast +)))
       nil))
  (is (=
       (meta (emit-form (ast ^Integer +)))
       {:tag 'Integer}))
  )

(deftest TheVarExpr-test
  ;; FIXME :form qualifies the original code
  (is (= #{:form}
         (leaf-diff
           (ast #'+)
           (taj #'+))))
  (is (= (:form (ast #'+))
         (emit-form (ast #'+))
         '(var clojure.core/+)))
  )

(deftest InstanceOfExpr-test
  (is (= 
        #{:column :line :form}
        (leaf-diff
          (ast (instance? Object 1))
          (taj (instance? Object 1))))))

(deftest EmptyExpr-test
  (is (= (ast {})
         (taj {})))
  (is (= (ast [])
         (taj [])))
  (is (= (ast #{})
         (taj #{})))
  (is (= #{:tag :o-tag}
         (leaf-diff 
           (ast ())
           (taj ())))))

(deftest MetaExpr-test
  (is (= 
        :with-meta
        (:op (ast ^{:a (+ 1)} #()))
        (:op (taj ^{:a (+ 1)} #()))))
  (is (and
        (= ;; qualified
           `{:a (+ 1)}
           (select-keys (emit-form (:meta (ast ^{:a (+ 1)} #()))) [:a]))
        ;; unqualified
        (= '{:a (+ 1)}
           (select-keys (emit-form (:meta (taj ^{:a (+ 1)} #()))) [:a]))))
  (is 
    (= `{:a (+ 1)}
       (meta (emit-form (ast ^{:a (+ 1)} #())))))
  (is 
    (= (meta (eval (emit-form (ast ^{:a (inc 1)} #()))))
       {:a 2}))
  )

(deftest IfExpr-test
  (is 
    ;; why is tag different but not o-tag?
    (= #{:line :tag}
       (leaf-diff
         (ast (if 1 2 3))
         (taj (if 1 2 3))))))

(deftest StaticMethodExpr-test
  (is (=
       #{:return-type :name :o-tag :declaring-class :parameter-types 
         :line :exception-types :tag :flags :column}
       (leaf-diff
         (ast (Long/valueOf "1"))
         (taj (Long/valueOf "1"))))))

(deftest StaticFieldExpr-test
  (is 
    (= 
      #{:o-tag :tag :validated? :raw-forms :line :column}
      (leaf-diff 
        (dissoc (ast Long/MAX_VALUE)
                :reflected-field)
        (taj Long/MAX_VALUE)))))

(deftest InstanceMethodExpr-test
  (is (=
       ;; constructors inherit :line and :column
       #{:name :declaring-class :column :parameter-types :line :flags :raw-forms}
       (leaf-diff
         (-> (ast (.getName (java.io.File. "a"))) :instance)
         (-> (taj (.getName (java.io.File. "a"))) :instance)))))

(deftest deftype-test
  (is (ast (deftype InstType [abc]
             Object
             (toString [this]
               (fn [] (.toString this))
               "")))))

(deftest invoke-meta-test
  (is (=
       '(nil)
       (-> (ast ^{:a (nil)} (#'+ 1 1))
           emit-form
           meta
           :a)))
  )

(taj 'Long)

(deftest defrecord-test
  (is (->
        (ast (defrecord InstRec [abc]
               Object
               (toString [this]
                 (fn [] (.toString this))
                 "")))
        emit-form
        )))

(deftest InstanceFieldExpr-test
  ;;FIXME
  #_
  (is 
    (do (deftype Inst [abc])
        (= 
          #{:children :loop-id :name :type :o-tag :declaring-class :m-or-f :column :line :class :context 
            :form :tag :atom :flags :validated? :assignable? :raw-forms}
          (leaf-diff
            (-> (ast (fn [^Inst a] (.abc a)))
                :methods first :body :ret)
            (-> (taj (fn [^Inst a] (.abc a)))
                :ret
                :methods first :body :ret))))))

(deftest SetExpr-test
  (is (=
       #{:o-tag :column :line :tag }
       (leaf-diff
         (ast #{(if 1 2 3)})
         (taj #{(if 1 2 3)})))))

(deftest VectorExpr-test
  (is (=
       #{:o-tag :column :line :tag }
       (leaf-diff
         (ast [(if 1 2 3)])
         (taj [(if 1 2 3)])))))

(deftest MapExpr-test
  (is (=
       #{:o-tag :column :line :tag }
       (leaf-diff
         (ast {'a (if 'a 'b 'c)})
         (taj {'a (if 'a 'b 'c)})))))

(deftest MonitorEnter-ExitExpr-test
  (is 
    (= #{:loop-locals :loop-id :o-tag :column :line :result :once 
         :top-level :context :tag :raw-forms}
       (leaf-diff
         (-> (ast (fn [] (monitor-enter 1))) :methods first :body :ret)
         (taj (monitor-enter 1)))))
  (is 
    (= #{:loop-locals :loop-id :o-tag :column :line :result :once 
         :top-level :context :tag :raw-forms}
       (leaf-diff
         (-> (ast (fn [] (monitor-exit 1))) :methods first :body :ret)
         (taj (monitor-exit 1))))))

(deftest ThrowExpr-Test
  (is (=
       #{:loop-locals :loop-id :name :ignore-tag :o-tag :declaring-class :column :line :result :once :top-level :context :form :tag :flags :raw-forms}
       (leaf-diff
         (-> (ast (fn [] (throw (Exception.))))
             :methods first :body :ret
             )
         (taj (throw (Exception.)))))))

(deftest ImportExpr-test
  (is (= 
        #{:o-tag :line :column :tag :validated? }
        (leaf-diff
          (ast (import 'java.lang.Object))
          (taj (import 'java.lang.Object))))))

(deftest NewInstanceExpr-test
  (is (=
       #{:loop-locals :loop-id :o-tag :line :once :context :class-name :tag :column}
        (leaf-diff
          (-> (ast (deftype A1 [])) :fn :methods first :body :ret :body :statements first)
          (-> (taj (deftype A1 [])) :body :statements first))))
  (is (=
        #{:loop-locals :loop-id :o-tag :line :once :context :tag :atom :column}
        (leaf-diff
          (-> (ast (deftype A2 [f])) :fn :methods first :body :ret :body :statements first :fields)
          (-> (taj (deftype A2 [f])) :body :statements first :fields))))
  (is (=
        #{:loop-locals :loop-id :o-tag :line :once :context :class-name :tag :atom :column}
        (leaf-diff
          (-> (ast (deftype A3 [f])) :fn :methods first :body :ret :body :statements first)
          (-> (taj (deftype A3 [f])) :body :statements first))))
  )


#_(require '[clojure.tools.trace :as tr])
#_(tr/untrace-vars clojure.tools.analyzer.passes.uniquify/-uniquify-locals)
#_(tr/untrace-vars clojure.tools.analyzer.passes.uniquify/uniquify-locals*)

;; FIXME add defprotocol to each test
#_
(deftest NewInstanceMethod-test
  (defprotocol Foo
    (bar [this a]))
  ; :this
  (is
    ;; :children is nil and absent
    (= #{:this :ns :file :o-tag :column :line :context :tag :atom}
       (leaf-diff
         (-> (ast (deftype A []
                    Foo
                    (bar [this a])))
             :fn :methods first :body :ret :body :statements first :methods first :this)
         (-> (taj (deftype A []
                    Foo
                    (bar [this a])))
             :body :statements first :methods first :this))))
  (is
    (= #{:loop-locals :ns :loop-id :name :file :op :o-tag :column :line :once :context :form :tag :atom :local}
       (leaf-diff
         (-> (ast (deftype Ab []
                    Foo
                    (bar [this a])))
             :fn :methods first :body :ret :body :statements first :methods first :body)
         (-> (taj (deftype Ab []
                    Foo
                    (bar [this a])))
             :body :statements first :methods first :body))))
  (is
    (= #{:loop-locals :loop-id :o-tag :column :line :once :context :tag :atom}
       (leaf-diff
         (-> (ast (deftype Abc []
                    Foo
                    (bar [this a])))
             :fn :methods first :body :ret :body :statements first :methods first :params first)
         (-> (taj (deftype Abc []
                    Foo
                    (bar [this a])))
             :body :statements first :methods first :params first))))
  #_  ;;FIXME what's different?
  (is
    (= #{:loop-locals :children :interface :this :locals :ns :loop-id :name :file 
         :op :o-tag :column :methods :line :once :context :form :tag :atom :local}
       (leaf-diff
         (-> (ast (deftype Abcd []
                    Foo
                    (bar [this a])))
             :fn :methods first :body :ret :body :statements first :methods first)
         (-> (taj (deftype Abcd []
                    Foo
                    (bar [this a])))
             :body :statements first :methods first))))
  ;; order of :implements is different
  #_(is (ppdiff 
        (-> (ast (deftype Abcde []
                   Foo
                   (bar [this a])))
            :fn :methods first :body :ret :body :statements first emit-form)
        (-> (taj (deftype Abcde []
                   Foo
                   (bar [this a])))
            :body :statements first emit-form)))
  (is (=
        (->> (ast (deftype Abcde []
                   Foo
                   (^:foo bar [this a])))
            :fn :methods first :body :ret :body :statements first emit-form
            (drop 6) first first meta)
        {:foo true}))
  (is (=
        (->> (ast (deftype Abcde []
                   Foo
                   (bar ^:foo [this a])))
            :fn :methods first :body :ret :body :statements first emit-form
            (drop 6) first second meta)
        {:foo true}))
  )

(deftest primitive-local-test
  (is (ast (let [a 1])))
  (is (ast (let [a 1] a))))

(deftest local-tag-test
  (is (=
       (-> (ast (let [a "a"]))
           emit-form
           first
           second
           second
           second
           first
           meta
           :tag)
       nil))
  (is (=
       (-> (ast (let [^Object a "a"]))
           emit-form
           first
           second
           second
           second
           first
           meta
           :tag)
       'Object))
  (is (=
       (-> (ast (let [a "a"]
                  ^Object a))
           emit-form
           first
           second
           second
           last
           meta
           :tag
           )
       'Object))
  (is (=
       (-> (ast (let [a "a"]
                  ^{:a (nil)} a))
           emit-form
           first
           second
           second
           last
           meta)
       '{:a (nil)}))
  (is (=
       (-> (ast (let [^Object a "a"]
                  ^Object a))
           emit-form
           first
           second
           second
           last
           meta
           :tag
           )
       'Object))
  (is (=
       '{:a (inc 1)}
       (-> (ast (let [^{:a (inc 1)} a "a"]
                  a))
           emit-form
           first
           second
           second
           second
           first
           meta
           ))))

(deftest fn-method-meta
  (is (= '{:a (nil)}
         (-> (ast (fn ^{:a (nil)} []))
             emit-form
             second
             first
             meta))))

(deftest def-metadata 
  (is (-> (ast (def ^:dynamic *blob*))
           emit-form
           second
           meta
           :dynamic))
  ;; arglists
  (is (= 
        (list 'quote '([a]))
        (-> (ast (defn foo [a]))
            emit-form
            second
            meta
            :arglists
            ))))

(deftest CaseExpr-test
  (is 
    (= #{:loop-locals :children :ns :loop-id :name :file :val :type :op :o-tag :literal? 
         :column :line :once :top-level :context :form :tag :atom :local :assignable?}
       (leaf-diff
         (-> (ast (fn [] (case 1 2 3)))
             :methods first :body :ret :body :ret :test
             )
         (-> (taj (case 1 2 3)) :body :ret :test))))
  ;shift
  (is
    (= (-> (ast (fn [] (case 1 2 3))) :methods first :body :ret :body :ret :shift)
       (-> (taj (case 1 2 3)) :body :ret :shift)))
  ;mask
  (is
    (= (-> (ast (fn [] (case 1 2 3))) :methods first :body :ret :body :ret :mask)
       (-> (taj (case 1 2 3)) :body :ret :mask)))
  ;low
  (is
    (= (-> (ast (fn [] (case 1 2 3))) :methods first :body :ret :body :ret :low)
       (-> (taj (case 1 2 3)) :body :ret :low)))
  ;high
  (is
    (= (-> (ast (fn [] (case 1 2 3))) :methods first :body :ret :body :ret :high)
       (-> (taj (case 1 2 3)) :body :ret :high)))
  ;switch-type
  (is
    (= (-> (ast (fn [] (case 1 2 3))) :methods first :body :ret :body :ret :switch-type)
       (-> (taj (case 1 2 3)) :body :ret :switch-type)))
  ;test-type
  (is
    (= (-> (ast (fn [] (case 1 2 3))) :methods first :body :ret :body :ret :test-type)
       (-> (taj (case 1 2 3)) :body :ret :test-type)))
  ;skip-check?
  (is
    (= (-> (ast (fn [] (case 1 2 3))) :methods first :body :ret :body :ret :skip-check?)
       (-> (taj (case 1 2 3)) :body :ret :skip-check?)))
  ;;FIXME more tests for children
  )

(deftest AssignExpr-test
  (is 
    (= #{:added :ns :name :o-tag :line :form :tag :arglists :doc :assignable?}
       (leaf-diff
         (ast (set! *warn-on-reflection* true))
         (taj (set! *warn-on-reflection* true))))))

;; from clojure.test-helper
(defmacro with-err-string-writer
  "Evaluate with err pointing to a temporary StringWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)]
     (binding [*err* s#]
       ~@body
       (str s#))))

;; from clojure.test-helper
(defmacro with-err-print-writer
  "Evaluate with err pointing to a temporary PrintWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)
         p# (java.io.PrintWriter. s#)]
     (binding [*err* p#]
       ~@body
       (str s#))))

;; from clojure.test-helper
(defmacro should-not-reflect
  "Turn on all warning flags, and test that reflection does not occur
   (as identified by messages to *err*)."
  [form]
  `(binding [*warn-on-reflection* true]
     (is (nil? (re-find #"^Reflection warning" (with-err-string-writer ~form))))
     (is (nil? (re-find #"^Reflection warning" (with-err-print-writer ~form))))))

(deftest local-reflection-test
  (is (should-not-reflect
        (ast (do (deftype Refl [a])
                 #(reify 
                    clojure.lang.ILookupThunk
                    (get [thunk target]
                      (.a ^Refl target)))))))
  (is (should-not-reflect
        (ast (fn [^Object a]
               (.getParent ^java.io.File a))))))

(deftest TryExpr-test
  ;; body
  #_ ;; as of CLJ-1793, try's without catches or finally just emit their body
  (is 
    (= 
      #{:no-recur :loop-locals :loop-id :line :once :context :form}
      (leaf-diff
        (-> (ast (try)) :fn :methods first :body :ret 
            :body)
        (-> (taj (try)) 
            :body))))
  ;; empty catches
  #_
  (is 
    (= (-> (ast (try)) :fn :methods first :body :ret 
           :catches)
       (-> (taj (try)) 
           :catches)))
  #_
  (is 
    (= #{:no-recur :loop-locals :loop-id :line :result :once :top-level :context :form :tag :raw-forms}
       (leaf-diff
         (-> (ast (try)) :fn :methods first :body :ret)
         (-> (taj (try))))))
  (is 
    (= #{:no-recur :loop-locals :loop-id :line :result :once :top-level :context :form :tag :raw-forms}
       (leaf-diff
         (-> (ast (try (finally))) :fn :methods first :body :ret)
         (-> (taj (try (finally))))))))

(deftest CatchExpr-test
  (is 
    ;; FIXME why is :ns different?
    (= #{:loop-locals :ns :loop-id :file :column :line :once :context :tag}
       (leaf-diff
         (-> (ast (try (catch Exception e))) :fn :methods first :body :ret
             :catches first :body :ret)
         (-> (taj (try (catch Exception e))) 
             :catches first :body :ret))))
  )

(deftest RecurExpr-test
  (is (= 
        #{:loop-locals :children :body? :ns :loop-id :file :ignore-tag :fixed-arity :op :o-tag :column :variadic? :line :result :once :context :max-fixed-arity :form :tag :arglists :raw-forms}
        (leaf-diff
          (-> (ast (fn [] (loop [] (recur)))))
          (-> (taj (fn [] (loop [] (recur)))))))))

(deftest LetFnExpr-test
  (is (=
        (-> (ast (letfn [])) :fn :methods first :body :ret :bindings)
        (-> (taj (letfn [])) :bindings)))
  (is (=
       #{:loop-locals :locals :ns :loop-id :name :file :op :o-tag :column :line 
         :once :context :form :tag :arglists :atom :local :raw-forms}
       (leaf-diff
         (-> (ast (letfn [(a [])])) :fn :methods first :body :ret :bindings)
         (-> (taj (letfn [(a [])])) :bindings))))
  (is (= (-> (ast (letfn [(a [])])) :fn :methods first :body :ret emit-form)
         (-> (taj (letfn [(a [])])) emit-form)))
  (is (=
       (-> (ast
             (letfn [(a [] b)
                     (b [])]))
           :fn :methods first
           :body
           :ret
           :bindings
           first
           :init
           :methods first :body :ret
           (select-keys [:local :name])
           )
       {:local :letfn
        :name 'b__#0}
      )))

(deftest ns-form-test
  #_
  (is (-> (ast (ns foo)) emit-form))
  #_(is (-> (ast {:form '(ns foo)}) :val))
  ; should not reflect
  (is (should-not-reflect
        (-> (ast (fn loading []
                   (.getClass ^Object loading)))
            emit-form))))

(deftest KeywordInvoke-test
  (is (= (:result (ast (#(:a %) {:a 1})))
         (eval (emit-form (ast (#(:a %) {:a 1}))))
         (eval (emit-form (taj (#(:a %) {:a 1})))))))

(deftest defmacro-test
  (is (eval (emit-form (ast (defmacro foob [& body]))))))

(deftest multi-test
  (is (emit-form (ast (let []))))
  (is (nil? (eval (emit-form (ast (defmulti blah first)))))))

(deftest host-emit-form
  (is (emit-form (ast (fn [a b] (.getParent a b))))))


(deftest macroexpand-eval-test
  (is (ast 
        (do
          (defmacro ^:private compile-if [test then else]
            (if (eval test)
              then
              else))
          (deftype FooFFoo []
            Object
            (toString [this]
              (compile-if (resolve 'clojure.core/hash-unordered-coll)
                          (hash-unordered-coll this)
                          (.hashCode this))))))))

(deftest newmaptype-test
  (is (emit-form
        (ast
            (deftype Impasdfdsf []
              clojure.lang.MapEquivalence))))
  (is (contains?
        (:result
          (ast
            (do
              (deftype Impls []
                clojure.lang.MapEquivalence)
              (supers Impls))))
        clojure.lang.MapEquivalence))
  (is (:result
        (ast
          (do
            (deftype MyMap [m]
              clojure.lang.MapEquivalence
              java.util.Map
              (containsKey [this k]
                (contains? m k))
              (size [this]
                (count m))
              (get [this k]
                (get m k))
              clojure.lang.IPersistentMap
              (equiv [this that]
                (= m that))
              (seq [this]
                (seq m)))
            (and
              (=
               (MyMap. {:a 1})
               {:a 1})
              (=
               {:a 1}
               (MyMap. {:a 1}))))))))

(deftest static-invoke-expr
  (is (ast (long 1))))

(deftest case-nil-expr
  (is (ast (case nil
             nil 2))))

#_(emit-form
(ast
(let* [v__4413__auto__ (def nth-path-multimethod)]
  (clojure.core/when-not 
    (clojure.core/and (.hasRoot v__4413__auto__) 
                      (clojure.core/instance? clojure.lang.MultiFn (clojure.core/deref v__4413__auto__))) 
    (def nth-path-multimethod 
      (new clojure.lang.MultiFn "nth-path-multimethod" first :default #'clojure.core/global-hierarchy))))
))

#_(defmacro juxt-ast [f]
  `(do (time (ast ~f))
       (time (taj ~f))
       nil))

#_(juxt-ast
  (defn foo [{:keys [a b c]}]
    [(inc a) (+ b c)]))

#_(ast (defn foo [{:keys [a b c]}]
       [(inc a) (+ b c)]))

#_(juxt-ast
  (doseq [{:keys [a b c]} [1 2 3]
          {:keys [d e f]} [4 5 6]
          {:keys [d e f]} [4 5 6]
          {:keys [d e f]} [4 5 6]
          {:keys [d e f]} [4 5 6]
          :when a]
    (inc a)))

#_(do
  (time (ast (defn foo [{:keys [a b c]}]
               [(inc a) (+ b c)])))
  nil)
#_(do
  (time (taj (defn foo [{:keys [a b c]}]
               [(inc a) (+ b c)])))
  nil)

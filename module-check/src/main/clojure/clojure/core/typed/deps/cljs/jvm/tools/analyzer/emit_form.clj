(ns clojure.core.typed.deps.cljs.jvm.tools.analyzer.emit-form
  (:require [cljs.compiler]
            [cljs.analyzer :as ana]
            [cljs.core]
            [clojure.string :as str]))

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

(defmacro def-default-emit-method [d args & body]
  `(defmethod map->form [~d emit-default]
     [~@args ~'mode]
     (let [~'map->form (fn [f#] (map->form f# ~'mode))]
       ~@body)))

(def-default-emit-method :invoke
  [{:keys [f args]}]
  `(~(map->form f)
       ~@(map map->form args)))

(def-default-emit-method :constant
  [{:keys [form] :as e}]
  form)

(def-default-emit-method :var
  [{:keys [info] :as e}]
  (:name info))
                 
(def-default-emit-method :vector
  [{:keys [items] :as e}]
  (mapv map->form items))

(def-default-emit-method :list
  [{:keys [items] :as e}]
  (list* (mapv map->form items)))

(def-default-emit-method :js
  [{:keys [segs args] :as e}]
  (list* 'js* (str/join "~{}" segs) (doall (map map->form args))))

(def-default-emit-method :dot
  [{:keys [target field method args] :as e}]
  (if field
    (list '. (map->form target) (symbol (str \- field)))
    (list '. (map->form target) (cons method  (doall (map map->form args))))))

(def-default-emit-method :new
  [{:keys [ctor args] :as e}]
  (list* 'new (map->form ctor) (doall (map map->form args))))

(def-default-emit-method :map
  [{:keys [keys vals] :as e}]
  (zipmap (map map->form keys)
          (map map->form vals)))

(def-default-emit-method :set
  [{:keys [items] :as e}]
  (set (map map->form items)))

(def-default-emit-method :loop
  [{:keys [bindings expr] :as e}]
  (list 'loop* (vec (mapcat (fn [{:keys [name init]}]
                             [name (map->form init)])
                           bindings))
        (map->form expr)))

(def-default-emit-method :let
  [{:keys [bindings expr] :as e}]
  (list 'let* (vec (mapcat (fn [{:keys [name init]}]
                             [name (map->form init)])
                           bindings))
        (map->form expr)))

(def-default-emit-method :letfn
  [{:keys [bindings expr] :as e}]
  (list 'letfn* (vec (mapcat (fn [{:keys [name init]}]
                               [name (map->form init)])
                             bindings))
        (map->form expr)))

(def-default-emit-method :do
  [{:keys [statements ret] :as e}]
  (list* 'do (doall (map map->form (concat statements [ret])))))

(def-default-emit-method :if
  [{:keys [test then else] :as e}]
  (list 'if (map->form test)
        (map->form then)
        (map->form else)))

(def-default-emit-method :recur
  [{:keys [exprs] :as e}]
  (list* 'recur (doall (map map->form exprs))))

(def-default-emit-method :fn
  [{:keys [methods name] :as e}]
  (list* 'fn* 
         (concat (when name
                   (assert (symbol? (:name name)))
                   [(:name name)])
                 (doall (map (fn [{:keys [params variadic expr]}]
                               (list (vec (concat ((if variadic butlast identity)
                                                   (map :name params))
                                                  (when variadic
                                                    ['& (:name (last params))])))
                                     (map->form expr)))
                             methods)))))

(def-default-emit-method :def
  [{:keys [name init] :as e}]
  (list* 'def (symbol (clojure.core/name name)) 
        (when init
          [(map->form init)])))

(def-default-emit-method :deftype*
  [{:keys [t fields] :as e}]
  (list 'deftype* t (vec fields)))

(def-default-emit-method :set!
  [{:keys [target val] :as e}]
  (list 'set! (map->form target) (map->form val)))

(def-default-emit-method :throw
  [{:keys [throw] :as e}]
  (list 'throw (map->form throw)))

(def-default-emit-method :try*
  [{:keys [try finally catch name] :as e}]
  (list* 'try (map->form try)
         (doall
           (concat (when catch
                     [(list 'catch name (map->form catch))])
                   (when finally
                     [(list 'finally (map->form finally))])))))

(def-default-emit-method :ns
  [{:keys [form] :as e}]
  form)

(comment
  (require '[clojure.core.typed.deps.cljs.jvm.tools.analyzer :refer [ast]])
  (defmacro frm [f]
    `(-> (ast ~f) emit-form))

  (frm 1)
  (frm :a)

  (:form (ast (+ 1 2)))
  (frm (+ 1 2))
  (frm (- 1 2))

  (frm (apply - 1 2))

  (frm (.var - 1 2))
  (frm (.-var -))

  (frm (Integer. 1))

  (frm ())

  (frm [1])
  (frm [(- 1)])
  (frm {(- 1) 1})
  (frm #{(- 1) 1})

  (frm (do 1 2 3))
  (frm (let [a '(1 2)]
         (first 1)))
  (frm (loop [a '(1 2)]
         (first 1)))

  (frm (loop [a '(1 2)]
         (if 1
           (first 1)
           (recur 1))))

  (frm (fn [{:keys [a]} b] 1))
  (frm (fn ([{:keys [a]} b] 1)
           ([a] a)))
  (frm (instance? Class 1))

  (frm nil)
  (frm (def a 1))
  (frm (defn ab [a] a))
  (frm (fn ab [a] a))

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

  (binding [cljs.analyzer/*cljs-ns* cljs.analyzer/*cljs-ns*]
    (frm (ns foo (:require [foo]))))

  (binding [cljs.analyzer/*cljs-ns* cljs.analyzer/*cljs-ns*]
    (frm (def a)))

  (binding [cljs.analyzer/*cljs-ns* cljs.analyzer/*cljs-ns*]
    (frm (def a 1)))
)

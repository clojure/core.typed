(ns ^:skip-wiki clojure.core.typed.utils
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.util-vars :refer [*current-env*] :as uvs]
            [clojure.core.typed.impl-protocols :as ps]
            [clojure.core.typed.ast-utils :as au]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.deps.clojure.core.contracts.constraints :as contracts]
            [clojure.repl :as repl]
            [clojure.core.typed.deps.clojure.core.contracts]
            [clojure.set :as set]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.profiling :as profiling]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(t/tc-ignore
(alter-meta! *ns* assoc :skip-wiki true)
  )

(t/ann ^:no-check taoensso.timbre/logging-enabled? [t/Any -> t/Any])
(t/ann ^:no-check taoensso.timbre.profiling/*pdata* (t/Atom1 t/Any))
(t/ann ^:no-check clojure.core.typed.current-impl/assert-clojure [-> t/Any])

(t/ann subtype-exn Exception)
(def subtype-exn (Exception. "Subtyping failed."))
(t/ann cs-gen-exn Exception)
(def cs-gen-exn (Exception. "Constraint generation failed."))

(defmacro handle-subtype-failure [& body]
  `(try
     ~@body
     (catch Exception e#
       (if (identical? subtype-exn e#)
         false
         (throw e#)))))

(defmacro handle-cs-gen-failure [& body]
  `(try
     ~@body
     (catch Exception e#
       (if (identical? cs-gen-exn e#)
         false
         (throw e#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

; Temp copy from core.contracts. Faster predicates.
;(defmacro defconstrainedrecord
;  [name slots inv-description invariants & etc]
;  (let [fields (vec slots)
;        ns-part (namespace-munge *ns*)
;        classname (symbol (str ns-part "." name))
;        ctor-name (symbol (str name \.))
;        positional-factory-name (symbol (str "->" name))
;        map-arrow-factory-name (symbol (str "map->" name))
;        pred-arg (gensym)
;        chk `(clojure.core.typed.deps.clojure.core.contracts/contract
;                ~(symbol (str "chk-" name))
;                ~inv-description
;                [{:keys ~fields :as m#}]
;                ~invariants)]
;    `(do
;       (clojure.core/defrecord ~name ~fields ~@etc)
;       (defn ~(symbol (str name \?)) [~pred-arg]
;         (instance? ~name ~pred-arg))
;
;       ~(@#'clojure.core.typed.deps.clojure.core.contracts.constraints/build-positional-factory name classname fields invariants chk)
;
;       (clojure.core.typed.deps.clojure.core.contracts.constraints/defconstrainedfn ~map-arrow-factory-name
;         ([{:keys ~fields :as m#}]
;            ~invariants
;            (with-meta
;              (merge (new ~name ~@(for [e fields] nil)) m#)
;              {:contract ~chk})))
;       ~name)))

;FIXME prefer def-utils.clj version
(defmacro defrecord [name slots inv-description invariants & etc]
  ;only define record if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(contracts/defconstrainedrecord ~name ~slots ~inv-description ~invariants ~@etc)))

;FIXME prefer def-utils.clj version
(defmacro defprotocol [name & args]
  ;only define record if symbol doesn't resolve, not completely sure if this behaves like defonce
  (when-not (resolve name)
    `(clojure.core/defprotocol ~name ~@args)))

(t/tc-ignore
(def third (comp second next))
)

(defmacro ann-record 
  "Like ann-record, but also adds an unchecked annotation for core.contract's generated
  nme? predicate."
  [nme & args]
  `(do ~(-> `(clojure.core.typed/ann-record ~nme ~@args)
            (with-meta (meta &form)))
       ~(-> `(clojure.core.typed/ann ~(with-meta (symbol (str nme "-maker")) {:no-check true})
                                     [~@(map #(nth % 2) (partition 3 (first args))) ~'-> ~nme])
            (with-meta (meta &form)))
       ~(-> `(clojure.core.typed/ann ~(with-meta (symbol (str nme "?")) {:no-check true}) ~(list `t/Pred nme))
            (with-meta (meta &form)))))

(defmacro ann-precord 
  "Like ann-precord, but also adds an unchecked annotation for core.contract's generated
  nme? predicate."
  [nme & args]
  `(do (clojure.core.typed/ann-precord ~nme ~@args)
       (clojure.core.typed/ann ~(with-meta (symbol (str nme "?")) {:no-check true}) ~(list `t/Pred nme))))




(t/tc-ignore
;(t/ann next-sequence-number (t/Atom1 SeqNumber))
(defonce ^:private 
  ^{:doc "The next number to use for sequence hashing"}
  next-sequence-number 
  (atom 0))

(defn inc-sequence-number []
  (swap! next-sequence-number inc))

(defn get-and-inc-id []
  (profiling/p :utils/get-and-inc-id
  (let [id @next-sequence-number
        _ (inc-sequence-number)]
    id)))

(def default-xor 1)

(defn ^:private inner-deftype [fields hash-field meta-field that name-sym type-hash gs
                               maker methods*]
  `(deftype ~name-sym [~@fields ~(with-meta hash-field {:unsynchronized-mutable true}) ~meta-field]
     clojure.lang.IHashEq
     (equals [_# ~that]
       (and (instance? ~name-sym ~that)
            ; do not shadow fields here!
            ~@(for [f fields]
                `(= (~(keyword f) ~that) ~f))))
     ; don't shadow fields here!
     (hasheq [this#] (if-let [h# ~hash-field]
                       h#
                       (let [h# ~(if-let [ts (seq (map (fn [f] `(hash ~f)) fields))]
                                   `(bit-xor ~type-hash ~@ts)
                                   `(bit-xor ~type-hash ~default-xor))]
                         (set! ~hash-field h#)
                         h#)))
     ; don't shadow fields here!
     (hashCode [this#] (if-let [h# ~hash-field]
                         h#
                         (let [h# ~(if-let [ts (seq (map (fn [f] `(hash ~f)) fields))]
                                     `(bit-xor ~type-hash ~@ts)
                                     `(bit-xor ~type-hash ~default-xor))]
                           (set! ~hash-field h#)
                           h#)))

     clojure.lang.IObj
     (meta [this#] ~meta-field)
     (withMeta [this# ~gs] (~maker ~@fields :meta ~gs))


     clojure.lang.ILookup
     (valAt [this# k# else#]
       (case k# ~@(mapcat (fn [fld] [(keyword fld) fld]) 
                          fields)
         (throw (UnsupportedOperationException. (str "lookup on " '~name-sym k#)))))
     (valAt [this# k#]
       (case k# ~@(mapcat (fn [fld] [(keyword fld) fld]) 
                          fields)
         (throw (UnsupportedOperationException. (str "lookup on " '~name-sym k#)))))

     clojure.lang.IKeywordLookup
     (getLookupThunk [this# k#]
       (let [~'gclass (class this#)]              
         (case k#
           ~@(let [hinted-target (with-meta 'gtarget {:tag name-sym})] 
               (mapcat 
                 (fn [fld]
                   [(keyword fld)
                    `(reify clojure.lang.ILookupThunk
                       (get [~'thunk ~'gtarget]
                         (if (identical? (class ~'gtarget) ~'gclass)
                           (. ~hinted-target ~(symbol (str "-" fld)))
                           ~'thunk)))])
                 fields))
           (throw (UnsupportedOperationException. (str "lookup on " '~name-sym k#))))))

     clojure.lang.IPersistentMap
     (assoc [this# k# ~gs]
       (condp identical? k#
         ~@(mapcat (fn [fld]
                     [(keyword fld) `(~maker ~@(replace {fld gs} fields) :meta ~meta-field)])
                   fields)
         (throw (UnsupportedOperationException. (str "assoc on " '~name-sym k#)))))
     (entryAt [this# k#] (throw (UnsupportedOperationException. (str "entryAt on " '~name-sym k#))))
     (count [this#] (throw (UnsupportedOperationException. (str "count on " '~name-sym))))
     (empty [this#] (throw (UnsupportedOperationException. (str "Can't create empty: " ~(str name-sym)))))
     (cons [this# e#] (throw (UnsupportedOperationException. (str "cons on " '~name-sym))))
     (equiv [_# ~that]
       (and (instance? ~name-sym ~that)
            ; do not shadow fields here!
            ~@(for [f fields]
                `(= (~(keyword f) ~that) ~f))))
     (containsKey [this# k#] (throw (UnsupportedOperationException. (str "containsKey on " '~name-sym))))
     (seq [this#] (seq [~@(map #(list `new `clojure.lang.MapEntry (keyword %) %) (concat fields [#_meta-field]))]))

     (iterator [this#] (throw (UnsupportedOperationException. (str "iterator on " '~name-sym))))
     (without [this# k#] (throw (UnsupportedOperationException. (str "without on " '~name-sym))))

     Comparable
     ~(let [this (gensym 'this)]
        `(compareTo [~this ~that]
                    ;returns 1 if we have 2 instances of name-sym with
                    ; identical hashs, but are not =
                    (cond (= ~this ~that) 0
                          (instance? ~name-sym ~that)
                          (if (< (hash ~this)
                                 (hash ~that))
                            -1
                            1)
                          :else (if (< (hash ~name-sym) (hash (class ~that)))
                                  -1
                                  1))))

     ~@methods*))

(defn emit-deftype [original-ns def-kind name-sym fields invariants methods*]
  (assert (symbol? name-sym))
  (let [classname (with-meta (symbol (str (namespace-munge *ns*) "." name-sym)) (meta name-sym))
        ->ctor (symbol (str "->" name-sym))
        maker (symbol (str name-sym "-maker"))
        that (gensym)
        gs (gensym)
        type-hash (hash classname)
        meta-field '_meta
        hash-field '_hash]
    `(do
       (declare ~maker)
       ~(inner-deftype fields hash-field meta-field that name-sym type-hash gs
                       maker methods*)

       (swap! ~(symbol (str original-ns) (str "all-" def-kind "s")) conj '~classname)

       (alter-meta! (var ~->ctor) assoc :private true)

       (defn ~(symbol (str name-sym "?")) [a#]
         (instance? ~name-sym a#))

       ; (Atom1 (Map t/Any Number))
       (defn ~maker [~@fields & {meta# :meta :as opt#}]
         {:pre ~invariants}
         (profiling/p ~(keyword "maker" (str name-sym))
          (profiling/p ~(keyword "maker" (str name-sym "-meta-check"))
           (let [extra# (set/difference (set (keys opt#)) #{:meta})]
             (assert (empty? extra#) (str "Extra arguments:" extra#))))
          ; ~@fields are in scope above
          (~->ctor ~@fields nil meta#))))))

(defmacro mk [original-ns def-kind name-sym fields invariants & {:keys [methods]}]
  (when-not (resolve name-sym)
    `(t/tc-ignore
       ~(emit-deftype original-ns def-kind name-sym fields invariants methods))))

(defmacro defspecial [name]
  (let [all-entries (symbol (str "all-" name "s"))]
    `(do (defn ~(symbol (str name "="))
           [t1# t2#]
           (= t1# t2#))
         (defn ~(symbol (str name "<"))
           [t1# t2#]
           (neg? (compare t1# t2#)))
         (defn ~(symbol (str name "-comparator"))
           [t1# t2#]
           (compare t1# t2#))
         (def ~all-entries (atom #{}))
         (defmacro ~(symbol (str "def-" name))
           [name# fields# doc# invariants# & opts#]
           `(mk ~'~(ns-name *ns*)
                ~'~name
                ~name# 
                ~fields# 
                ~invariants# 
                ~@opts#)))))

(defspecial type)
(defspecial filter)
(defspecial object)
(defspecial path)

)

(defmacro add-defmethod-generator 
  "Generates a macro called mm-name, which can be used instead
  of defmethod of the multimethod called mm-name.
  The generated macro adds a meaningful name to the local function
  of the defmethod, and profiling information via timbre for each
  defmethod.
  
  Usage: (add-mm-name-method check)

  (defmethod check  ...) then becomes (add-check-method ...)"
  [mm-name]
  `(defmacro ~(symbol (str "add-" mm-name "-method")) 
     [~'nme ~'params & ~'body]
     (let [[~'assertmap ~'body] (if (and (map? (first ~'body))
                                         (< 1 (count ~'body)))
                                  [(first ~'body) (next ~'body)]
                                  [nil ~'body])]
       `(defmethod 
          ;the multimethod to install methods to
          ~'~mm-name 
          ;the dispatch value
          ~~'nme
          ;the local fn name of this defmethod, gensymed to
          ;avoid reloading conflicts
          ~(symbol (str ~(str mm-name " ") (str ~'nme) (gensym "")))
          ;the param list
          ~~'params
          ;the pre/post condition map
          ~~'assertmap
          ;the body, wrapped in a profiling macro
          (u/p ~(keyword (str '~mm-name) (str ~'nme))
               (trace '~(symbol (str '~mm-name) (str ~'nme)))
               ~@~'body)))))

;; Aliases for profiling stuff
(defmacro p [& args]
  `(profiling/p ~@args))

(defmacro profile [& args]
  `(profiling/profile ~@args))

(t/ann typed-ns-opts [t/Any -> t/Any])
(defn typed-ns-opts [ns]
  (-> ns meta :core.typed))

(t/ann ^:no-check demunge-ns [(t/U t/Sym String) -> t/Sym])
(defn demunge-ns [nsym]
  (symbol (clojure.repl/demunge (str nsym))))


; debug code from https://groups.google.com/d/msg/clojure/cOXClow1Wn4/UkvtICjvgrIJ
(t/ann ^:no-check pprint-str [t/Any -> t/Any])
(defn pprint-str
  [x]
  (with-out-str (pprint/pprint x)))

(defmacro dbg
  [x]
  `(let [x# ~x]
     (printf "dbg %s:%s> %s is %s\n"
             ~*ns*
             ~(:line (meta &form))
             ~(pr-str x)
             (pprint-str x#))
     (flush)
     x#))

(t/tc-ignore
;; multimethods for dispatching on special forms like (do ::special-form ::foobar ...)
(defn internal-dispatch-val [expr]
  (:form (second (:statements expr))))

(defn enforce-do-folding [{:keys [statements] :as expr} kw]
  (when-not (#{0 1} (count 
                      (filter #{kw} 
                              (map :val statements))))
    (err/int-error (str "Folded special-forms detected " (au/emit-form-fn expr)))))

(defmacro special-do-op
  "Define a multimethod that takes an expr and an expected type
  and dispatches on the second statement"
  [kw nme]
  `(defmulti ~nme (fn [expr# & _#] (internal-dispatch-val expr#))))

(defn internal-form? [expr kw]
  (= kw (:form (first (:statements expr)))))

(defn ns? [n]
  (instance? clojure.lang.Namespace n))

(def expr-type :clojure.core.typed.check/expr-type)

;(t/ann tc-warning [t/Any * -> nil])
(defn tc-warning [& ss]
  (let [env uvs/*current-env*]
    (binding [*out* *err*]
      (apply println "WARNING: "
             (str "(" (:file env) ":" (:line env) 
                  (when-let [col (:column env)]
                    (str ":" col))
                  ") ")
             ss)
      (flush))))

(defmacro with-tracing [& body]
  `(binding [uvs/*trace-checker* true]
     ~@body))

(defmacro trace [& ss]
  `(when uvs/*trace-checker*
     (println 
       "TRACE: " 
       " "
       (:line uvs/*current-env*)
       ~@ss)
     (flush)))

(defmacro trace-when [p & ss]
  `(when uvs/*trace-checker*
     (when ~p
       (println 
         "TRACE: " 
         " "
         (:line uvs/*current-env*)
         ~@ss)
       (flush))))

(defmacro trace-when-let [p & ss]
  `(when uvs/*trace-checker*
     (when-let ~p
       (println 
         "TRACE: " 
         " "
         (:line uvs/*current-env*)
         ~@ss)
       (flush))))

(defn pad-right
  "Returns a sequence of length cnt that is s padded to the right with copies
  of v."
  [^long cnt s v]
  {:pre [(integer? cnt)]}
  (concat s
          (repeat (- cnt (count s)) v)))

)

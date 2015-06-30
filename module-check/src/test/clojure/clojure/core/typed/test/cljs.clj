(ns clojure.core.typed.test.cljs
  (:require [clojure.core.typed.test.cljs-utils :refer :all]
            [clojure.test :refer :all]
            [clojure.core.typed.type-rep :as r]
            [clojure.core.typed.subtype :as sub]
            [cljs.core.typed :as t]
            [clojure.core.typed.util-cljs :as ucljs]
            [clojure.core.typed.type-ctors :as c]
            [clojure.core.typed.parse-unparse :as prs]

            [clojure.core.typed.base-env-common :refer [delay-and-cache-env]
             :as common]
            [clojure.core.typed.var-env :as var-env]
            [clojure.core.typed.test.cljs-core :as core-test]))

(deftest parse-prims-cljs-test
  (is-cljs (= (prs/parse-cljs 'number)
              (r/NumberCLJS-maker)))
  (is-cljs (= (prs/parse-cljs 'int)
              (r/IntegerCLJS-maker)))
  (is-cljs (= (prs/parse-cljs 'boolean)
              (r/BooleanCLJS-maker)))
  (is-cljs (= (prs/parse-cljs 'object)
              (r/ObjectCLJS-maker)))
  (is-cljs (= (prs/parse-cljs 'string)
              (r/StringCLJS-maker))))

(deftest parse-array-cljs-test
  (is-cljs (= (prs/parse-cljs '(Array number))
              (r/ArrayCLJS-maker (prs/parse-cljs 'number)
                                 (prs/parse-cljs 'number)))))

(deftest unparse-prims-cljs-test
  (is-cljs (= 'number
              (prs/unparse-type (prs/parse-cljs 'number))))
  (is-cljs (= 'boolean
              (prs/unparse-type (prs/parse-cljs 'boolean))))
  (is-cljs (= 'int
              (prs/unparse-type (prs/parse-cljs 'int))))
  (is-cljs (= '(Array number)
              (prs/unparse-type (prs/parse-cljs '(Array number)))))
  (is-cljs (= '(Array2 number boolean)
              (prs/unparse-type (prs/parse-cljs '(Array2 number boolean))))))

(deftest subtype-prims-cljs-test
  (is-cljs (sub/subtype? (r/-val 1) (prs/parse-cljs 'number))))

(deftest ann-test
  (is-tc-e (t/ann foo number)))

(deftest check-ns-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.ann)))

(deftest parse-protocol-test 
  (is-cljs (prs/parse-cljs '(cljs.core/IMap number number))))

(deftest Protocol-of-test
  (is-cljs (c/Protocol-of 'cljs.core/IMap [(r/NumberCLJS-maker)
                                           (r/NumberCLJS-maker)])))

(deftest heterogeneous-ds-test
  (is-tc-e [1 2]
           '[number number])
  (is-tc-e [1 2]
           (IVector number))
  (is-tc-e {:a 1}
           '{:a number})
  (is-tc-e {1 1}
           (IMap number number))
  (is-tc-e #{1}
           (ISet number))
  (is-tc-e (let [a 1] #{1 a})
           (ISet number)))

(deftest js*-test
  (is-tc-e (+ 1 1)))

(deftest fn-test
  (is-tc-e (fn a [b] a))
  (is-tc-e (fn [a] a)
           (t/All [x] [x -> x])))

(deftest inst-test
  (is-tc-e (let [f (-> (fn [a] a)
                       (t/ann-form (t/All [x] [x -> x])))]
             ((t/inst f number) 1))))

(deftest letfn-test
  (is-tc-e (t/letfn> [a :- (t/All [x] [x -> x])
                      (a [b] b)]
             (a 1))))

#_(deftest async-test
  (is-cljs (t/check-ns* 'cljs.core.typed.async)))

(deftest inline-annotation-test
  ; code from David Nolen's blog
  ;FIXME
  #_(is-tc-e
    (defn ^{:ann '[(t/U nil (ISeqable t/Any)) t/Any -> int]}
      index-of [xs x]
      (let [len (count xs)]
        (t/loop>
         [i :- int, 0]
         (if (< i len)
           (if (= (nth xs i) x)
             i
             (recur (inc i)))
           -1))))))

#_(clojure.core.typed.analyze-cljs/ast-for-form '(fn [x] (instance? Atom x)))

(deftest simple-polymorphic-test
  (is-cljs (t/check-ns* 'cljs.core.typed.test.identity)))

(deftest value-supertype-test
  (is-tc-e 'a Symbol)
  (is-tc-e :a Keyword)
  (is-tc-e 1 int)
  (is-tc-e 1.1 number)
  (is-tc-e 1 number)
  (is-tc-e true boolean)
  (is-tc-e "a" string))

(deftest ns-deps-test
  (is (t/check-ns* 'cljs.core.typed.test.dep-one))
  (is (t/check-ns* 'cljs.core.typed.test.dep-two)))

(deftest hvec-infer
  (is-tc-e (fn [a]
             (a [1 2]))
           [[(cljs.core/IVector t/Any) -> t/Any]
            -> t/Any])
  (is-tc-e (fn [a]
             (a [1 2]))
           [(t/All [x] [(cljs.core/IVector x) -> x])
            -> t/Any]))

(deftest seq-test
  (is-tc-e [1 2 3] (t/Coll int))
  (is-tc-e [1 2 3] (t/Seqable int))  ;;not sure if it should be...
  (is-tc-e (seq [1 2 3]) (t/NonEmptyASeq int)))

                                        ;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.dom)
                                        ;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.reactive)
                                        ;(t/check-ns* 'cljs.core.typed.test.dnolen.utils.helpers)
                                        ;(t/check-ns* 'cljs.core.typed.async)


(deftest core-fns-test
  (t/check-ns* 'cljs.core.typed.test.ympbyc.test-base-env))

(declare cljs-core-vars)

(deftest annotation-coverage
  (let [n-core-vars    (count cljs-core-vars)
        n-common-anns  (count @common/common-var-annotations)
        n-cljs-anns    (count @var-env/CLJS-VAR-ANNOTATIONS)]
    (or (= n-core-vars n-cljs-anns)
        (do
          (println (str "number of vars in cljs.core:       " n-core-vars))
          (println (str "number of vars in base-env-common: " n-common-anns))
          (println (str "number of specific cljs vars:      " (- n-cljs-anns
                                                                 n-common-anns)))
          (println (str "Coverage: "
                        (* 100 (float (/ n-cljs-anns  n-core-vars)))
                        "% ("
                        (- n-core-vars n-cljs-anns)
                        " vars are missing its annotations)"))
          (println (clojure.set/difference (set (map name cljs-core-vars))
                                           (set (map name (keys @var-env/CLJS-VAR-ANNOTATIONS)))))
          false))))


(comment
  "evaluate this in cljs repl to get cljs.core symbols"
  (->> cljs.core
       (.keys js/Object)
       vec
       (map (fn [x] (-> x
             (clojure.string/replace #"_BANG_" "!")
             (clojure.string/replace #"_QMARK_" "?")
             (clojure.string/replace #"_GT_" ">")
             (clojure.string/replace #"_LT_" "<")
             (clojure.string/replace #"_EQ_" "=")
             (clojure.string/replace #"_STAR_" "*")
             (clojure.string/replace #"_" "-"))))
       (map symbol)
       set))


(def cljs-core-vars
  '#{sorted-map re-pattern keyword? unchecked-inc-int val chunked-seq? ->VectorNode equiv-sequential pop-tail *main-cli-fn* object-array pr-sequential-writer ->ObjMap max-key hash-coll list* pr-seq-writer booleans == chunk-buffer array-map-index-of longs TransientArrayMap shorts array->transient-hash-map instance? ->ArrayNode tv-editable-root ->Reduced pr-str-with-opts prn-str-with-opts sequential? fn? empty TransientVector dorun remove-method KeySeq gensym not= ->PersistentQueue Keyword unchecked-multiply bit-or hash-set fixture2 add-watch unchecked-dec some nil? push-tail Subvec string? second keys ->Atom long-array hash-collision-node-find-index tv-ensure-editable ex-cause truth- pv-aset bit-set get-global-hierarchy is-proto- sorted? bit-count fixture1 float$ char-escapes Atom inode-kv-reduce gensym-counter false? ->TransientVector true? clone-and-set boolean$ repeat ->NeverEquiv zipmap distinct string-print get-in bit-xor complement get-validator seqable? ->PersistentHashMap ->MultiFn js->clj pop! derive ChunkedCons PersistentTreeMap bitpos ExceptionInfo PersistentArrayMap prefers* partition-by rem PersistentQueue odd? create-tree-map-seq symbol? ->BlackNode mapv *print-level* TransientHashMap StringBufferWriter js-mod compare-symbols double$ js-obj filterv key->js pv-clone-node re-matches split-with add-to-string-hash-cache tree-map-remove accumulating-seq-count spread next symbol vals ->ArrayChunk select-keys reduceable? rand deref tv-push-tail tail-off unchecked-inc sequence Box make-hierarchy balance-left-del number? throw-no-method-error assoc! descendants linear-traversal-nth into-array last some-fn unchecked-negate integer? LazySeq ->PersistentHashSet reduced? ->ChunkedCons MultiFn prn ->PersistentTreeMap with-meta floats TransientHashSet * butlast RSeq - lookup-sentinel NeverEquiv reversible? rseq flatten1 seq? ci-reduce pack-array-node identical? print array-map-index-of-identical? vary-meta PersistentTreeSet bit-flip zero? bit-and key-test first-array-for-longvec newline edit-and-set ->Symbol replicate balance-right-del keep-indexed native-satisfies? distinct? vec tree-map-replace obj-map-compare-keys concat update-in vector seq-reduce ->RedNode conj find-and-cache-best-method short$ ->StringBufferWriter unchecked-add assoc fix unchecked-remainder-int int$ neg? long$ doubles js-delete isa? remove-watch print-str rsubseq *flush-on-newline* HashCollisionNode vector? split-at chunk-cons + int-array ->KeySeq unchecked-long mk-bound-fn map counted? double-array NodeSeq / clone *print-length* frequencies chars rand-int unchecked-short prn-str iterate chunk-append unchecked-double unchecked-int mapcat assoc-in special-symbol? build-subvec conj! inc RedNode ASeq every-pred array-chunk create-array-node-seq persistent-array-map-seq ChunkedSeq ->RSeq unsigned-bit-shift-right chunked-seq shuffle re-find BitmapIndexedNode bit-not seq unchecked-multiply-int to-array-2d array-map-index-of-equiv? sorted-map-by filter bounded-count js-keys alter-meta! unchecked-dec-int key ->Keyword equiv-map re-seq empty? ->PersistentVector ->Range name list? array-map-index-of-keyword? ->NodeSeq pr-opts ->PersistentArrayMap unchecked-array-for aset nnext bit-shift-right-zero-fill doall not-any? PersistentHashMap reductions into tv-pop-tail object? *print-newline* ffirst bit-clear hash-symbol set-print-fn! pr-sb-with-opts hash char$ compare-indexed associative? Delay tv-editable-tail scan-array drop-last replace ArrayChunk parents map? keyword-identical? prefers quot chunk-rest unchecked-negate-int reverse unchecked-substract count set ->HashCollisionNode ex-info ->ValSeq extend-object! fn->comparator ->ArrayNodeSeq comp nth dissoc! never-equiv dominates constantly namespace swap-global-hierarchy! pr-str < sort-by cycle peek pr-with-opts reduce clj->js interleave ->TransientHashSet print-map pv-aget cons str ArrayNode type->str remove-all-methods first ->LazySeq PersistentHashSet = transient$ array-reduce vector-index-out-of-bounds memoize unchecked-float PersistentVector range *clojurescript-version* tree-seq set-validator! ->Box ->EmptyList unchecked-divide-int prefer-method partition-all write-all reduced not-every? array-map-extend-kv > max create-node identity ints fnext min-key ->BitmapIndexedNode List reset-meta! array *unchecked-if* unchecked-add-int subs array-map-index-of-nil? ->IndexedSeq *print-fn* >= hash-imap reduce-kv reset! even? bit-shift-left methods$ new-path balance-left compare string-hash-cache PersistentQueueSeq sorted-set-by PersistentArrayMapSeq group-by ->Subvec ->UUID bitmap-indexed-node-index EmptyList get <= mask tree-map-add ->ExceptionInfo bytes regexp? fnil ->PersistentQueueSeq string-hash-cache-count force create-inode-seq partial set-from-indexed-seq BlackNode comparator array-seq pos? take-while balance-right IChunkedNext unchecked-editable-array-for underive ex-message ancestors hash-combine persistent! partition map-indexed ChunkBuffer contains? interpose chunk ifn? remove-pair enable-console-print! obj-map->hash-map apply swap! array-copy ->PersistentArrayMapSeq subvec rest keyword ->PersistentTreeMapSeq indexed? cloneable? quote-string ->Cons mod nfirst Fn nthnext pv-fresh-node tree-map-seq-push array-for ->List array-map unchecked-char tree-map-kv-reduce dec undefined? println array-copy-downward aget ->TransientHashMap pr drop *print-dup* array? aclone UUID byte$ pop ->ChunkBuffer atom ->PersistentTreeSet bit-shift-right not-native delay? Cons realized? disj *print-readably* merge-with take-nth *print-meta* array-map-index-of-symbol? obj-map take-last take unchecked-byte check-string-hash-cache apply-to obj-clone set? make-array rand-nth juxt alength chunk-first tree-map-append to-array hash-map Range bit-and-not compare-and-set! type repeatedly trampoline remove reset-cache find do-assoc unchecked-substract-int pr-str* coll? drop-while not-empty flatten ex-data pr-writer println-str ArrayNodeSeq list ValSeq chunk-next every? flush ->ChunkedSeq Reduced sort dissoc not get-method sorted-set merge subseq ObjMap missing-protocol min Symbol bit-test keep disj! VectorNode meta ->Delay PersistentTreeMapSeq hash-iset prim-seq})

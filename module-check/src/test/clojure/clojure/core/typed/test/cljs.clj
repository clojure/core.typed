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
            [clojure.core.typed.var-env :as var-env]))

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
  (is-tc-e
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
  (is-tc-e true boolean))

(deftest ns-deps-test
  (is (t/check-ns* 'cljs.core.typed.test.dep-one))
  (is (t/check-ns* 'cljs.core.typed.test.dep-two)))

(deftest hvec-infer
  (is-tc-e (fn [a]
             (a [1 2]))
           [[(cljs.core/IVector Any) -> Any]
            -> Any])
  (is-tc-e (fn [a]
             (a [1 2]))
           [(t/All [x] [(cljs.core/IVector x) -> x])
            -> Any]))

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
        n-common-anns  (count common/common-var-annotations)
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
          (println (clojure.set/difference (set (map name (keys (ns-map 'cljs.core))))
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
  '#{sorted-map re-pattern keyword? unchecked-inc-int val -key->js chunked-seq? ->VectorNode equiv-sequential pop-tail *main-cli-fn* object-array pr-sequential-writer ->ObjMap max-key hash-coll list* IEditableCollection -contains-key? pr-seq-writer booleans == chunk-buffer -entry-key -next array-map-index-of longs TransientArrayMap shorts IHash array->transient-hash-map instance? -flush ->ArrayNode tv-editable-root ->Reduced pr-str-with-opts prn-str-with-opts sequential? fn? empty TransientVector dorun remove-method KeySeq gensym not= ->PersistentQueue Keyword -namespace unchecked-multiply bit-or hash-set ILookup -first fixture2 add-watch unchecked-dec some nil? push-tail IndexedSeq Subvec string? second keys IEncodeJS ->Atom long-array hash-collision-node-find-index tv-ensure-editable ex-cause truth- pv-aset bit-set get-global-hierarchy IAssociative is-proto- sorted? bit-count fixture1 float$ char-escapes Atom -sorted-seq-from inode-kv-reduce gensym-counter false? ->TransientVector true? clone-and-set -notify-watches boolean$ repeat ->NeverEquiv IFn ISwap zipmap distinct string-print get-in bit-xor complement get-validator seqable? ->PersistentHashMap -dissoc ->MultiFn js->clj pop! derive INamed ChunkedCons PersistentTreeMap bitpos ExceptionInfo PersistentArrayMap prefers* partition-by rem PersistentQueue odd? create-tree-map-seq symbol? ->BlackNode mapv *print-level* TransientHashMap StringBufferWriter js-mod compare-symbols double$ -assoc! js-obj filterv key->js -remove-watch pv-clone-node re-matches split-with add-to-string-hash-cache IChunk tree-map-remove accumulating-seq-count spread next symbol vals ->ArrayChunk select-keys reduceable? rand deref tv-push-tail IEncodeClojure -get-method -compare tail-off unchecked-inc sequence Box make-hierarchy balance-left-del -disjoin! number? throw-no-method-error assoc! descendants linear-traversal-nth into-array last some-fn unchecked-negate integer? LazySeq ->PersistentHashSet reduced? ->ChunkedCons MultiFn -prefers prn ->PersistentTreeMap with-meta -add-watch floats TransientHashSet * butlast RSeq - lookup-sentinel -empty NeverEquiv reversible? rseq flatten1 seq? ci-reduce pack-array-node identical? print array-map-index-of-identical? vary-meta PersistentTreeSet bit-flip zero? bit-and key-test first-array-for-longvec -comparator newline IDeref edit-and-set ->Symbol replicate balance-right-del keep-indexed native-satisfies? distinct? vec tree-map-replace -equiv obj-map-compare-keys concat update-in vector seq-reduce ->RedNode conj find-and-cache-best-method short$ ->StringBufferWriter unchecked-add assoc fix unchecked-remainder-int int$ -nth neg? long$ doubles ISequential js-delete -reset isa? -deref-with-timeout -pr-writer -kv-reduce remove-watch print-str rsubseq *flush-on-newline* HashCollisionNode vector? split-at chunk-cons -PLUS- int-array ->KeySeq unchecked-long mk-bound-fn map INext -as-transient counted? double-array IPrintWithWriter IKVReduce NodeSeq -SLASH- clone *print-length* frequencies chars rand-int -add-method unchecked-short prn-str iterate IRecord chunk-append unchecked-double unchecked-int mapcat ICollection assoc-in special-symbol? build-subvec conj! inc RedNode ASeq every-pred IReversible -realized? array-chunk create-array-node-seq persistent-array-map-seq ChunkedSeq ->RSeq unsigned-bit-shift-right chunked-seq shuffle re-find BitmapIndexedNode bit-not IMapEntry seq unchecked-multiply-int to-array-2d array-map-index-of-equiv? sorted-map-by filter -key bounded-count js-keys alter-meta! unchecked-dec-int key -seq ->Keyword equiv-map re-seq empty? ->PersistentVector ->Range ITransientSet name list? array-map-index-of-keyword? ->NodeSeq pr-opts ->PersistentArrayMap unchecked-array-for aset nnext bit-shift-right-zero-fill doall -swap! not-any? PersistentHashMap reductions IWatchable into tv-pop-tail object? *print-newline* ffirst bit-clear hash-symbol set-print-fn! pr-sb-with-opts -clj->js hash char$ compare-indexed IPending associative? Delay tv-editable-tail scan-array drop-last replace ArrayChunk parents map? keyword-identical? prefers quot chunk-rest unchecked-negate-int reverse unchecked-substract IReduce count set ->HashCollisionNode -chunked-rest ex-info ->ValSeq extend-object! -chunked-first fn->comparator ->ArrayNodeSeq comp nth dissoc! -peek -assoc never-equiv dominates -prefer-method ISorted constantly ITransientMap namespace swap-global-hierarchy! pr-str < sort-by cycle peek pr-with-opts reduce clj->js interleave -sorted-seq ->TransientHashSet print-map pv-aget cons ->TransientArrayMap str ArrayNode type->str -rseq remove-all-methods ISeqable first ->LazySeq PersistentHashSet = transient$ array-reduce vector-index-out-of-bounds memoize -remove-method unchecked-float PersistentVector range *clojurescript-version* tree-seq set-validator! -js->clj ->Box -lookup ->EmptyList unchecked-divide-int prefer-method partition-all write-all reduced not-every? array-map-extend-kv ITransientVector > max create-node identity ints fnext min-key ->BitmapIndexedNode List reset-meta! array *unchecked-if* unchecked-add-int subs ISet array-map-index-of-nil? -drop-first ICounted ->IndexedSeq -methods ITransientCollection -invoke -deref *print-fn* IStack >= hash-imap reduce-kv reset! even? bit-shift-left methods$ new-path balance-left compare IMultiFn string-hash-cache PersistentQueueSeq sorted-set-by -assoc-n! -meta PersistentArrayMapSeq group-by ->Subvec ->UUID bitmap-indexed-node-index EmptyList get <= mask tree-map-add -rest ->ExceptionInfo bytes regexp? -chunked-next fnil ->PersistentQueueSeq string-hash-cache-count force create-inode-seq partial -disjoin set-from-indexed-seq BlackNode comparator -conj! array-seq pos? ISeq take-while balance-right IVector IChunkedNext unchecked-editable-array-for underive ex-message ancestors hash-combine IEquiv persistent! partition map-indexed ChunkBuffer contains? interpose chunk ifn? remove-pair IWithMeta enable-console-print! obj-map->hash-map ICloneable IMeta apply swap! array-copy ->PersistentArrayMapSeq subvec rest -val keyword -assoc-n ->PersistentTreeMapSeq IComparable indexed? cloneable? quote-string ->Cons mod nfirst Fn nthnext pv-fresh-node tree-map-seq-push array-for ->List array-map unchecked-char -count IIndexed tree-map-kv-reduce dec undefined? println array-copy-downward aget ->TransientHashMap pr drop -conj *print-dup* IWriter array? IEmptyableCollection aclone UUID byte$ pop ->ChunkBuffer atom ->PersistentTreeSet IReset bit-shift-right not-native delay? Cons -dissoc! realized? disj *print-readably* merge-with take-nth IChunkedSeq *print-meta* array-map-index-of-symbol? -hash obj-map take-last take unchecked-byte -persistent! check-string-hash-cache apply-to obj-clone IList set? make-array rand-nth juxt alength IAtom chunk-first tree-map-append to-array hash-map Range bit-and-not compare-and-set! -pop! ITransientAssociative type -clone repeatedly trampoline remove reset-cache find do-assoc unchecked-substract-int pr-str* coll? drop-while not-empty flatten ex-data pr-writer -reduce println-str ArrayNodeSeq IMap list ValSeq chunk-next every? flush ->ChunkedSeq Reduced sort -pop dissoc not IDerefWithTimeout -reset! get-method sorted-set merge subseq ObjMap missing-protocol min Symbol -global-hierarchy bit-test keep disj! VectorNode meta ->Delay -write -name PersistentTreeMapSeq hash-iset prim-seq -with-meta})

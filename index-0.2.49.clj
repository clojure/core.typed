{:namespaces
 ({:source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed-api.html",
   :name "clojure.core.typed",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/185c52a5636ae00eb978de9477e1b666b3c18974/src/main/clojure/clojure/core/typed/async.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.async-api.html",
   :name "clojure.core.typed.async",
   :doc
   "This namespace contains annotations and helper macros for type\nchecking core.async code. Ensure clojure.core.async is require'd\nbefore performing type checking.\n\ngo\n  use go>\n\nchan\n  use chan>\n\nbuffer\n  use buffer> (similar for other buffer constructors)\n"}
  {:source-url
   "https://github.com/clojure/core.typed/blob/92198b007bfdbddb537b13e8a7f829d4eae625fd/src/main/clojure/clojure/core/typed/check/funapp.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.funapp-api.html",
   :name "clojure.core.typed.check.funapp",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/isa.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.isa-api.html",
   :name "clojure.core.typed.check.isa",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/92198b007bfdbddb537b13e8a7f829d4eae625fd/src/main/clojure/clojure/core/typed/check/map.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.map-api.html",
   :name "clojure.core.typed.check.map",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/utils.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.utils-api.html",
   :name "clojure.core.typed.check.utils",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/3aa23baeee6cc53b0ab5109b3c6c515b76abf613/src/main/clojure/clojure/core/typed/coerce_utils.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.coerce-utils-api.html",
   :name "clojure.core.typed.coerce-utils",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/1e09597531e797ead73a0bbbfc570499361a8f6b/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.hole-api.html",
   :name "clojure.core.typed.hole",
   :doc
   "This namespace contains easy tools for hole driven development"}
  {:source-url
   "https://github.com/clojure/core.typed/blob/92198b007bfdbddb537b13e8a7f829d4eae625fd/src/main/clojure/clojure/core/typed/open_result.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.open-result-api.html",
   :name "clojure.core.typed.open-result",
   :doc nil}),
 :vars
 ({:name "*collect-on-eval*",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1762",
   :deprecated "0.2.45",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/*collect-on-eval*",
   :doc
   "If a true value, global annotations are collected by the\ntype checker when their respective forms are evaluated (eg. ann).",
   :var-type "var",
   :line 1762,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:name "*verbose-forms*",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2276",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/*verbose-forms*",
   :doc
   "If true, print complete forms in error messages. Bind\naround a type checking form like cf or check-ns.\n\neg.\n(binding [*verbose-forms* true]\n  (cf ['deep ['deep ['deep ['deep]]]] Number))\n;=> <full form in error>",
   :var-type "var",
   :line 2276,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:name "*verbose-types*",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2263",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/*verbose-types*",
   :doc
   "If true, print fully qualified types in error messages\nand return values. Bind around a type checking form like \ncf or check-ns.\n\neg. \n(binding [*verbose-types* true] \n  (cf 1 Number))\n;=> java.lang.Number",
   :var-type "var",
   :line 2263,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Agent1",
   :namespace "clojure.core.typed",
   :forms [(Agent1 t)],
   :var-type "type alias",
   :doc
   "An agent that can read and write type x.\n\n(TFn [[x :variance :invariant]] (clojure.lang.Agent x x))",
   :name "Agent1"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Agent2",
   :namespace "clojure.core.typed",
   :forms [(Agent2 t t)],
   :var-type "type alias",
   :doc
   "An agent that can write type w and read type r.\n\n(TFn\n [[w :variance :contravariant] [r :variance :covariant]]\n (clojure.lang.Agent w r))",
   :name "Agent2"}
  {:forms [(All binder type)],
   :name "All",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1483",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/All",
   :doc "A polymorphic binder",
   :var-type "type alias",
   :line 1483,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:forms [Any],
   :name "Any",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1365",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Any",
   :doc "Any is the top type that contains all types.",
   :var-type "type alias",
   :line 1365,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/AnyInteger",
   :namespace "clojure.core.typed",
   :forms [AnyInteger],
   :var-type "type alias",
   :doc
   "A type that returns true for clojure.core/integer?\n\n(U Integer Long clojure.lang.BigInt BigInteger Short Byte)",
   :name "AnyInteger"}
  {:forms [(Assoc type type-pairs*)],
   :name "Assoc",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1462",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Assoc",
   :doc "A type representing an assoc operation",
   :var-type "type alias",
   :line 1462,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Atom1",
   :namespace "clojure.core.typed",
   :forms [(Atom1 t)],
   :var-type "type alias",
   :doc
   "An atom that can read and write type x.\n\n(TFn [[x :variance :invariant]] (clojure.lang.Atom x x))",
   :name "Atom1"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Atom2",
   :namespace "clojure.core.typed",
   :forms [(Atom2 t)],
   :var-type "type alias",
   :doc
   "An atom that can write type w and read type r.\n\n(TFn\n [[w :variance :contravariant] [r :variance :covariant]]\n (clojure.lang.Atom w r))",
   :name "Atom2"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/BlockingDeref",
   :namespace "clojure.core.typed",
   :forms [(BlockingDeref t)],
   :var-type "type alias",
   :doc
   "A Clojure blocking derefable (see clojure.core/deref).\n\n(TFn [[x :variance :covariant]] (clojure.lang.IBlockingDeref x))",
   :name "BlockingDeref"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Coll",
   :namespace "clojure.core.typed",
   :forms [(Coll t)],
   :var-type "type alias",
   :doc
   "A persistent collection with member type x.\n\n(TFn [[x :variance :covariant]] (clojure.lang.IPersistentCollection x))",
   :name "Coll"}
  {:forms [(CountRange Integer) (CountRange Integer Integer)],
   :name "CountRange",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1392",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/CountRange",
   :doc "A type representing a range of counts for a collection",
   :var-type "type alias",
   :line 1392,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Delay",
   :namespace "clojure.core.typed",
   :forms [(Delay t)],
   :var-type "type alias",
   :doc
   "A Clojure delay (see clojure.core/{delay,force}).\n\n(TFn [[x :variance :covariant]] (clojure.lang.Delay x))",
   :name "Delay"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Deref",
   :namespace "clojure.core.typed",
   :forms [(Deref t)],
   :var-type "type alias",
   :doc
   "A Clojure derefable (see clojure.core/deref).\n\n(TFn [[x :variance :covariant]] (clojure.lang.IDeref x))",
   :name "Deref"}
  {:forms [(Difference type type type*)],
   :name "Difference",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1403",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Difference",
   :doc
   "Difference represents a difference of types.\n\n(Difference t s) is the same as type t with type s removed.\n\neg. (Difference (U Int Long) Int) => Long\n    (Difference (U Num nil) nil)  => Num\n",
   :var-type "type alias",
   :line 1403,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Dissoc type type*)],
   :name "Dissoc",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1467",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Dissoc",
   :doc "A type representing a dissoc operation",
   :var-type "type alias",
   :line 1467,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/EmptyCount",
   :namespace "clojure.core.typed",
   :forms [EmptyCount],
   :var-type "type alias",
   :doc
   "The type of all things with count 0. Use as part of an intersection.\neg. See EmptySeqable.\n\n(ExactCount 0)",
   :name "EmptyCount"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/EmptySeqable",
   :namespace "clojure.core.typed",
   :forms [(EmptySeqable t)],
   :var-type "type alias",
   :doc
   "A type that can be used to create a sequence of member type x\nwith count 0.\n\n(TFn\n [[x :variance :covariant]]\n (I (clojure.lang.Seqable x) (ExactCount 0)))",
   :name "EmptySeqable"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ExInfo",
   :namespace "clojure.core.typed",
   :forms [ExInfo],
   :var-type "type alias",
   :doc
   "A Clojure custom exception type.\n\n(I clojure.lang.IExceptionInfo RuntimeException)",
   :name "ExInfo"}
  {:forms [(ExactCount Integer)],
   :name "ExactCount",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1398",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ExactCount",
   :doc "A type representing a precise count for a collection",
   :var-type "type alias",
   :line 1398,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:forms
   [(FnCase ArityVec+)
    [fixed*
     ->
     ret
     :filters
     {:then fl, :else fl}
     :object
     {:id Foo, :path Bar}]
    [fixed*
     rest
     *
     ->
     ret
     :filters
     {:then fl, :else fl}
     :object
     {:id Foo, :path Bar}]
    [fixed*
     drest
     ...
     bound
     ->
     ret
     :filters
     {:then fl, :else fl}
     :object
     {:id Foo, :path Bar}]],
   :name "FnCase",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1449",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/FnCase",
   :doc "An ordered intersection type of function arities.",
   :var-type "type alias",
   :line 1449,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Future",
   :namespace "clojure.core.typed",
   :forms [(Future t)],
   :var-type "type alias",
   :doc
   "A Clojure future (see clojure.core/{future-call,future}).\n\n(TFn\n [[x :variance :covariant]]\n (Extends\n  [(clojure.lang.IDeref x)\n   (clojure.lang.IBlockingDeref x)\n   clojure.lang.IPending\n   java.util.concurrent.Future]))",
   :name "Future"}
  {:forms [(Get type type) (Get type type type)],
   :name "Get",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1472",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Get",
   :doc "A type representing a get operation",
   :var-type "type alias",
   :line 1472,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:forms
   [(HMap
     :mandatory
     {Constant Type*}
     :optional
     {Constant Type*}
     :absent-keys
     #{Constant*}
     :complete?
     Boolean)
    '{Constant Type*}],
   :name "HMap",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1426",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HMap",
   :doc "HMap is a type for heterogeneous maps.",
   :var-type "type alias",
   :line 1426,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:forms
   [(HSeq [fixed*] :filter-sets [FS*] :objects [obj*])
    (HSeq [fixed* rest *] :filter-sets [FS*] :objects [obj*])
    (HSeq
     [fixed* drest ... bound]
     :filter-sets
     [FS*]
     :objects
     [obj*])],
   :name "HSeq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1442",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HSeq",
   :doc "HSeq is a type for heterogeneous seqs",
   :var-type "type alias",
   :line 1442,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:forms
   [(HSequential [fixed*] :filter-sets [FS*] :objects [obj*])
    (HSequential [fixed* rest *] :filter-sets [FS*] :objects [obj*])
    (HSequential
     [fixed* drest ... bound]
     :filter-sets
     [FS*]
     :objects
     [obj*])],
   :name "HSequential",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1435",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HSequential",
   :doc
   "HSequential is a type for heterogeneous sequential collections",
   :var-type "type alias",
   :line 1435,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:forms
   [(HVec [fixed*] :filter-sets [FS*] :objects [obj*])
    (HVec [fixed* type *] :filter-sets [FS*] :objects [obj*])
    (HVec [fixed* type ... bound] :filter-sets [FS*] :objects [obj*])
    '[fixed*]
    '[fixed* type *]
    '[fixed* type ... bound]],
   :name "HVec",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1414",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HVec",
   :doc
   "HVec is a type for heterogeneous vectors.\nIt extends clojure.core.typed/Vec and is a subtype\nof clojure.core.typed/HSequential.",
   :var-type "type alias",
   :line 1414,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Hierarchy",
   :namespace "clojure.core.typed",
   :forms [Hierarchy],
   :var-type "type alias",
   :doc
   "A hierarchy for use with derive, isa? etc.\n\n'{:parents (clojure.lang.IPersistentMap Any Any),\n  :ancestors (clojure.lang.IPersistentMap Any Any),\n  :descendants (clojure.lang.IPersistentMap Any Any)}",
   :name "Hierarchy"}
  {:forms [(I type*)],
   :name "I",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1381",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/I",
   :doc "I represents an intersection of types",
   :var-type "type alias",
   :line 1381,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Id",
   :namespace "clojure.core.typed",
   :forms [Id],
   :var-type "type alias",
   :doc
   "The identity function at the type level.\n\n(TFn [[x :variance :covariant]] x)",
   :name "Id"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Int",
   :namespace "clojure.core.typed",
   :forms [Int],
   :var-type "type alias",
   :doc
   "A type that returns true for clojure.core/integer?\n\n(U Integer Long clojure.lang.BigInt BigInteger Short Byte)",
   :name "Int"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Keyword",
   :namespace "clojure.core.typed",
   :forms [Keyword],
   :var-type "type alias",
   :doc "A keyword\n\nclojure.lang.Keyword",
   :name "Keyword"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Kw",
   :namespace "clojure.core.typed",
   :forms [Kw],
   :var-type "type alias",
   :doc "A keyword\n\nclojure.lang.Keyword",
   :name "Kw"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/List",
   :namespace "clojure.core.typed",
   :forms [(List t)],
   :var-type "type alias",
   :doc
   "A Clojure persistent list.\n\n(TFn [[x :variance :covariant]] (clojure.lang.IPersistentList x))",
   :name "List"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Map",
   :namespace "clojure.core.typed",
   :forms [(Map t t)],
   :var-type "type alias",
   :doc
   "A persistent map with keys k and vals v.\n\n(TFn\n [[k :variance :covariant] [v :variance :covariant]]\n (clojure.lang.IPersistentMap k v))",
   :name "Map"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Multi",
   :namespace "clojure.core.typed",
   :forms [Multi],
   :var-type "type alias",
   :doc "A Clojure multimethod.\n\nclojure.lang.MultiFn",
   :name "Multi"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Namespace",
   :namespace "clojure.core.typed",
   :forms [Namespace],
   :var-type "type alias",
   :doc "A namespace\n\nclojure.lang.Namespace",
   :name "Namespace"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Nilable",
   :namespace "clojure.core.typed",
   :forms [(Nilable t)],
   :var-type "type alias",
   :doc
   "A union of x and nil.\n\n(TFn [[x :variance :covariant]] (U nil x))",
   :name "Nilable"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NilableNonEmptySeq",
   :namespace "clojure.core.typed",
   :forms [(NilableNonEmptySeq t)],
   :var-type "type alias",
   :doc
   "A persistent sequence of member type x with count greater than 0, or nil.\n\n(TFn\n [[x :variance :covariant]]\n (U nil (I (clojure.lang.ISeq x) (CountRange 1))))",
   :name "NilableNonEmptySeq"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyColl",
   :namespace "clojure.core.typed",
   :forms [(NonEmptyColl t)],
   :var-type "type alias",
   :doc
   "A persistent collection with member type x and count greater than 0.\n\n(TFn\n [[x :variance :covariant]]\n (I (clojure.lang.IPersistentCollection x) (CountRange 1)))",
   :name "NonEmptyColl"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyCount",
   :namespace "clojure.core.typed",
   :forms [NonEmptyCount],
   :var-type "type alias",
   :doc
   "The type of all things with count greater than 0. Use as part of an intersection.\neg. See NonEmptySeq\n\n(CountRange 1)",
   :name "NonEmptyCount"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyLazySeq",
   :namespace "clojure.core.typed",
   :forms [(NonEmptyLazySeq t)],
   :var-type "type alias",
   :doc
   "A non-empty lazy sequence of type t\n\n(TFn\n [[t :variance :covariant]]\n (I (clojure.lang.LazySeq t) (CountRange 1)))",
   :name "NonEmptyLazySeq"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptySeq",
   :namespace "clojure.core.typed",
   :forms [(NonEmptySeq t)],
   :var-type "type alias",
   :doc
   "A persistent sequence of member type x with count greater than 0.\n\n(TFn\n [[x :variance :covariant]]\n (I (clojure.lang.ISeq x) (CountRange 1)))",
   :name "NonEmptySeq"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptySeqable",
   :namespace "clojure.core.typed",
   :forms [(NonEmptySeqable t)],
   :var-type "type alias",
   :doc
   "A type that can be used to create a sequence of member type x\nwith count greater than 0.\n\n(TFn\n [[x :variance :covariant]]\n (I (clojure.lang.Seqable x) (CountRange 1)))",
   :name "NonEmptySeqable"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyVec",
   :namespace "clojure.core.typed",
   :forms [(NonEmptyVec t)],
   :var-type "type alias",
   :doc
   "A persistent vector with member type x and count greater than 0.\n\n(TFn\n [[x :variance :covariant]]\n (I (clojure.lang.IPersistentVector x) (CountRange 1)))",
   :name "NonEmptyVec"}
  {:forms [Nothing],
   :name "Nothing",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1375",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Nothing",
   :doc
   "Nothing is the bottom type that inhabits no types\nexcept itself.",
   :var-type "type alias",
   :line 1375,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Num",
   :namespace "clojure.core.typed",
   :forms [Num],
   :var-type "type alias",
   :doc "A type that returns true for clojure.core/number?\n\nNumber",
   :name "Num"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Option",
   :namespace "clojure.core.typed",
   :forms [(Option t)],
   :var-type "type alias",
   :doc
   "A union of x and nil.\n\n(TFn [[x :variance :covariant]] (U nil x))",
   :name "Option"}
  {:forms [(Pred type)],
   :name "Pred",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1457",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Pred",
   :doc "A predicate for the given type.",
   :var-type "type alias",
   :line 1457,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Promise",
   :namespace "clojure.core.typed",
   :forms [(Promise t)],
   :var-type "type alias",
   :doc
   "A Clojure promise (see clojure.core/{promise,deliver}).\n\n(TFn\n [[x :variance :invariant]]\n (Rec\n  [p]\n  (I\n   (Extends\n    [(clojure.lang.IDeref x)\n     (clojure.lang.IBlockingDeref x)\n     clojure.lang.IPending])\n   [x -> (U nil p)])))",
   :name "Promise"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Proxy",
   :namespace "clojure.core.typed",
   :forms [Proxy],
   :var-type "type alias",
   :doc "A Clojure proxy.\n\nclojure.lang.IProxy",
   :name "Proxy"}
  {:forms [(Rec binder type)],
   :name "Rec",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1478",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Rec",
   :doc "A recursive type",
   :var-type "type alias",
   :line 1478,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Ref1",
   :namespace "clojure.core.typed",
   :forms [(Ref1 t)],
   :var-type "type alias",
   :doc
   "A ref that can read and write type x.\n\n(TFn [[x :variance :invariant]] (clojure.lang.Ref x x))",
   :name "Ref1"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Ref2",
   :namespace "clojure.core.typed",
   :forms [(Ref2 w r)],
   :var-type "type alias",
   :doc
   "A ref that can write type w and read type r.\n\n(TFn\n [[w :variance :contravariant] [r :variance :covariant]]\n (clojure.lang.Ref w r))",
   :name "Ref2"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Reversible",
   :namespace "clojure.core.typed",
   :forms [(Reversible t)],
   :var-type "type alias",
   :doc
   "A Clojure reversible collection.\n\n(TFn [[x :variance :covariant]] (clojure.lang.Reversible x))",
   :name "Reversible"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Seq",
   :namespace "clojure.core.typed",
   :forms [(Seq t)],
   :var-type "type alias",
   :doc
   "A persistent sequence of member type x.\n\n(TFn [[x :variance :covariant]] (clojure.lang.ISeq x))",
   :name "Seq"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Seqable",
   :namespace "clojure.core.typed",
   :forms [(Seqable t)],
   :var-type "type alias",
   :doc
   "A type that can be used to create a sequence of member type x.\n\n(TFn [[x :variance :covariant]] (clojure.lang.Seqable x))",
   :name "Seqable"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Set",
   :namespace "clojure.core.typed",
   :forms [(Set t)],
   :var-type "type alias",
   :doc
   "A persistent set with member type x\n\n(TFn [[x :variance :covariant]] (clojure.lang.IPersistentSet x))",
   :name "Set"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/SortedSet",
   :namespace "clojure.core.typed",
   :forms [(SortedSet t)],
   :var-type "type alias",
   :doc
   "A sorted persistent set with member type x\n\n(TFn\n [[x :variance :covariant]]\n (Extends [(clojure.lang.IPersistentSet x) clojure.lang.Sorted]))",
   :name "SortedSet"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Stack",
   :namespace "clojure.core.typed",
   :forms [(Stack t)],
   :var-type "type alias",
   :doc
   "A Clojure stack.\n\n(TFn [[x :variance :covariant]] (clojure.lang.IPersistentStack x))",
   :name "Stack"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Str",
   :namespace "clojure.core.typed",
   :forms [Str],
   :var-type "type alias",
   :doc "A string\n\njava.lang.String",
   :name "Str"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Sym",
   :namespace "clojure.core.typed",
   :forms [Sym],
   :var-type "type alias",
   :doc "A symbol\n\nclojure.lang.Symbol",
   :name "Sym"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Symbol",
   :namespace "clojure.core.typed",
   :forms [Symbol],
   :var-type "type alias",
   :doc "A symbol\n\nclojure.lang.Symbol",
   :name "Symbol"}
  {:forms [(TFn binder type)],
   :name "TFn",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1488",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/TFn",
   :doc "A type function",
   :var-type "type alias",
   :line 1488,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:forms [(U type*)],
   :name "U",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1370",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/U",
   :doc "U represents a union of types",
   :var-type "type alias",
   :line 1370,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Value Constant) 'Constant],
   :name "Value",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1386",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Value",
   :doc "A singleton type for a constant value.",
   :var-type "type alias",
   :line 1386,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Var1",
   :namespace "clojure.core.typed",
   :forms [(Var1 t)],
   :var-type "type alias",
   :doc
   "An var that can read and write type x.\n\n(TFn [[x :variance :invariant]] (clojure.lang.Var x x))",
   :name "Var1"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Var2",
   :namespace "clojure.core.typed",
   :forms [(Var2 w r)],
   :var-type "type alias",
   :doc
   "An var that can write type w and read type r.\n\n(TFn\n [[w :variance :contravariant] [r :variance :covariant]]\n (clojure.lang.Var w r))",
   :name "Var2"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Vec",
   :namespace "clojure.core.typed",
   :forms [(Vec t)],
   :var-type "type alias",
   :doc
   "A persistent vector with member type x.\n\n(TFn [[x :variance :covariant]] (clojure.lang.IPersistentVector x))",
   :name "Vec"}
  {:arglists ([varsym typesyn]),
   :name "ann",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1714",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann",
   :doc
   "Annotate varsym with type. If unqualified, qualify in the current namespace.\nIf varsym has metadata {:no-check true}, ignore definitions of varsym \nwhile type checking.\n\nIf annotating vars in namespaces other than the current one, a fully\nqualified symbol must be provided. Note that namespace aliases are not\nrecognised: the *full* namespace must be given in the first part of the symbol.\n\neg. ; annotate the var foo in this namespace\n    (ann foo [Number -> Number])\n\n    ; annotate a var in another namespace\n    (ann another.ns/bar [-> nil])\n \n    ; don't check this var\n    (ann ^:no-check foobar [Integer -> String])",
   :var-type "macro",
   :line 1714,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :forms
   [(ann-datatype dname [field :- type*] opts*)
    (ann-datatype binder dname [field :- type*] opts*)],
   :name "ann-datatype",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1776",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-datatype",
   :doc
   "Annotate datatype Class name dname with expected fields.\nIf unqualified, qualify in the current namespace.\nTakes an optional type variable binder before the name.\n\nFields must be specified in the same order as presented \nin deftype, with exactly the same field names.\n\nAlso annotates datatype factories and constructors.\n\nBinder is a vector of specs. Each spec is a vector\nwith the variable name as the first entry, followed by\nkeyword arguments:\n- :variance (mandatory)\n  The declared variance of the type variable. Possible\n  values are :covariant, :contravariant and :invariant.\n- :< (optional)\n  The upper type bound of the type variable. Defaults to\n  Any, or the most general type of the same rank as the\n  lower bound.\n- :> (optional)\n  The lower type bound of the type variable. Defaults to\n  Nothing, or the least general type of the same rank as the\n  upper bound.\n\neg. ; a datatype in the current namespace\n    (ann-datatype MyDatatype [a :- Number,\n                              b :- Long])\n\n    ; a datatype in another namespace\n    (ann-datatype another.ns.TheirDatatype\n                  [str :- String,\n                   vec :- (Vec Number)])\n\n    ; a datatype, polymorphic in a\n    (ann-datatype [[a :variance :covariant]]\n                  MyPolyDatatype\n                  [str :- String,\n                   vec :- (Vec Number)\n                   ply :- (Set a)])",
   :var-type "macro",
   :line 1776,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([form ty]),
   :name "ann-form",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L123",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-form",
   :doc "Annotate a form with an expected type.",
   :var-type "macro",
   :line 123,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :forms
   [(ann-interface vbnd varsym & methods)
    (ann-interface varsym & methods)],
   :name "ann-interface",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2008",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-interface",
   :doc
   "Annotate a possibly polymorphic interface (created with definterface) with method types.\n\nNote: Unlike ann-protocol, omit the target ('this') argument in the method signatures.\n\neg. (ann-interface IFoo\n      bar\n      (Fn [-> Any]\n          [Number Symbol -> Any])\n      baz\n      [Number -> Number])\n    (definterface IFoo\n      (bar [] [n s])\n      (baz [n]))\n\n    ; polymorphic protocol\n    ; x is scoped in the methods\n    (ann-protocol [[x :variance :covariant]]\n      IFooPoly\n      bar\n      (Fn [-> Any]\n          [Number Symbol -> Any])\n      baz\n      [Number -> Number])\n    (definterface IFooPoly\n      (bar [] [n s])\n      (baz [n]))",
   :var-type "macro",
   :line 2008,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([t & vs]),
   :name "ann-many",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1754",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-many",
   :doc
   "Annotate several vars with type t.\n\neg. (ann-many FakeSearch\n              web1 web2 image1 image2 video1 video2)",
   :var-type "macro",
   :line 1754,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists
   ([dname
     vbnd
     fields
     &
     {ancests :unchecked-ancestors, rplc :replace, :as opt}]),
   :name "ann-precord",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1935",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-precord",
   :doc
   "Annotate record Class name dname with a polymorphic binder and expected fields.\nIf unqualified, qualify in the current namespace.",
   :var-type "macro",
   :line 1935,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :forms
   [(ann-protocol vbnd varsym & methods)
    (ann-protocol varsym & methods)],
   :name "ann-protocol",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1949",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-protocol",
   :doc
   "Annotate a possibly polymorphic protocol var with method types.\n\neg. (ann-protocol IFoo\n      bar\n      (Fn [IFoo -> Any]\n          [IFoo Number Symbol -> Any])\n      baz\n      [IFoo Number -> Number])\n    (defprotocol> IFoo\n      (bar [this] [this n s])\n      (baz [this n]))\n\n    ; polymorphic protocol\n    ; x is scoped in the methods\n    (ann-protocol [[x :variance :covariant]]\n      IFooPoly\n      bar\n      (Fn [(IFooPoly x) -> Any]\n          [(IFooPoly x) Number Symbol -> Any])\n      baz\n      [(IFooPoly x) Number -> Number])\n    (defprotocol> IFooPoly\n      (bar [this] [this n s])\n      (baz [this n]))",
   :var-type "macro",
   :line 1949,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :forms
   [(ann-record dname [field :- type*] opts*)
    (ann-record binder dname [field :- type*] opts*)],
   :name "ann-record",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1864",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-record",
   :doc
   "Annotate record Class name dname with expected fields.\nIf unqualified, qualify in the current namespace.\nTakes an optional type variable binder before the name.\n\nFields must be specified in the same order as presented \nin defrecord, with exactly the same field names.\n\nAlso annotates record factories and constructors.\n\nBinder is a vector of specs. Each spec is a vector\nwith the variable name as the first entry, followed by\nkeyword arguments:\n- :variance (mandatory)\n  The declared variance of the type variable. Possible\n  values are :covariant, :contravariant and :invariant.\n- :< (optional)\n  The upper type bound of the type variable. Defaults to\n  Any, or the most general type of the same rank as the\n  lower bound.\n- :> (optional)\n  The lower type bound of the type variable. Defaults to\n  Nothing, or the least general type of the same rank as the\n  upper bound.\n\neg. ; a record in the current namespace\n    (ann-record MyRecord [a :- Number,\n                          b :- Long])\n\n    ; a record in another namespace\n    (ann-record another.ns.TheirRecord\n                  [str :- String,\n                   vec :- (Vec Number)])\n\n    ; a record, polymorphic in a\n    (ann-record [[a :variance :covariant]]\n                MyPolyRecord\n                [str :- String,\n                 vec :- (Vec Number)\n                 ply :- (Set a)])",
   :var-type "macro",
   :line 1864,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([t init & args]),
   :name "atom>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2124",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/atom>",
   :doc
   "Like atom, but creates an Atom1 of type t.\n\nSame as (atom (ann-form init t) args*)\n\neg. (atom> Number 1)\n    (atom> (Vec Any) [])",
   :var-type "macro",
   :line 2124,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([form] [form expected]),
   :name "cf",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2196",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/cf",
   :doc
   "Takes a form and an optional expected type and\nreturns a human-readable inferred type for that form.\nThrows an exception if type checking fails.\n\nDo not use cf inside a typed namespace. cf is intended to be\nused at the REPL or within a unit test. Note that testing for\ntruthiness is not sufficient to unit test a call to cf, as nil\nand false are valid type syntax.\n\ncf preserves annotations from previous calls to check-ns or cf,\nand keeps any new ones collected during a cf. This is useful for\ndebugging and experimentation. cf may be less strict than check-ns\nwith type checker warnings.\n\neg. (cf 1) \n    ;=> Long\n\n    (cf #(inc %) [Number -> Number])\n    ;=> [Number -> Number]",
   :var-type "macro",
   :line 2196,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([form] [form expected] [form expected type-provided?]),
   :name "check-form*",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2176",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-form*",
   :doc
   "Takes a (quoted) form and optional expected type syntax and\ntype checks the form. If expected is provided, type-provided?\nmust be true.",
   :var-type "function",
   :line 2176,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists
   ([form & {:keys [expected type-provided? profile file-mapping]}]),
   :name "check-form-info",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2334",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-form-info",
   :doc
   "Type checks a (quoted) form and returns a map of results from type checking the\nform.\n\nOptions\n- :expected        Type syntax representing the expected type for this form\n                   type-provided? option must be true to utilise the type.\n- :type-provided?  If true, use the expected type to check the form\n- :profile         Use Timbre to profile the type checker. Timbre must be\n                   added as a dependency.\n- :file-mapping    If true, return map provides entry :file-mapping, a hash-map\n                   of (Map '{:line Int :column Int :file Str} Str).",
   :var-type "function",
   :line 2334,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists
   ([] [ns-or-syms & {:keys [collect-only trace profile], :as kw}]),
   :name "check-ns",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2489",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-ns",
   :doc
   "Type check a namespace/s (a symbol or Namespace, or collection).\nIf not provided default to current namespace.\nReturns a true value if type checking is successful, otherwise\nthrows an Exception.\n\nDo not use check-ns within a checked namespace.\nIt is intended to be used at the REPL or within a unit test.\nSuggested idiom for clojure.test: (is (check-ns 'your.ns))\n\ncheck-ns resets annotations collected from \nprevious check-ns calls or cf. A successful check-ns call will\npreserve any type annotations collect during that checking run.\n\nKeyword arguments:\n- :collect-only  if true, collect type annotations but don't type check code.\n                 Useful for debugging purposes.\n- :trace         if true, print some basic tracing of the type checker\n- :profile       if true, use Timbre to profile type checking. Must include\n                 Timbre as a dependency.\n\nIf providing keyword arguments, the namespace to check must be provided\nas the first argument.\n\nBind *verbose-types* to true to print fully qualified types.\nBind *verbose-forms* to print full forms in error messages.\n\neg. (check-ns 'myns.typed)\n    ;=> :ok\n   \n    ; implicitly check current namespace\n    (check-ns)\n    ;=> :ok\n\n    ; collect but don't check the current namespace\n    (check-ns *ns* :collect-only true)",
   :var-type "function",
   :line 2489,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists
   ([]
    [ns-or-syms & {:keys [collect-only trace profile file-mapping]}]),
   :name "check-ns-info",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2379",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-ns-info",
   :doc
   "Same as check-ns, but returns a map of results from type checking the\nnamespace.\n\nOptions\n- :collect-only    Don't type check the given namespace/s, but collect the \n                   top level type annotations like ann, ann-record.\n- :type-provided?  If true, use the expected type to check the form\n- :profile         Use Timbre to profile the type checker. Timbre must be\n                   added as a dependency.\n- :file-mapping    If true, return map provides entry :file-mapping, a hash-map\n                   of (Map '{:line Int :column Int :file Str} Str).",
   :var-type "function",
   :line 2379,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([sym ty]),
   :name "declare-alias-kind",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1267",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-alias-kind",
   :doc
   "Declare a kind for an alias, similar to declare but on the kind level.",
   :var-type "macro",
   :line 1267,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& syms]),
   :name "declare-datatypes",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1245",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-datatypes",
   :doc "Declare datatypes, similar to declare but on the type level.",
   :var-type "macro",
   :line 1245,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& syms]),
   :name "declare-names",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1280",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-names",
   :doc "Declare names, similar to declare but on the type level.",
   :var-type "macro",
   :line 1280,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& syms]),
   :name "declare-protocols",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1256",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-protocols",
   :doc "Declare protocols, similar to declare but on the type level.",
   :var-type "macro",
   :line 1256,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([name & fdecl]),
   :forms [(def name docstring? :- type? expr)],
   :name "def",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L19",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/def",
   :doc
   "Like clojure.core/def with optional type annotations\n\nNB: it is impossible to refer a var called `def` as it is a\nspecial form. Use an alias prefix (eg. `t/def`).\n\nIf an annotation is provided, a corresponding `ann` form\nis generated, otherwise it expands identically to clojure.core/def\n\neg. ;same as clojure.core/def\n    (def vname 1)\n    \n    ;with Number `ann`\n    (def vname :- Number 1)\n\n    ;doc\n    (def vname\n      \"Docstring\"\n      :- Long\n      1)",
   :var-type "macro",
   :line 19,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([sym doc-str t] [sym t]),
   :name "def-alias",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1303",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/def-alias",
   :doc
   "DEPRECATED: use defalias\n\nDefine a type alias. Takes an optional doc-string as a second\nargument.\n\nUpdates the corresponding var with documentation.\n\neg. (def-alias MyAlias\n      \"Here is my alias\"\n      (U nil String))",
   :var-type "macro",
   :line 1303,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([name & fdecl]),
   :forms [(def> name docstring? :- type expr)],
   :name "def>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L839",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/def>",
   :doc
   "DEPRECATED: use clojure.core.typed/def\n\nLike def, but with annotations.\n\neg. (def> vname :- Long 1)\n\n;doc\n(def> vname\n  \"Docstring\"\n  :- Long\n  1)",
   :var-type "macro",
   :line 839,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([sym doc-str t] [sym t]),
   :name "defalias",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1332",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defalias",
   :doc
   "Define a type alias. Takes an optional doc-string as a second\nargument.\n\nUpdates the corresponding var with documentation.\n\neg. (defalias MyAlias\n      \"Here is my alias\"\n      (U nil String))",
   :var-type "macro",
   :line 1332,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([name & fdecl]),
   :forms
   [(defn> name docstring? :- type [param :- type *] exprs*)
    (defn> name docstring? (:- type [param :- type *] exprs*) +)],
   :name "defn>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L814",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defn>",
   :doc
   "Like defn, but with annotations. Annotations are mandatory for\nparameters and for return type.\n\neg. (defn> fname :- Integer [a :- Number, b :- (U Symbol nil)] ...)\n\n;annotate return\n(defn> fname :- String [a :- String] ...)\n\n;multi-arity\n(defn> fname \n  (:- String [a :- String] ...)\n  (:- Long   [a :- String, b :- Number] ...))",
   :var-type "macro",
   :line 814,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([seq-exprs & body]),
   :name "doseq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L652",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/doseq",
   :doc
   "Like clojure.core/doseq with optional annotations.\n\n:let option uses clojure.core.typed/let\n\neg.\n(doseq [a :- (U nil AnyInteger) [1 nil 2 3]\n        :when a]\n   (inc a))",
   :var-type "macro",
   :line 652,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([seq-exprs & body]),
   :name "doseq>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L553",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/doseq>",
   :doc
   "DEPRECATED: use clojure.core.typed/doseq\n\nLike doseq but requires annotation for each loop variable: \n[a [1 2]] becomes [a :- Long [1 2]]\n\neg.\n(doseq> [a :- (U nil AnyInteger) [1 nil 2 3]\n         :when a]\n   (inc a))",
   :var-type "macro",
   :line 553,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bindings & body]),
   :name "dotimes",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L263",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/dotimes",
   :doc
   "Like clojure.core/dotimes, but with optional annotations.\n\nIf annotation for binding is omitted, defaults to Int.\n\neg. (dotimes [_ 100]\n      (println \"like normal\"))\n\n    (dotimes [x :- Number, 100.123]\n      (println \"like normal\" x))",
   :var-type "macro",
   :line 263,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bindings & body]),
   :name "dotimes>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L244",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/dotimes>",
   :doc
   "DEPRECATED: Use clojure.core.typed/dotimes\n\nLike dotimes.\n\neg. (dotimes> [_ 100]\n      (println \"like normal\"))",
   :var-type "macro",
   :line 244,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& forms]),
   :forms
   [(fn name? [param :- type* & param :- type * ?] :- type? exprs*)
    (fn
     name?
     ([param :- type* & param :- type * ?] :- type? exprs*)
     +)],
   :name "fn",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L60",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/fn",
   :doc
   "Like clojure.core/fn, but with optional annotations.\n\neg. ;these forms are equivalent\n    (fn [a] b)\n    (fn [a :- Any] b)\n    (fn [a :- Any] :- Any b)\n    (fn [a] :- Any b)\n\n    ;annotate return\n    (fn [a :- String] :- String body)\n\n    ;named fn\n    (fn fname [a :- String] :- String body)\n\n    ;rest parameter\n    (fn [a :- String & b :- Number *] body)\n\n    ;dotted rest parameter\n    (fn [a :- String & b :- Number ... x] body)\n\n    ;multi-arity\n    (fn fname \n      ([a :- String] :- String ...)\n      ([a :- String, b :- Number] :- String ...))",
   :var-type "macro",
   :line 60,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& forms]),
   :forms
   [(fn> name? :- type? [param :- type* & param :- type * ?] exprs*)
    (fn>
     name?
     (:- type? [param :- type* & param :- type * ?] exprs*)
     +)],
   :name "fn>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L775",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/fn>",
   :doc
   "DEPRECATED: use clojure.core.typed/fn\n\nLike fn, but with annotations. Annotations are mandatory\nfor parameters, with optional annotations for return type.\nIf fn is named, return type annotation is mandatory.\n\nSuggested idiom: use commas between parameter annotation triples.\n\neg. (fn> [a :- Number, b :- (U Symbol nil)] ...)\n\n    ;annotate return\n    (fn> :- String [a :- String] ...)\n\n    ;named fn\n    (fn> fname :- String [a :- String] ...)\n\n    ;multi-arity\n    (fn> fname \n      (:- String [a :- String] ...)\n      (:- Long   [a :- String, b :- Number] ...))",
   :var-type "macro",
   :line 775,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([seq-exprs & maybe-ann-body-expr]),
   :name "for",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L412",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/for",
   :doc
   "Like clojure.core/for with optional type annotations.\n\nAll types default to Any.\n\nThe :let option uses clojure.core.typed/let.\n\neg. (for [a :- (U nil Int) [1 nil 2 3]\n          :when a]\n      :- Number\n      (inc a))",
   :var-type "macro",
   :line 412,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([tk ret-ann seq-exprs body-expr]),
   :name "for>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L285",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/for>",
   :doc
   "DEPRECATED: use clojure.core.typed/for\n\nLike for but requires annotation for each loop variable: [a [1 2]] becomes [a :- Long [1 2]]\nAlso requires annotation for return type.\n\neg. (for> :- Number\n      [a :- (U nil AnyInteger) [1 nil 2 3]\n       :when a]\n      (inc a))",
   :var-type "macro",
   :line 285,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([inst-of & types]),
   :name "inst",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L209",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/inst",
   :doc
   "Instantiate a polymorphic type with a number of types.\n\neg. (inst foo-fn t1 t2 t3 ...)",
   :var-type "macro",
   :line 209,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([inst-of & types]),
   :name "inst-ctor",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L216",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/inst-ctor",
   :doc
   "Instantiate a call to a constructor with a number of types.\nFirst argument must be an immediate call to a constructor.\nReturns exactly the instantiatee (the first argument).\n\neg. (inst-ctor (PolyCtor. a b c)\n               t1 t2 ...)",
   :var-type "macro",
   :line 216,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists
   ([cljt coll] [javat cljt coll] [into-array-syn javat cljt coll]),
   :name "into-array>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1649",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/into-array>",
   :doc
   "Make a Java array with Java class javat and Typed Clojure type\ncljt. Resulting array will be of type javat, but elements of coll must be under\ncljt. cljt should be a subtype of javat (the same or more specific).\n\n*Temporary hack*\ninto-array-syn is exactly the syntax to put as the first argument to into-array.\nCalling resolve on this syntax should give the correct class.",
   :var-type "macro",
   :line 1649,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bvec & forms]),
   :forms [(let [binding :- type? init*] exprs*)],
   :name "let",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L111",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/let",
   :doc
   "Like clojure.core/let but supports optional type annotations.\n\neg. (let [a :- Type, b\n          a2 1.2]\n      body)",
   :var-type "macro",
   :line 111,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([fn-specs-and-annotations & body]),
   :forms [(letfn> [fn-spec-or-annotation*] expr*)],
   :name "letfn>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L867",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/letfn>",
   :doc
   "Like letfn, but each function spec must be annotated.\n\neg. (letfn> [a :- [Number -> Number]\n             (a [b] 2)\n\n             c :- [Symbol -> nil]\n             (c [s] nil)]\n      ...)",
   :var-type "macro",
   :line 867,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([]),
   :name "load-if-needed",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2303",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/load-if-needed",
   :doc "Load and initialize all of core.typed if not already",
   :var-type "function",
   :line 2303,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bindings & exprs]),
   :forms [(loop [binding :- type? init*] exprs*)],
   :name "loop",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L95",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/loop",
   :doc
   "Like clojure.core/loop, and supports optional type annotations.\nArguments default to a generalised type based on the initial value.\n\neg. (loop [a :- Number 1\n           b :- (U nil Number) nil]\n      ...)",
   :var-type "macro",
   :line 95,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bndings* & forms]),
   :forms [(loop> [binding :- type init*] exprs*)],
   :name "loop>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1195",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/loop>",
   :doc
   "DEPRECATED: use clojure.core.typed/loop\n\nLike loop, except loop variables require annotation.\n\nSuggested idiom: use a comma between the type and the initial\nexpression.\n\neg. (loop> [a :- Number, 1\n            b :- (U nil Number), nil]\n      ...)",
   :var-type "macro",
   :line 1195,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([mname]),
   :name "method-type",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L162",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/method-type",
   :doc
   "Given a method symbol, print the core.typed types assigned to it.\nIntended for use at the REPL.",
   :var-type "function",
   :line 162,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([msym mmap]),
   :name "nilable-param",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1688",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/nilable-param",
   :doc
   "Override which parameters in qualified method msym may accept\nnilable values. If the parameter is a parameterised type or\nan Array, this also declares the parameterised types and the Array type as nilable.\n\nmmap is a map mapping arity parameter number to a set of parameter\npositions (integers). If the map contains the key :all then this overrides\nother entries. The key can also be :all, which declares all parameters nilable.",
   :var-type "macro",
   :line 1688,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([msym arities]),
   :name "non-nil-return",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1671",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/non-nil-return",
   :doc
   "Override the return type of fully qualified method msym to be non-nil.\nTakes a set of relevant arities,\nrepresented by the number of parameters it takes (rest parameter counts as one),\nor :all which overrides all arities.\n\neg. ; must use full class name\n    (non-nil-return java.lang.Class/getDeclaredMethod :all)",
   :var-type "macro",
   :line 1671,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([ctorsym typesyn]),
   :name "override-constructor",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2077",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/override-constructor",
   :doc "Override all constructors for Class ctorsym with type.",
   :var-type "macro",
   :line 2077,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([methodsym typesyn]),
   :name "override-method",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2088",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/override-method",
   :doc "Override type for qualified method methodsym.",
   :var-type "macro",
   :line 2088,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& forms]),
   :name "pfn>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L759",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/pfn>",
   :doc
   "Define a polymorphic typed anonymous function.\n(pfn> name? [binder+] :- type? [[param :- type]* & [param :- type *]?] exprs*)\n(pfn> name? [binder+] (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)",
   :var-type "macro",
   :line 759,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([t]),
   :name "pred",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2608",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/pred",
   :doc
   "Generate a flat (runtime) predicate for type that returns true if the\nargument is a subtype of the type, otherwise false.\n\nThe current type variable and dotted type variable scope is cleared before parsing.\n\neg. ((pred Number) 1)\n    ;=> true",
   :var-type "macro",
   :line 2608,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([debug-str]),
   :name "print-env",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1702",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/print-env",
   :doc
   "During type checking, print the type environment to *out*,\npreceeded by literal string debug-str.",
   :var-type "function",
   :line 1702,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([debug-string frm]),
   :name "print-filterset",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L187",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/print-filterset",
   :doc
   "During type checking, print the filter set attached to form, \npreceeded by literal string debug-string.\nReturns nil.\n\neg. (let [s (seq (get-a-seqable))]\n      (print-filterset \"Here now\" s))",
   :var-type "function",
   :line 187,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([t init & args]),
   :name "ref>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2134",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ref>",
   :doc
   "Like ref, but creates a Ref1 of type t.\n\nSame as (ref (ann-form init t) args*)\n\neg. (ref> Number 1)\n    (ref> (Vec Any) [])",
   :var-type "macro",
   :line 2134,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([]),
   :name "reset-caches",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2321",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/reset-caches",
   :doc "Reset internal type caches.",
   :var-type "function",
   :line 2321,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([nsyms]),
   :name "statistics",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2545",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/statistics",
   :doc
   "Takes a collection of namespace symbols and returns a map mapping the namespace\nsymbols to a map of data",
   :var-type "function",
   :line 2545,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& body]),
   :name "tc-ignore",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L1358",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/tc-ignore",
   :doc "Ignore forms in body during type checking",
   :var-type "macro",
   :line 1358,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :name "typed-deps",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2099",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/typed-deps",
   :doc
   "Declare namespaces which should be checked before the current namespace.\nAccepts any number of symbols. Only has effect via check-ns.\n\neg. (typed-deps clojure.core.typed.holes\n                myns.types)",
   :var-type "macro",
   :line 2099,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([] [nsyms-or-nsym]),
   :name "var-coverage",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2572",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/var-coverage",
   :doc
   "Summarises annotated var coverage statistics to *out*\nfor namespaces nsyms, a collection of symbols or a symbol/namespace.\nDefaults to the current namespace if no argument provided.",
   :var-type "function",
   :line 2572,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([sym]),
   :name "var>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2148",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/var>",
   :doc
   "Like var, but resolves at runtime like ns-resolve and is understood by\nthe type checker. sym must be fully qualified (without aliases).\n\neg. (var> clojure.core/+)",
   :var-type "macro",
   :line 2148,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([]),
   :name "warn-on-unannotated-vars",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L2162",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/warn-on-unannotated-vars",
   :doc
   "Allow unannotated vars in the current namespace. \n\nEmits a warning instead of a type error when checking\na def without a corresponding expected type.\n\nDisables automatic inference of `def` expressions.\n\neg. (warn-on-unannotated-vars)",
   :var-type "macro",
   :line 2162,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([b & body]),
   :name "when-let-fail",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj#L768",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/970c139380e34c84e75b327cf9e9d4074f65486c/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/when-let-fail",
   :doc
   "Like when-let, but fails if the binding yields a false value.",
   :var-type "macro",
   :line 768,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([t]),
   :name "ifn-ancestor",
   :namespace "clojure.core.typed.check.funapp",
   :source-url
   "https://github.com/clojure/core.typed/blob/92198b007bfdbddb537b13e8a7f829d4eae625fd/src/main/clojure/clojure/core/typed/check/funapp.clj#L20",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/92198b007bfdbddb537b13e8a7f829d4eae625fd/src/main/clojure/clojure/core/typed/check/funapp.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.funapp/ifn-ancestor",
   :doc
   "If this type can be treated like a function, return one of its\npossibly polymorphic function ancestors.\n\nAssumes the type is not a union",
   :var-type "function",
   :line 20,
   :file "src/main/clojure/clojure/core/typed/check/funapp.clj"}
  {:arglists ([child-ret parent-ret]),
   :name "tc-isa?",
   :namespace "clojure.core.typed.check.isa",
   :source-url
   "https://github.com/clojure/core.typed/blob/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/isa.clj#L14",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/isa.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.isa/tc-isa?",
   :doc
   "Type check a call to isa?. Assumes global hierarchy.\nAlso supports the case where both elements are vectors, but not recursively.",
   :var-type "function",
   :line 14,
   :file "src/main/clojure/clojure/core/typed/check/isa.clj"}
  {:arglists ([key-types expected]),
   :name "expected-vals",
   :namespace "clojure.core.typed.check.map",
   :source-url
   "https://github.com/clojure/core.typed/blob/92198b007bfdbddb537b13e8a7f829d4eae625fd/src/main/clojure/clojure/core/typed/check/map.clj#L6",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/92198b007bfdbddb537b13e8a7f829d4eae625fd/src/main/clojure/clojure/core/typed/check/map.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.map/expected-vals",
   :doc
   "Returns a sequence of (Nilable TCResults) to use as expected types for type\nchecking the values of a literal map expression",
   :var-type "function",
   :line 6,
   :file "src/main/clojure/clojure/core/typed/check/map.clj"}
  {:arglists ([expected]),
   :name "error-ret",
   :namespace "clojure.core.typed.check.utils",
   :source-url
   "https://github.com/clojure/core.typed/blob/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/utils.clj#L59",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/utils.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.utils/error-ret",
   :doc
   "Return a TCResult appropriate for when a type\nerror occurs, with expected type expected.\n\nUse *only* in case of a type error.",
   :var-type "function",
   :line 59,
   :file "src/main/clojure/clojure/core/typed/check/utils.clj"}
  {:arglists ([target-type expected]),
   :name "extend-method-expected",
   :namespace "clojure.core.typed.check.utils",
   :source-url
   "https://github.com/clojure/core.typed/blob/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/utils.clj#L187",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/utils.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.utils/extend-method-expected",
   :doc
   "Returns the expected type with target-type intersected with the first argument",
   :var-type "function",
   :line 187,
   :file "src/main/clojure/clojure/core/typed/check/utils.clj"}
  {:arglists ([dt nms] [dt]),
   :name "unwrap-datatype",
   :namespace "clojure.core.typed.check.utils",
   :source-url
   "https://github.com/clojure/core.typed/blob/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/utils.clj#L296",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/utils.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.utils/unwrap-datatype",
   :doc
   "Takes a DataType that might be wrapped in a TypeFn and returns the \nDataType after instantiating it",
   :var-type "function",
   :line 296,
   :file "src/main/clojure/clojure/core/typed/check/utils.clj"}
  {:arglists ([t]),
   :name "unwrap-poly",
   :namespace "clojure.core.typed.check.utils",
   :source-url
   "https://github.com/clojure/core.typed/blob/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/utils.clj#L72",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/bb656efa6372db3ffcb5a1edce5b38d9ac4aee8f/src/main/clojure/clojure/core/typed/check/utils.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.utils/unwrap-poly",
   :doc
   "Return a pair vector of the instantiated body of the possibly polymorphic\ntype and the names used",
   :var-type "function",
   :line 72,
   :file "src/main/clojure/clojure/core/typed/check/utils.clj"}
  {:arglists ([cls]),
   :name "ctor-Class->symbol",
   :namespace "clojure.core.typed.coerce-utils",
   :source-url
   "https://github.com/clojure/core.typed/blob/3aa23baeee6cc53b0ab5109b3c6c515b76abf613/src/main/clojure/clojure/core/typed/coerce_utils.clj#L38",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/3aa23baeee6cc53b0ab5109b3c6c515b76abf613/src/main/clojure/clojure/core/typed/coerce_utils.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.coerce-utils/ctor-Class->symbol",
   :doc
   "Returns a symbol representing this constructor's Class, removing any compiler stubs.",
   :var-type "function",
   :line 38,
   :file "src/main/clojure/clojure/core/typed/coerce_utils.clj"}
  {:arglists ([sym]),
   :name "symbol->Class",
   :namespace "clojure.core.typed.coerce-utils",
   :source-url
   "https://github.com/clojure/core.typed/blob/3aa23baeee6cc53b0ab5109b3c6c515b76abf613/src/main/clojure/clojure/core/typed/coerce_utils.clj#L5",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/3aa23baeee6cc53b0ab5109b3c6c515b76abf613/src/main/clojure/clojure/core/typed/coerce_utils.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.coerce-utils/symbol->Class",
   :doc
   "Returns the Class represented by the symbol. Works for\nprimitives (eg. byte, int). Does not further resolve the symbol.",
   :var-type "function",
   :line 5,
   :file "src/main/clojure/clojure/core/typed/coerce_utils.clj"}
  {:arglists ([]),
   :name "->NoisyHole",
   :namespace "clojure.core.typed.hole",
   :source-url
   "https://github.com/clojure/core.typed/blob/1e09597531e797ead73a0bbbfc570499361a8f6b/src/main/clojure/clojure/core/typed/hole.clj#L19",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/1e09597531e797ead73a0bbbfc570499361a8f6b/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/->NoisyHole",
   :doc
   "Positional factory function for class clojure.core.typed.hole.NoisyHole.",
   :var-type "function",
   :line 19,
   :file "src/main/clojure/clojure/core/typed/hole.clj"}
  {:arglists ([]),
   :name "noisy-hole",
   :namespace "clojure.core.typed.hole",
   :source-url
   "https://github.com/clojure/core.typed/blob/1e09597531e797ead73a0bbbfc570499361a8f6b/src/main/clojure/clojure/core/typed/hole.clj#L23",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/1e09597531e797ead73a0bbbfc570499361a8f6b/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/noisy-hole",
   :doc
   "A noisy hole. The type system will complain when\n(noisy-hole) is used in positions that expect a type\nmore specific than Object or Any.\nUse (noisy-hole) as a placeholder for code.\nThrows an exception when evaluted.",
   :var-type "function",
   :line 23,
   :file "src/main/clojure/clojure/core/typed/hole.clj"}
  {:arglists ([]),
   :name "silent-hole",
   :namespace "clojure.core.typed.hole",
   :source-url
   "https://github.com/clojure/core.typed/blob/1e09597531e797ead73a0bbbfc570499361a8f6b/src/main/clojure/clojure/core/typed/hole.clj#L9",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/1e09597531e797ead73a0bbbfc570499361a8f6b/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/silent-hole",
   :doc
   "A silent hole. (silent-hole) passes for any other type\nwhen type checking.\nUse (silent-hole) as a placeholder for code.\nThrows an exception when evaluted.",
   :var-type "function",
   :line 9,
   :file "src/main/clojure/clojure/core/typed/hole.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/NoisyHole",
   :namespace "clojure.core.typed.hole",
   :var-type "type",
   :name "NoisyHole"}
  {:arglists
   ([{t :t, fs :fl, old-obj :o, :keys [flow], :as r} objs & [ts]]),
   :name "open-Result",
   :namespace "clojure.core.typed.open-result",
   :source-url
   "https://github.com/clojure/core.typed/blob/92198b007bfdbddb537b13e8a7f829d4eae625fd/src/main/clojure/clojure/core/typed/open_result.clj#L32",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/92198b007bfdbddb537b13e8a7f829d4eae625fd/src/main/clojure/clojure/core/typed/open_result.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.open-result/open-Result",
   :doc "Substitute ids for objs in Result t",
   :var-type "function",
   :line 32,
   :file "src/main/clojure/clojure/core/typed/open_result.clj"})}

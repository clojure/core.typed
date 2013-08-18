{:namespaces
 ({:source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed-api.html",
   :name "clojure.core.typed",
   :doc
   "This namespace contains typed wrapper macros, type aliases\nand functions for type checking Clojure code. check-ns is the interface\nfor checking namespaces, cf for checking individual forms."}
  {:source-url
   "https://github.com/clojure/core.typed/blob/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.async-api.html",
   :name "clojure.core.typed.async",
   :doc
   "This namespace contains annotations and helper macros for type\nchecking core.async code. Ensure clojure.core.async is require'd\nbefore performing type checking.\n\ngo\n  use go>\n\nchan\n  use chan>\n\nbuffer\n  use buffer> (similar for other buffer constructors)\n"}
  {:source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.filter-rep-api.html",
   :name "clojure.core.typed.filter-rep",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/6b5231ad882ee07b0ef6c5e978e09c09aed083a6/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.hole-api.html",
   :name "clojure.core.typed.hole",
   :doc
   "This namespace contains easy tools for hole driven development"}
  {:source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.object-rep-api.html",
   :name "clojure.core.typed.object-rep",
   :doc nil}),
 :vars
 ({:name "*verbose-forms*",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L1159",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/*verbose-forms*",
   :doc "If true, print complete forms in error messages.",
   :var-type "var",
   :line 1159,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:name "*verbose-types*",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L1155",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/*verbose-types*",
   :doc
   "If true, print fully qualified types in error messages\nand return values.",
   :var-type "var",
   :line 1155,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([varsym typesyn]),
   :name "ann",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L816",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann",
   :doc
   "Annotate varsym with type. If unqualified, qualify in the current namespace.\nIf varsym has metadata {:no-check true}, ignore definitions of varsym \nwhile type checking.\n\neg. ; annotate the var foo in this namespace\n    (ann foo [Number -> Number])\n\n    ; annotate a var in another namespace\n    (ann another.ns/bar [-> nil])\n \n    ; don't check this var\n    (ann ^:no-check foobar [Integer -> String])",
   :var-type "macro",
   :line 816,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :name "ann-datatype",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L858",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-datatype",
   :doc
   "Annotate datatype Class name dname with expected fields.\nIf unqualified, qualify in the current namespace.\n\neg. (ann-datatype MyDatatype [a :- Number,\n                              b :- Long])\n    \n    (ann-datatype another.ns.TheirDatatype\n                  [str :- String,\n                   vec :- (IPersistentVector Number)])",
   :var-type "macro",
   :line 858,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([form ty]),
   :name "ann-form",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L743",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-form",
   :doc "Annotate a form with an expected type.",
   :var-type "macro",
   :line 743,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([t & vs]),
   :name "ann-many",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L844",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-many",
   :doc
   "Annotate several vars with type t.\n\neg. (ann-many FakeSearch\n              web1 web2 image1 image2 video1 video2)",
   :var-type "macro",
   :line 844,
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
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L918",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-precord",
   :doc
   "Annotate record Class name dname with a polymorphic binder and expected fields.\nIf unqualified, qualify in the current namespace.",
   :var-type "macro",
   :line 918,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :name "ann-protocol",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L930",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-protocol",
   :doc
   "Annotate a possibly polymorphic protocol var with method types.\n\neg. (ann-protocol IFoo\n      bar\n      [IFoo -> Any]\n      baz\n      [IFoo -> Number])\n\n    ; polymorphic\n    (ann-protocol [[x :variance :covariant]]\n      IFoo\n      bar\n      [IFoo -> Any]\n      baz\n      [IFoo -> Number])",
   :var-type "macro",
   :line 930,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists
   ([dname
     fields
     &
     {ancests :unchecked-ancestors, rplc :replace, :as opt}]),
   :name "ann-record",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L906",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-record",
   :doc
   "Annotate record Class name dname with expected fields.\nIf unqualified, qualify in the current namespace.",
   :var-type "macro",
   :line 906,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([form] [form expected]),
   :name "cf",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L1043",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/cf",
   :doc
   "Takes a form and an optional expected type and\nreturns a human-readable inferred type for that form.\n\neg. (cf 1) \n    ;=> \"Long\"\n\n    (cf #(inc %) [Number -> Number)\n    ;=> \"[Number -> Number]",
   :var-type "macro",
   :line 1043,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([] [ns-or-sym]),
   :name "check-ns",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L1181",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-ns",
   :doc
   "Type check a namespace. If not provided default to current namespace.\nReturns a true value if type checking is successful, otherwise\nthrows an Exception.\n\nBind *verbose-types* to true to print fully qualified types.\nBind *verbose-forms* to print full forms in error messages.\n\neg. (check-ns 'myns.typed)\n    ;=> :ok\n   \n    ;implicitly check current namespace\n    (binding [*ns* (find-ns 'myns.typed)]\n      (check-ns))\n    ;=> :ok",
   :var-type "function",
   :line 1181,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([sym ty]),
   :name "declare-alias-kind",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L649",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-alias-kind",
   :doc
   "Declare a kind for an alias, similar to declare but on the kind level.",
   :var-type "macro",
   :line 649,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& syms]),
   :name "declare-datatypes",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L627",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-datatypes",
   :doc "Declare datatypes, similar to declare but on the type level.",
   :var-type "macro",
   :line 627,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& syms]),
   :name "declare-names",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L662",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-names",
   :doc "Declare names, similar to declare but on the type level.",
   :var-type "macro",
   :line 662,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& syms]),
   :name "declare-protocols",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L638",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-protocols",
   :doc "Declare protocols, similar to declare but on the type level.",
   :var-type "macro",
   :line 638,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([sym doc-str t] [sym t]),
   :name "def-alias",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L673",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/def-alias",
   :doc
   "Define a type alias. Takes an optional doc-string as a second\nargument.\n\nUpdates the corresponding var with documentation.\n\neg. (def-alias MyAlias\n      \"Here is my alias\"\n      (U nil String))",
   :var-type "macro",
   :line 673,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([seq-exprs & body]),
   :name "doseq>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L300",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/doseq>",
   :doc
   "Like doseq but requires annotation for each loop variable: \n[a [1 2]] becomes [a :- Long [1 2]]\n\neg.\n(doseq> [a :- (U nil AnyInteger) [1 nil 2 3]\n         :when a]\n   (inc a))",
   :var-type "macro",
   :line 300,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bindings & body]),
   :name "dotimes>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L158",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/dotimes>",
   :doc
   "Like dotimes.\n\neg. (dotimes> [_ 100]\n      (println \"like normal\"))",
   :var-type "macro",
   :line 158,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& forms]),
   :name "fn>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L502",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/fn>",
   :doc
   "Like fn, but with annotations. Annotations are mandatory\nfor parameters, with optional annotations for return type.\nIf fn is named, return type annotation is mandatory.\n\nSuggested idiom: use commas between parameter annotation triples.\n\neg. (fn> [a :- Number, b :- (U Symbol nil)] ...)\n\n    ;annotate return\n    (fn> :- String [a :- String] ...)\n\n    ;named fn\n    (fn> fname :- String [a :- String] ...)\n\n    ;multi-arity\n    (fn> fname \n      (:- String [a :- String] ...)\n      (:- Long   [a :- String, b :- Number] ...))",
   :var-type "macro",
   :line 502,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([tk ret-ann seq-exprs body-expr]),
   :name "for>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L175",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/for>",
   :doc
   "Like for but requires annotation for each loop variable: [a [1 2]] becomes [a :- Long [1 2]]\nAlso requires annotation for return type.\n\neg. (for> :- Number\n      [a :- (U nil AnyInteger) [1 nil 2 3]\n       :when a]\n      (inc a))",
   :var-type "macro",
   :line 175,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([inst-of & types]),
   :name "inst",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L128",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/inst",
   :doc "Instantiate a polymorphic type with a number of types",
   :var-type "macro",
   :line 128,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([inst-of & types]),
   :name "inst-ctor",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L133",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/inst-ctor",
   :doc
   "Instantiate a call to a constructor with a number of types.\nFirst argument must be an immediate call to a constructor.\nReturns exactly the instantiatee (the first argument).",
   :var-type "macro",
   :line 133,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists
   ([cljt coll] [javat cljt coll] [into-array-syn javat cljt coll]),
   :name "into-array>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L722",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/into-array>",
   :doc
   "Make a Java array with Java class javat and Typed Clojure type\ncljt. Resulting array will be of type javat, but elements of coll must be under\ncljt. cljt should be a subtype of javat (the same or more specific).\n\n*Temporary hack*\ninto-array-syn is exactly the syntax to put as the first argument to into-array.\nCalling resolve on this syntax should give the correct class.",
   :var-type "macro",
   :line 722,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([fn-specs-and-annotations & body]),
   :name "letfn>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L528",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/letfn>",
   :doc
   "Like letfn, but each function spec must be annotated.\n\neg. (letfn> [a :- [Number -> Number]\n             (a [b] 2)\n\n             c :- [Symbol -> nil]\n             (c [s] nil)]\n      ...)",
   :var-type "macro",
   :line 528,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([]),
   :name "load-if-needed",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L32",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/load-if-needed",
   :doc "Load and initialize all of core.typed if not already",
   :var-type "function",
   :line 32,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bndings* & forms]),
   :name "loop>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L581",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/loop>",
   :doc
   "Like loop, except loop variables require annotation.\n\nSuggested idiom: use a comma between the type and the initial\nexpression.\n\neg. (loop> [a :- Number, 1\n            b :- (U nil Number), nil]\n      ...)",
   :var-type "macro",
   :line 581,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([mname]),
   :name "method-type",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L81",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/method-type",
   :doc
   "Given a method symbol, print the core.typed types assigned to it.\nIntended for use at the REPL.",
   :var-type "function",
   :line 81,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([msym mmap]),
   :name "nilable-param",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L790",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/nilable-param",
   :doc
   "Override which parameters in qualified method msym may accept\nnilable values. If the parameter is a parameterised type or\nan Array, this also declares the parameterised types and the Array type as nilable.\n\nmmap is a map mapping arity parameter number to a set of parameter\npositions (integers). If the map contains the key :all then this overrides\nother entries. The key can also be :all, which declares all parameters nilable.",
   :var-type "macro",
   :line 790,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([msym arities]),
   :name "non-nil-return",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L774",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/non-nil-return",
   :doc
   "Override the return type of qualified method msym to be non-nil.\nTakes a set of relevant arities,\nrepresented by the number of parameters it takes (rest parameter counts as one),\nor :all which overrides all arities.\n\neg.  (non-nil-return java.lang.Class/getDeclaredMethod :all)",
   :var-type "macro",
   :line 774,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([ctorsym typesyn]),
   :name "override-constructor",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L977",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/override-constructor",
   :doc "Override all constructors for Class ctorsym with type.",
   :var-type "macro",
   :line 977,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([methodsym typesyn]),
   :name "override-method",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L988",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/override-method",
   :doc "Override type for qualified method methodsym.",
   :var-type "macro",
   :line 988,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& forms]),
   :name "pfn>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L486",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/pfn>",
   :doc
   "Define a polymorphic typed anonymous function.\n(pfn> name? [binder+] :- type? [[param :- type]* & [param :- type *]?] exprs*)\n(pfn> name? [binder+] (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)",
   :var-type "macro",
   :line 486,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([debug-str]),
   :name "print-env",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L804",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/print-env",
   :doc
   "During type checking, print the type environment to *out*,\npreceeded by literal string debug-str.",
   :var-type "function",
   :line 804,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([debug-string frm]),
   :name "print-filterset",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L106",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/print-filterset",
   :doc
   "During type checking, print the filter set attached to form, \npreceeded by literal string debug-string.\nReturns nil.\n\neg. (let [s (seq (get-a-seqable))]\n      (print-filterset \"Here now\" s))",
   :var-type "function",
   :line 106,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([]),
   :name "reset-caches",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L1172",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/reset-caches",
   :doc "Reset internal type caches.",
   :var-type "function",
   :line 1172,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& body]),
   :name "tc-ignore",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L763",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/tc-ignore",
   :doc "Ignore forms in body during type checking",
   :var-type "macro",
   :line 763,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :name "typed-deps",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L999",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/typed-deps",
   :doc
   "Declare namespaces which should be checked before the current namespace.\nAccepts any number of symbols.\n\neg. (typed-deps clojure.core.typed.holes\n                myns.types)",
   :var-type "macro",
   :line 999,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([]),
   :name "warn-on-unannotated-vars",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L1030",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/warn-on-unannotated-vars",
   :doc
   "Allow unannotated vars in the current namespace. \n\nEmits a warning instead of a type error when checking\na def without a corresponding expected type.\n\neg. (warn-on-unannotated-vars)",
   :var-type "macro",
   :line 1030,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([b & body]),
   :name "when-let-fail",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj#L495",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/c55d196c0dd48d20698155a99d4c90d111ffc0ef/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/when-let-fail",
   :doc
   "Like when-let, but fails if the binding yields a false value.",
   :var-type "macro",
   :line 495,
   :file "src/main/clojure/clojure/core/typed.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/Buffer",
   :namespace "clojure.core.typed.async",
   :var-type "var",
   :doc
   "Type Alias\n\nA buffer of type x.\n\n(TFn\n [[x :variance :invariant]]\n (clojure.core.async.impl.protocols/Buffer x))",
   :name "Buffer"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/Chan",
   :namespace "clojure.core.typed.async",
   :var-type "var",
   :doc
   "Type Alias\n\nA core.async channel\n\n(TFn\n [[x :variance :invariant]]\n (Extends\n  [(clojure.core.async.impl.protocols/WritePort x)\n   (clojure.core.async.impl.protocols/ReadPort x)\n   (clojure.core.async.impl.protocols/Channel x x)]))",
   :name "Chan"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/Port",
   :namespace "clojure.core.typed.async",
   :var-type "var",
   :doc
   "Type Alias\n\nA port that can read and write type x\n\n(TFn\n [[x :variance :invariant]]\n (Extends\n  [(clojure.core.async.impl.protocols/ReadPort x)\n   (clojure.core.async.impl.protocols/WritePort x)]))",
   :name "Port"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/ReadOnlyChan",
   :namespace "clojure.core.typed.async",
   :var-type "var",
   :doc
   "Type Alias\n\nA core.async channel that statically disallows writes.\n\n(TFn\n [[r :variance :covariant]]\n (Extends\n  [(clojure.core.async.impl.protocols/WritePort Nothing)\n   (clojure.core.async.impl.protocols/ReadPort r)\n   (clojure.core.async.impl.protocols/Channel Nothing r)]))",
   :name "ReadOnlyChan"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/ReadOnlyPort",
   :namespace "clojure.core.typed.async",
   :var-type "var",
   :doc
   "Type Alias\n\nA read-only port that can read type x\n\n(TFn\n [[r :variance :covariant]]\n (Extends\n  [(clojure.core.async.impl.protocols/ReadPort r)\n   (clojure.core.async.impl.protocols/WritePort Nothing)]))",
   :name "ReadOnlyPort"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/TimeoutChan",
   :namespace "clojure.core.typed.async",
   :var-type "var",
   :doc "Type Alias\n\nA timeout channel\n\n(Chan Any)",
   :name "TimeoutChan"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/WriteOnlyPort",
   :namespace "clojure.core.typed.async",
   :var-type "var",
   :doc
   "Type Alias\n\nA write-only port that can write type x\n\n(TFn\n [[x :variance :invariant]]\n (Extends\n  [(clojure.core.async.impl.protocols/ReadPort x)\n   (clojure.core.async.impl.protocols/WritePort x)]))",
   :name "WriteOnlyPort"}
  {:arglists ([t & args]),
   :name "buffer>",
   :namespace "clojure.core.typed.async",
   :source-url
   "https://github.com/clojure/core.typed/blob/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj#L184",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/buffer>",
   :doc
   "A statically typed core.async buffer. \n\n(buffer> t ...) creates a buffer that can read and write type t.\nSubsequent arguments are passed directly to clojure.core.async/buffer.\n\nNote: (buffer> t ...) is the same as ((inst buffer t) ...)",
   :var-type "macro",
   :line 184,
   :file "src/main/clojure/clojure/core/typed/async.clj"}
  {:arglists ([t & args]),
   :name "chan>",
   :namespace "clojure.core.typed.async",
   :source-url
   "https://github.com/clojure/core.typed/blob/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj#L173",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/chan>",
   :doc
   "A statically typed core.async channel. \n\n(chan> t ...) creates a buffer that can read and write type t.\nSubsequent arguments are passed directly to clojure.core.async/chan.\n\nNote: \n(chan> t ...) is the same as ((inst chan t) ...)",
   :var-type "macro",
   :line 173,
   :file "src/main/clojure/clojure/core/typed/async.clj"}
  {:arglists ([t & args]),
   :name "dropping-buffer>",
   :namespace "clojure.core.typed.async",
   :source-url
   "https://github.com/clojure/core.typed/blob/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj#L204",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/dropping-buffer>",
   :doc
   "A statically typed core.async dropping buffer. \n\n(dropping-buffer> t ...) creates a dropping buffer that can read and write type t.\nSubsequent arguments are passed directly to clojure.core.async/dropping-buffer.\n\nNote: (dropping-buffer> t ...) is the same as ((inst dropping-buffer t) ...)",
   :var-type "macro",
   :line 204,
   :file "src/main/clojure/clojure/core/typed/async.clj"}
  {:arglists ([& body]),
   :name "go>",
   :namespace "clojure.core.typed.async",
   :source-url
   "https://github.com/clojure/core.typed/blob/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj#L147",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/go>",
   :doc
   "Asynchronously executes the body, returning immediately to the\ncalling thread. Additionally, any visible calls to <!, >! and alt!/alts!\nchannel operations within the body will block (if necessary) by\n'parking' the calling thread rather than tying up an OS thread (or\nthe only JS thread when in ClojureScript). Upon completion of the\noperation, the body will be resumed.\n\nReturns a channel which will receive the result of the body when\ncompleted",
   :var-type "macro",
   :line 147,
   :file "src/main/clojure/clojure/core/typed/async.clj"}
  {:arglists ([t & args]),
   :name "sliding-buffer>",
   :namespace "clojure.core.typed.async",
   :source-url
   "https://github.com/clojure/core.typed/blob/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj#L194",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/75da3a91f5a78b759d020961e5e8a1c4772e8475/src/main/clojure/clojure/core/typed/async.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.async/sliding-buffer>",
   :doc
   "A statically typed core.async sliding buffer. \n\n(sliding-buffer> t ...) creates a sliding buffer that can read and write type t.\nSubsequent arguments are passed directly to clojure.core.async/sliding-buffer.\n\nNote: (sliding-buffer> t ...) is the same as ((inst sliding-buffer t) ...)",
   :var-type "macro",
   :line 194,
   :file "src/main/clojure/clojure/core/typed/async.clj"}
  {:arglists ([fs]),
   :name "->AndFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L73",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/->AndFilter",
   :doc "",
   :var-type "function",
   :line 73,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([]),
   :name "->BotFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L33",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/->BotFilter",
   :doc "",
   :var-type "function",
   :line 33,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([then else]),
   :name "->FilterSet",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L95",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/->FilterSet",
   :doc "",
   :var-type "function",
   :line 95,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([a c]),
   :name "->ImpFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L88",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/->ImpFilter",
   :doc "",
   :var-type "function",
   :line 88,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([]),
   :name "->NoFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L48",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/->NoFilter",
   :doc "",
   :var-type "function",
   :line 48,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([type path id]),
   :name "->NotTypeFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L65",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/->NotTypeFilter",
   :doc "",
   :var-type "function",
   :line 65,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([fs]),
   :name "->OrFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L80",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/->OrFilter",
   :doc "",
   :var-type "function",
   :line 80,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([]),
   :name "->TopFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L38",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/->TopFilter",
   :doc "",
   :var-type "function",
   :line 38,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([type path id]),
   :name "->TypeFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L55",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/->TypeFilter",
   :doc "",
   :var-type "function",
   :line 55,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/NameRef",
   :namespace "clojure.core.typed.filter-rep",
   :var-type "var",
   :doc
   "Type Alias\n\nA name for a type variable, either a symbol or a number.\n\n(U Symbol Number)",
   :name "NameRef"}
  {:arglists ([{:as m__4844__auto__, :keys [fs]}]),
   :name "map->AndFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L73",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/map->AndFilter",
   :doc "",
   :var-type "function",
   :line 73,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys []}]),
   :name "map->BotFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L33",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/map->BotFilter",
   :doc "",
   :var-type "function",
   :line 33,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys [then else]}]),
   :name "map->FilterSet",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L95",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/map->FilterSet",
   :doc "",
   :var-type "function",
   :line 95,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys [a c]}]),
   :name "map->ImpFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L88",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/map->ImpFilter",
   :doc "",
   :var-type "function",
   :line 88,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys []}]),
   :name "map->NoFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L48",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/map->NoFilter",
   :doc "",
   :var-type "function",
   :line 48,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys [type path id]}]),
   :name "map->NotTypeFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L65",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/map->NotTypeFilter",
   :doc "",
   :var-type "function",
   :line 65,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys [fs]}]),
   :name "map->OrFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L80",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/map->OrFilter",
   :doc "",
   :var-type "function",
   :line 80,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys []}]),
   :name "map->TopFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L38",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/map->TopFilter",
   :doc "",
   :var-type "function",
   :line 38,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys [type path id]}]),
   :name "map->TypeFilter",
   :namespace "clojure.core.typed.filter-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj#L55",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/filter_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/map->TypeFilter",
   :doc "",
   :var-type "function",
   :line 55,
   :file "src/main/clojure/clojure/core/typed/filter_rep.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/AndFilter",
   :namespace "clojure.core.typed.filter-rep",
   :var-type "record",
   :name "AndFilter"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/BotFilter",
   :namespace "clojure.core.typed.filter-rep",
   :var-type "record",
   :name "BotFilter"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/FilterSet",
   :namespace "clojure.core.typed.filter-rep",
   :var-type "record",
   :name "FilterSet"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/ImpFilter",
   :namespace "clojure.core.typed.filter-rep",
   :var-type "record",
   :name "ImpFilter"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/NoFilter",
   :namespace "clojure.core.typed.filter-rep",
   :var-type "record",
   :name "NoFilter"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/NotTypeFilter",
   :namespace "clojure.core.typed.filter-rep",
   :var-type "record",
   :name "NotTypeFilter"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/OrFilter",
   :namespace "clojure.core.typed.filter-rep",
   :var-type "record",
   :name "OrFilter"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/TopFilter",
   :namespace "clojure.core.typed.filter-rep",
   :var-type "record",
   :name "TopFilter"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.filter-rep/TypeFilter",
   :namespace "clojure.core.typed.filter-rep",
   :var-type "record",
   :name "TypeFilter"}
  {:arglists ([]),
   :name "->NoisyHole",
   :namespace "clojure.core.typed.hole",
   :source-url
   "https://github.com/clojure/core.typed/blob/6b5231ad882ee07b0ef6c5e978e09c09aed083a6/src/main/clojure/clojure/core/typed/hole.clj#L17",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/6b5231ad882ee07b0ef6c5e978e09c09aed083a6/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/->NoisyHole",
   :doc
   "Positional factory function for class clojure.core.typed.hole.NoisyHole.",
   :var-type "function",
   :line 17,
   :file "src/main/clojure/clojure/core/typed/hole.clj"}
  {:arglists ([]),
   :name "noisy-hole",
   :namespace "clojure.core.typed.hole",
   :source-url
   "https://github.com/clojure/core.typed/blob/6b5231ad882ee07b0ef6c5e978e09c09aed083a6/src/main/clojure/clojure/core/typed/hole.clj#L20",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/6b5231ad882ee07b0ef6c5e978e09c09aed083a6/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/noisy-hole",
   :doc
   "A noisy hole. The type system will complain when\n(noisy-hole) is used in positions that expect a type\nmore specific than Object or Any.\nUse (noisy-hole) as a placeholder for code.\nThrows an exception when evaluted.",
   :var-type "function",
   :line 20,
   :file "src/main/clojure/clojure/core/typed/hole.clj"}
  {:arglists ([]),
   :name "silent-hole",
   :namespace "clojure.core.typed.hole",
   :source-url
   "https://github.com/clojure/core.typed/blob/6b5231ad882ee07b0ef6c5e978e09c09aed083a6/src/main/clojure/clojure/core/typed/hole.clj#L8",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/6b5231ad882ee07b0ef6c5e978e09c09aed083a6/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/silent-hole",
   :doc
   "A silent hole. (silent-hole) passes for any other type\nwhen type checking.\nUse (silent-hole) as a placeholder for code.\nThrows an exception when evaluted.",
   :var-type "function",
   :line 8,
   :file "src/main/clojure/clojure/core/typed/hole.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/NoisyHole",
   :namespace "clojure.core.typed.hole",
   :var-type "type",
   :name "NoisyHole"}
  {:arglists ([]),
   :name "->EmptyObject",
   :namespace "clojure.core.typed.object-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj#L25",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.object-rep/->EmptyObject",
   :doc "",
   :var-type "function",
   :line 25,
   :file "src/main/clojure/clojure/core/typed/object_rep.clj"}
  {:arglists ([]),
   :name "->NoObject",
   :namespace "clojure.core.typed.object-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj#L44",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.object-rep/->NoObject",
   :doc "",
   :var-type "function",
   :line 44,
   :file "src/main/clojure/clojure/core/typed/object_rep.clj"}
  {:arglists ([path id]),
   :name "->Path",
   :namespace "clojure.core.typed.object-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj#L34",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.object-rep/->Path",
   :doc "",
   :var-type "function",
   :line 34,
   :file "src/main/clojure/clojure/core/typed/object_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys []}]),
   :name "map->EmptyObject",
   :namespace "clojure.core.typed.object-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj#L25",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.object-rep/map->EmptyObject",
   :doc "",
   :var-type "function",
   :line 25,
   :file "src/main/clojure/clojure/core/typed/object_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys []}]),
   :name "map->NoObject",
   :namespace "clojure.core.typed.object-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj#L44",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.object-rep/map->NoObject",
   :doc "",
   :var-type "function",
   :line 44,
   :file "src/main/clojure/clojure/core/typed/object_rep.clj"}
  {:arglists ([{:as m__4844__auto__, :keys [path id]}]),
   :name "map->Path",
   :namespace "clojure.core.typed.object-rep",
   :source-url
   "https://github.com/clojure/core.typed/blob/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj#L34",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/01f5802054988df89c51e2eb7b133d8dc5f634ca/src/main/clojure/clojure/core/typed/object_rep.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.object-rep/map->Path",
   :doc "",
   :var-type "function",
   :line 34,
   :file "src/main/clojure/clojure/core/typed/object_rep.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.object-rep/EmptyObject",
   :namespace "clojure.core.typed.object-rep",
   :var-type "record",
   :name "EmptyObject"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.object-rep/NoObject",
   :namespace "clojure.core.typed.object-rep",
   :var-type "record",
   :name "NoObject"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.object-rep/Path",
   :namespace "clojure.core.typed.object-rep",
   :var-type "record",
   :name "Path"})}

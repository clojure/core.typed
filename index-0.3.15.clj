{:namespaces
 ({:source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed-api.html",
   :name "clojure.core.typed",
   :doc
   "This namespace contains typed wrapper macros, type aliases\nand functions for type checking Clojure code. check-ns is the interface\nfor checking namespaces, cf for checking individual forms."}
  {:source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.async-api.html",
   :name "clojure.core.typed.async",
   :doc
   "This namespace contains annotations and helper macros for type\nchecking core.async code. Ensure clojure.core.async is require'd\nbefore performing type checking.\n\ngo\n  use go\n\nchan\n  use chan\n\nbuffer\n  use buffer (similar for other buffer constructors)\n"}
  {:source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/base_env_common.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.base-env-common-api.html",
   :name "clojure.core.typed.base-env-common",
   :doc "Utilities for all implementations of the type checker"}
  {:source-url
   "https://github.com/clojure/core.typed/blob/935e7c9277a1954cdd011907cb5a8abe3134b05e/module-check/src/main/clojure/clojure/core/typed/check_form_cljs.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check-form-cljs-api.html",
   :name "clojure.core.typed.check-form-cljs",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/7d7264105b8f1d07915413cb75743a4f6b847dc4/module-check/src/main/clojure/clojure/core/typed/check_ns_clj.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check-ns-clj-api.html",
   :name "clojure.core.typed.check-ns-clj",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/488c9279bd44cee8eacd5167b1dc4773943a8666/module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.fn-methods-api.html",
   :name "clojure.core.typed.check.fn-methods",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/81a3bc793db822eaeee5d59f1498fbaa24639cdd/module-check/src/main/clojure/clojure/core/typed/check/monitor.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.monitor-api.html",
   :name "clojure.core.typed.check.monitor",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/check/value.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.value-api.html",
   :name "clojure.core.typed.check.value",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/collect_utils.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.collect-utils-api.html",
   :name "clojure.core.typed.collect-utils",
   :doc nil}
  {:source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.current-impl-api.html",
   :name "clojure.core.typed.current-impl",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.hole-api.html",
   :name "clojure.core.typed.hole",
   :doc
   "This namespace contains easy tools for hole driven development"}
  {:source-url
   "https://github.com/clojure/core.typed/blob/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.lang-api.html",
   :name "clojure.core.typed.lang",
   :doc
   "Extensible languages in Clojure, a la Racket's #lang.\n\nThis is a simple library that monkey patches clojure.core/load\nto be extensible to different backends.\n\n`monkey-patch-extensible-load` does the actual monkey-patching and\nmust be called explicitly.\n\n`lang-dispatch` is a map from keywords to alternative `load` functions\n(of type [String -> nil]). The corresponding function will be used to\nload a file according its :lang metadata entry in the `ns` form.\n\nTo add a new implementation, use\n  (alter-var-root lang-dispatch assoc :new-impl my-load)\n\neg. A file with a `ns` form\n      (ns fancy-ns-form\n        {:lang :new-impl})\n    will use `my-load` to load the file.\n"}
  {:source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.macros-api.html",
   :name "clojure.core.typed.macros",
   :doc nil}
  {:source-url
   "https://github.com/clojure/core.typed/blob/0947387913babb0e8db52b560a3c0e42b45cb40b/module-check/src/main/clojure/clojure/core/typed/statistics.clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.statistics-api.html",
   :name "clojure.core.typed.statistics",
   :doc nil}
  {:source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.util-vars-api.html",
   :name "clojure.core.typed.util-vars",
   :doc nil}),
 :vars
 ({:forms [(ASeq t)],
   :name "ASeq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ASeq",
   :doc "A sequential seq returned from clojure.core/seq",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(AVec t)],
   :name "AVec",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/AVec",
   :doc
   "A persistent vector returned from clojure.core/vector (and others)",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Agent1 t)],
   :name "Agent1",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Agent1",
   :doc "An agent that can read and write type x.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Agent2 t t)],
   :name "Agent2",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Agent2",
   :doc "An agent that can write type w and read type r.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(All binder type)],
   :name "All",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1475",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/All",
   :doc "A polymorphic binder",
   :var-type "type alias",
   :line 1475,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Any],
   :name "Any",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1335",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Any",
   :doc "Any is the top type that contains all types.",
   :var-type "type alias",
   :line 1335,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [AnyInteger],
   :name "AnyInteger",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/AnyInteger",
   :doc "A type that returns true for clojure.core/integer?",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [AnyValue],
   :name "AnyValue",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1340",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/AnyValue",
   :doc "AnyValue contains all Value singleton types",
   :var-type "type alias",
   :line 1340,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Assoc type type-pairs*)],
   :name "Assoc",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1454",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Assoc",
   :doc "A type representing an assoc operation",
   :var-type "type alias",
   :line 1454,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Atom1 t)],
   :name "Atom1",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Atom1",
   :doc "An atom that can read and write type x.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Atom2 t)],
   :name "Atom2",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Atom2",
   :doc "An atom that can write type w and read type r.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(BlockingDeref t)],
   :name "BlockingDeref",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/BlockingDeref",
   :doc "A Clojure blocking derefable (see clojure.core/deref).",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Bool],
   :name "Bool",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Bool",
   :doc "A boolean",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Coll t)],
   :name "Coll",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Coll",
   :doc "A persistent collection with member type x.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(CountRange Integer) (CountRange Integer Integer)],
   :name "CountRange",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1373",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/CountRange",
   :doc "A type representing a range of counts for a collection",
   :var-type "type alias",
   :line 1373,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Delay t)],
   :name "Delay",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Delay",
   :doc "A Clojure delay (see clojure.core/{delay,force}).",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Deref t)],
   :name "Deref",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Deref",
   :doc "A Clojure derefable (see clojure.core/deref).",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Difference type type type*)],
   :name "Difference",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1384",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Difference",
   :doc
   "Difference represents a difference of types.\n\n(Difference t s) is the same as type t with type s removed.\n\neg. (Difference (U Int Long) Int) => Long\n    (Difference (U Num nil) nil)  => Num\n",
   :var-type "type alias",
   :line 1384,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Dissoc type type*)],
   :name "Dissoc",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1459",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Dissoc",
   :doc "A type representing a dissoc operation",
   :var-type "type alias",
   :line 1459,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [EmptyCount],
   :name "EmptyCount",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/EmptyCount",
   :doc
   "The type of all things with count 0. Use as part of an intersection.\neg. See EmptySeqable.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(EmptySeqable t)],
   :name "EmptySeqable",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/EmptySeqable",
   :doc
   "A type that can be used to create a sequence of member type x\nwith count 0.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [ExInfo],
   :name "ExInfo",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ExInfo",
   :doc "A Clojure custom exception type.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(ExactCount Integer)],
   :name "ExactCount",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1379",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ExactCount",
   :doc "A type representing a precise count for a collection",
   :var-type "type alias",
   :line 1379,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Fn],
   :name "Fn",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Fn",
   :doc "A type that returns true for clojure.core/fn?",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Future t)],
   :name "Future",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Future",
   :doc "A Clojure future (see clojure.core/{future-call,future}).",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Get type type) (Get type type type)],
   :name "Get",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1464",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Get",
   :doc "A type representing a get operation",
   :var-type "type alias",
   :line 1464,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
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
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1407",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HMap",
   :doc "HMap is a type for heterogeneous maps.",
   :var-type "type alias",
   :line 1407,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
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
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1423",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HSeq",
   :doc "HSeq is a type for heterogeneous seqs",
   :var-type "type alias",
   :line 1423,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
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
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1416",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HSequential",
   :doc
   "HSequential is a type for heterogeneous sequential collections",
   :var-type "type alias",
   :line 1416,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(HSet #{fixed*} :complete? Boolean)],
   :name "HSet",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1430",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HSet",
   :doc
   "HSet is a type for heterogeneous sets.\nTakes a set of simple values. By default\n:complete? is true.\n\neg. (HSet #{:a :b :c} :complete? true)",
   :var-type "type alias",
   :line 1430,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
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
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1395",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HVec",
   :doc
   "HVec is a type for heterogeneous vectors.\nIt extends clojure.core.typed/Vec and is a subtype\nof clojure.core.typed/HSequential.",
   :var-type "type alias",
   :line 1395,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Hierarchy],
   :name "Hierarchy",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Hierarchy",
   :doc "A hierarchy for use with derive, isa? etc.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(I type*)],
   :name "I",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1356",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/I",
   :doc "I represents an intersection of types",
   :var-type "type alias",
   :line 1356,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms
   [(IFn ArityVec+)
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
   :name "IFn",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1439",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/IFn",
   :doc "An ordered intersection type of function arities.",
   :var-type "type alias",
   :line 1439,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Id],
   :name "Id",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Id",
   :doc "The identity function at the type level.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Int],
   :name "Int",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Int",
   :doc "A type that returns true for clojure.core/integer?",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Keyword],
   :name "Keyword",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Keyword",
   :doc "A keyword",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Kw],
   :name "Kw",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Kw",
   :doc "A keyword",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(List t)],
   :name "List",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/List",
   :doc "A Clojure persistent list.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Map t t)],
   :name "Map",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Map",
   :doc "A persistent map with keys k and vals v.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Multi],
   :name "Multi",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Multi",
   :doc "A Clojure multimethod.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Namespace],
   :name "Namespace",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Namespace",
   :doc "A namespace",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Nilable t)],
   :name "Nilable",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Nilable",
   :doc "A union of x and nil.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(NilableNonEmptyASeq t)],
   :name "NilableNonEmptyASeq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NilableNonEmptyASeq",
   :doc "The result of clojure.core/seq.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(NilableNonEmptySeq t)],
   :name "NilableNonEmptySeq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NilableNonEmptySeq",
   :doc
   "A persistent sequence of member type x with count greater than 0, or nil.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(NonEmptyASeq t)],
   :name "NonEmptyASeq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyASeq",
   :doc "A sequential non-empty seq retured from clojure.core/seq",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(NonEmptyAVec t)],
   :name "NonEmptyAVec",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyAVec",
   :doc
   "A persistent vector returned from clojure.core/vector (and others) and count greater than 0.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(NonEmptyColl t)],
   :name "NonEmptyColl",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyColl",
   :doc
   "A persistent collection with member type x and count greater than 0.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [NonEmptyCount],
   :name "NonEmptyCount",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyCount",
   :doc
   "The type of all things with count greater than 0. Use as part of an intersection.\neg. See NonEmptySeq",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(NonEmptyLazySeq t)],
   :name "NonEmptyLazySeq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyLazySeq",
   :doc "A non-empty lazy sequence of type t",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(NonEmptySeq t)],
   :name "NonEmptySeq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptySeq",
   :doc
   "A persistent sequence of member type x with count greater than 0.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(NonEmptySeqable t)],
   :name "NonEmptySeqable",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptySeqable",
   :doc
   "A type that can be used to create a sequence of member type x\nwith count greater than 0.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(NonEmptyVec t)],
   :name "NonEmptyVec",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyVec",
   :doc
   "A persistent vector with member type x and count greater than 0.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Nothing],
   :name "Nothing",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1350",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Nothing",
   :doc
   "Nothing is the bottom type that inhabits no types\nexcept itself.",
   :var-type "type alias",
   :line 1350,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Num],
   :name "Num",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Num",
   :doc "A type that returns true for clojure.core/number?",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Option t)],
   :name "Option",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Option",
   :doc "A union of x and nil.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Pred type)],
   :name "Pred",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1447",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Pred",
   :doc
   "A predicate for the given type.\n\neg. Type for integer?: (Pred Int)",
   :var-type "type alias",
   :line 1447,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Promise t)],
   :name "Promise",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Promise",
   :doc "A Clojure promise (see clojure.core/{promise,deliver}).",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Proxy],
   :name "Proxy",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Proxy",
   :doc "A Clojure proxy.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Rec binder type)],
   :name "Rec",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1470",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Rec",
   :doc "A recursive type",
   :var-type "type alias",
   :line 1470,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Ref1 t)],
   :name "Ref1",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Ref1",
   :doc "A ref that can read and write type x.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Ref2 w r)],
   :name "Ref2",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Ref2",
   :doc "A ref that can write type w and read type r.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Reversible t)],
   :name "Reversible",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Reversible",
   :doc "A Clojure reversible collection.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Seq t)],
   :name "Seq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Seq",
   :doc "A persistent sequence of member type x.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Seqable t)],
   :name "Seqable",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Seqable",
   :doc
   "A type that can be used to create a sequence of member type x.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Sequential],
   :name "Sequential",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Sequential",
   :doc "A sequential collection.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(SequentialSeq t)],
   :name "SequentialSeq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/SequentialSeq",
   :doc
   "A Clojure sequential sequence. Seq's aren't always Sequential.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(SequentialSeqable t)],
   :name "SequentialSeqable",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/SequentialSeqable",
   :doc
   "A sequential, seqable collection. Seq's aren't always Sequential.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Set t)],
   :name "Set",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Set",
   :doc "A persistent set with member type x",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(SortedSet t)],
   :name "SortedSet",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/SortedSet",
   :doc "A sorted persistent set with member type x",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Stack t)],
   :name "Stack",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Stack",
   :doc "A Clojure stack.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Str],
   :name "Str",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Str",
   :doc "A string",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Sym],
   :name "Sym",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Sym",
   :doc "A symbol",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [Symbol],
   :name "Symbol",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Symbol",
   :doc "A symbol",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(TFn binder type)],
   :name "TFn",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1480",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/TFn",
   :doc "A type function",
   :var-type "type alias",
   :line 1480,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(U type*)],
   :name "U",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1345",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/U",
   :doc "U represents a union of types",
   :var-type "type alias",
   :line 1345,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Val Constant) 'Constant],
   :name "Val",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1361",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Val",
   :doc "A singleton type for a constant value.",
   :var-type "type alias",
   :line 1361,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Value Constant) 'Constant],
   :name "Value",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1367",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Value",
   :doc "A singleton type for a constant value.",
   :var-type "type alias",
   :line 1367,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Var1 t)],
   :name "Var1",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Var1",
   :doc "An var that can read and write type x.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Var2 w r)],
   :name "Var2",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Var2",
   :doc "An var that can write type w and read type r.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:forms [(Vec t)],
   :name "Vec",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Vec",
   :doc "A persistent vector with member type x.",
   :var-type "type alias",
   :line 1499,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([varsym typesyn]),
   :name "ann",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1721",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann",
   :doc
   "Annotate varsym with type. If unqualified, qualify in the current namespace.\nIf varsym has metadata {:no-check true}, ignore definitions of varsym \nwhile type checking.\n\nIf annotating vars in namespaces other than the current one, a fully\nqualified symbol must be provided. Note that namespace aliases are not\nrecognised: the *full* namespace must be given in the first part of the symbol.\n\neg. ; annotate the var foo in this namespace\n    (ann foo [Number -> Number])\n\n    ; annotate a var in another namespace\n    (ann another.ns/bar [-> nil])\n \n    ; don't check this var\n    (ann ^:no-check foobar [Integer -> String])",
   :var-type "macro",
   :line 1721,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :forms
   [(ann-datatype dname [field :- type*] opts*)
    (ann-datatype binder dname [field :- type*] opts*)],
   :name "ann-datatype",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1772",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-datatype",
   :doc
   "Annotate datatype Class name dname with expected fields.\nIf unqualified, qualify in the current namespace.\nTakes an optional type variable binder before the name.\n\nFields must be specified in the same order as presented \nin deftype, with exactly the same field names.\n\nAlso annotates datatype factories and constructors.\n\nBinder is a vector of specs. Each spec is a vector\nwith the variable name as the first entry, followed by\nkeyword arguments:\n- :variance (mandatory)\n  The declared variance of the type variable. Possible\n  values are :covariant, :contravariant and :invariant.\n- :< (optional)\n  The upper type bound of the type variable. Defaults to\n  Any, or the most general type of the same rank as the\n  lower bound.\n- :> (optional)\n  The lower type bound of the type variable. Defaults to\n  Nothing, or the least general type of the same rank as the\n  upper bound.\n\neg. ; a datatype in the current namespace\n    (ann-datatype MyDatatype [a :- Number,\n                              b :- Long])\n\n    ; a datatype in another namespace\n    (ann-datatype another.ns.TheirDatatype\n                  [str :- String,\n                   vec :- (Vec Number)])\n\n    ; a datatype, polymorphic in a\n    (ann-datatype [[a :variance :covariant]]\n                  MyPolyDatatype\n                  [str :- String,\n                   vec :- (Vec Number)\n                   ply :- (Set a)])",
   :var-type "macro",
   :line 1772,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([form ty]),
   :name "ann-form",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-form",
   :doc "Annotate a form with an expected type.",
   :var-type "macro",
   :line 130,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([& args]),
   :forms
   [(ann-interface vbnd varsym & methods)
    (ann-interface varsym & methods)],
   :name "ann-interface",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2005",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-interface",
   :doc
   "Annotate a possibly polymorphic interface (created with definterface) with method types.\n\nNote: Unlike ann-protocol, omit the target ('this') argument in the method signatures.\n\neg. (ann-interface IFoo\n      bar\n      (Fn [-> Any]\n          [Number Symbol -> Any])\n      baz\n      [Number -> Number])\n    (definterface IFoo\n      (bar [] [n s])\n      (baz [n]))\n\n    ; polymorphic protocol\n    ; x is scoped in the methods\n    (ann-protocol [[x :variance :covariant]]\n      IFooPoly\n      bar\n      (Fn [-> Any]\n          [Number Symbol -> Any])\n      baz\n      [Number -> Number])\n    (definterface IFooPoly\n      (bar [] [n s])\n      (baz [n]))",
   :var-type "macro",
   :line 2005,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([t & vs]),
   :name "ann-many",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1758",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-many",
   :doc
   "Annotate several vars with type t.\n\neg. (ann-many FakeSearch\n              web1 web2 image1 image2 video1 video2)",
   :var-type "macro",
   :line 1758,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists
   ([dname
     vbnd
     fields
     &
     {ancests :unchecked-ancestors, rplc :replace, :as opt}]),
   :name "ann-precord",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1931",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-precord",
   :doc
   "Annotate record Class name dname with a polymorphic binder and expected fields.\nIf unqualified, qualify in the current namespace.",
   :var-type "macro",
   :line 1931,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :forms
   [(ann-protocol vbnd varsym & methods)
    (ann-protocol varsym & methods)],
   :name "ann-protocol",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1945",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-protocol",
   :doc
   "Annotate a possibly polymorphic protocol var with method types.\n\neg. (ann-protocol IFoo\n      bar\n      (Fn [IFoo -> Any]\n          [IFoo Number Symbol -> Any])\n      baz\n      [IFoo Number -> Number])\n    (defprotocol> IFoo\n      (bar [this] [this n s])\n      (baz [this n]))\n\n    ; polymorphic protocol\n    ; x is scoped in the methods\n    (ann-protocol [[x :variance :covariant]]\n      IFooPoly\n      bar\n      (Fn [(IFooPoly x) -> Any]\n          [(IFooPoly x) Number Symbol -> Any])\n      baz\n      [(IFooPoly x) Number -> Number])\n    (defprotocol> IFooPoly\n      (bar [this] [this n s])\n      (baz [this n]))",
   :var-type "macro",
   :line 1945,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :forms
   [(ann-record dname [field :- type*] opts*)
    (ann-record binder dname [field :- type*] opts*)],
   :name "ann-record",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1860",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-record",
   :doc
   "Annotate record Class name dname with expected fields.\nIf unqualified, qualify in the current namespace.\nTakes an optional type variable binder before the name.\n\nFields must be specified in the same order as presented \nin defrecord, with exactly the same field names.\n\nAlso annotates record factories and constructors.\n\nBinder is a vector of specs. Each spec is a vector\nwith the variable name as the first entry, followed by\nkeyword arguments:\n- :variance (mandatory)\n  The declared variance of the type variable. Possible\n  values are :covariant, :contravariant and :invariant.\n- :< (optional)\n  The upper type bound of the type variable. Defaults to\n  Any, or the most general type of the same rank as the\n  lower bound.\n- :> (optional)\n  The lower type bound of the type variable. Defaults to\n  Nothing, or the least general type of the same rank as the\n  upper bound.\n\neg. ; a record in the current namespace\n    (ann-record MyRecord [a :- Number,\n                          b :- Long])\n\n    ; a record in another namespace\n    (ann-record another.ns.TheirRecord\n                  [str :- String,\n                   vec :- (Vec Number)])\n\n    ; a record, polymorphic in a\n    (ann-record [[a :variance :covariant]]\n                MyPolyRecord\n                [str :- String,\n                 vec :- (Vec Number)\n                 ply :- (Set a)])",
   :var-type "macro",
   :line 1860,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :name "atom",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/atom",
   :doc
   "Like atom, but with optional type annotations.\n\nSame as (atom (ann-form init t) args*)\n\neg. (atom 1) : (Atom1 (Value 1))\n    (atom :- Num, 1) : (Atom1 Num)",
   :var-type "macro",
   :line 198,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([t init & args]),
   :name "atom>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1224",
   :deprecated "0.2.58",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/atom>",
   :doc
   "DEPRECATED: use clojure.core.typed/atom\n\nLike atom, but creates an Atom1 of type t.\n\nSame as (atom (ann-form init t) args*)\n\neg. (atom> Number 1)\n    (atom> (Vec Any) [])",
   :var-type "macro",
   :line 1224,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([form] [form expected]),
   :name "cf",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2210",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/cf",
   :doc
   "Takes a form and an optional expected type and\nreturns a human-readable inferred type for that form.\nThrows an exception if type checking fails.\n\nDo not use cf inside a typed namespace. cf is intended to be\nused at the REPL or within a unit test. Note that testing for\ntruthiness is not sufficient to unit test a call to cf, as nil\nand false are valid type syntax.\n\ncf preserves annotations from previous calls to check-ns or cf,\nand keeps any new ones collected during a cf. This is useful for\ndebugging and experimentation. cf may be less strict than check-ns\nwith type checker warnings.\n\neg. (cf 1) \n    ;=> Long\n\n    (cf #(inc %) [Number -> Number])\n    ;=> [Number -> Number]",
   :var-type "macro",
   :line 2210,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([form] [form expected] [form expected type-provided?]),
   :name "check-form*",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2198",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-form*",
   :doc
   "Takes a (quoted) form and optional expected type syntax and\ntype checks the form. If expected is provided, type-provided?\nmust be true.",
   :var-type "function",
   :line 2198,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([form & opt]),
   :name "check-form-info",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2168",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-form-info",
   :doc
   "Type checks a (quoted) form and returns a map of results from type checking the\nform.\n\nOptions\n- :expected        Type syntax representing the expected type for this form\n                   type-provided? option must be true to utilise the type.\n- :type-provided?  If true, use the expected type to check the form.\n- :profile         Use Timbre to profile the type checker. Timbre must be\n                   added as a dependency.\n- :file-mapping    If true, return map provides entry :file-mapping, a hash-map\n                   of (Map '{:line Int :column Int :file Str} Str).\n- :checked-ast     Returns the entire AST for the given form as the :checked-ast entry,\n                   annotated with the static types inferred after checking.\n                   If a fatal error occurs, mapped to nil.\n- :no-eval         If true, don't evaluate :out-form. Removes :result return value.\n                   It is highly recommended to evaluate :out-form manually.\n\nDefault return map\n- :ret             TCResult inferred for the current form\n- :out-form        The macroexpanded result of type-checking, if successful. \n- :result          The evaluated result of :out-form, unless :no-eval is provided.\n- :ex              If an exception was thrown during evaluation, this key will be present\n                   with the exception as the value.\nDEPRECATED\n- :delayed-errors  A sequence of delayed errors (ex-info instances)",
   :var-type "function",
   :line 2168,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([] [ns-or-syms & opt]),
   :name "check-ns",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2253",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-ns",
   :doc
   "Type check a namespace/s (a symbol or Namespace, or collection).\nIf not provided default to current namespace.\nReturns a true value if type checking is successful, otherwise\nthrows an Exception.\n\nDo not use check-ns within a checked namespace.\nIt is intended to be used at the REPL or within a unit test.\nSuggested idiom for clojure.test: (is (check-ns 'your.ns))\n\ncheck-ns resets annotations collected from \nprevious check-ns calls or cf. A successful check-ns call will\npreserve any type annotations collect during that checking run.\n\nKeyword arguments:\n- :collect-only  if true, collect type annotations but don't type check code.\n                 Useful for debugging purposes.\n- :trace         if true, print some basic tracing of the type checker\n- :profile       if true, use Timbre to profile type checking. Must include\n                 Timbre as a dependency.\n\nIf providing keyword arguments, the namespace to check must be provided\nas the first argument.\n\nBind clojure.core.typed.util-vars/*verbose-types* to true to print fully qualified types.\nBind clojure.core.typed.util-vars/*verbose-forms* to print full forms in error messages.\n\neg. (check-ns 'myns.typed)\n    ;=> :ok\n   \n    ; implicitly check current namespace\n    (check-ns)\n    ;=> :ok\n\n    ; collect but don't check the current namespace\n    (check-ns *ns* :collect-only true)",
   :var-type "function",
   :line 2253,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([] [ns-or-syms & opt]),
   :name "check-ns-info",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2233",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-ns-info",
   :doc
   "Same as check-ns, but returns a map of results from type checking the\nnamespace.\n\nOptions\n- :collect-only    Don't type check the given namespace/s, but collect the \n                   top level type annotations like ann, ann-record.\n- :type-provided?  If true, use the expected type to check the form\n- :profile         Use Timbre to profile the type checker. Timbre must be\n                   added as a dependency.\n- :file-mapping    If true, return map provides entry :file-mapping, a hash-map\n                   of (Map '{:line Int :column Int :file Str} Str).\n\nDefault return map\n- :delayed-errors  A sequence of delayed errors (ex-info instances)",
   :var-type "function",
   :line 2233,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([sym ty]),
   :name "declare-alias-kind",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1169",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-alias-kind",
   :doc
   "Declare a kind for an alias, similar to declare but on the kind level.",
   :var-type "macro",
   :line 1169,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& syms]),
   :name "declare-datatypes",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1147",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-datatypes",
   :doc "Declare datatypes, similar to declare but on the type level.",
   :var-type "macro",
   :line 1147,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& syms]),
   :name "declare-names",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1182",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-names",
   :doc "Declare names, similar to declare but on the type level.",
   :var-type "macro",
   :line 1182,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& syms]),
   :name "declare-protocols",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1158",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-protocols",
   :doc "Declare protocols, similar to declare but on the type level.",
   :var-type "macro",
   :line 1158,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([name & fdecl]),
   :forms [(def name docstring? :- type? expr)],
   :name "def",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/def",
   :doc
   "Like clojure.core/def with optional type annotations\n\nNB: in Clojure it is impossible to refer a var called `def` as it is a\nspecial form. Use an alias prefix (eg. `t/def`).\n\nIf an annotation is provided, a corresponding `ann` form\nis generated, otherwise it expands identically to clojure.core/def\n\neg. ;same as clojure.core/def\n    (def vname 1)\n    \n    ;with Number `ann`\n    (def vname :- Number 1)\n\n    ;doc\n    (def vname\n      \"Docstring\"\n      :- Long\n      1)",
   :var-type "macro",
   :line 22,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([sym doc-str t] [sym t]),
   :name "def-alias",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1260",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/def-alias",
   :doc
   "DEPRECATED: use defalias\n\nDefine a type alias. Takes an optional doc-string as a second\nargument.\n\nUpdates the corresponding var with documentation.\n\neg. (def-alias MyAlias\n      \"Here is my alias\"\n      (U nil String))",
   :var-type "macro",
   :line 1260,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([name & fdecl]),
   :forms [(def> name docstring? :- type expr)],
   :name "def>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L769",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/def>",
   :doc
   "DEPRECATED: use clojure.core.typed/def\n\nLike def, but with annotations.\n\neg. (def> vname :- Long 1)\n\n;doc\n(def> vname\n  \"Docstring\"\n  :- Long\n  1)",
   :var-type "macro",
   :line 769,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([sym doc-str t] [sym t]),
   :name "defalias",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1293",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defalias",
   :doc
   "Define a recursive type alias. Takes an optional doc-string as a second\nargument.\n\nUpdates the corresponding var with documentation.\n\neg. (defalias MyAlias\n      \"Here is my alias\"\n      (U nil String))\n\n    ;; recursive alias\n    (defalias Expr\n      (U '{:op ':if :test Expr :then Expr :else Expr}\n         '{:op ':const :val Any}))",
   :var-type "macro",
   :line 1293,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :forms
   [(defn
     kw-args?
     name
     docstring?
     attr-map?
     [param :- type *]
     :-
     type
     exprs*)
    (defn
     kw-args?
     name
     docstring?
     attr-map?
     ([param :- type *] :- type exprs*)
     +)],
   :name "defn",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defn",
   :doc
   "Like defn, but expands to clojure.core.typed/fn. If a polymorphic binder is\nsupplied before the var name, expands to clojure.core.typed/pfn.\n\neg. (defn fname [a :- Number, b :- (U Symbol nil)] :- Integer ...)\n\n;annotate return\n(defn fname [a :- String] :- String ...)\n\n;multi-arity\n(defn fname \n  ([a :- String] :- String ...)\n  ([a :- String, b :- Number] :- Long ...))\n\n;polymorphic function\n(defn :forall [x y]\n  fname \n  ([a :- x] :- (Coll y) ...)\n  ([a :- Str, b :- y] :- y ...))",
   :var-type "macro",
   :line 228,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([name & fdecl]),
   :forms
   [(defn> name docstring? :- type [param :- type *] exprs*)
    (defn> name docstring? (:- type [param :- type *] exprs*) +)],
   :name "defn>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L736",
   :deprecated "0.2.57",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defn>",
   :doc
   "DEPRECATED: Use defn\n\nLike defn, but with annotations. Annotations are mandatory for\nparameters and for return type.\n\neg. (defn> fname :- Integer [a :- Number, b :- (U Symbol nil)] ...)\n\n;annotate return\n(defn> fname :- String [a :- String] ...)\n\n;multi-arity\n(defn> fname \n  (:- String [a :- String] ...)\n  (:- Long   [a :- String, b :- Number] ...))",
   :var-type "macro",
   :line 736,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& body]),
   :name "defprotocol",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defprotocol",
   :doc
   "Like defprotocol, but with optional type annotations.\n\nOmitted annotations default to Any. The first argument\nof a protocol cannot be annotated.\n\nAdd a binder before the protocol name to define a polymorphic\nprotocol. A binder before the method name defines a polymorphic\nmethod, however a method binder must not shadow type variables\nintroduced by a protocol binder.\n\nReturn types for each method arity can be annotated.\n\nUnlike clojure.core/defprotocol, successive methods can\nhave the same arity. Semantically, providing multiple successive\nmethods of the same arity is the same as just providing the left-most\nmethod. However the types for these methods will be accumulated into\na Fn type.\n\neg. ;annotate single method\n(defprotocol MyProtocol\n  (a [this a :- Integer] :- Number))\n\n;polymorphic protocol\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  (a [this a :- Integer] :- Number))\n\n;multiple types for the same method\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  (a [this a :- Integer] :- Integer\n     [this a :- Long] :- Long\n     [this a :- Number] :- Number))\n\n;polymorphic method+protocol\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  ([y] a [this a :- x, b :- y] :- y))\n",
   :var-type "macro",
   :line 138,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([& body]),
   :name "defprotocol>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1073",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defprotocol>",
   :doc
   "DEPRECATED: use clojure.core.typed/defprotocol\n\nLike defprotocol, but required for type checking\nits macroexpansion.\n\neg. (defprotocol> MyProtocol\n      (a [this]))",
   :var-type "macro",
   :line 1073,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([seq-exprs & body]),
   :name "doseq",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L577",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/doseq",
   :doc
   "Like clojure.core/doseq with optional annotations.\n\n:let option uses clojure.core.typed/let\n\neg.\n(doseq [a :- (U nil AnyInteger) [1 nil 2 3]\n        :when a]\n   (inc a))",
   :var-type "macro",
   :line 577,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([seq-exprs & body]),
   :name "doseq>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L472",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/doseq>",
   :doc
   "DEPRECATED: use clojure.core.typed/doseq\n\nLike doseq but requires annotation for each loop variable: \n[a [1 2]] becomes [a :- Long [1 2]]\n\neg.\n(doseq> [a :- (U nil AnyInteger) [1 nil 2 3]\n         :when a]\n   (inc a))",
   :var-type "macro",
   :line 472,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bindings & body]),
   :name "dotimes",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L171",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/dotimes",
   :doc
   "Like clojure.core/dotimes, but with optional annotations.\n\nIf annotation for binding is omitted, defaults to Int.\n\neg. (dotimes [_ 100]\n      (println \"like normal\"))\n\n    (dotimes [x :- Num, 100.123]\n      (println \"like normal\" x))",
   :var-type "macro",
   :line 171,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bindings & body]),
   :name "dotimes>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L148",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/dotimes>",
   :doc
   "DEPRECATED: Use clojure.core.typed/dotimes\n\nLike dotimes.\n\neg. (dotimes> [_ 100]\n      (println \"like normal\"))",
   :var-type "macro",
   :line 148,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([]),
   :name "envs",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2313",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/envs",
   :doc
   "Returns a map of type environments, according to the current state of the\ntype checker.\n\nOutput map:\n- :vars      map from var symbols to their verbosely printed types\n- :aliases   map from alias var symbols (made with defalias) to their verbosely printed types\n- :special-types  a set of Vars that are special to the type checker (like Any, U, I)\n",
   :var-type "function",
   :line 2313,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& forms]),
   :forms
   [(fn name? [param :- type* & param :- type * ?] :- type? exprs*)
    (fn
     name?
     ([param :- type* & param :- type * ?] :- type? exprs*)
     +)],
   :name "fn",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/fn",
   :doc
   "Like clojure.core/fn, but with optional annotations.\n\neg. ;these forms are equivalent\n    (fn [a] b)\n    (fn [a :- Any] b)\n    (fn [a :- Any] :- Any b)\n    (fn [a] :- Any b)\n\n    ;annotate return\n    (fn [a :- String] :- String body)\n\n    ;named fn\n    (fn fname [a :- String] :- String body)\n\n    ;rest parameter\n    (fn [a :- String & b :- Number *] body)\n\n    ;dotted rest parameter\n    (fn [a :- String & b :- Number ... x] body)\n\n    ;multi-arity\n    (fn fname \n      ([a :- String] :- String ...)\n      ([a :- String, b :- Number] :- String ...))\n\n    ; polymorphic binder\n    (fn :forall [x y z]\n      fname \n      ([a :- String] :- String ...)\n      ([a :- String, b :- Number] :- String ...))\n",
   :var-type "macro",
   :line 64,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
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
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L693",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/fn>",
   :doc
   "DEPRECATED: use clojure.core.typed/fn\n\nLike fn, but with annotations. Annotations are mandatory\nfor parameters, with optional annotations for return type.\nIf fn is named, return type annotation is mandatory.\n\nSuggested idiom: use commas between parameter annotation triples.\n\neg. (fn> [a :- Number, b :- (U Symbol nil)] ...)\n\n    ;annotate return\n    (fn> :- String [a :- String] ...)\n\n    ;named fn\n    (fn> fname :- String [a :- String] ...)\n\n    ;multi-arity\n    (fn> fname \n      (:- String [a :- String] ...)\n      (:- Long   [a :- String, b :- Number] ...))",
   :var-type "macro",
   :line 693,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([seq-exprs & maybe-ann-body-expr]),
   :name "for",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L331",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/for",
   :doc
   "Like clojure.core/for with optional type annotations.\n\nAll types default to Any.\n\nThe :let option uses clojure.core.typed/let.\n\neg. (for [a :- (U nil Int) [1 nil 2 3]\n          :when a]\n      :- Number\n      (inc a))",
   :var-type "macro",
   :line 331,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([tk ret-ann seq-exprs body-expr]),
   :name "for>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L198",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/for>",
   :doc
   "DEPRECATED: use clojure.core.typed/for\n\nLike for but requires annotation for each loop variable: [a [1 2]] becomes [a :- Long [1 2]]\nAlso requires annotation for return type.\n\neg. (for> :- Number\n      [a :- (U nil AnyInteger) [1 nil 2 3]\n       :when a]\n      (inc a))",
   :var-type "macro",
   :line 198,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([inst-of & types]),
   :name "inst",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L112",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/inst",
   :doc
   "Instantiate a polymorphic type with a number of types.\n\neg. (inst foo-fn t1 t2 t3 ...)",
   :var-type "macro",
   :line 112,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([inst-of & types]),
   :name "inst-ctor",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L120",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/inst-ctor",
   :doc
   "Instantiate a call to a constructor with a number of types.\nFirst argument must be an immediate call to a constructor.\nReturns exactly the instantiatee (the first argument).\n\neg. (inst-ctor (PolyCtor. a b c)\n               t1 t2 ...)",
   :var-type "macro",
   :line 120,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists
   ([cljt coll] [javat cljt coll] [into-array-syn javat cljt coll]),
   :name "into-array>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1642",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/into-array>",
   :doc
   "Make a Java array with Java class javat and Typed Clojure type\ncljt. Resulting array will be of type javat, but elements of coll must be under\ncljt. cljt should be a subtype of javat (the same or more specific).\n\n*Temporary hack*\ninto-array-syn is exactly the syntax to put as the first argument to into-array.\nCalling resolve on this syntax should give the correct class.",
   :var-type "macro",
   :line 1642,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bvec & forms]),
   :forms [(let [binding :- type? init*] exprs*)],
   :name "let",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/let",
   :doc
   "Like clojure.core/let but supports optional type annotations.\n\neg. (let [a :- Type, b\n          a2 1.2]\n      body)",
   :var-type "macro",
   :line 118,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([fn-specs-and-annotations & body]),
   :forms [(letfn> [fn-spec-or-annotation*] expr*)],
   :name "letfn>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L801",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/letfn>",
   :doc
   "Like letfn, but each function spec must be annotated.\n\neg. (letfn> [a :- [Number -> Number]\n             (a [b] 2)\n\n             c :- [Symbol -> nil]\n             (c [s] nil)]\n      ...)",
   :var-type "macro",
   :line 801,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([]),
   :name "load-if-needed",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L51",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/load-if-needed",
   :doc "Load and initialize all of core.typed if not already",
   :var-type "function",
   :line 51,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([bindings & exprs]),
   :forms [(loop [binding :- type? init*] exprs*)],
   :name "loop",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/loop",
   :doc
   "Like clojure.core/loop, and supports optional type annotations.\nArguments default to a generalised type based on the initial value.\n\neg. (loop [a :- Number 1\n           b :- (U nil Number) nil]\n      ...)",
   :var-type "macro",
   :line 102,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([bndings* & forms]),
   :forms [(loop> [binding :- type init*] exprs*)],
   :name "loop>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1091",
   :deprecated "0.2.45",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/loop>",
   :doc
   "DEPRECATED: use clojure.core.typed/loop\n\nLike loop, except loop variables require annotation.\n\nSuggested idiom: use a comma between the type and the initial\nexpression.\n\neg. (loop> [a :- Number, 1\n            b :- (U nil Number), nil]\n      ...)",
   :var-type "macro",
   :line 1091,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([mname]),
   :name "method-type",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L64",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/method-type",
   :doc
   "Given a method symbol, print the core.typed types assigned to it.\nIntended for use at the REPL.",
   :var-type "function",
   :line 64,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([msym mmap]),
   :name "nilable-param",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1681",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/nilable-param",
   :doc
   "Override which parameters in qualified method msym may accept\nnilable values. If the parameter is a parameterised type or\nan Array, this also declares the parameterised types and the Array type as nilable.\n\nmmap is a map mapping arity parameter number to a set of parameter\npositions (integers). If the map contains the key :all then this overrides\nother entries. The key can also be :all, which declares all parameters nilable.",
   :var-type "macro",
   :line 1681,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([msym arities]),
   :name "non-nil-return",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1664",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/non-nil-return",
   :doc
   "Override the return type of fully qualified method msym to be non-nil.\nTakes a set of relevant arities,\nrepresented by the number of parameters it takes (rest parameter counts as one),\nor :all which overrides all arities.\n\neg. ; must use full class name\n    (non-nil-return java.lang.Class/getDeclaredMethod :all)",
   :var-type "macro",
   :line 1664,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([ctorsym typesyn]),
   :name "override-constructor",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2074",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/override-constructor",
   :doc "Override all constructors for Class ctorsym with type.",
   :var-type "macro",
   :line 2074,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([methodsym typesyn]),
   :name "override-method",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2085",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/override-method",
   :doc
   "Override type for qualified method methodsym.\n\nmethodsym identifies the method to override and should be a\nnamespace-qualified symbol in the form <class>/<method-name>.\nThe class name needs to be fully qualified.\n\ntypesyn uses the same annotation syntax as functions.\n\nUse non-nil-return instead of override-method if you want to\ndeclare that a method can never return nil.\n\nExample:\n\n  (override-method java.util.Properties/stringPropertyNames\n                   [-> (java.util.Set String)])\n\nThis overrides the return type of method stringPropertyNames\nof class java.util.Properties to be (java.util.Set String).",
   :var-type "macro",
   :line 2085,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& forms]),
   :name "pfn>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L684",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/pfn>",
   :doc
   "Define a polymorphic typed anonymous function.\n(pfn> name? [binder+] :- type? [[param :- type]* & [param :- type *]?] exprs*)\n(pfn> name? [binder+] (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)",
   :var-type "macro",
   :line 684,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([t]),
   :name "pred",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2334",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/pred",
   :doc
   "Generate a flat (runtime) predicate for type that returns true if the\nargument is a subtype of the type, otherwise false.\n\nThe current type variable and dotted type variable scope is cleared before parsing.\n\neg. ((pred Number) 1)\n    ;=> true",
   :var-type "macro",
   :line 2334,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([debug-str]),
   :name "print-env",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1695",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/print-env",
   :doc
   "During type checking, print the type environment to *out*,\npreceeded by literal string debug-str.",
   :var-type "function",
   :line 1695,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([debug-string frm]),
   :name "print-filterset",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L89",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/print-filterset",
   :doc
   "During type checking, print the filter set attached to form, \npreceeded by literal string debug-string.\nReturns nil.\n\neg. (let [s (seq (get-a-seqable))]\n      (print-filterset \"Here now\" s))",
   :var-type "function",
   :line 89,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& args]),
   :name "ref",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ref",
   :doc
   "Like ref, but with optional type annotations.\n\nSame as (ref (ann-form init t) args*)\n\neg. (ref 1) : (Ref1 (Value 1))\n    (ref :- Num, 1) : (Ref1 Num)",
   :var-type "macro",
   :line 213,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([t init & args]),
   :name "ref>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1242",
   :deprecated "0.2.58",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ref>",
   :doc
   "DEPRECATED: use clojure.core.typed/ref\n\nLike ref, but creates a Ref1 of type t.\n\nSame as (ref (ann-form init t) args*)\n\neg. (ref> Number 1)\n    (ref> (Vec Any) [])",
   :var-type "macro",
   :line 1242,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([]),
   :name "reset-caches",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L56",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/reset-caches",
   :doc "Reset internal type caches.",
   :var-type "function",
   :line 56,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([nsyms]),
   :name "statistics",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2296",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/statistics",
   :doc
   "Takes a collection of namespace symbols and returns a map mapping the namespace\nsymbols to a map of data",
   :var-type "function",
   :line 2296,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([& body]),
   :name "tc-ignore",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/tc-ignore",
   :doc "Ignore forms in body during type checking",
   :var-type "macro",
   :line 184,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([& args]),
   :name "typed-deps",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2113",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/typed-deps",
   :doc
   "Declare namespaces which should be checked before the current namespace.\nAccepts any number of symbols. Only has effect via check-ns.\n\neg. (typed-deps clojure.core.typed.holes\n                myns.types)",
   :var-type "macro",
   :line 2113,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([varsym typesyn]),
   :name "untyped-var",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L1707",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/untyped-var",
   :doc "Check a given var has the specified type at runtime.",
   :var-type "macro",
   :line 1707,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([] [nsyms-or-nsym]),
   :name "var-coverage",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2304",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/var-coverage",
   :doc
   "Summarises annotated var coverage statistics to *out*\nfor namespaces nsyms, a collection of symbols or a symbol/namespace.\nDefaults to the current namespace if no argument provided.",
   :var-type "function",
   :line 2304,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([sym]),
   :name "var>",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2142",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/var>",
   :doc
   "Like var, but resolves at runtime like ns-resolve and is understood by\nthe type checker. sym must be fully qualified (without aliases).\n\neg. (var> clojure.core/+)",
   :var-type "macro",
   :line 2142,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([]),
   :name "warn-on-unannotated-vars",
   :namespace "clojure.core.typed",
   :source-url
   "https://github.com/clojure/core.typed/blob/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj#L2156",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/925dfe80589f31179c3aad1eff62d2daf6c9397c/module-rt/src/main/clojure/clojure/core/typed.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/warn-on-unannotated-vars",
   :doc
   "Allow unannotated vars in the current namespace. \n\nEmits a warning instead of a type error when checking\na def without a corresponding expected type.\n\nDisables automatic inference of `def` expressions.\n\neg. (warn-on-unannotated-vars)",
   :var-type "macro",
   :line 2156,
   :file "module-rt/src/main/clojure/clojure/core/typed.clj"}
  {:arglists ([b & body]),
   :name "when-let-fail",
   :namespace "clojure.core.typed",
   :source-url nil,
   :raw-source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/when-let-fail",
   :doc
   "Like when-let, but fails if the binding yields a false value.",
   :var-type "macro",
   :line 191,
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([form expected expected-provided?]),
   :name "check-form-cljs",
   :namespace "clojure.core.typed.check-form-cljs",
   :source-url
   "https://github.com/clojure/core.typed/blob/935e7c9277a1954cdd011907cb5a8abe3134b05e/module-check/src/main/clojure/clojure/core/typed/check_form_cljs.clj#L25",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/935e7c9277a1954cdd011907cb5a8abe3134b05e/module-check/src/main/clojure/clojure/core/typed/check_form_cljs.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check-form-cljs/check-form-cljs",
   :doc
   "Check a single form with an optional expected type.\nIntended to be called from Clojure. For evaluation at the Clojurescript\nREPL see cf.",
   :var-type "function",
   :line 25,
   :file
   "module-check/src/main/clojure/clojure/core/typed/check_form_cljs.clj"}
  {:arglists ([ns-or-syms & opt]),
   :name "check-ns-info",
   :namespace "clojure.core.typed.check-ns-clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/7d7264105b8f1d07915413cb75743a4f6b847dc4/module-check/src/main/clojure/clojure/core/typed/check_ns_clj.clj#L6",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/7d7264105b8f1d07915413cb75743a4f6b847dc4/module-check/src/main/clojure/clojure/core/typed/check_ns_clj.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check-ns-clj/check-ns-info",
   :doc
   "Same as check-ns, but returns a map of results from type checking the\nnamespace.\n\nOptions\n- :collect-only    Don't type check the given namespace/s, but collect the \n                   top level type annotations like ann, ann-record.\n- :type-provided?  If true, use the expected type to check the form\n- :profile         Use Timbre to profile the type checker. Timbre must be\n                   added as a dependency.\n- :file-mapping    If true, return map provides entry :file-mapping, a hash-map\n                   of (Map '{:line Int :column Int :file Str} Str).",
   :var-type "function",
   :line 6,
   :file
   "module-check/src/main/clojure/clojure/core/typed/check_ns_clj.clj"}
  {:arglists
   ([expected
     mthods
     {:keys [recur-target-fn validate-expected-fn self-name],
      :as opt}]),
   :name "check-fni",
   :namespace "clojure.core.typed.check.fn-methods",
   :source-url
   "https://github.com/clojure/core.typed/blob/488c9279bd44cee8eacd5167b1dc4773943a8666/module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj#L78",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/488c9279bd44cee8eacd5167b1dc4773943a8666/module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.fn-methods/check-fni",
   :doc
   "Check a vector of :method AST nodes mthods against\nan expected type that is a possibly-polymorphic function\nintersection.\n\nReturns a vector in the same order as the passed in methods,\nbut each method replaced with a vector of type checked methods.",
   :var-type "function",
   :line 78,
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj"}
  {:arglists
   ([{:keys [fixed-arity], :as method}
     {:keys [dom rest drest kws], :as f}]),
   :name "expected-for-method",
   :namespace "clojure.core.typed.check.fn-methods",
   :source-url
   "https://github.com/clojure/core.typed/blob/488c9279bd44cee8eacd5167b1dc4773943a8666/module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj#L39",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/488c9279bd44cee8eacd5167b1dc4773943a8666/module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.fn-methods/expected-for-method",
   :doc
   "Takes a :method AST node and a single Function arity type,\nand returns the Function if the :method node should be checked\nagainst the Function, otherwise returns nil.",
   :var-type "function",
   :line 39,
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj"}
  {:arglists ([check {:keys [target], :as expr} expected]),
   :name "check-monitor",
   :namespace "clojure.core.typed.check.monitor",
   :source-url
   "https://github.com/clojure/core.typed/blob/81a3bc793db822eaeee5d59f1498fbaa24639cdd/module-check/src/main/clojure/clojure/core/typed/check/monitor.clj#L11",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/81a3bc793db822eaeee5d59f1498fbaa24639cdd/module-check/src/main/clojure/clojure/core/typed/check/monitor.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.monitor/check-monitor",
   :doc
   "monitor-enter and monitor-exit both take any object and return nil",
   :var-type "function",
   :line 11,
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/monitor.clj"}
  {:arglists ([{:keys [val], :as expr} expected quoted?]),
   :name "check-value",
   :namespace "clojure.core.typed.check.value",
   :source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/check/value.clj#L47",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/check/value.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.value/check-value",
   :doc
   "Given a :const node and an expected type returns a new :const\nnode annotated with its type.\n\nquoted? should be true if this :const node is nested inside a\n:quote node, otherwise should be false",
   :var-type "function",
   :line 47,
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/value.clj"}
  {:arglists ([val]),
   :name "unquote-val",
   :namespace "clojure.core.typed.check.value",
   :source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/check/value.clj#L24",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/check/value.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.value/unquote-val",
   :doc
   "Convert the syntax representation of a unevaluated value to\nan actual evaluated value.\n\neg. ['a] is represented as [(quote a)] and evaluates to [a]",
   :var-type "function",
   :line 24,
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/value.clj"}
  {:arglists ([nsym {:keys [ast-for-ns collect-asts collect-ns]}]),
   :name "collect-ns*",
   :namespace "clojure.core.typed.collect-utils",
   :source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/collect_utils.clj#L53",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/collect_utils.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.collect-utils/collect-ns*",
   :doc
   "Collect type annotations and dependency information\nfor namespace symbol nsym, and recursively check \ndeclared typed namespace dependencies.",
   :var-type "function",
   :line 53,
   :file
   "module-check/src/main/clojure/clojure/core/typed/collect_utils.clj"}
  {:arglists ([n]),
   :name "create-env",
   :namespace "clojure.core.typed.current-impl",
   :source-url
   "https://github.com/clojure/core.typed/blob/0947387913babb0e8db52b560a3c0e42b45cb40b/module-rt/src/main/clojure/clojure/core/typed/current_impl.clj#L7",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/0947387913babb0e8db52b560a3c0e42b45cb40b/module-rt/src/main/clojure/clojure/core/typed/current_impl.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.current-impl/create-env",
   :doc
   "For name n, creates defs for {n}, {n}-kw, add-{n},\nand reset-{n}!",
   :var-type "macro",
   :line 7,
   :file
   "module-rt/src/main/clojure/clojure/core/typed/current_impl.clj"}
  {:arglists ([]),
   :name "->NoisyHole",
   :namespace "clojure.core.typed.hole",
   :source-url
   "https://github.com/clojure/core.typed/blob/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj#L17",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/->NoisyHole",
   :doc
   "Positional factory function for class clojure.core.typed.hole.NoisyHole.",
   :var-type "function",
   :line 17,
   :file "module-check/src/main/clojure/clojure/core/typed/hole.clj"}
  {:arglists ([]),
   :name "noisy-hole",
   :namespace "clojure.core.typed.hole",
   :source-url
   "https://github.com/clojure/core.typed/blob/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj#L20",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/noisy-hole",
   :doc
   "A noisy hole. The type system will complain when\n(noisy-hole) is used in positions that expect a type\nmore specific than Object or Any.\nUse (noisy-hole) as a placeholder for code.\nThrows an exception when evaluted.",
   :var-type "function",
   :line 20,
   :file "module-check/src/main/clojure/clojure/core/typed/hole.clj"}
  {:arglists ([]),
   :name "silent-hole",
   :namespace "clojure.core.typed.hole",
   :source-url
   "https://github.com/clojure/core.typed/blob/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj#L8",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/silent-hole",
   :doc
   "A silent hole. (silent-hole) passes for any other type\nwhen type checking.\nUse (silent-hole) as a placeholder for code.\nThrows an exception when evaluted.",
   :var-type "function",
   :line 8,
   :file "module-check/src/main/clojure/clojure/core/typed/hole.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/NoisyHole",
   :namespace "clojure.core.typed.hole",
   :var-type "type",
   :name "NoisyHole"}
  {:arglists ([base-resource-path]),
   :name "default-load1",
   :namespace "clojure.core.typed.lang",
   :source-url
   "https://github.com/clojure/core.typed/blob/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj#L32",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/default-load1",
   :doc "Roughly equivalent to clojure.core/load.",
   :var-type "function",
   :line 32,
   :file "module-check/src/main/clojure/clojure/core/typed/lang.clj"}
  {:arglists ([& paths]),
   :name "extensible-load",
   :namespace "clojure.core.typed.lang",
   :source-url
   "https://github.com/clojure/core.typed/blob/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj#L47",
   :added "1.0",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/extensible-load",
   :doc
   "Loads Clojure code from resources in classpath. A path is interpreted as\nclasspath-relative if it begins with a slash or relative to the root\ndirectory for the current namespace otherwise.",
   :var-type "function",
   :line 47,
   :file "module-check/src/main/clojure/clojure/core/typed/lang.clj"}
  {:arglists ([res]),
   :name "file-lang",
   :namespace "clojure.core.typed.lang",
   :source-url
   "https://github.com/clojure/core.typed/blob/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj#L38",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/file-lang",
   :doc "Returns the :lang entry in ns form in the given namespace.",
   :var-type "function",
   :line 38,
   :file "module-check/src/main/clojure/clojure/core/typed/lang.clj"}
  {:file "module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj#L27",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/lang-dispatch",
   :namespace "clojure.core.typed.lang",
   :line 27,
   :var-type "var",
   :doc
   "A map from :lang entries to their corresponding `load` alternatives.",
   :name "lang-dispatch"}
  {:file "module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj#L264",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/monkey-patch-extensible-load",
   :namespace "clojure.core.typed.lang",
   :line 264,
   :var-type "var",
   :doc
   "A no-argument function that installs the core.typed `load` function\nover clojure.core/load.",
   :name "monkey-patch-extensible-load"}
  {:arglists ([& args]),
   :name "require",
   :namespace "clojure.core.typed.lang",
   :source-url
   "https://github.com/clojure/core.typed/blob/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj#L185",
   :added "1.0",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/require",
   :doc
   "Loads libs, skipping any that are already loaded. Each argument is\neither a libspec that identifies a lib, a prefix list that identifies\nmultiple libs whose names share a common prefix, or a flag that modifies\nhow all the identified libs are loaded. Use :require in the ns macro\nin preference to calling this directly.\n\nLibs\n\nA 'lib' is a named set of resources in classpath whose contents define a\nlibrary of Clojure code. Lib names are symbols and each lib is associated\nwith a Clojure namespace and a Java package that share its name. A lib's\nname also locates its root directory within classpath using Java's\npackage name to classpath-relative path mapping. All resources in a lib\nshould be contained in the directory structure under its root directory.\nAll definitions a lib makes should be in its associated namespace.\n\n'require loads a lib by loading its root resource. The root resource path\nis derived from the lib name in the following manner:\nConsider a lib named by the symbol 'x.y.z; it has the root directory\n<classpath>/x/y/, and its root resource is <classpath>/x/y/z.clj. The root\nresource should contain code to create the lib's namespace (usually by using\nthe ns macro) and load any additional lib resources.\n\nLibspecs\n\nA libspec is a lib name or a vector containing a lib name followed by\noptions expressed as sequential keywords and arguments.\n\nRecognized options:\n:as takes a symbol as its argument and makes that symbol an alias to the\n  lib's namespace in the current namespace.\n:refer takes a list of symbols to refer from the namespace or the :all\n  keyword to bring in all public vars.\n\nPrefix Lists\n\nIt's common for Clojure code to depend on several libs whose names have\nthe same prefix. When specifying libs, prefix lists can be used to reduce\nrepetition. A prefix list contains the shared prefix followed by libspecs\nwith the shared prefix removed from the lib names. After removing the\nprefix, the names that remain must not contain any periods.\n\nFlags\n\nA flag is a keyword.\nRecognized flags: :reload, :reload-all, :verbose\n:reload forces loading of all the identified libs even if they are\n  already loaded\n:reload-all implies :reload and also forces loading of all libs that the\n  identified libs directly or indirectly load via require or use\n:verbose triggers printing information about each load, alias, and refer\n\nExample:\n\nThe following would load the libraries clojure.zip and clojure.set\nabbreviated as 's'.\n\n(require '(clojure zip [set :as s]))",
   :var-type "function",
   :line 185,
   :file "module-check/src/main/clojure/clojure/core/typed/lang.clj"}
  {:arglists ([& args]),
   :name "use",
   :namespace "clojure.core.typed.lang",
   :source-url
   "https://github.com/clojure/core.typed/blob/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj#L249",
   :added "1.0",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/fe7ae4a816924d51983d54c477dd922f5f207625/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/use",
   :doc
   "Like 'require, but also refers to each lib's namespace using\nclojure.core/refer. Use :use in the ns macro in preference to calling\nthis directly.\n\n'use accepts additional options in libspecs: :exclude, :only, :rename.\nThe arguments and semantics for :exclude, :only, and :rename are the same\nas those documented for clojure.core/refer.",
   :var-type "function",
   :line 249,
   :file "module-check/src/main/clojure/clojure/core/typed/lang.clj"}
  {:arglists ([form ty]),
   :name "ann-form",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L130",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/ann-form",
   :doc "Annotate a form with an expected type.",
   :var-type "macro",
   :line 130,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([& args]),
   :name "atom",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L198",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/atom",
   :doc
   "Like atom, but with optional type annotations.\n\nSame as (atom (ann-form init t) args*)\n\neg. (atom 1) : (Atom1 (Value 1))\n    (atom :- Num, 1) : (Atom1 Num)",
   :var-type "macro",
   :line 198,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([name & fdecl]),
   :forms [(def name docstring? :- type? expr)],
   :name "def",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L22",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/def",
   :doc
   "Like clojure.core/def with optional type annotations\n\nNB: in Clojure it is impossible to refer a var called `def` as it is a\nspecial form. Use an alias prefix (eg. `t/def`).\n\nIf an annotation is provided, a corresponding `ann` form\nis generated, otherwise it expands identically to clojure.core/def\n\neg. ;same as clojure.core/def\n    (def vname 1)\n    \n    ;with Number `ann`\n    (def vname :- Number 1)\n\n    ;doc\n    (def vname\n      \"Docstring\"\n      :- Long\n      1)",
   :var-type "macro",
   :line 22,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([& args]),
   :forms
   [(defn
     kw-args?
     name
     docstring?
     attr-map?
     [param :- type *]
     :-
     type
     exprs*)
    (defn
     kw-args?
     name
     docstring?
     attr-map?
     ([param :- type *] :- type exprs*)
     +)],
   :name "defn",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L228",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/defn",
   :doc
   "Like defn, but expands to clojure.core.typed/fn. If a polymorphic binder is\nsupplied before the var name, expands to clojure.core.typed/pfn.\n\neg. (defn fname [a :- Number, b :- (U Symbol nil)] :- Integer ...)\n\n;annotate return\n(defn fname [a :- String] :- String ...)\n\n;multi-arity\n(defn fname \n  ([a :- String] :- String ...)\n  ([a :- String, b :- Number] :- Long ...))\n\n;polymorphic function\n(defn :forall [x y]\n  fname \n  ([a :- x] :- (Coll y) ...)\n  ([a :- Str, b :- y] :- y ...))",
   :var-type "macro",
   :line 228,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([& body]),
   :name "defprotocol",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L138",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/defprotocol",
   :doc
   "Like defprotocol, but with optional type annotations.\n\nOmitted annotations default to Any. The first argument\nof a protocol cannot be annotated.\n\nAdd a binder before the protocol name to define a polymorphic\nprotocol. A binder before the method name defines a polymorphic\nmethod, however a method binder must not shadow type variables\nintroduced by a protocol binder.\n\nReturn types for each method arity can be annotated.\n\nUnlike clojure.core/defprotocol, successive methods can\nhave the same arity. Semantically, providing multiple successive\nmethods of the same arity is the same as just providing the left-most\nmethod. However the types for these methods will be accumulated into\na Fn type.\n\neg. ;annotate single method\n(defprotocol MyProtocol\n  (a [this a :- Integer] :- Number))\n\n;polymorphic protocol\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  (a [this a :- Integer] :- Number))\n\n;multiple types for the same method\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  (a [this a :- Integer] :- Integer\n     [this a :- Long] :- Long\n     [this a :- Number] :- Number))\n\n;polymorphic method+protocol\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  ([y] a [this a :- x, b :- y] :- y))\n",
   :var-type "macro",
   :line 138,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([& forms]),
   :forms
   [(fn name? [param :- type* & param :- type * ?] :- type? exprs*)
    (fn
     name?
     ([param :- type* & param :- type * ?] :- type? exprs*)
     +)],
   :name "fn",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L64",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/fn",
   :doc
   "Like clojure.core/fn, but with optional annotations.\n\neg. ;these forms are equivalent\n    (fn [a] b)\n    (fn [a :- Any] b)\n    (fn [a :- Any] :- Any b)\n    (fn [a] :- Any b)\n\n    ;annotate return\n    (fn [a :- String] :- String body)\n\n    ;named fn\n    (fn fname [a :- String] :- String body)\n\n    ;rest parameter\n    (fn [a :- String & b :- Number *] body)\n\n    ;dotted rest parameter\n    (fn [a :- String & b :- Number ... x] body)\n\n    ;multi-arity\n    (fn fname \n      ([a :- String] :- String ...)\n      ([a :- String, b :- Number] :- String ...))\n\n    ; polymorphic binder\n    (fn :forall [x y z]\n      fname \n      ([a :- String] :- String ...)\n      ([a :- String, b :- Number] :- String ...))\n",
   :var-type "macro",
   :line 64,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([bvec & forms]),
   :forms [(let [binding :- type? init*] exprs*)],
   :name "let",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L118",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/let",
   :doc
   "Like clojure.core/let but supports optional type annotations.\n\neg. (let [a :- Type, b\n          a2 1.2]\n      body)",
   :var-type "macro",
   :line 118,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([bindings & exprs]),
   :forms [(loop [binding :- type? init*] exprs*)],
   :name "loop",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L102",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/loop",
   :doc
   "Like clojure.core/loop, and supports optional type annotations.\nArguments default to a generalised type based on the initial value.\n\neg. (loop [a :- Number 1\n           b :- (U nil Number) nil]\n      ...)",
   :var-type "macro",
   :line 102,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([fdecl name]),
   :name "parse-colon",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L14",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/parse-colon",
   :doc "Returns a vector of [provided? t args]",
   :var-type "function",
   :line 14,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([& args]),
   :name "ref",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L213",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/ref",
   :doc
   "Like ref, but with optional type annotations.\n\nSame as (ref (ann-form init t) args*)\n\neg. (ref 1) : (Ref1 (Value 1))\n    (ref :- Num, 1) : (Ref1 Num)",
   :var-type "macro",
   :line 213,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([& body]),
   :name "tc-ignore",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L184",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/tc-ignore",
   :doc "Ignore forms in body during type checking",
   :var-type "macro",
   :line 184,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([b & body]),
   :name "when-let-fail",
   :namespace "clojure.core.typed.macros",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L191",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/when-let-fail",
   :doc
   "Like when-let, but fails if the binding yields a false value.",
   :var-type "macro",
   :line 191,
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj"}
  {:arglists ([nsyms]),
   :name "statistics",
   :namespace "clojure.core.typed.statistics",
   :source-url
   "https://github.com/clojure/core.typed/blob/0947387913babb0e8db52b560a3c0e42b45cb40b/module-check/src/main/clojure/clojure/core/typed/statistics.clj#L22",
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/0947387913babb0e8db52b560a3c0e42b45cb40b/module-check/src/main/clojure/clojure/core/typed/statistics.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.statistics/statistics",
   :doc
   "Takes a collection of namespace symbols and returns a map mapping the namespace\nsymbols to a map of data",
   :var-type "function",
   :line 22,
   :file
   "module-check/src/main/clojure/clojure/core/typed/statistics.clj"}
  {:name "*trace-checker*",
   :namespace "clojure.core.typed.util-vars",
   :source-url
   "https://github.com/clojure/core.typed/blob/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj#L32",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.util-vars/*trace-checker*",
   :doc "If true, print tracing information during type checking.",
   :var-type "var",
   :line 32,
   :file "module-rt/src/main/clojure/clojure/core/typed/util_vars.clj"}
  {:name "*verbose-forms*",
   :namespace "clojure.core.typed.util-vars",
   :source-url
   "https://github.com/clojure/core.typed/blob/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj#L20",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.util-vars/*verbose-forms*",
   :doc
   "If true, print complete forms in error messages. Bind\naround a type checking form like cf or check-ns.\n\neg.\n(binding [*verbose-forms* true]\n  (cf ['deep ['deep ['deep ['deep]]]] Number))\n;=> <full form in error>",
   :var-type "var",
   :line 20,
   :file "module-rt/src/main/clojure/clojure/core/typed/util_vars.clj"}
  {:name "*verbose-types*",
   :namespace "clojure.core.typed.util-vars",
   :source-url
   "https://github.com/clojure/core.typed/blob/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj#L7",
   :dynamic true,
   :raw-source-url
   "https://github.com/clojure/core.typed/raw/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.util-vars/*verbose-types*",
   :doc
   "If true, print fully qualified types in error messages\nand return values. Bind around a type checking form like \ncf or check-ns.\n\neg. \n(binding [*verbose-types* true] \n  (cf 1 Number))\n;=> java.lang.Number",
   :var-type "var",
   :line 7,
   :file
   "module-rt/src/main/clojure/clojure/core/typed/util_vars.clj"})}

{:namespaces
 ({:doc
   "This namespace contains typed wrapper macros, type aliases\nand functions for type checking Clojure code. check-ns is the interface\nfor checking namespaces, cf for checking individual forms.",
   :name "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed-api.html",
   :source-url nil}
  {:doc
   "This namespace contains annotations and helper macros for type\nchecking core.async code. Ensure clojure.core.async is require'd\nbefore performing type checking.\n\ngo\n  use go\n\nchan\n  use chan\n\nbuffer\n  use buffer (similar for other buffer constructors)\n",
   :name "clojure.core.typed.async",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.async-api.html",
   :source-url nil}
  {:doc "Utilities for all implementations of the type checker",
   :name "clojure.core.typed.base-env-common",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.base-env-common-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/base_env_common.clj"}
  {:doc nil,
   :name "clojure.core.typed.check-form-cljs",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check-form-cljs-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/935e7c9277a1954cdd011907cb5a8abe3134b05e/module-check/src/main/clojure/clojure/core/typed/check_form_cljs.clj"}
  {:doc nil,
   :name "clojure.core.typed.check-ns-clj",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check-ns-clj-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/7d7264105b8f1d07915413cb75743a4f6b847dc4/module-check/src/main/clojure/clojure/core/typed/check_ns_clj.clj"}
  {:doc nil,
   :name "clojure.core.typed.check.def",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.def-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj"}
  {:doc nil,
   :name "clojure.core.typed.check.fn-methods",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.fn-methods-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/488c9279bd44cee8eacd5167b1dc4773943a8666/module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj"}
  {:doc nil,
   :name "clojure.core.typed.check.monitor",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.monitor-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/81a3bc793db822eaeee5d59f1498fbaa24639cdd/module-check/src/main/clojure/clojure/core/typed/check/monitor.clj"}
  {:doc nil,
   :name "clojure.core.typed.check.special.ann-form",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.special.ann-form-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj"}
  {:doc nil,
   :name "clojure.core.typed.check.value",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.check.value-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/check/value.clj"}
  {:doc nil,
   :name "clojure.core.typed.collect-utils",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.collect-utils-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/collect_utils.clj"}
  {:doc
   "A contract system a la racket/contract.\n\nMain entry point is the `contract` macro.",
   :name "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.contract-api.html",
   :source-url nil}
  {:doc nil,
   :name "clojure.core.typed.current-impl",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.current-impl-api.html",
   :source-url nil}
  {:doc
   "This namespace contains easy tools for hole driven development",
   :name "clojure.core.typed.hole",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.hole-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj"}
  {:doc
   "Extensible languages in Clojure, a la Racket's #lang.\n\nThis is a simple library that monkey patches clojure.core/load\nto be extensible to different backends.\n\n`monkey-patch-extensible-load` does the actual monkey-patching and\nmust be called explicitly.\n\n`lang-dispatch` is a map from keywords to alternative `load` functions\n(of type [String -> nil]). The corresponding function will be used to\nload a file according its :lang metadata entry in the `ns` form.\n\nTo add a new implementation, use\n  (alter-var-root lang-dispatch assoc :new-impl my-load)\n\neg. A file with a `ns` form\n      (ns fancy-ns-form\n        {:lang :new-impl})\n    will use `my-load` to load the file.\n",
   :name "clojure.core.typed.lang",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.lang-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj"}
  {:doc nil,
   :name "clojure.core.typed.load",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.load-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/261cc6bec6ae84767f7b64c848f61ec3a4f94ee3/module-check/src/main/clojure/clojure/core/typed/load.clj"}
  {:doc nil,
   :name "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.macros-api.html",
   :source-url nil}
  {:doc
   "Adds runtime checks where annotations are instead of type checking",
   :name "clojure.core.typed.runtime-check",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.runtime-check-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/runtime_check.clj"}
  {:doc nil,
   :name "clojure.core.typed.statistics",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.statistics-api.html",
   :source-url
   "https://github.com/clojure/core.typed/blob/0947387913babb0e8db52b560a3c0e42b45cb40b/module-check/src/main/clojure/clojure/core/typed/statistics.clj"}
  {:doc nil,
   :name "clojure.core.typed.util-vars",
   :wiki-url
   "http://clojure.github.com/core.typed/clojure.core.typed.util-vars-api.html",
   :source-url nil}),
 :vars
 ({:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ASeq",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A sequential seq returned from clojure.core/seq",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ASeq",
   :forms [(ASeq t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "AVec",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A persistent vector returned from clojure.core/vector (and others)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/AVec",
   :forms [(AVec t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Agent1",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "An agent that can read and write type x.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Agent1",
   :forms [(Agent1 t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Agent2",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "An agent that can write type w and read type r.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Agent2",
   :forms [(Agent2 t t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "All",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1476",
   :line 1476,
   :var-type "type alias",
   :arglists nil,
   :doc "A polymorphic binder",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/All",
   :forms [(All binder type)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Any",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1336",
   :line 1336,
   :var-type "type alias",
   :arglists nil,
   :doc "Any is the top type that contains all types.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Any",
   :forms [Any]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "AnyInteger",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A type that returns true for clojure.core/integer?",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/AnyInteger",
   :forms [AnyInteger]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "AnyValue",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1341",
   :line 1341,
   :var-type "type alias",
   :arglists nil,
   :doc "AnyValue contains all Value singleton types",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/AnyValue",
   :forms [AnyValue]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Assoc",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1455",
   :line 1455,
   :var-type "type alias",
   :arglists nil,
   :doc "A type representing an assoc operation",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Assoc",
   :forms [(Assoc type type-pairs*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Atom1",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "An atom that can read and write type x.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Atom1",
   :forms [(Atom1 t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Atom2",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "An atom that can write type w and read type r.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Atom2",
   :forms [(Atom2 t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "BlockingDeref",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure blocking derefable (see clojure.core/deref).",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/BlockingDeref",
   :forms [(BlockingDeref t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Bool",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A boolean",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Bool",
   :forms [Bool]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Coll",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A persistent collection with member type x.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Coll",
   :forms [(Coll t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "CountRange",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1374",
   :line 1374,
   :var-type "type alias",
   :arglists nil,
   :doc "A type representing a range of counts for a collection",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/CountRange",
   :forms [(CountRange Integer) (CountRange Integer Integer)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Delay",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure delay (see clojure.core/{delay,force}).",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Delay",
   :forms [(Delay t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Deref",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure derefable (see clojure.core/deref).",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Deref",
   :forms [(Deref t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Difference",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1385",
   :line 1385,
   :var-type "type alias",
   :arglists nil,
   :doc
   "Difference represents a difference of types.\n\n(Difference t s) is the same as type t with type s removed.\n\neg. (Difference (U Int Long) Int) => Long\n    (Difference (U Num nil) nil)  => Num\n",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Difference",
   :forms [(Difference type type type*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Dissoc",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1460",
   :line 1460,
   :var-type "type alias",
   :arglists nil,
   :doc "A type representing a dissoc operation",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Dissoc",
   :forms [(Dissoc type type*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "EmptyCount",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "The type of all things with count 0. Use as part of an intersection.\neg. See EmptySeqable.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/EmptyCount",
   :forms [EmptyCount]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "EmptySeqable",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A type that can be used to create a sequence of member type x\nwith count 0.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/EmptySeqable",
   :forms [(EmptySeqable t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ExInfo",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure custom exception type.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ExInfo",
   :forms [ExInfo]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ExactCount",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1380",
   :line 1380,
   :var-type "type alias",
   :arglists nil,
   :doc "A type representing a precise count for a collection",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ExactCount",
   :forms [(ExactCount Integer)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Fn",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A type that returns true for clojure.core/fn?",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Fn",
   :forms [Fn]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Future",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure future (see clojure.core/{future-call,future}).",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Future",
   :forms [(Future t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Get",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1465",
   :line 1465,
   :var-type "type alias",
   :arglists nil,
   :doc "A type representing a get operation",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Get",
   :forms [(Get type type) (Get type type type)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "HMap",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1408",
   :line 1408,
   :var-type "type alias",
   :arglists nil,
   :doc "HMap is a type for heterogeneous maps.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HMap",
   :forms
   [(HMap
     :mandatory
     {Constant Type*}
     :optional
     {Constant Type*}
     :absent-keys
     #{Constant*}
     :complete?
     Boolean)
    '{Constant Type*}]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "HSeq",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1424",
   :line 1424,
   :var-type "type alias",
   :arglists nil,
   :doc "HSeq is a type for heterogeneous seqs",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HSeq",
   :forms
   [(HSeq [fixed*] :filter-sets [FS*] :objects [obj*])
    (HSeq [fixed* rest *] :filter-sets [FS*] :objects [obj*])
    (HSeq
     [fixed* drest ... bound]
     :filter-sets
     [FS*]
     :objects
     [obj*])]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "HSequential",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1417",
   :line 1417,
   :var-type "type alias",
   :arglists nil,
   :doc
   "HSequential is a type for heterogeneous sequential collections",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HSequential",
   :forms
   [(HSequential [fixed*] :filter-sets [FS*] :objects [obj*])
    (HSequential [fixed* rest *] :filter-sets [FS*] :objects [obj*])
    (HSequential
     [fixed* drest ... bound]
     :filter-sets
     [FS*]
     :objects
     [obj*])]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "HSet",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1431",
   :line 1431,
   :var-type "type alias",
   :arglists nil,
   :doc
   "HSet is a type for heterogeneous sets.\nTakes a set of simple values. By default\n:complete? is true.\n\neg. (HSet #{:a :b :c} :complete? true)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HSet",
   :forms [(HSet #{fixed*} :complete? Boolean)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "HVec",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1396",
   :line 1396,
   :var-type "type alias",
   :arglists nil,
   :doc
   "HVec is a type for heterogeneous vectors.\nIt extends clojure.core.typed/Vec and is a subtype\nof clojure.core.typed/HSequential.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/HVec",
   :forms
   [(HVec [fixed*] :filter-sets [FS*] :objects [obj*])
    (HVec [fixed* type *] :filter-sets [FS*] :objects [obj*])
    (HVec [fixed* type ... bound] :filter-sets [FS*] :objects [obj*])
    '[fixed*]
    '[fixed* type *]
    '[fixed* type ... bound]]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Hierarchy",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A hierarchy for use with derive, isa? etc.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Hierarchy",
   :forms [Hierarchy]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "I",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1357",
   :line 1357,
   :var-type "type alias",
   :arglists nil,
   :doc "I represents an intersection of types",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/I",
   :forms [(I type*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "IFn",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1440",
   :line 1440,
   :var-type "type alias",
   :arglists nil,
   :doc "An ordered intersection type of function arities.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/IFn",
   :forms
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
     {:id Foo, :path Bar}]]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Id",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "The identity function at the type level.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Id",
   :forms [Id]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Int",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A type that returns true for clojure.core/integer?",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Int",
   :forms [Int]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Keyword",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A keyword",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Keyword",
   :forms [Keyword]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Kw",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A keyword",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Kw",
   :forms [Kw]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "List",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure persistent list.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/List",
   :forms [(List t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Map",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A persistent map with keys k and vals v.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Map",
   :forms [(Map t t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Multi",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure multimethod.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Multi",
   :forms [Multi]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Namespace",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A namespace",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Namespace",
   :forms [Namespace]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Nilable",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A union of x and nil.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Nilable",
   :forms [(Nilable t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "NilableNonEmptyASeq",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "The result of clojure.core/seq.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NilableNonEmptyASeq",
   :forms [(NilableNonEmptyASeq t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "NilableNonEmptySeq",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A persistent sequence of member type x with count greater than 0, or nil.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NilableNonEmptySeq",
   :forms [(NilableNonEmptySeq t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "NonEmptyASeq",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A sequential non-empty seq retured from clojure.core/seq",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyASeq",
   :forms [(NonEmptyASeq t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "NonEmptyAVec",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A persistent vector returned from clojure.core/vector (and others) and count greater than 0.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyAVec",
   :forms [(NonEmptyAVec t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "NonEmptyColl",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A persistent collection with member type x and count greater than 0.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyColl",
   :forms [(NonEmptyColl t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "NonEmptyCount",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "The type of all things with count greater than 0. Use as part of an intersection.\neg. See NonEmptySeq",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyCount",
   :forms [NonEmptyCount]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "NonEmptyLazySeq",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A non-empty lazy sequence of type t",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyLazySeq",
   :forms [(NonEmptyLazySeq t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "NonEmptySeq",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A persistent sequence of member type x with count greater than 0.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptySeq",
   :forms [(NonEmptySeq t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "NonEmptySeqable",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A type that can be used to create a sequence of member type x\nwith count greater than 0.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptySeqable",
   :forms [(NonEmptySeqable t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "NonEmptyVec",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A persistent vector with member type x and count greater than 0.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/NonEmptyVec",
   :forms [(NonEmptyVec t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Nothing",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1351",
   :line 1351,
   :var-type "type alias",
   :arglists nil,
   :doc
   "Nothing is the bottom type that inhabits no types\nexcept itself.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Nothing",
   :forms [Nothing]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Num",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A type that returns true for clojure.core/number?",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Num",
   :forms [Num]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Option",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A union of x and nil.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Option",
   :forms [(Option t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Pred",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1448",
   :line 1448,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A predicate for the given type.\n\neg. Type for integer?: (Pred Int)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Pred",
   :forms [(Pred type)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Promise",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure promise (see clojure.core/{promise,deliver}).",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Promise",
   :forms [(Promise t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Proxy",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure proxy.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Proxy",
   :forms [Proxy]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Rec",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1471",
   :line 1471,
   :var-type "type alias",
   :arglists nil,
   :doc "A recursive type",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Rec",
   :forms [(Rec binder type)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Ref1",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A ref that can read and write type x.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Ref1",
   :forms [(Ref1 t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Ref2",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A ref that can write type w and read type r.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Ref2",
   :forms [(Ref2 w r)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Reversible",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure reversible collection.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Reversible",
   :forms [(Reversible t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Seq",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A persistent sequence of member type x.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Seq",
   :forms [(Seq t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Seqable",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A type that can be used to create a sequence of member type x.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Seqable",
   :forms [(Seqable t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Sequential",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A sequential collection.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Sequential",
   :forms [Sequential]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "SequentialSeq",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A Clojure sequential sequence. Seq's aren't always Sequential.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/SequentialSeq",
   :forms [(SequentialSeq t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "SequentialSeqable",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc
   "A sequential, seqable collection. Seq's aren't always Sequential.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/SequentialSeqable",
   :forms [(SequentialSeqable t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Set",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A persistent set with member type x",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Set",
   :forms [(Set t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "SortedSet",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A sorted persistent set with member type x",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/SortedSet",
   :forms [(SortedSet t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Stack",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A Clojure stack.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Stack",
   :forms [(Stack t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Str",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A string",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Str",
   :forms [Str]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Sym",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A symbol",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Sym",
   :forms [Sym]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Symbol",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A symbol",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Symbol",
   :forms [Symbol]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "TFn",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1481",
   :line 1481,
   :var-type "type alias",
   :arglists nil,
   :doc "A type function",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/TFn",
   :forms [(TFn binder type)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "U",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1346",
   :line 1346,
   :var-type "type alias",
   :arglists nil,
   :doc "U represents a union of types",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/U",
   :forms [(U type*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Val",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1362",
   :line 1362,
   :var-type "type alias",
   :arglists nil,
   :doc "A singleton type for a constant value.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Val",
   :forms [(Val Constant) 'Constant]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Value",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1368",
   :line 1368,
   :var-type "type alias",
   :arglists nil,
   :doc "A singleton type for a constant value.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Value",
   :forms [(Value Constant) 'Constant]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Var1",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "An var that can read and write type x.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Var1",
   :forms [(Var1 t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Var2",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "An var that can write type w and read type r.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Var2",
   :forms [(Var2 w r)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "Vec",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1500",
   :line 1500,
   :var-type "type alias",
   :arglists nil,
   :doc "A persistent vector with member type x.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/Vec",
   :forms [(Vec t)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ann",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1722",
   :line 1722,
   :var-type "macro",
   :arglists ([varsym typesyn]),
   :doc
   "Annotate varsym with type. If unqualified, qualify in the current namespace.\nIf varsym has metadata {:no-check true}, ignore definitions of varsym \nwhile type checking.\n\nIf annotating vars in namespaces other than the current one, a fully\nqualified symbol must be provided. Note that namespace aliases are not\nrecognised: the *full* namespace must be given in the first part of the symbol.\n\neg. ; annotate the var foo in this namespace\n    (ann foo [Number -> Number])\n\n    ; annotate a var in another namespace\n    (ann another.ns/bar [-> nil])\n \n    ; don't check this var\n    (ann ^:no-check foobar [Integer -> String])",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ann-datatype",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1773",
   :line 1773,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Annotate datatype Class name dname with expected fields.\nIf unqualified, qualify in the current namespace.\nTakes an optional type variable binder before the name.\n\nFields must be specified in the same order as presented \nin deftype, with exactly the same field names.\n\nAlso annotates datatype factories and constructors.\n\nBinder is a vector of specs. Each spec is a vector\nwith the variable name as the first entry, followed by\nkeyword arguments:\n- :variance (mandatory)\n  The declared variance of the type variable. Possible\n  values are :covariant, :contravariant and :invariant.\n- :< (optional)\n  The upper type bound of the type variable. Defaults to\n  Any, or the most general type of the same rank as the\n  lower bound.\n- :> (optional)\n  The lower type bound of the type variable. Defaults to\n  Nothing, or the least general type of the same rank as the\n  upper bound.\n\neg. ; a datatype in the current namespace\n    (ann-datatype MyDatatype [a :- Number,\n                              b :- Long])\n\n    ; a datatype in another namespace\n    (ann-datatype another.ns.TheirDatatype\n                  [str :- String,\n                   vec :- (Vec Number)])\n\n    ; a datatype, polymorphic in a\n    (ann-datatype [[a :variance :covariant]]\n                  MyPolyDatatype\n                  [str :- String,\n                   vec :- (Vec Number)\n                   ply :- (Set a)])",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-datatype",
   :forms
   [(ann-datatype dname [field :- type*] opts*)
    (ann-datatype binder dname [field :- type*] opts*)]}
  {:raw-source-url nil,
   :name "ann-form",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 130,
   :var-type "macro",
   :arglists ([form ty]),
   :doc "Annotate a form with an expected type.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-form"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ann-interface",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2006",
   :line 2006,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Annotate a possibly polymorphic interface (created with definterface) with method types.\n\nNote: Unlike ann-protocol, omit the target ('this') argument in the method signatures.\n\neg. (ann-interface IFoo\n      bar\n      (Fn [-> Any]\n          [Number Symbol -> Any])\n      baz\n      [Number -> Number])\n    (definterface IFoo\n      (bar [] [n s])\n      (baz [n]))\n\n    ; polymorphic protocol\n    ; x is scoped in the methods\n    (ann-protocol [[x :variance :covariant]]\n      IFooPoly\n      bar\n      (Fn [-> Any]\n          [Number Symbol -> Any])\n      baz\n      [Number -> Number])\n    (definterface IFooPoly\n      (bar [] [n s])\n      (baz [n]))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-interface",
   :forms
   [(ann-interface vbnd varsym & methods)
    (ann-interface varsym & methods)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ann-many",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1759",
   :line 1759,
   :var-type "macro",
   :arglists ([t & vs]),
   :doc
   "Annotate several vars with type t.\n\neg. (ann-many FakeSearch\n              web1 web2 image1 image2 video1 video2)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-many"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ann-precord",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1932",
   :line 1932,
   :var-type "macro",
   :arglists
   ([dname
     vbnd
     fields
     &
     {ancests :unchecked-ancestors, rplc :replace, :as opt}]),
   :doc
   "Annotate record Class name dname with a polymorphic binder and expected fields.\nIf unqualified, qualify in the current namespace.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-precord"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ann-protocol",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1946",
   :line 1946,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Annotate a possibly polymorphic protocol var with method types.\n\neg. (ann-protocol IFoo\n      bar\n      (Fn [IFoo -> Any]\n          [IFoo Number Symbol -> Any])\n      baz\n      [IFoo Number -> Number])\n    (defprotocol> IFoo\n      (bar [this] [this n s])\n      (baz [this n]))\n\n    ; polymorphic protocol\n    ; x is scoped in the methods\n    (ann-protocol [[x :variance :covariant]]\n      IFooPoly\n      bar\n      (Fn [(IFooPoly x) -> Any]\n          [(IFooPoly x) Number Symbol -> Any])\n      baz\n      [(IFooPoly x) Number -> Number])\n    (defprotocol> IFooPoly\n      (bar [this] [this n s])\n      (baz [this n]))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-protocol",
   :forms
   [(ann-protocol vbnd varsym & methods)
    (ann-protocol varsym & methods)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ann-record",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1861",
   :line 1861,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Annotate record Class name dname with expected fields.\nIf unqualified, qualify in the current namespace.\nTakes an optional type variable binder before the name.\n\nFields must be specified in the same order as presented \nin defrecord, with exactly the same field names.\n\nAlso annotates record factories and constructors.\n\nBinder is a vector of specs. Each spec is a vector\nwith the variable name as the first entry, followed by\nkeyword arguments:\n- :variance (mandatory)\n  The declared variance of the type variable. Possible\n  values are :covariant, :contravariant and :invariant.\n- :< (optional)\n  The upper type bound of the type variable. Defaults to\n  Any, or the most general type of the same rank as the\n  lower bound.\n- :> (optional)\n  The lower type bound of the type variable. Defaults to\n  Nothing, or the least general type of the same rank as the\n  upper bound.\n\neg. ; a record in the current namespace\n    (ann-record MyRecord [a :- Number,\n                          b :- Long])\n\n    ; a record in another namespace\n    (ann-record another.ns.TheirRecord\n                  [str :- String,\n                   vec :- (Vec Number)])\n\n    ; a record, polymorphic in a\n    (ann-record [[a :variance :covariant]]\n                MyPolyRecord\n                [str :- String,\n                 vec :- (Vec Number)\n                 ply :- (Set a)])",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ann-record",
   :forms
   [(ann-record dname [field :- type*] opts*)
    (ann-record binder dname [field :- type*] opts*)]}
  {:raw-source-url nil,
   :name "atom",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 198,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Like atom, but with optional type annotations.\n\nSame as (atom (ann-form init t) args*)\n\neg. (atom 1) : (Atom1 (Value 1))\n    (atom :- Num, 1) : (Atom1 Num)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/atom"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "atom>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1225",
   :line 1225,
   :deprecated "0.2.58",
   :var-type "macro",
   :arglists ([t init & args]),
   :doc
   "DEPRECATED: use clojure.core.typed/atom\n\nLike atom, but creates an Atom1 of type t.\n\nSame as (atom (ann-form init t) args*)\n\neg. (atom> Number 1)\n    (atom> (Vec Any) [])",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/atom>"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "cast",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2350",
   :line 2350,
   :var-type "macro",
   :arglists ([t x] [t x opt]),
   :doc
   "Cast a value to a type. Returns a new value that conforms\nto the given type, otherwise throws an error with blame.\n\neg. (cast Int 1)\n    ;=> 1\n\n    (cast Int nil)\n    ; Fail, <blame positive ...>\n\n    ((cast [Int -> Int] identity)\n     1)\n    ;=> 1\n\n    ((cast [Int -> Int] identity)\n     nil)\n    ; Fail, <blame negative ...>\n\n    (cast [Int -> Int] nil)\n    ; Fail, <blame positive ...>\n\n(defalias Options\n  (HMap :optional {:positive (U Sym Str),\n                   :negative (U Sym Str)\n                   :file (U Str nil)\n                   :line (U Int nil)\n                   :column (U Int nil)}))\n\n(IFn [Contract Any -> Any]\n     [Contract Any Options -> Any]\n\nOptions:\n- :positive   positive blame, (U Sym Str)\n- :negative   negative blame, (U Sym Str)\n- :file       file name where contract is checked, (U Str nil)\n- :line       line number where contract is checked, (U Int nil)\n- :column     column number where contract is checked, (U Int nil)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/cast"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "cf",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2211",
   :line 2211,
   :var-type "macro",
   :arglists ([form] [form expected]),
   :doc
   "Takes a form and an optional expected type and\nreturns a human-readable inferred type for that form.\nThrows an exception if type checking fails.\n\nDo not use cf inside a typed namespace. cf is intended to be\nused at the REPL or within a unit test. Note that testing for\ntruthiness is not sufficient to unit test a call to cf, as nil\nand false are valid type syntax.\n\ncf preserves annotations from previous calls to check-ns or cf,\nand keeps any new ones collected during a cf. This is useful for\ndebugging and experimentation. cf may be less strict than check-ns\nwith type checker warnings.\n\neg. (cf 1) \n    ;=> Long\n\n    (cf #(inc %) [Number -> Number])\n    ;=> [Number -> Number]",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/cf"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "check-form*",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2199",
   :line 2199,
   :var-type "function",
   :arglists ([form] [form expected] [form expected type-provided?]),
   :doc
   "Takes a (quoted) form and optional expected type syntax and\ntype checks the form. If expected is provided, type-provided?\nmust be true.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-form*"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "check-form-info",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2169",
   :line 2169,
   :var-type "function",
   :arglists ([form & opt]),
   :doc
   "Type checks a (quoted) form and returns a map of results from type checking the\nform.\n\nOptions\n- :expected        Type syntax representing the expected type for this form\n                   type-provided? option must be true to utilise the type.\n- :type-provided?  If true, use the expected type to check the form.\n- :profile         Use Timbre to profile the type checker. Timbre must be\n                   added as a dependency.\n- :file-mapping    If true, return map provides entry :file-mapping, a hash-map\n                   of (Map '{:line Int :column Int :file Str} Str).\n- :checked-ast     Returns the entire AST for the given form as the :checked-ast entry,\n                   annotated with the static types inferred after checking.\n                   If a fatal error occurs, mapped to nil.\n- :no-eval         If true, don't evaluate :out-form. Removes :result return value.\n                   It is highly recommended to evaluate :out-form manually.\n\nDefault return map\n- :ret             TCResult inferred for the current form\n- :out-form        The macroexpanded result of type-checking, if successful. \n- :result          The evaluated result of :out-form, unless :no-eval is provided.\n- :ex              If an exception was thrown during evaluation, this key will be present\n                   with the exception as the value.\nDEPRECATED\n- :delayed-errors  A sequence of delayed errors (ex-info instances)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-form-info"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "check-ns",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2254",
   :line 2254,
   :var-type "function",
   :arglists ([] [ns-or-syms & opt]),
   :doc
   "Type check a namespace/s (a symbol or Namespace, or collection).\nIf not provided default to current namespace.\nReturns a true value if type checking is successful, otherwise\nthrows an Exception.\n\nDo not use check-ns within a checked namespace.\nIt is intended to be used at the REPL or within a unit test.\nSuggested idiom for clojure.test: (is (check-ns 'your.ns))\n\ncheck-ns resets annotations collected from \nprevious check-ns calls or cf. A successful check-ns call will\npreserve any type annotations collect during that checking run.\n\nKeyword arguments:\n- :collect-only  if true, collect type annotations but don't type check code.\n                 Useful for debugging purposes.\n- :trace         if true, print some basic tracing of the type checker\n- :profile       if true, use Timbre to profile type checking. Must include\n                 Timbre as a dependency.\n\nIf providing keyword arguments, the namespace to check must be provided\nas the first argument.\n\nBind clojure.core.typed.util-vars/*verbose-types* to true to print fully qualified types.\nBind clojure.core.typed.util-vars/*verbose-forms* to print full forms in error messages.\n\neg. (check-ns 'myns.typed)\n    ;=> :ok\n   \n    ; implicitly check current namespace\n    (check-ns)\n    ;=> :ok\n\n    ; collect but don't check the current namespace\n    (check-ns *ns* :collect-only true)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-ns"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "check-ns-info",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2234",
   :line 2234,
   :var-type "function",
   :arglists ([] [ns-or-syms & opt]),
   :doc
   "Same as check-ns, but returns a map of results from type checking the\nnamespace.\n\nOptions\n- :collect-only    Don't type check the given namespace/s, but collect the \n                   top level type annotations like ann, ann-record.\n- :type-provided?  If true, use the expected type to check the form\n- :profile         Use Timbre to profile the type checker. Timbre must be\n                   added as a dependency.\n- :file-mapping    If true, return map provides entry :file-mapping, a hash-map\n                   of (Map '{:line Int :column Int :file Str} Str).\n\nDefault return map\n- :delayed-errors  A sequence of delayed errors (ex-info instances)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/check-ns-info"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "declare-alias-kind",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1170",
   :line 1170,
   :var-type "macro",
   :arglists ([sym ty]),
   :doc
   "Declare a kind for an alias, similar to declare but on the kind level.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-alias-kind"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "declare-datatypes",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1148",
   :line 1148,
   :var-type "macro",
   :arglists ([& syms]),
   :doc "Declare datatypes, similar to declare but on the type level.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-datatypes"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "declare-names",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1183",
   :line 1183,
   :var-type "macro",
   :arglists ([& syms]),
   :doc "Declare names, similar to declare but on the type level.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-names"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "declare-protocols",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1159",
   :line 1159,
   :var-type "macro",
   :arglists ([& syms]),
   :doc "Declare protocols, similar to declare but on the type level.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/declare-protocols"}
  {:raw-source-url nil,
   :name "def",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 22,
   :var-type "macro",
   :arglists ([name & fdecl]),
   :doc
   "Like clojure.core/def with optional type annotations\n\nNB: in Clojure it is impossible to refer a var called `def` as it is a\nspecial form. Use an alias prefix (eg. `t/def`).\n\nIf an annotation is provided, a corresponding `ann` form\nis generated, otherwise it expands identically to clojure.core/def\n\neg. ;same as clojure.core/def\n    (def vname 1)\n    \n    ;with Number `ann`\n    (def vname :- Number 1)\n\n    ;doc\n    (def vname\n      \"Docstring\"\n      :- Long\n      1)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/def",
   :forms [(def name docstring? :- type? expr)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "def-alias",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1261",
   :line 1261,
   :deprecated "0.2.45",
   :var-type "macro",
   :arglists ([sym doc-str t] [sym t]),
   :doc
   "DEPRECATED: use defalias\n\nDefine a type alias. Takes an optional doc-string as a second\nargument.\n\nUpdates the corresponding var with documentation.\n\neg. (def-alias MyAlias\n      \"Here is my alias\"\n      (U nil String))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/def-alias"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "def>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L770",
   :line 770,
   :deprecated "0.2.45",
   :var-type "macro",
   :arglists ([name & fdecl]),
   :doc
   "DEPRECATED: use clojure.core.typed/def\n\nLike def, but with annotations.\n\neg. (def> vname :- Long 1)\n\n;doc\n(def> vname\n  \"Docstring\"\n  :- Long\n  1)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/def>",
   :forms [(def> name docstring? :- type expr)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "defalias",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1294",
   :line 1294,
   :var-type "macro",
   :arglists ([sym doc-str t] [sym t]),
   :doc
   "Define a recursive type alias. Takes an optional doc-string as a second\nargument.\n\nUpdates the corresponding var with documentation.\n\neg. (defalias MyAlias\n      \"Here is my alias\"\n      (U nil String))\n\n    ;; recursive alias\n    (defalias Expr\n      (U '{:op ':if :test Expr :then Expr :else Expr}\n         '{:op ':const :val Any}))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defalias"}
  {:raw-source-url nil,
   :name "defn",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 228,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Like defn, but expands to clojure.core.typed/fn. If a polymorphic binder is\nsupplied before the var name, expands to clojure.core.typed/pfn.\n\neg. (defn fname [a :- Number, b :- (U Symbol nil)] :- Integer ...)\n\n;annotate return\n(defn fname [a :- String] :- String ...)\n\n;multi-arity\n(defn fname \n  ([a :- String] :- String ...)\n  ([a :- String, b :- Number] :- Long ...))\n\n;polymorphic function\n(defn :forall [x y]\n  fname \n  ([a :- x] :- (Coll y) ...)\n  ([a :- Str, b :- y] :- y ...))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defn",
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
     +)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "defn>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L737",
   :line 737,
   :deprecated "0.2.57",
   :var-type "macro",
   :arglists ([name & fdecl]),
   :doc
   "DEPRECATED: Use defn\n\nLike defn, but with annotations. Annotations are mandatory for\nparameters and for return type.\n\neg. (defn> fname :- Integer [a :- Number, b :- (U Symbol nil)] ...)\n\n;annotate return\n(defn> fname :- String [a :- String] ...)\n\n;multi-arity\n(defn> fname \n  (:- String [a :- String] ...)\n  (:- Long   [a :- String, b :- Number] ...))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defn>",
   :forms
   [(defn> name docstring? :- type [param :- type *] exprs*)
    (defn> name docstring? (:- type [param :- type *] exprs*) +)]}
  {:raw-source-url nil,
   :name "defprotocol",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 138,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "Like defprotocol, but with optional type annotations.\n\nOmitted annotations default to Any. The first argument\nof a protocol cannot be annotated.\n\nAdd a binder before the protocol name to define a polymorphic\nprotocol. A binder before the method name defines a polymorphic\nmethod, however a method binder must not shadow type variables\nintroduced by a protocol binder.\n\nReturn types for each method arity can be annotated.\n\nUnlike clojure.core/defprotocol, successive methods can\nhave the same arity. Semantically, providing multiple successive\nmethods of the same arity is the same as just providing the left-most\nmethod. However the types for these methods will be accumulated into\na Fn type.\n\neg. ;annotate single method\n(defprotocol MyProtocol\n  (a [this a :- Integer] :- Number))\n\n;polymorphic protocol\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  (a [this a :- Integer] :- Number))\n\n;multiple types for the same method\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  (a [this a :- Integer] :- Integer\n     [this a :- Long] :- Long\n     [this a :- Number] :- Number))\n\n;polymorphic method+protocol\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  ([y] a [this a :- x, b :- y] :- y))\n",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defprotocol"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "defprotocol>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1074",
   :line 1074,
   :deprecated "0.2.45",
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "DEPRECATED: use clojure.core.typed/defprotocol\n\nLike defprotocol, but required for type checking\nits macroexpansion.\n\neg. (defprotocol> MyProtocol\n      (a [this]))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/defprotocol>"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "doseq",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L578",
   :line 578,
   :var-type "macro",
   :arglists ([seq-exprs & body]),
   :doc
   "Like clojure.core/doseq with optional annotations.\n\n:let option uses clojure.core.typed/let\n\neg.\n(doseq [a :- (U nil AnyInteger) [1 nil 2 3]\n        :when a]\n   (inc a))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/doseq"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "doseq>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L473",
   :line 473,
   :deprecated "0.2.45",
   :var-type "macro",
   :arglists ([seq-exprs & body]),
   :doc
   "DEPRECATED: use clojure.core.typed/doseq\n\nLike doseq but requires annotation for each loop variable: \n[a [1 2]] becomes [a :- Long [1 2]]\n\neg.\n(doseq> [a :- (U nil AnyInteger) [1 nil 2 3]\n         :when a]\n   (inc a))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/doseq>"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "dotimes",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L172",
   :line 172,
   :var-type "macro",
   :arglists ([bindings & body]),
   :doc
   "Like clojure.core/dotimes, but with optional annotations.\n\nIf annotation for binding is omitted, defaults to Int.\n\neg. (dotimes [_ 100]\n      (println \"like normal\"))\n\n    (dotimes [x :- Num, 100.123]\n      (println \"like normal\" x))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/dotimes"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "dotimes>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L149",
   :line 149,
   :deprecated "0.2.45",
   :var-type "macro",
   :arglists ([bindings & body]),
   :doc
   "DEPRECATED: Use clojure.core.typed/dotimes\n\nLike dotimes.\n\neg. (dotimes> [_ 100]\n      (println \"like normal\"))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/dotimes>"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "envs",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2314",
   :line 2314,
   :var-type "function",
   :arglists ([]),
   :doc
   "Returns a map of type environments, according to the current state of the\ntype checker.\n\nOutput map:\n- :vars      map from var symbols to their verbosely printed types\n- :aliases   map from alias var symbols (made with defalias) to their verbosely printed types\n- :special-types  a set of Vars that are special to the type checker (like Any, U, I)\n",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/envs"}
  {:raw-source-url nil,
   :name "fn",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 64,
   :var-type "macro",
   :arglists ([& forms]),
   :doc
   "Like clojure.core/fn, but with optional annotations.\n\neg. ;these forms are equivalent\n    (fn [a] b)\n    (fn [a :- Any] b)\n    (fn [a :- Any] :- Any b)\n    (fn [a] :- Any b)\n\n    ;annotate return\n    (fn [a :- String] :- String body)\n\n    ;named fn\n    (fn fname [a :- String] :- String body)\n\n    ;rest parameter\n    (fn [a :- String & b :- Number *] body)\n\n    ;dotted rest parameter\n    (fn [a :- String & b :- Number ... x] body)\n\n    ;multi-arity\n    (fn fname \n      ([a :- String] :- String ...)\n      ([a :- String, b :- Number] :- String ...))\n\n    ; polymorphic binder\n    (fn :forall [x y z]\n      fname \n      ([a :- String] :- String ...)\n      ([a :- String, b :- Number] :- String ...))\n",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/fn",
   :forms
   [(fn name? [param :- type* & param :- type * ?] :- type? exprs*)
    (fn
     name?
     ([param :- type* & param :- type * ?] :- type? exprs*)
     +)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "fn>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L694",
   :line 694,
   :deprecated "0.2.45",
   :var-type "macro",
   :arglists ([& forms]),
   :doc
   "DEPRECATED: use clojure.core.typed/fn\n\nLike fn, but with annotations. Annotations are mandatory\nfor parameters, with optional annotations for return type.\nIf fn is named, return type annotation is mandatory.\n\nSuggested idiom: use commas between parameter annotation triples.\n\neg. (fn> [a :- Number, b :- (U Symbol nil)] ...)\n\n    ;annotate return\n    (fn> :- String [a :- String] ...)\n\n    ;named fn\n    (fn> fname :- String [a :- String] ...)\n\n    ;multi-arity\n    (fn> fname \n      (:- String [a :- String] ...)\n      (:- Long   [a :- String, b :- Number] ...))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/fn>",
   :forms
   [(fn> name? :- type? [param :- type* & param :- type * ?] exprs*)
    (fn>
     name?
     (:- type? [param :- type* & param :- type * ?] exprs*)
     +)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "for",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L332",
   :line 332,
   :var-type "macro",
   :arglists ([seq-exprs & maybe-ann-body-expr]),
   :doc
   "Like clojure.core/for with optional type annotations.\n\nAll types default to Any.\n\nThe :let option uses clojure.core.typed/let.\n\neg. (for [a :- (U nil Int) [1 nil 2 3]\n          :when a]\n      :- Number\n      (inc a))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/for"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "for>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L199",
   :line 199,
   :deprecated "0.2.45",
   :var-type "macro",
   :arglists ([tk ret-ann seq-exprs body-expr]),
   :doc
   "DEPRECATED: use clojure.core.typed/for\n\nLike for but requires annotation for each loop variable: [a [1 2]] becomes [a :- Long [1 2]]\nAlso requires annotation for return type.\n\neg. (for> :- Number\n      [a :- (U nil AnyInteger) [1 nil 2 3]\n       :when a]\n      (inc a))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/for>"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "inst",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L113",
   :line 113,
   :var-type "macro",
   :arglists ([inst-of & types]),
   :doc
   "Instantiate a polymorphic type with a number of types.\n\neg. (inst foo-fn t1 t2 t3 ...)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/inst"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "inst-ctor",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L121",
   :line 121,
   :var-type "macro",
   :arglists ([inst-of & types]),
   :doc
   "Instantiate a call to a constructor with a number of types.\nFirst argument must be an immediate call to a constructor.\nReturns exactly the instantiatee (the first argument).\n\neg. (inst-ctor (PolyCtor. a b c)\n               t1 t2 ...)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/inst-ctor"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "into-array>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1643",
   :line 1643,
   :var-type "macro",
   :arglists
   ([cljt coll] [javat cljt coll] [into-array-syn javat cljt coll]),
   :doc
   "Make a Java array with Java class javat and Typed Clojure type\ncljt. Resulting array will be of type javat, but elements of coll must be under\ncljt. cljt should be a subtype of javat (the same or more specific).\n\n*Temporary hack*\ninto-array-syn is exactly the syntax to put as the first argument to into-array.\nCalling resolve on this syntax should give the correct class.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/into-array>"}
  {:raw-source-url nil,
   :name "let",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 118,
   :var-type "macro",
   :arglists ([bvec & forms]),
   :doc
   "Like clojure.core/let but supports optional type annotations.\n\neg. (let [a :- Type, b\n          a2 1.2]\n      body)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/let",
   :forms [(let [binding :- type? init*] exprs*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "letfn>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L802",
   :line 802,
   :var-type "macro",
   :arglists ([fn-specs-and-annotations & body]),
   :doc
   "Like letfn, but each function spec must be annotated.\n\neg. (letfn> [a :- [Number -> Number]\n             (a [b] 2)\n\n             c :- [Symbol -> nil]\n             (c [s] nil)]\n      ...)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/letfn>",
   :forms [(letfn> [fn-spec-or-annotation*] expr*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "load-if-needed",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L52",
   :line 52,
   :var-type "function",
   :arglists ([]),
   :doc "Load and initialize all of core.typed if not already",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/load-if-needed"}
  {:raw-source-url nil,
   :name "loop",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 102,
   :var-type "macro",
   :arglists ([bindings & exprs]),
   :doc
   "Like clojure.core/loop, and supports optional type annotations.\nArguments default to a generalised type based on the initial value.\n\neg. (loop [a :- Number 1\n           b :- (U nil Number) nil]\n      ...)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/loop",
   :forms [(loop [binding :- type? init*] exprs*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "loop>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1092",
   :line 1092,
   :deprecated "0.2.45",
   :var-type "macro",
   :arglists ([bndings* & forms]),
   :doc
   "DEPRECATED: use clojure.core.typed/loop\n\nLike loop, except loop variables require annotation.\n\nSuggested idiom: use a comma between the type and the initial\nexpression.\n\neg. (loop> [a :- Number, 1\n            b :- (U nil Number), nil]\n      ...)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/loop>",
   :forms [(loop> [binding :- type init*] exprs*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "method-type",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L65",
   :line 65,
   :var-type "function",
   :arglists ([mname]),
   :doc
   "Given a method symbol, print the core.typed types assigned to it.\nIntended for use at the REPL.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/method-type"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "nilable-param",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1682",
   :line 1682,
   :var-type "macro",
   :arglists ([msym mmap]),
   :doc
   "Override which parameters in qualified method msym may accept\nnilable values. If the parameter is a parameterised type or\nan Array, this also declares the parameterised types and the Array type as nilable.\n\nmmap is a map mapping arity parameter number to a set of parameter\npositions (integers). If the map contains the key :all then this overrides\nother entries. The key can also be :all, which declares all parameters nilable.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/nilable-param"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "non-nil-return",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1665",
   :line 1665,
   :var-type "macro",
   :arglists ([msym arities]),
   :doc
   "Override the return type of fully qualified method msym to be non-nil.\nTakes a set of relevant arities,\nrepresented by the number of parameters it takes (rest parameter counts as one),\nor :all which overrides all arities.\n\neg. ; must use full class name\n    (non-nil-return java.lang.Class/getDeclaredMethod :all)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/non-nil-return"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "override-constructor",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2075",
   :line 2075,
   :var-type "macro",
   :arglists ([ctorsym typesyn]),
   :doc "Override all constructors for Class ctorsym with type.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/override-constructor"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "override-method",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2086",
   :line 2086,
   :var-type "macro",
   :arglists ([methodsym typesyn]),
   :doc
   "Override type for qualified method methodsym.\n\nmethodsym identifies the method to override and should be a\nnamespace-qualified symbol in the form <class>/<method-name>.\nThe class name needs to be fully qualified.\n\ntypesyn uses the same annotation syntax as functions.\n\nUse non-nil-return instead of override-method if you want to\ndeclare that a method can never return nil.\n\nExample:\n\n  (override-method java.util.Properties/stringPropertyNames\n                   [-> (java.util.Set String)])\n\nThis overrides the return type of method stringPropertyNames\nof class java.util.Properties to be (java.util.Set String).",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/override-method"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "pfn>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L685",
   :line 685,
   :var-type "macro",
   :arglists ([& forms]),
   :doc
   "Define a polymorphic typed anonymous function.\n(pfn> name? [binder+] :- type? [[param :- type]* & [param :- type *]?] exprs*)\n(pfn> name? [binder+] (:- type? [[param :- type]* & [param :- type *]?] exprs*)+)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/pfn>"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "pred",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2335",
   :line 2335,
   :var-type "macro",
   :arglists ([t]),
   :doc
   "Generate a flat (runtime) predicate for type that returns true if the\nargument is a subtype of the type, otherwise false.\n\nThe current type variable and dotted type variable scope is cleared before parsing.\n\neg. ((pred Number) 1)\n    ;=> true",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/pred"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "print-env",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1696",
   :line 1696,
   :var-type "function",
   :arglists ([debug-str]),
   :doc
   "During type checking, print the type environment to *out*,\npreceeded by literal string debug-str.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/print-env"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "print-filterset",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L90",
   :line 90,
   :var-type "function",
   :arglists ([debug-string frm]),
   :doc
   "During type checking, print the filter set attached to form, \npreceeded by literal string debug-string.\nReturns nil.\n\neg. (let [s (seq (get-a-seqable))]\n      (print-filterset \"Here now\" s))",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/print-filterset"}
  {:raw-source-url nil,
   :name "ref",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 213,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Like ref, but with optional type annotations.\n\nSame as (ref (ann-form init t) args*)\n\neg. (ref 1) : (Ref1 (Value 1))\n    (ref :- Num, 1) : (Ref1 Num)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ref"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "ref>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1243",
   :line 1243,
   :deprecated "0.2.58",
   :var-type "macro",
   :arglists ([t init & args]),
   :doc
   "DEPRECATED: use clojure.core.typed/ref\n\nLike ref, but creates a Ref1 of type t.\n\nSame as (ref (ann-form init t) args*)\n\neg. (ref> Number 1)\n    (ref> (Vec Any) [])",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/ref>"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "reset-caches",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L57",
   :line 57,
   :var-type "function",
   :arglists ([]),
   :doc "Reset internal type caches.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/reset-caches"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "statistics",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2297",
   :line 2297,
   :var-type "function",
   :arglists ([nsyms]),
   :doc
   "Takes a collection of namespace symbols and returns a map mapping the namespace\nsymbols to a map of data",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/statistics"}
  {:raw-source-url nil,
   :name "tc-ignore",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 184,
   :var-type "macro",
   :arglists ([& body]),
   :doc "Ignore forms in body during type checking",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/tc-ignore"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "typed-deps",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2114",
   :line 2114,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Declare namespaces which should be checked before the current namespace.\nAccepts any number of symbols. Only has effect via check-ns.\n\neg. (typed-deps clojure.core.typed.holes\n                myns.types)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/typed-deps"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "untyped-var",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L1708",
   :line 1708,
   :var-type "macro",
   :arglists ([varsym typesyn]),
   :doc "Check a given var has the specified type at runtime.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/untyped-var"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "var-coverage",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2305",
   :line 2305,
   :var-type "function",
   :arglists ([] [nsyms-or-nsym]),
   :doc
   "Summarises annotated var coverage statistics to *out*\nfor namespaces nsyms, a collection of symbols or a symbol/namespace.\nDefaults to the current namespace if no argument provided.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/var-coverage"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "var>",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2143",
   :line 2143,
   :var-type "macro",
   :arglists ([sym]),
   :doc
   "Like var, but resolves at runtime like ns-resolve and is understood by\nthe type checker. sym must be fully qualified (without aliases).\n\neg. (var> clojure.core/+)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/var>"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj",
   :name "warn-on-unannotated-vars",
   :file "module-rt/src/main/clojure/clojure/core/typed.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/46260f6d1acde61a9d95a1b150e8847eddac086a/module-rt/src/main/clojure/clojure/core/typed.clj#L2157",
   :line 2157,
   :var-type "macro",
   :arglists ([]),
   :doc
   "Allow unannotated vars in the current namespace. \n\nEmits a warning instead of a type error when checking\na def without a corresponding expected type.\n\nDisables automatic inference of `def` expressions.\n\neg. (warn-on-unannotated-vars)",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/warn-on-unannotated-vars"}
  {:raw-source-url nil,
   :name "when-let-fail",
   :file "module-check/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url nil,
   :line 191,
   :var-type "macro",
   :arglists ([b & body]),
   :doc
   "Like when-let, but fails if the binding yields a false value.",
   :namespace "clojure.core.typed",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed/when-let-fail"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/935e7c9277a1954cdd011907cb5a8abe3134b05e/module-check/src/main/clojure/clojure/core/typed/check_form_cljs.clj",
   :name "check-form-cljs",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check_form_cljs.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/935e7c9277a1954cdd011907cb5a8abe3134b05e/module-check/src/main/clojure/clojure/core/typed/check_form_cljs.clj#L25",
   :line 25,
   :var-type "function",
   :arglists ([form expected expected-provided?]),
   :doc
   "Check a single form with an optional expected type.\nIntended to be called from Clojure. For evaluation at the Clojurescript\nREPL see cf.",
   :namespace "clojure.core.typed.check-form-cljs",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check-form-cljs/check-form-cljs"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/7d7264105b8f1d07915413cb75743a4f6b847dc4/module-check/src/main/clojure/clojure/core/typed/check_ns_clj.clj",
   :name "check-ns-info",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check_ns_clj.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/7d7264105b8f1d07915413cb75743a4f6b847dc4/module-check/src/main/clojure/clojure/core/typed/check_ns_clj.clj#L6",
   :line 6,
   :var-type "function",
   :arglists ([ns-or-syms & opt]),
   :doc
   "Same as check-ns, but returns a map of results from type checking the\nnamespace.\n\nOptions\n- :collect-only    Don't type check the given namespace/s, but collect the \n                   top level type annotations like ann, ann-record.\n- :type-provided?  If true, use the expected type to check the form\n- :profile         Use Timbre to profile the type checker. Timbre must be\n                   added as a dependency.\n- :file-mapping    If true, return map provides entry :file-mapping, a hash-map\n                   of (Map '{:line Int :column Int :file Str} Str).",
   :namespace "clojure.core.typed.check-ns-clj",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check-ns-clj/check-ns-info"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj",
   :name "add-checks-normal-def",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/def.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj#L123",
   :line 123,
   :var-type "function",
   :arglists ([check-fn expr expected]),
   :doc "Add runtime checks to a def with an initial value.",
   :namespace "clojure.core.typed.check.def",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.def/add-checks-normal-def"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj",
   :name "check-def",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/def.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj#L111",
   :line 111,
   :var-type "function",
   :arglists ([check-fn {:keys [var init env], :as expr} expected]),
   :doc
   "Check a def. If it is a declare or a defmacro, don't try and check it.",
   :namespace "clojure.core.typed.check.def",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.def/check-def"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj",
   :name "check-defmacro-or-declare",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/def.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj#L101",
   :line 101,
   :var-type "function",
   :arglists ([expr expected]),
   :doc
   "To check a defmacro or declare, just assign it the most general\nVar type and ignore the body.",
   :namespace "clojure.core.typed.check.def",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.def/check-defmacro-or-declare"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj",
   :name "check-normal-def",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/def.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj#L22",
   :line 22,
   :var-type "function",
   :arglists ([check-fn {:keys [init env], :as expr} & [expected]]),
   :doc "Checks a def that isn't a macro definition.",
   :namespace "clojure.core.typed.check.def",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.def/check-normal-def"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj",
   :name "defmacro-or-declare?",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/def.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/def.clj#L95",
   :line 95,
   :var-type "function",
   :arglists ([{:keys [var], :as expr}]),
   :doc
   "Returns true if this :def AST originally a defmacro or declare.",
   :namespace "clojure.core.typed.check.def",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.def/defmacro-or-declare?"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/488c9279bd44cee8eacd5167b1dc4773943a8666/module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj",
   :name "check-fni",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/488c9279bd44cee8eacd5167b1dc4773943a8666/module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj#L78",
   :line 78,
   :var-type "function",
   :arglists
   ([expected
     mthods
     {:keys [recur-target-fn validate-expected-fn self-name],
      :as opt}]),
   :doc
   "Check a vector of :method AST nodes mthods against\nan expected type that is a possibly-polymorphic function\nintersection.\n\nReturns a vector in the same order as the passed in methods,\nbut each method replaced with a vector of type checked methods.",
   :namespace "clojure.core.typed.check.fn-methods",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.fn-methods/check-fni"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/488c9279bd44cee8eacd5167b1dc4773943a8666/module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj",
   :name "expected-for-method",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/488c9279bd44cee8eacd5167b1dc4773943a8666/module-check/src/main/clojure/clojure/core/typed/check/fn_methods.clj#L39",
   :line 39,
   :var-type "function",
   :arglists
   ([{:keys [fixed-arity], :as method}
     {:keys [dom rest drest kws], :as f}]),
   :doc
   "Takes a :method AST node and a single Function arity type,\nand returns the Function if the :method node should be checked\nagainst the Function, otherwise returns nil.",
   :namespace "clojure.core.typed.check.fn-methods",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.fn-methods/expected-for-method"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/81a3bc793db822eaeee5d59f1498fbaa24639cdd/module-check/src/main/clojure/clojure/core/typed/check/monitor.clj",
   :name "check-monitor",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/monitor.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/81a3bc793db822eaeee5d59f1498fbaa24639cdd/module-check/src/main/clojure/clojure/core/typed/check/monitor.clj#L11",
   :line 11,
   :var-type "function",
   :arglists ([check {:keys [target], :as expr} expected]),
   :doc
   "monitor-enter and monitor-exit both take any object and return nil",
   :namespace "clojure.core.typed.check.monitor",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.monitor/check-monitor"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj",
   :name "add-checks-ann-form",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj#L61",
   :line 61,
   :var-type "function",
   :arglists
   ([check {:keys [statements env], frm :ret, :as expr} expected]),
   :doc
   "Add runtime checks to an ann-form expression. Propagates its annotation\ninwards to the inner expression.",
   :namespace "clojure.core.typed.check.special.ann-form",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.special.ann-form/add-checks-ann-form"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj",
   :name "ann-form-annotation",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj#L15",
   :line 15,
   :var-type "function",
   :arglists ([{:keys [statements], :as expr}]),
   :doc "Return the raw type annotation from the ann-form expression.",
   :namespace "clojure.core.typed.check.special.ann-form",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.special.ann-form/ann-form-annotation"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj",
   :name "check-ann-form",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj#L38",
   :line 38,
   :var-type "function",
   :arglists
   ([check {:keys [statements env], frm :ret, :as expr} expected]),
   :doc
   "Type check an ann-form expression. Propagates its annotation\ninwards to the inner expression.",
   :namespace "clojure.core.typed.check.special.ann-form",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.special.ann-form/check-ann-form"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj",
   :name "parse-annotation",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/check/special/ann_form.clj#L30",
   :line 30,
   :var-type "function",
   :arglists ([tsyn {:keys [env], :as expr}]),
   :doc "Parse the raw type annotation tsyn in the context of expr",
   :namespace "clojure.core.typed.check.special.ann-form",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.special.ann-form/parse-annotation"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/check/value.clj",
   :name "check-value",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/value.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/check/value.clj#L47",
   :line 47,
   :var-type "function",
   :arglists ([{:keys [val], :as expr} expected quoted?]),
   :doc
   "Given a :const node and an expected type returns a new :const\nnode annotated with its type.\n\nquoted? should be true if this :const node is nested inside a\n:quote node, otherwise should be false",
   :namespace "clojure.core.typed.check.value",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.value/check-value"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/check/value.clj",
   :name "unquote-val",
   :file
   "module-check/src/main/clojure/clojure/core/typed/check/value.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/check/value.clj#L24",
   :line 24,
   :var-type "function",
   :arglists ([val]),
   :doc
   "Convert the syntax representation of a unevaluated value to\nan actual evaluated value.\n\neg. ['a] is represented as [(quote a)] and evaluates to [a]",
   :namespace "clojure.core.typed.check.value",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.check.value/unquote-val"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/collect_utils.clj",
   :name "collect-ns*",
   :file
   "module-check/src/main/clojure/clojure/core/typed/collect_utils.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/afed234808448bcdd851c2b15e8baf6eb8853b36/module-check/src/main/clojure/clojure/core/typed/collect_utils.clj#L53",
   :line 53,
   :var-type "function",
   :arglists ([nsym {:keys [ast-for-ns collect-asts collect-ns]}]),
   :doc
   "Collect type annotations and dependency information\nfor namespace symbol nsym, and recursively check \ndeclared typed namespace dependencies.",
   :namespace "clojure.core.typed.collect-utils",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.collect-utils/collect-ns*"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "->Blame",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L44",
   :line 44,
   :var-type "function",
   :arglists ([positive negative name contract file line column]),
   :doc
   "Positional factory function for class clojure.core.typed.contract.Blame.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/->Blame"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "->CheckedISeq",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L279",
   :line 279,
   :var-type "function",
   :arglists ([s c b]),
   :doc
   "Positional factory function for class clojure.core.typed.contract.CheckedISeq.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/->CheckedISeq"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "->Contract",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L29",
   :line 29,
   :var-type "function",
   :arglists ([name first-order projection flat?]),
   :doc
   "Positional factory function for class clojure.core.typed.contract.Contract.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/->Contract"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "and-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L381",
   :line 381,
   :var-type "function",
   :arglists ([& cs]),
   :doc
   "Returns a contract that ensures a value passes each contract `cs`.\n\nAt most *one* higher-order contract may be passed to `and-c`, and\nany number of flat contracts.\n\n[Contract * -> Contract]\n\neg. (and-c (instance-c Boolean) true-c)  ;; (I Boolean true)",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/and-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "any-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L189",
   :line 189,
   :var-type "var",
   :arglists nil,
   :doc "Contract that allows any value.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/any-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "contract",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L123",
   :line 123,
   :var-type "macro",
   :arglists ([c x] [c x b]),
   :doc
   "Check a contract against a value, with an optional Blame object.\n\n(IFn [Contract Any -> Any]\n     [Contract Any Blame -> Any])",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/contract"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "count-range-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L196",
   :line 196,
   :var-type "function",
   :arglists ([lower] [lower upper]),
   :doc
   "Returns a flat contract that allows values with `count`\ngreater-or-equal-to lower, and less-or-equal-to upper.\nUpper can be nil for positive infinity.\n\n(IFn [Int -> Contract]\n     [Int (U nil Int) -> Contract])\n\neg. (count-range-c 0 10)\n    (count-range-c 0 nil)",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/count-range-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "equiv-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L217",
   :line 217,
   :var-type "function",
   :arglists ([y]),
   :doc
   "Returns a flat contract that returns true if a value is `=`\nto y.\n\n[Any -> Contract]",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/equiv-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "false-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L184",
   :line 184,
   :var-type "var",
   :arglists nil,
   :doc "Contract that checks for `false`.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/false-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "flat-val-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L169",
   :line 169,
   :var-type "function",
   :arglists ([name pred]),
   :doc "Contract generation for flat predicates.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/flat-val-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "hmap-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L422",
   :line 422,
   :var-type "function",
   :arglists
   ([&
     {:keys [mandatory optional absent-keys complete?],
      :or
      {absent-keys #{}, mandatory {}, optional {}, complete? false}}]),
   :doc
   "Takes a map of mandatory and optional entry contracts,\na set of absent keys, and :complete? true if this is a fully\nspecified map. Intended to work with keyword keys, but should\nwork with any keys looked up via =.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/hmap-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "identical-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L228",
   :line 228,
   :var-type "function",
   :arglists ([y]),
   :doc
   "Returns a flat contract that returns true if a value is `identical?`\nto y.\n\n[Any -> Contract]",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/identical-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "ifn-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L240",
   :line 240,
   :var-type "function",
   :arglists ([cs c2]),
   :doc
   "Returns a function contract that checks a function has\nfixed domain that passes contracts `cs` and return value\nthat passes contact `c2`.\n\n[(Vec Contract) Contract -> Contract]\n\neg. (ifn-c [int-c] int-c)  ;; [Int -> Int] contract",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/ifn-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "instance-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L159",
   :line 159,
   :var-type "macro",
   :arglists ([c]),
   :doc "Flat contracts for instance? checks on Class's.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/instance-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "int-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L154",
   :line 154,
   :var-type "var",
   :arglists nil,
   :doc "Flat contract for values that pass `integer?`.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/int-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "make-blame",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L110",
   :line 110,
   :var-type "function",
   :arglists ([& {:as bls}]),
   :doc
   "Make a new blame object.\n\nKeyword arguments:\n- :message    A string message, String\n- :positive   Positive blame party, (U String Symbol)\n- :negative   Negative blame party, (U String Symbol)\n- :file       File that contains contract, (U Str nil)\n- :line       Line where contract occurs, (U Int nil)\n- :column     Column where contract occurs, (U Int nil)",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/make-blame"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "make-contract",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L69",
   :line 69,
   :var-type "function",
   :arglists
   ([&
     {:keys [name first-order projection flat?], :or {flat? false}}]),
   :doc
   "Make a new contract.\n\nKeyword arguments: (see Contract datatype for more details)\n- :name         Name of the contract, (U Symbol String)\n- :first-order  First-order predicate for this contract, [Any -> Any]\n- :projection   Curried function taking blame and the value to check,\n                and returns a new checked value, or throws blame.\n                [Blame -> [Any -> Any]]\n- :flat?        True if this is a flat contract, Boolean",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/make-contract"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "make-flat-contract",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L98",
   :line 98,
   :var-type "function",
   :arglists ([& args]),
   :doc
   "Calls `make-contract` but also passes `:flat? true` as the first arguments.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/make-flat-contract"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "map->Blame",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L44",
   :line 44,
   :var-type "function",
   :arglists ([m#]),
   :doc
   "Factory function for class clojure.core.typed.contract.Blame, taking a map of keywords to field values.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/map->Blame"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "map->Contract",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L29",
   :line 29,
   :var-type "function",
   :arglists ([m#]),
   :doc
   "Factory function for class clojure.core.typed.contract.Contract, taking a map of keywords to field values.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/map->Contract"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "nil-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L178",
   :line 178,
   :var-type "var",
   :arglists nil,
   :doc "Contract that checks for `nil`.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/nil-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "or-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L327",
   :line 327,
   :var-type "function",
   :arglists ([& cs]),
   :doc
   "Returns a contract that checks a value passes at least\none of the contracts `cs`.\n\nAny number of flat contracts may be passed to or-c. However,\nif more than one higher-order contract is provided, each time\nthis contract is used, at most *one* may pass its first-order\npredicate.\n\nFor example, (or-c (ifn-c [int-c] int-c) (ifn-c [] int-c))\ncannot be checked against `clojure.core/+` because\nthe first-order check for both contracts (`ifn?`) passes.\n\n[Contract * -> Contract]\n\neg. (or-c int-c nil-c) ;; (U Int nil)\n    (or-c int-c (ifn-c [int-c] int-c)) ;; (U Int [Int -> Int])\n",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/or-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "seqable-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L307",
   :line 307,
   :var-type "function",
   :arglists ([c]),
   :doc
   "Alpha - subject to change.\n\nReturns a contract that checks Seqable things.\n\n[Contract -> Contract]",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/seqable-c"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "swap-blame",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L142",
   :line 142,
   :var-type "function",
   :arglists ([x]),
   :doc "Swap a blame object's blame parties.\n\n[Blame -> Blame]",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/swap-blame"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "throw-blame",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L47",
   :line 47,
   :var-type "function",
   :arglists
   ([{:keys [message positive negative file line column], :as b}]),
   :doc "Throw a blame object\n\n[Blame -> Nothing]",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/throw-blame"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :name "true-c",
   :file "module-rt/src/main/clojure/clojure/core/typed/contract.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/a34bcd127ebe2db79748f87e30c67551ccdc4242/module-rt/src/main/clojure/clojure/core/typed/contract.clj#L181",
   :line 181,
   :var-type "var",
   :arglists nil,
   :doc "Contract that checks for `true`.",
   :namespace "clojure.core.typed.contract",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/true-c"}
  {:name "Blame",
   :var-type "record",
   :namespace "clojure.core.typed.contract",
   :arglists nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/Blame",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "CheckedISeq",
   :var-type "type",
   :namespace "clojure.core.typed.contract",
   :arglists nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/CheckedISeq",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "Contract",
   :var-type "record",
   :namespace "clojure.core.typed.contract",
   :arglists nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.contract/Contract",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/0947387913babb0e8db52b560a3c0e42b45cb40b/module-rt/src/main/clojure/clojure/core/typed/current_impl.clj",
   :name "create-env",
   :file
   "module-rt/src/main/clojure/clojure/core/typed/current_impl.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/0947387913babb0e8db52b560a3c0e42b45cb40b/module-rt/src/main/clojure/clojure/core/typed/current_impl.clj#L7",
   :line 7,
   :var-type "macro",
   :arglists ([n]),
   :doc
   "For name n, creates defs for {n}, {n}-kw, add-{n},\nand reset-{n}!",
   :namespace "clojure.core.typed.current-impl",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.current-impl/create-env"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj",
   :name "->NoisyHole",
   :file "module-check/src/main/clojure/clojure/core/typed/hole.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj#L17",
   :line 17,
   :var-type "function",
   :arglists ([]),
   :doc
   "Positional factory function for class clojure.core.typed.hole.NoisyHole.",
   :namespace "clojure.core.typed.hole",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/->NoisyHole"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj",
   :name "noisy-hole",
   :file "module-check/src/main/clojure/clojure/core/typed/hole.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj#L20",
   :line 20,
   :var-type "function",
   :arglists ([]),
   :doc
   "A noisy hole. The type system will complain when\n(noisy-hole) is used in positions that expect a type\nmore specific than Object or Any.\nUse (noisy-hole) as a placeholder for code.\nThrows an exception when evaluted.",
   :namespace "clojure.core.typed.hole",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/noisy-hole"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj",
   :name "silent-hole",
   :file "module-check/src/main/clojure/clojure/core/typed/hole.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/05369b9cbc4c9a26a79c0c97fcc161bfb723408c/module-check/src/main/clojure/clojure/core/typed/hole.clj#L8",
   :line 8,
   :var-type "function",
   :arglists ([]),
   :doc
   "A silent hole. (silent-hole) passes for any other type\nwhen type checking.\nUse (silent-hole) as a placeholder for code.\nThrows an exception when evaluted.",
   :namespace "clojure.core.typed.hole",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/silent-hole"}
  {:name "NoisyHole",
   :var-type "type",
   :namespace "clojure.core.typed.hole",
   :arglists nil,
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.hole/NoisyHole",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :name "default-load1",
   :file "module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj#L30",
   :line 30,
   :var-type "function",
   :arglists ([base-resource-path]),
   :doc "Roughly equivalent to clojure.core/load.",
   :namespace "clojure.core.typed.lang",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/default-load1"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :added "1.0",
   :name "extensible-load",
   :file "module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj#L45",
   :line 45,
   :var-type "function",
   :arglists ([& paths]),
   :doc
   "Loads Clojure code from resources in classpath. A path is interpreted as\nclasspath-relative if it begins with a slash or relative to the root\ndirectory for the current namespace otherwise.",
   :namespace "clojure.core.typed.lang",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/extensible-load"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :name "file-lang",
   :file "module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj#L36",
   :line 36,
   :var-type "function",
   :arglists ([res]),
   :doc "Returns the :lang entry in ns form in the given namespace.",
   :namespace "clojure.core.typed.lang",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/file-lang"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :name "lang-dispatch",
   :file "module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj#L25",
   :line 25,
   :var-type "var",
   :arglists nil,
   :doc
   "A map from :lang entries to their corresponding `load` alternatives.",
   :namespace "clojure.core.typed.lang",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/lang-dispatch"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :name "monkey-patch-extensible-load",
   :file "module-check/src/main/clojure/clojure/core/typed/lang.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/6b92bb2c35d504d336561e6fdfd5809fdf262b8c/module-check/src/main/clojure/clojure/core/typed/lang.clj#L68",
   :line 68,
   :var-type "var",
   :arglists nil,
   :doc
   "A no-argument function that installs the core.typed `load` function\nover clojure.core/load.",
   :namespace "clojure.core.typed.lang",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.lang/monkey-patch-extensible-load"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/261cc6bec6ae84767f7b64c848f61ec3a4f94ee3/module-check/src/main/clojure/clojure/core/typed/load.clj",
   :name "install-typed-load",
   :file "module-check/src/main/clojure/clojure/core/typed/load.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/261cc6bec6ae84767f7b64c848f61ec3a4f94ee3/module-check/src/main/clojure/clojure/core/typed/load.clj#L87",
   :line 87,
   :var-type "function",
   :arglists ([]),
   :doc
   "Extend the :lang dispatch table with the :core.typed language",
   :namespace "clojure.core.typed.load",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.load/install-typed-load"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/261cc6bec6ae84767f7b64c848f61ec3a4f94ee3/module-check/src/main/clojure/clojure/core/typed/load.clj",
   :name "load-typed-file",
   :file "module-check/src/main/clojure/clojure/core/typed/load.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/261cc6bec6ae84767f7b64c848f61ec3a4f94ee3/module-check/src/main/clojure/clojure/core/typed/load.clj#L29",
   :line 29,
   :var-type "function",
   :arglists ([filename] [filename env] [filename env opts]),
   :doc
   "Loads a whole typed namespace, returns nil. Assumes the file is typed.",
   :namespace "clojure.core.typed.load",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.load/load-typed-file"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/261cc6bec6ae84767f7b64c848f61ec3a4f94ee3/module-check/src/main/clojure/clojure/core/typed/load.clj",
   :name "monkey-patch-typed-load",
   :file "module-check/src/main/clojure/clojure/core/typed/load.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/261cc6bec6ae84767f7b64c848f61ec3a4f94ee3/module-check/src/main/clojure/clojure/core/typed/load.clj#L95",
   :line 95,
   :var-type "function",
   :arglists ([]),
   :doc "Install the :core.typed :lang, and monkey patch `load`",
   :namespace "clojure.core.typed.load",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.load/monkey-patch-typed-load"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/261cc6bec6ae84767f7b64c848f61ec3a4f94ee3/module-check/src/main/clojure/clojure/core/typed/load.clj",
   :name "typed-load1",
   :file "module-check/src/main/clojure/clojure/core/typed/load.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/261cc6bec6ae84767f7b64c848f61ec3a4f94ee3/module-check/src/main/clojure/clojure/core/typed/load.clj#L70",
   :line 70,
   :var-type "function",
   :arglists ([base-resource-path]),
   :doc
   "Checks if the given file is typed, and loads it with core.typed if so,\notherwise with clojure.core/load",
   :namespace "clojure.core.typed.load",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.load/typed-load1"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "ann-form",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L130",
   :line 130,
   :var-type "macro",
   :arglists ([form ty]),
   :doc "Annotate a form with an expected type.",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/ann-form"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "atom",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L198",
   :line 198,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Like atom, but with optional type annotations.\n\nSame as (atom (ann-form init t) args*)\n\neg. (atom 1) : (Atom1 (Value 1))\n    (atom :- Num, 1) : (Atom1 Num)",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/atom"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "def",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L22",
   :line 22,
   :var-type "macro",
   :arglists ([name & fdecl]),
   :doc
   "Like clojure.core/def with optional type annotations\n\nNB: in Clojure it is impossible to refer a var called `def` as it is a\nspecial form. Use an alias prefix (eg. `t/def`).\n\nIf an annotation is provided, a corresponding `ann` form\nis generated, otherwise it expands identically to clojure.core/def\n\neg. ;same as clojure.core/def\n    (def vname 1)\n    \n    ;with Number `ann`\n    (def vname :- Number 1)\n\n    ;doc\n    (def vname\n      \"Docstring\"\n      :- Long\n      1)",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/def",
   :forms [(def name docstring? :- type? expr)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "defn",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L228",
   :line 228,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Like defn, but expands to clojure.core.typed/fn. If a polymorphic binder is\nsupplied before the var name, expands to clojure.core.typed/pfn.\n\neg. (defn fname [a :- Number, b :- (U Symbol nil)] :- Integer ...)\n\n;annotate return\n(defn fname [a :- String] :- String ...)\n\n;multi-arity\n(defn fname \n  ([a :- String] :- String ...)\n  ([a :- String, b :- Number] :- Long ...))\n\n;polymorphic function\n(defn :forall [x y]\n  fname \n  ([a :- x] :- (Coll y) ...)\n  ([a :- Str, b :- y] :- y ...))",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/defn",
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
     +)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "defprotocol",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L138",
   :line 138,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "Like defprotocol, but with optional type annotations.\n\nOmitted annotations default to Any. The first argument\nof a protocol cannot be annotated.\n\nAdd a binder before the protocol name to define a polymorphic\nprotocol. A binder before the method name defines a polymorphic\nmethod, however a method binder must not shadow type variables\nintroduced by a protocol binder.\n\nReturn types for each method arity can be annotated.\n\nUnlike clojure.core/defprotocol, successive methods can\nhave the same arity. Semantically, providing multiple successive\nmethods of the same arity is the same as just providing the left-most\nmethod. However the types for these methods will be accumulated into\na Fn type.\n\neg. ;annotate single method\n(defprotocol MyProtocol\n  (a [this a :- Integer] :- Number))\n\n;polymorphic protocol\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  (a [this a :- Integer] :- Number))\n\n;multiple types for the same method\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  (a [this a :- Integer] :- Integer\n     [this a :- Long] :- Long\n     [this a :- Number] :- Number))\n\n;polymorphic method+protocol\n(defprotocol [[x :variance :covariant]]\n  MyProtocol\n  ([y] a [this a :- x, b :- y] :- y))\n",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/defprotocol"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "fn",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L64",
   :line 64,
   :var-type "macro",
   :arglists ([& forms]),
   :doc
   "Like clojure.core/fn, but with optional annotations.\n\neg. ;these forms are equivalent\n    (fn [a] b)\n    (fn [a :- Any] b)\n    (fn [a :- Any] :- Any b)\n    (fn [a] :- Any b)\n\n    ;annotate return\n    (fn [a :- String] :- String body)\n\n    ;named fn\n    (fn fname [a :- String] :- String body)\n\n    ;rest parameter\n    (fn [a :- String & b :- Number *] body)\n\n    ;dotted rest parameter\n    (fn [a :- String & b :- Number ... x] body)\n\n    ;multi-arity\n    (fn fname \n      ([a :- String] :- String ...)\n      ([a :- String, b :- Number] :- String ...))\n\n    ; polymorphic binder\n    (fn :forall [x y z]\n      fname \n      ([a :- String] :- String ...)\n      ([a :- String, b :- Number] :- String ...))\n",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/fn",
   :forms
   [(fn name? [param :- type* & param :- type * ?] :- type? exprs*)
    (fn
     name?
     ([param :- type* & param :- type * ?] :- type? exprs*)
     +)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "let",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L118",
   :line 118,
   :var-type "macro",
   :arglists ([bvec & forms]),
   :doc
   "Like clojure.core/let but supports optional type annotations.\n\neg. (let [a :- Type, b\n          a2 1.2]\n      body)",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/let",
   :forms [(let [binding :- type? init*] exprs*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "loop",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L102",
   :line 102,
   :var-type "macro",
   :arglists ([bindings & exprs]),
   :doc
   "Like clojure.core/loop, and supports optional type annotations.\nArguments default to a generalised type based on the initial value.\n\neg. (loop [a :- Number 1\n           b :- (U nil Number) nil]\n      ...)",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/loop",
   :forms [(loop [binding :- type? init*] exprs*)]}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "parse-colon",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L14",
   :line 14,
   :var-type "function",
   :arglists ([fdecl name]),
   :doc "Returns a vector of [provided? t args]",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/parse-colon"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "ref",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L213",
   :line 213,
   :var-type "macro",
   :arglists ([& args]),
   :doc
   "Like ref, but with optional type annotations.\n\nSame as (ref (ann-form init t) args*)\n\neg. (ref 1) : (Ref1 (Value 1))\n    (ref :- Num, 1) : (Ref1 Num)",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/ref"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "tc-ignore",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L184",
   :line 184,
   :var-type "macro",
   :arglists ([& body]),
   :doc "Ignore forms in body during type checking",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/tc-ignore"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :name "when-let-fail",
   :file "module-rt/src/main/clojure/clojure/core/typed/macros.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/73d6be1f8024bf40b61015e65de0f94fb8e0597e/module-rt/src/main/clojure/clojure/core/typed/macros.clj#L191",
   :line 191,
   :var-type "macro",
   :arglists ([b & body]),
   :doc
   "Like when-let, but fails if the binding yields a false value.",
   :namespace "clojure.core.typed.macros",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.macros/when-let-fail"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/runtime_check.clj",
   :name "check",
   :file
   "module-check/src/main/clojure/clojure/core/typed/runtime_check.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/868a4ff39a2ce57658126c6facd4d558fc3df246/module-check/src/main/clojure/clojure/core/typed/runtime_check.clj#L9",
   :line 9,
   :var-type "function",
   :arglists ([expr] [expr expected]),
   :doc
   "Add runtime checks to the output AST, propagating just enough types\nfor immediate ann-form expressions to propagate to fn expected types.\n\nStatic checking is disabled, outside ill-formed types.\n\nUnsafe contracts can be generated, and contract generation cannot fail.\n\nAssumes collect-expr is already called on this AST.",
   :namespace "clojure.core.typed.runtime-check",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.runtime-check/check"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/0947387913babb0e8db52b560a3c0e42b45cb40b/module-check/src/main/clojure/clojure/core/typed/statistics.clj",
   :name "statistics",
   :file
   "module-check/src/main/clojure/clojure/core/typed/statistics.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/0947387913babb0e8db52b560a3c0e42b45cb40b/module-check/src/main/clojure/clojure/core/typed/statistics.clj#L22",
   :line 22,
   :var-type "function",
   :arglists ([nsyms]),
   :doc
   "Takes a collection of namespace symbols and returns a map mapping the namespace\nsymbols to a map of data",
   :namespace "clojure.core.typed.statistics",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.statistics/statistics"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj",
   :name "*trace-checker*",
   :file "module-rt/src/main/clojure/clojure/core/typed/util_vars.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj#L32",
   :dynamic true,
   :line 32,
   :var-type "var",
   :arglists nil,
   :doc "If true, print tracing information during type checking.",
   :namespace "clojure.core.typed.util-vars",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.util-vars/*trace-checker*"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj",
   :name "*verbose-forms*",
   :file "module-rt/src/main/clojure/clojure/core/typed/util_vars.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj#L20",
   :dynamic true,
   :line 20,
   :var-type "var",
   :arglists nil,
   :doc
   "If true, print complete forms in error messages. Bind\naround a type checking form like cf or check-ns.\n\neg.\n(binding [*verbose-forms* true]\n  (cf ['deep ['deep ['deep ['deep]]]] Number))\n;=> <full form in error>",
   :namespace "clojure.core.typed.util-vars",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.util-vars/*verbose-forms*"}
  {:raw-source-url
   "https://github.com/clojure/core.typed/raw/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj",
   :name "*verbose-types*",
   :file "module-rt/src/main/clojure/clojure/core/typed/util_vars.clj",
   :source-url
   "https://github.com/clojure/core.typed/blob/9e7b73eaa332f5d3fe20a0f4b4940cce29e563ce/module-rt/src/main/clojure/clojure/core/typed/util_vars.clj#L7",
   :dynamic true,
   :line 7,
   :var-type "var",
   :arglists nil,
   :doc
   "If true, print fully qualified types in error messages\nand return values. Bind around a type checking form like \ncf or check-ns.\n\neg. \n(binding [*verbose-types* true] \n  (cf 1 Number))\n;=> java.lang.Number",
   :namespace "clojure.core.typed.util-vars",
   :wiki-url
   "http://clojure.github.com/core.typed//clojure.core.typed-api.html#clojure.core.typed.util-vars/*verbose-types*"})}

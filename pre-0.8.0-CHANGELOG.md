# 0.6.0 - 8 November 2018

- Breaking: Removed deprecated/replaced macros (since 0.2.45) to improve future loading times
  - clojure.core.typed/dotimes> (use t/dotimes)
  - clojure.core.typed/for> (use t/for)
  - clojure.core.typed/doseq> (use t/doseq)
  - clojure.core.typed/pfn> (use t/fn)
  - clojure.core.typed/fn> (use t/fn)
  - clojure.core.typed/defn> (use t/defn)
  - clojure.core.typed/def> (use t/defn)
  - clojure.core.typed/defprotocol> (use t/defprotocol)
  - clojure.core.typed/loop> (use t/loop)
  - clojure.core.typed/atom> (use t/atom)
  - clojure.core.typed/ref> (use t/ref)
  - clojure.core.typed/def-alias (use t/defalias)
  - clojure.core.typed/ann-precord (use t/ann-record)
  - clojure.core.typed/ann-pprotocol (use t/ann-protocol)
- Breaking: Lazily load core type aliases
  - since var's like t/Int are lazily interned, they cannot reliably be `:refer`ed.
    - support for `:refer`ing `deftype`s and special types altogether may be
      removed in a future release for a leaner runtime
  - First step in removing interned vars backing `defalias` (inspired by clojure.spec)
- Removed tools.analyzer.env usage (`update-ns-map!` etc.)
  - ~30% performance improvement in analysis
- Various cleanups
  - Removed old `:profiling` support for `check-ns` etc.
  - remove project.clj, migrate to Clojure CLI

# 0.5.4 - 13 September 2018

- add annotations for some clojure.core Transducers (but usages are still too hard to infer for now)
- fix bad `clojure.core/take-while` annotation

# 0.5.3 - 21 June 2018

- fix `s/keys` generation

# 0.5.2 - 8 May 2018

- Split namespaces into maven submodules
  - automatic annotation work in `module-infer`
    - with artifact id `org.clojure/core.typed.infer`
  - tools.analyzer fork in `module-analyzer-jvm`
    - with artifact id `org.clojure/core.typed.analyzer.jvm`
- merge WIP custom typing rules
- refactor checking logic
  - eg. `let` and `if`
- support propagation of `fn` argument types while still
  inferring the body

# 0.5.1 - 18 April 2018

- Add circleci workflow to generate annotations
- Fix auto annotation bug
  - types now are generally smaller throughout the inference algorithm, instead of just
    at the end.
  - described [here](http://frenchy64.github.io/2018/04/13/how-to-merge-recursive-specs.html)

# 0.5.0 - 12 April 2018

Breaking changes:

- `check-ns` et. al no longer recursively checks file
  dependencies. Use `(check-ns *ns* :check-deps true)`
  to recursively check dependencies.

Features:

- support ns aliases in `ann`
- remove support for deprecated `ann` `:nocheck` syntax
- CTYP-297: Allow defmulti without expected type

Internal:

- Use custom single pass variant of tools.analyzer (`clojure.core.typed.analyzer2.jvm`)

# 0.4.3 - 13 October 2017

- add :spec-macros option to `spec-infer`

# 0.4.2 - 12 October 2017

- port CLJ-2110, CLJ-1793, CLJ-1714 Compiler.java changes
- spec generation improvements
  - added options to `prepare-infer-ns`

# 0.4.1 - 8 September 2017

- Upgrade to Clojure 1.9.0-alpha20
- type/spec generation
  - lots of small fixes
  - extensive performance improvements in generation algorithm
  - support namespaced `s/keys
  - added configuration options to {runtime,spec}-infer

# 0.4.0 - 6 July 2017

Deprecations

- `{runtime,spec}-infer` now take keyword arguments, single namespace argument
  is deprecated

Features/fixes

- remove documentation promising `check-ns`
  resets all environments on each run. This actually
  changed a long time ago.
- update tools.analyzer.jvm to 0.7.0
  - some changes needed to uses of `analyze+eval`
- update core.async to 0.3.44
  - fragile workaround needed since core.async depends on
    `clojure.lang.Compiler.LocalBinding`. Seems to work.
  - updated the typed `go` wrapper
- update Clojure to 1.9.0-alpha17
- add metadata type annotations to `for`, `loop`
  - use `:clojure.core.typed/ann` keyword metadata to associate
    types to loop bindings
- various improvements to runtime inference
  - infer local type annotations with runtime inference
- add/fix annotations
  - `sorted-map`
  - `frequencies`
  - `big{dec,int,integer}` now also accept strings
  - `sort` pretends a nil return isn't possible, ignore unlikely
    edge case.
  - `seqable?` now tests for `(Seqable Any)`
- set/vectors now invokable
- better handle infinitely expanding map types
- Add new `{runtime,spec}-infer` keyword options
  - :ns, :fuel, :debug
- Monkeypatched typed load now takes variable arguments like
  `clojure.core/load`
- Added `prepare-infer-ns` function to instrument a namespace
  for runtime type inference without using monkeypatching.

# 0.3.32 - 16 January 2017

- merge Aravind's GSoC annotations
  - added annotations for core clojure libraries

# 0.3.30 - 15 January 2017 

- merge Di Xu's GSoC 2014 code
  - Add pdot/prest support
  - update core type annotations
- move core.async dep from `core.typed.rt` to `core.typed` pom

# 0.3.29 - 7 January 2017 

Release Notes - core.typed - Version 0.3.29

<h2>        Defect
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-318'>CTYP-318</a>] -         Ensure :static-method has :class entry
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-319'>CTYP-319</a>] -         Single-pass compiler misses interfaces
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-320'>CTYP-320</a>] -         Don&#39;t track internals of track implementation
</li>
</ul>
    
<h2>        Enhancement
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-294'>CTYP-294</a>] -         Infer untyped vars via unsafe Dyn-like type
</li>
</ul>

- improve line numbers in errors
- update Clojure to 1.9-alpha14
- update Compiler.java port

# 0.3.{27,28} - 1 September 2016 

- instrument primitive functions for runtime inference (CTYP-316)

# 0.3.26 - 31 August 2016 

- remove core.contracts dependency
- include AOT files in org.clojure/core.typed jar

# 0.3.25 - 31 August 2016 

- Use modified `clojure.lang.Compiler` as analyzer
  instead of tools.analyzer.
- Remove CLJS dependency
- Add Clojure 1.9.0-alpha11 compatibility
  - fix dependencies with spec errors
  - update vector destructuring type checking
    - uses seq/next instead of nthnext
- Add runtime inference generation
  - `runtime-infer` generates core.typed types
  - `spec-infer` generates clojure.spec specs

# 0.3.23 - 16 April 2016 

Release Notes - core.typed - Version 0.3.23

<h2>        Defect
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-313'>CTYP-313</a>] -         subtyping optional keywords in HMaps
</li>
</ul>

<h2>        Task
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-308'>CTYP-308</a>] -         Enable sanity-checking compilation of core.typed-rt project
</li>
</ul>

# 0.3.{21,22} - 31 January 2016 

Prefer 0.3.22, which removes some debugging statements.

We now monkey-patch `eval` via the
<a href='http://clojure.github.io/core.typed/#clojure.core.typed/install'>clojure.core.typed/install</a>
function. See the docstring for how it works.

The new (Leiningen) bootstrapping code is now:

```clojure
:injections [(require 'clojure.core.typed)
             (clojure.core.typed/install)]
```

<h2>        Enhancement
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-307'>CTYP-307</a>] -         Override `eval` to type check code in typed namespaces
</li>
</ul>

# 0.3.20 - 30 January 2016 
    
<h2>        Defect
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-283'>CTYP-283</a>] -         Directly-linking in Clojure 1.8.0 interferes with load monkey-patching 
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-285'>CTYP-285</a>] -         Cljc reader conditionals in ns form cause core.typed to skip checking namespace
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-305'>CTYP-305</a>] -         t/cast does not run under rewriting type checking
</li>
</ul>
    
<h2>        Enhancement
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-284'>CTYP-284</a>] -         Remove redundant checking in typed load
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-300'>CTYP-300</a>] -         Support HMap contract generation
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-304'>CTYP-304</a>] -         Upgrade to Clojure 1.8.0
</li>
<li>[<a href='https://github.com/clojure/core.typed#getting-started'>New Getting Started tutorial</a>]
</li>
</ul>
    

# 0.3.19 - 18 November 2015 
    
<h2>        Defect
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-296'>CTYP-296</a>] -         Overlap of free variables and other types should not be empty
</li>
</ul>
    
<h2>        Enhancement
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-295'>CTYP-295</a>] -         Update ns wrapper macro with Clojure 1.8 changes
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-299'>CTYP-299</a>] -         Add per-namespace flag to check annotations at runtime
</li>
</ul>
    

# 0.3.18 - 12 November 2015 

<h2>        Enhancement
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-288'>CTYP-288</a>] -         Type checker should understand clojure.core.typed/cast
</li>
</ul>

# 0.3.17 - 11 November 2015 

<h2>        Defect
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-291'>CTYP-291</a>] -         ClassNotFoundException clojure.core.cache.CacheProtocol
</li>
</ul>

# 0.3.16 - 8 November 2015 (broken, see [<a href='http://dev.clojure.org/jira/browse/CTYP-291'>CTYP-291</a>])

<h2>        Enhancement
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-287'>CTYP-287</a>] -         Add contract system and `cast` expression
</li>
</ul>
    
# 0.3.15 - 5 November 2015 (broken, see [<a href='http://dev.clojure.org/jira/browse/CTYP-291'>CTYP-291</a>])

Release Notes - core.typed - Version 0.3.15

<h2>        Defect
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-283'>CTYP-283</a>] - Directly-linking in Clojure 1.8.0 interferes with load monkey-patching 
</li>
</ul>

<h2>        Task
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-286'>CTYP-286</a>] - Convert inlined dependencies back to regular jars
</li>
</ul>

<h2>        Misc
</h2>
<ul>
<li>Bump tools.reader, tools.analyzer.jvm versions
</li>
</ul>

# 0.3.14 - 26 October 2015 (0.3.13 was a bad release)

Release Notes - core.typed - Version 0.3.14
        
<h2>        Enhancement
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-246'>CTYP-246</a>] -         Gradually typed namespaces should import untyped vars with a contract
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-281'>CTYP-281</a>] -         Separate `load` monkey-patch and typed REPL
</li>
</ul>

# 0.3.12 - 19 October 2015

Release Notes - core.typed - Version 0.3.12

<h2>        Enhancement
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-239'>CTYP-239</a>] -         Search for .cljc files when checking a namespace 
</li>
</ul>

# 0.3.11 - 14 August 2015

<a href='http://dev.clojure.org/jira/browse/CTYP/fixforversion/10652'>Release Notes - core.typed - Version 0.3.11</a>

<h2>        Enhancement
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-269'>CTYP-269</a>] -         Combine mutable environments into one atom
</li>
</ul>

# 0.3.10 - 3 August 2015

<a href='http://dev.clojure.org/jira/secure/ReleaseNote.jspa?projectId=10271&version=10650'>Release Notes</a> - core.typed - Version 0.3.10

<h2>Use static types to resolve simple Java interoperability</h2>

In situations where it is appropriate to rewrite a function, like in the typed
REPL or when a function body is checked only once, we can use static types to
resolve Java interoperability.

For example, the following code is non-reflective.

```clojure
(fn [a :- java.io.File]
  (.getParent a))
```

The same rules apply as usual with occurrence typing. The static type of the target
and arguments at the site of the interop is used to avoid reflection.

```clojure
(fn [a] 
  {:pre [(instance? java.io.File a)]}
  (.getParent a))
```

For now, only very simple types that are easily converted to type hints are used
as type hints (like `File` or `String`). 
Furthermore, only methods and constructors are rewritten, and only local bindings
can propagate type information.

Please report how effective this is in practice.
    
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-250'>CTYP-250</a>] -         Resolve Java interoperability based on static type information
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-259'>CTYP-259</a>] -         Rewrite function bodies if they are only checked once
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-247'>CTYP-247</a>] -         Function bodies should rewrite themselves if possible
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-264'>CTYP-264</a>] -         deftype should rewrite method bodies
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-265'>CTYP-265</a>] -         Anonymous functions should rewrite body
</li>
</ul>

<h2>Elide checking on output of ns form</h2>

This change shaved 2-3 minutes from running the test suite and should save 500ms on
each `check-ns` call.

<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-266'>CTYP-266</a>] -         Elide checking of ns macro output for performance
</li>
</ul>

<h2>Defect</h2>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-263'>CTYP-263</a>] -         Unnecessary type hint required in catch expression
</li>
</ul>

# 0.3.9 - 24 July 2015

<a href='http://dev.clojure.org/jira/secure/ReleaseNote.jspa?projectId=10271&version=10556'>Release Notes</a> - core.typed - Version 0.3.9

This release mostly prepares for AST rewriting. Some bugs around
types and propositions are fixed.
    
<h2>Defect</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-210'>CTYP-210</a>] -         (long AnyInteger) doesn&#39;t typecheck
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-257'>CTYP-257</a>] -         Empty intersection should be Top, not Bottom
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-258'>CTYP-258</a>] -         Correctly simplify negative type propositions in constructor
</li>
</ul>
    
<h2>Enhancement</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-172'>CTYP-172</a>] -         ExactCount should work with destructuring
</li>
</ul>
    
<h2>Task</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-251'>CTYP-251</a>] -         Remove dead code clojure.core.typed.check.fn-method
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-252'>CTYP-252</a>] -         Suppress tools.analyzer&#39;s warn-on-reflection
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-253'>CTYP-253</a>] -         Remove static/instance flag in check-method
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-254'>CTYP-254</a>] -         Add flag to enable AST rewriting
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-255'>CTYP-255</a>] -         Unparse should be flexible to unknown implementations
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-256'>CTYP-256</a>] -         Add :unknown implementation to impl-case
</li>
</ul>


# 0.3.8 - 21 July 2015

<a href='http://dev.clojure.org/jira/browse/CTYP/fixforversion/10555'>JIRA Release notes</a>

Release Notes - core.typed - Version 0.3.8

This release contains mainly bug fixes. The typed `defn` now supports
a metadata map and `:arglists` metadata (<a href='http://dev.clojure.org/jira/browse/CTYP-168'>CTYP-168</a>).

Thanks to Tobian Kortkamp for contributing a patch.
    
<h2>        Defect
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-27'>CTYP-27</a>] -         clojure.lang.RT/nth&#39;s type doesn&#39;t currently allow nil as the first argument
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-80'>CTYP-80</a>] -         Issue with filter subtyping/simplification
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-203'>CTYP-203</a>] -         Unreproducable internal error
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-212'>CTYP-212</a>] -         Can&#39;t create a promise of the same type as a record field
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-234'>CTYP-234</a>] -         Setting :collect-only attribute for a namespace does not collect type aliases
</li>
</ul>
    
<h2>        Enhancement
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-113'>CTYP-113</a>] -         Better documentation for override-method
</li>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-168'>CTYP-168</a>] -         Support metadata map and :arglists in clojure.core.typed/defn
</li>
</ul>
    
<h2>        Task
</h2>
<ul>
<li>[<a href='http://dev.clojure.org/jira/browse/CTYP-248'>CTYP-248</a>] -         Move lexical environment to an easily accessible location 
</li>
</ul>


# 0.3.{1-7} - 10 July 2015

## Inlined all dependencies

`org.clojure/core.typed` now has no external dependencies.
They are all copied with a flat prefix to `clojure.core.typed.deps.*`.

The current versions of copied libraries are in `project.clj`,
prefixed by `^:source-dep`.

## Dropped Clojure 1.6 support

`core.typed` should work fine with 1.6 for a while, but I might start
using some Clojure 1.7 features and change the base-env to annotate
Clojure 1.7.0 core vars.

## Add back AOT class files

The standard `org.clojure/core.typed` jar has AOT classes for `core.typed`
and its dependencies. 
Beforehand, AOT files for old CLJS versions was causing havoc.
This is hopefully less of a problem now &mdash;
since all dependencies are in `clojure.core.typed.deps.*`, 
it's easy to create a filer to delete these extra files.

`[org.clojure/core.typed "0.3.7" :classifier "slim"]` gives you just
the Clojure sources.

## Support quoted '"string" syntax for string singletons (Val "string")

```
(ann M (U '{:op '"left" :left Int}
          '{:op '"right" :right Bool}))
```

## `keyword` now accepts `Any` and is smarter

- [Commit 1](https://github.com/clojure/core.typed/commit/39bef22f2a4955ed19d9c1314f8661245211042b)
- [Commit 2](https://github.com/clojure/core.typed/commit/b60ebf31b9873bdddd3e1cd604a8a00dd7d1ebd3)

```clojure
(ann M (U '{:op '"left" :left Int}
          '{:op '"right" :right Bool}))

(ann f [M -> Any])
(defmulti f (fn [m :- M] (keyword (:op m))))
(defmethod f :left [{:keys [left]}] (inc left))
(defmethod f :right [{:keys [right]}] (not right))
```

## Other changes

- Fix: `def` expressions now check their expected types
- Revert 0.3.0 change: `check-ns` now always eagerly checks transitive namespaces
- Add various tracing and metrics for POPL submission

# 0.3.0 - 25 June 2015

- Breaking change: (REVERTED 0.3.1)
  - all `check-ns` operations now avoid rechecking transitive dependencies.
    To explicitly recheck dependencies use the `:clean` keyword parameter.

- Fix [CTYP-214](http://dev.clojure.org/jira/browse/CTYP-214)
- support quoted type syntax with string literals
  - '"a" == (Val "a")
- `clojure.core/keyword` annotation is now more permissive.
- add `keyword` path element
  - see `keyword-pe-test` for new idioms

# 0.3.0-alpha5 - 2 June 2015

- restore original ns in nREPL eval 

# 0.3.0-alpha4 - 31 May 2015

- support .cljc files in `load`
- warn if :no-check is being removed from a var
- annotate
  - clojure.repl/print-doc
  - complete.core annotations

# 0.3.0-alpha3 - 30 May 2015

- REPL tweaks

# 0.3.0-alpha2 - 29 May 2015

- support :load-file nREPL packets

# 0.3.0-alpha1 - 28 May 2015

- improved typed REPL interactions
- fix `*e` annotation, can be nil
- depend on clojure 1.7.0-RC1

## Breaking

`clojure.core.typed/check-form-info` return map entry `:delayed-errors`
deprecated for `:ex`.

# 0.2.92 - 20 May 2015

- removed `*checking*` var
- bump tools.reader to 0.9.2

# 0.2.91 - 19 May 2015

- improve application error messages
  - find most general function signature

# 0.2.90 - 18 May 2015

- tweaks to REPL

# 0.2.89 - 18 May 2015

Thanks to Allen Rohner who provided patches for this release.

- Fix CTYP-215
- Fix CTYP-174
- Fix CTYP-181
  - better types for primitive casts
- Fix CTYP-170
- Fix CTYP-200

# 0.2.88 - 18 May 2015

## Typed REPL

core.typed is now integrated into the compilation process for namespaces with
`:core.typed` metadata mapped to a true value.

To enable the typed REPL, add this to your project.clj:

```clojure
  :repl-options {:nrepl-middleware [clojure.core.typed.repl/wrap-clj-repl]}
```

Then the typed REPL can be invoked by code such as this.

```clojure
(ns ^:core.typed typed)

(inc 'a)
```

# 0.2.87 - 1 April 2015

- Remove AOT files from jar again

# 0.2.86 - 1 April 2015

- depend on clojure 1.7.0-alpha6
- bump math.combinatorics dep to 0.1.1

# 0.2.85 - 1 April 2015

- document defalias is recursive
- erase 3rd party AOT files from `org.clojure/core.typed` jar

# 0.2.84 - 11 March 2015

- Fix group-by annotation [CTYP-199](http://dev.clojure.org/jira/browse/CTYP-199)
- Fix infinite overlap checking
- fix [CTYP-189](http://dev.clojure.org/jira/browse/CTYP-189)

# 0.2.83 - 23 Feb 2015

- fix `(fn [m :- (HMap :optional {:a (U Num)})] (if (:a m) (inc (:a m)) 0))`

# 0.2.82 - 23 Feb 2015

- better handling of non-nil default values for kw-lookup and get

# 0.2.81 - 22 Feb 2015

- fix isa? and = not checking filters
- generalise ClassPE case in update
- fix +ve proposition for keyword-invoke

# 0.2.80 - 20 Feb 2015

Thanks to Nathan Sorenson who contributed patches included in this release.

- Fix bad propositions in isa?
- Fix [CTYP-198](http://dev.clojure.org/jira/browse/CTYP-198)
- Fix a bunch of special type checked forms (eg. merge) not checking expected type
- defalias's that "overwrite" existing imported vars no longer silently fail
  - defalias must take an unqualified symbol, and its expansion includes a `declare`

BREAKING CHANGE

- defalias :forms metadata must be quoted to avoid evaluation

# 0.2.78 - 8 Feb 2015

- Fix [CTYP-196](http://dev.clojure.org/jira/browse/CTYP-196)

# 0.2.77 - 4 January 2015

- Complete [CTYP-186](http://dev.clojure.org/jira/browse/CTYP-186) - type check `restore-right`
  - includes useful tweaks to occurrence typing
- Fix [CTYP-185](http://dev.clojure.org/jira/browse/CTYP-185) - base-env load ordering issues
  - reported by dblarons

# 0.2.76 - 3 January 2015

- let aliasing tests
- Improve `clojure.core/flatten` type

# 0.2.75 - 2 January 2015

- Fix [CTYP-169](http://dev.clojure.org/jira/browse/CTYP-169)
- update* is like Typed Racket's update
- obviously truthy local bindings now infer filters of {:then tt :else ff}
  instead of {:then (! (U nil false) x) :else (is (U nil false) x)}
  - taken from Typed Racket
- Add [let-aliasing](http://andmkent.com/blog/2014/12/20/let-aliasing-in-typed-racket/)
  - Thanks to [Andrew Kent](https://twitter.com/andmkent_) for original design/implementation and help
- Let-aliasing fixes [CTYP-108](http://dev.clojure.org/jira/browse/CTYP-108)

# 0.2.74 - 30 December 2014

- Tests for monitor-{enter,exit}
  - Use new check-below style for checking expected type

# 0.2.73 - 30 December 2014

Thanks to Kyle Kingsbury for contributing to this release.

## Features

- `clojure.core.typed/{fn,defn}` supports a polymorphic binder with `:forall [x]` before all arguments
```clojure
(defn :forall [x]
  identity [a :- x]

(fn :forall [x] [a :- x] a)
```

## Breaking Changes

### defmulti inference

- defmulti no longer propagates an expected type to dispatch function
  - If your dispatch function relies on the input type of the multimethod,
    it must be explicitly annotated
```clojure
;old behaviour
(ann f [Num -> Num])
(defmulti f (fn [a] (inc a)))

;new behaviour
(ann f [Num -> Num])
(defmulti f (fn [a :- Num] (inc a)))
```

### fn range inference

- `clojure.core.typed/fn` no longer infers a better type for the body if it is provided an expected type for the body
  - ie. `(fn [a] :- Any 1)` is `[Any -> Any]`, not `[Any -> Num]`

## Fixes

- Major bug: propagate expected types to `if` expressions correctly

- Fix `get` operations with keyword key and default value
- parse annotations in base-env as needed
- improved log message that explains why a namespace was skipped during type checking
- annotate `clojure.core/sequential?`
- better line number when checking map literal entries
- allow non-heterogeneous types in map destructuring
  - made PersistentHashMap/create more flexible, which is used in map destructuring

- fix recur arguments not being checked correctly

# Notes

- fn> without expected return type now always returns Any
  - fn> will be removed soon, use clojure.core.typed/fn

- support :monitor-{enter,exit} AST nodes
  - by Kyle Kingsbury
- fix isReduced inlining annotation
  - by Kyle Kingsbury

## Internal

- add GetType fold case
- add various promote/demote cases

# 0.2.72 - 9rd October 2014

- Fix subtyping for higher-kinded type variables
  - identical applications are trivially subtypes
    - (m a) <: (m a)
  - when operands vary, subtyping checks m's upper bound and does
    subtyping check in the direction indicated by variance
    - (m a) <: (m b), where a <: b and upper bound of m is (TFn [[x :variance :covariant]] Any)
    - (m b) <: (m a), where b <: a and upper bound of m is (TFn [[x :variance :contravariant]] Any)
  - Unions are checked properly
    - (m a) <: (U (Reduced Int) (m a)
  - restrictions:
    - operators must be the same variable
      - this can probably be improved slightly by doing a subtype
        check between operators in the TApp-TApp subtyping case 
        instead of an = check

# 0.2.71 - 3rd October 2014

- Fix CTYP-175
  - type checker does not load without cljs dependency
  - affects 0.2.{69,70}

# 0.2.70 - 2nd October 2014

- fix `find-var` annotation

# 0.2.69 - 1st October 2014

- Only load cljs.core once
- Give a better line number for variance errors
- Type check more internals
- Improve destructuring support for homogenous types
  - see `count-set-test`
- Remove redundant type errors
- Better line numbers for type errors
  - in theory, only bare non-IMeta literals lack line numbers like `(cf :a Int)`

# 0.2.68 - 1st September 2014

- Fix major memory leak 
  - removed interning, replaced with normal cached hashes
  - Fixes [CTYP-83](http://dev.clojure.org/jira/browse/CTYP-83)

# 0.2.67 - 21st August 2014

Thanks to Minori Yamashita (ympbyc) for his GSoC work this summer!
This release merges his progress.

## ClojureScript 

Lots of cljs.core annotations.

## Dependency Changes

- `[org.clojure/clojure "1.6.0"]` (minimum)
- `[org.clojure/clojurescript "0.0-2268"]`
- JDK 1.7 minimum

# 0.2.{65,66} - 6th August 2014

(0.2.64 is a dud)

## clojure.string Annotations

Thanks to Aravind K N for this patch.

- annotations for clojure.string/{blank?,capitalize,lower-case,replace{,-first},reverse,trim{,rl}}

## Namespace dependencies missing Source-code

Thanks to Allen Rohner for this patch.

- [CTYP-166](http://dev.clojure.org/jira/browse/CTYP-166) - more forgiving on ns dependencies without source files

## Dependency changes

- core.cache 0.6.4

# 0.2.63 - 24st July 2014

## Set membership idiom

- Support `(#{:a :b} x)` set membership idiom
  - literal symbols, keywords, strings, numbers, booleans, characters and nil are supported
    as set members
  - also correctly handles the nil/false special cases

```clojure
(let [foo :- (U false nil ':a ':b), :a]
  (if (#{:a :b false nil} foo)
    (ann-form foo (U ':a ':b false nil))
    (ann-form foo (U false nil ':a ':b))))
```


# 0.2.62 - 23st July 2014

- (cast Integer nil) => nil
  - now handled correctly
  - previously assumed equivalent to (assert (instance? Integer nil))
- core.async
  - add Buffer protocol methods
  - annotate Unblocking protocol

- prepare for future tools.analyzer breaking changes (Thanks Nicola Mometto for the heads up)

# 0.2.61 - 21st July 2014

- add rand-nth annotation
- core.typed.async
  - <!!, <! and >! use ports. <! now takes 1 type variable
- add core.async example

# 0.2.60 - 21st July 2014

- [CTYP-81](http://dev.clojure.org/jira/browse/CTYP-81): bad annotation for alts{!,!!}
- Better formatting for c.c/apply error message
- add clojure.core.typed/envs for querying the current type checker state
- Bump math.combinatorics to 0.0.8

# 0.2.59 - 21st July 2014

- [CTYP-157](http://dev.clojure.org/jira/browse/CTYP-157): compilation issue fixed with lein-typed 0.3.5
- [CTYP-161](http://dev.clojure.org/jira/browse/CTYP-161): support intersections of HMaps in assoc
- [CTYP-163](http://dev.clojure.org/jira/browse/CTYP-163): fix some annotations (Thanks Andy Fingerhut)

## Breaking

- core.typed.async
  - <!! now takes 1 type variable
  - Fixed some annotations

# 0.2.58 - 19th July 2014

- Add `clojure.core.typed/defn`
- Deprecate `clojure.core.typed/defn>`
- Fix docstrings for imported `clojure.core.typed` macros
- `clojure.core.typed/def` uses `ann-form` to propagate expected type
  instead of `ann`

# 0.2.57 - 17th July 2014

- `clojure.core.typed/{fn,defprotocol}` preserves metadata in parameter vector
- imported clojure.core.typed macros preserve their docstrings
- Bump tools.namespace 0.2.5

# 0.2.56 - 5th July 2014

- 0.2.55 is bad

- Fix bug where push-thread-bindings is checked, even though
  it's special
  - this broke `binding` checks
- Update contrib-annotations to new namespaced syntax

# 0.2.54 - 4th July 2014

- Improve warning messages
- Add UPGRADING.md
- Fix defprotocol with docstring
- Upgrade to tools.analyzer 0.3.0

# 0.2.53 - 3rd July 2014

## Deprecations

- almost all unnamespaced type syntax are now namespaced under
  clojure.core.typed
  - Deprecation messages will direct what has changed
  - eg. HMap -> clojure.core.typed/HMap
  - Fn is now clojure.core.typed/IFn
  - some exceptions: Array syntax is the same

## core.async annotations

- Deprecated chan>/{,sliding-,dropping-}buffer>
  - use chan/{,sliding-,dropping-}buffer
  - Note new syntax in docstrings
- Type checking in go macro now actually works
  - clojure.core.typed/go> is deprecated, use go

## Packaging

- Libraries should depend on `org.clojure/core.typed.rt`
  - minimum dependency runtime support
  - Thanks to Chas Emerick

# 0.2.48 - Released 17 May 2014

- Improvements to local file mapping
  - vim-typedclojure now uses it for mouseovers

# 0.2.47 - Released 17 May 2014

0.2.46 doesn't work with :file-mapping

## Deprecations

- `Seq*` is deprecated, use `HSeq`
- `Vector*` is deprecated, use `HVec`

## Enhancements

- Add `:file-mapping` option for `check-{ns,form}-info`
  - eg. 
```clojure
(-> (check-ns-info *ns* :file-mapping true)
    :file-mapping)
;=> {{:line 1 :column 1 :file "current/file.clj"} "Int"
;    {:line 2 :column 4 :file "current/file.clj"} "Any"
;    ...}
```

- Add namespaced versions of special forms like [HMap](http://clojure.github.io/core.typed/#clojure.core.typed/HMap)
  - non-namespaced versions will be deprecated soon, but will still work

# 0.2.45 - Released 13 May 2014

Thanks to Di Xu and Nicola Mometto who contributed patches.

This release deprecates most of the > suffixed annotation macros
and replaces them with nicer named versions. In some cases the
syntaxes are different (return type in `for` goes after the binding vector),
so check the respective docstrings. The old versions are kept for backwards
compatibility.

Some big usability enhancements: 
- vars are now inferred from their `def`s
- macros like `ann-form` don't interfere with normal type hint inference
- failed interop calls now display a list of possible annotations

## Breaking Changes

- Annotation for `clojure.core/vector` now uses dotted variables
  - `inst`ing the new type will give a different type
  - The first type variable is for the "rest" type that delegates to
    `Vec`, second onwards instantiate the HVec positions.
  - eg. `(map (inst vector Any Num Sym) [1 2 3] ['a 'b 'c])`
- Changed `partial` annotation
  - removed some arities
- Changed `complement` annotation
  - remove unused type variable
- Parameterise several Java interfaces
  - java.util.{List,Collection,Set,RandomAccess}
  - java.lang.{Iterable}

## Deprecations

- `clojure.core.typed/{fn>, loop>, defprotocol>, dotimes>, doseq>, for>, def>}`
  - use more flexible forms without `>` suffix
  - note syntax changes in docstrings
- `clojure.core.typed/def-alias`
  - prefer `clojure.core.typed/defalias`
  - renamed for consistency with core Clojure

## Enhancements

- [CTYP-130](http://dev.clojure.org/jira/browse/CTYP-130) Add HSequential which abstracts over HSeq/HVec/HList/HSeq
  - Contributed by Di Xu + Ambrose
- Recognise Java types that work with `nth`, like CharSequence
  - achieved by pretending they extend `clojure.lang.Indexed`
- Annotate `clojure.lang.PersistentHashMap`
- Wrapping in forms like `tc-ignore` or `ann-form` no longer interfere with normal type hint inference
- Add `clojure.core.typed/{def, defn, let, fn, loop, dotimes, for, doseq, defprotocol}`
- Add `clojure.core.typed/defalias`
- Unannotated `def` forms now attempts to infer a type based on the initial expression instead
  of failing with an "Unannotated var" type error
  - `warn-on-unannotated-vars` disables this inference
- Add aliases
  - `clojure.core.typed/{Sym,Kw}`

## Fixes

- Use correct classloader to reflect on analyzed class
  - Contributed by Nicola Mometto
- Fix a bunch of bogus cs-gen/subtype cases with hetereogeneous collection types
  that have rest/drest
  - part of CTYP-130
- Be more robust in handling reflection calls
  - [CTYP-132](http://dev.clojure.org/jira/browse/CTYP-132)
- Fix type annotation for `nth` inlining
  - collection argument must be Sequential or Indexed

## Internal

- remove `tc-ignore-forms*`
- special typed forms use a `do` expression tagged with `:clojure.core.typed/special-form`
  instead of special functions

## Dependencies

- Move to `tools.analyzer.jvm` 0.1.0-beta13
- Move to ClojureScript 0.0-2202

# 0.2.44 - 3 April 2014

0.2.42 did not bind *file* correctly and gives useless tmp files
with type errors.

0.2.43 attempted to clean up the AOT jar, 0.2.44 reverted that.

## Enhancements

- move Clojure AST checking to `tools.analyzer.jvm`

# 0.2.41 - 26 March 2014

Thanks to Di Xu who contributed patches.

## Enhancements

- Fix [CTYP-126](http://dev.clojure.org/jira/browse/CTYP-126) Better support for checking
  dotted functions

# 0.2.40 - 24 March 2014

## Fixes

- Fix [pred](http://clojure.github.io/core.typed/#clojure.core.typed/pred) bug where types did not resolve in the current namespace

## Enhancements

- [Promise](http://clojure.github.io/core.typed/#clojure.core.typed/Promise) alias is now invokable 
- Add type aliases
  - [Delay](http://clojure.github.io/core.typed/#clojure.core.typed/Delay) (fixes [CTYP-125](http://dev.clojure.org/jira/browse/CTYP-125))
  - [Deref](http://clojure.github.io/core.typed/#clojure.core.typed/Deref)
  - [BlockingDeref](http://clojure.github.io/core.typed/#clojure.core.typed/BlockingDeref)
  - [List](http://clojure.github.io/core.typed/#clojure.core.typed/List)
  - [ExInfo](http://clojure.github.io/core.typed/#clojure.core.typed/ExInfo)
  - [Proxy](http://clojure.github.io/core.typed/#clojure.core.typed/Proxy)
  - [Stack](http://clojure.github.io/core.typed/#clojure.core.typed/Stack)
  - [Reversible](http://clojure.github.io/core.typed/#clojure.core.typed/Reversible)
  - [Multi](http://clojure.github.io/core.typed/#clojure.core.typed/Multi)
- Partial fix to [CTYP-124](http://dev.clojure.org/jira/browse/CTYP-124)
  - we now propagate negative information related to a `case` expression's 
    target expression in the default branch. However the `case` macro aliases
    the target expression with a fresh local, so core.typed isn't smart enough
    actually use this information. Need equality/aliasing filters.
- Various error message & documentation improvements
- Fix [CTYP-127](http://dev.clojure.org/jira/browse/CTYP-127)
  - support keyword lookups on bounded type variables

## Internal

- Base type aliases are now in `c.c.t/current-impl`

# 0.2.39 - Released 21 March 2014

- Add `Seq*` and `List*` cases to runtime type parsing

# 0.2.38 - Released 17 March 2014

## Enhancements

- [CTYP-121](http://dev.clojure.org/jira/browse/CTYP-121)

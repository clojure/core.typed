# 0.3.0-SNAPSHOT

- Breaking change:
  - all `check-ns` operations now avoid rechecking transitive dependencies.
    To explicitly recheck dependencies use the `:clean` keyword parameter.

- Fix CTYP-214

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

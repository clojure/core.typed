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

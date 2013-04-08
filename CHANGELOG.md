0.1.12 - Released 8 April 2013
BREAKING CHANGES
- Evaluting forms like `ann` and `ann-record` no longer rely on side effects
  while evaluting. Instead the side effects are performed during the first phase
  of type checking. In practice, this means REPL interactions with `ann` should
  be wrapped in `cf`, and all typed namespaces should declare their typed 
  dependencies with `typed-deps`.
- `check-ns` resets global type state before checking a namespace.

ENHANCEMENTS
- Add `clojure.core.typed/typed-deps` for declaring typed dependencies. (CTYP-7)
- Basic support for records. See `ann-record` and `ann-precord`. Can associate known keys with assoc. (Progress on CTYP-10)

FIXES
- Type resolution works correctly when checking namespaces other than the current one. (CTYP-19)

MISC
- Refactored files into separate namespaces (previously everything was in `clojure.core.typed`)
- Maintain the base type environment in vars for easy refreshing of global state. (See `c.c.t.base-env` and `c.c.t.init`)
- Type checking is split into two phases:
    1. Collect type annotations (`c.c.t.collect-phase`)
    2. Type Checking (`c.c.t.check`)

0.1.10
- Added: doseq>, for>, dotimes>. (CTYP-2)
- Recognise filters from macroexpansion of `and`

0.1.9 - Released 25 March 2013
- Move to contrib infrastructure
- Misc minor bugfixes
- Getting keyword value keys from map-like things is more flexible
- Eliminate reflection warnings in deps

0.1.8 - Released 17 March 2013
ENHANCEMENTS
- Better line numbers for checking `do`
- Distinguish between complete and partial hmap types
- Support static fields
- Java enums are non-nil
- Strings and CharSequences are (Seqable Character)
- `cf` can be inserted as a top level expression in a typed namespace

FIXES
- Fix bug when comparing a dotted function to a normal one
- Fix constraint generation/subtyping between heterogeneous sequence-like types
- Resolve Names in constraint generation
- check-ns cannot loop infinitely when placed in a typed namespace
- re-* functions too specific

MISC
- Move to core.contracts from Trammel
- Move to jvm.tools.analyzer from analyze
- Add hole-driven core.typed tutorial
- Java enums are non-nil

0.1.7 - Released 26 February 2013
- Support simple multimethods 
  - no multi-dispatch
- Support simple flow analysis
  - most sequential asserts recognised eg.
    ```clojure 
    (let [a (read-string "1")
          _ (assert (integer? a))]
      (+ 10 a))
    ```

0.1.6
- Ensure `Result` is not introduced when performing type inference on drest fn apps
- `tc-ignore` is more do-like. 
  
  Workaround for a quirk in the Clojure compiler where protocols only get generated in
  a top-level `do`.

```clojure
  (identity (do (def-protocol foo (bar [this]))
              bar)) ;; <-- bar cannot be resolved
  vs.
  (do (def-protocol foo (bar [this]))
    bar)  ;; <-- bar is resolvable
```

  (patch by Stephen Compall, issue #3)
- Fix typo in `into-array` logic
  (patch by Stephen Compall, issue #4)
- `into-array>` generalises Java types, does not need redundant type annotations. See User Documentation in wiki.
- Improve type of `clojure.core/class`.
  (class <non-nil>) is always a Class
  (class nil) is always a nil
- Move documentation to [wiki](https://github.com/frenchy64/typed-clojure/wiki).

0.1.5
- Better errors for Java methods and polymorphic function applications, borrow error messages from Typed Racket
- Change `ann-datatype`, `ann-protocol`, `ann-pprotocol` syntax to be flatter
  (ann-protocol pname
                method-name method-type ...)
  (ann-dataype dname
               [field-name :- field-type ...])
- Add `defprotocol>`

0.1.4
- Support Clojure 1.4.0+
- Better errors, print macro-expanded form from AST

0.1.3
  - Refactor typed.core into individual files
  - Add `method-type`
    - `(method-type 'java.io.File/getName)` prints the current Typed Clojure type for the getName method of File
  - Add types for some clojure.core coersion functions
  - Preliminary support for ClojureScript

0.1.2
  - Fix objects and filters being lost during polymorphic and dotted function applications
    - Add tests for (if (seq a) (first a) 0) filter example.
  - Can annotate datatypes outside current namespace
  - Improve type of `seq`, `next`, `conj`
  - tc-pr-env -> print-env
  - tc-pr-filters -> print-filterset
  - Alter APersistentMap
  - Check that local binding occurrences match with expected types
  - Heterogeneous maps are APersistentMap's instead of IPersistentMap's
  - Heterogeneous vectors are APersistentVector's instead of IPersistentVector's

0.1.1

- Ensure `ann-form` finally checks its expression is of the expected type
- Improve simplifying of intersections involving Java classes

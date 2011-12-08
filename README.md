# typed-clojure

Clojure with a type system. 

## Utilizing the ClojureScript analyzer

Type checking will be performed a la carte. It will not be complected with compilation.

If a compelling reason is found to run type checking at compilation, it may be incorporated.
But for now, a separate tool is both easier conceptually and in implementation.

(Thanks rhickey and dnolen for explaining this)

### Type checking usage

Type checking could be performed at the REPL

```clojure
(type-check "src/typed-clojure/test.clj")
```

This would trigger some preparatory events

- `clojure.core` is loaded into the analyzer's namespaces mechanism

Types should be namespaced as usual.

```clojure
(ns typed-clojure.test
  (:require [typed-clojure.types :as t]))
```

`T` form is for annotating global bindings.

```
(ns typed-clojure.test
  (:require [typed-clojure.types :as t]))

(t/T one :- t/IntegerT)
(def one 1)
```

`T` is a noop during normal compilation. At type-checking time, it
adds `:type` entry to the analyzer's namespace mechanism. For example, the
previous example adds the following entry (roughly)

```clojure
{typed-clojure.test {:defs {one {:type typed-clojure.types/IntegerT}}}}
```

### 

## Clojure Type Hints

We should not reuse the syntax of type hints.

```clojure
(defn ^Integer foo [^Double a])
  (int a))
```

This seems like a role for a separate tool.

## Predicates and Occurances

Typed Racket uses predicates to refine the type of occurances. For example
in the following snippet, we know the 'then' branch returns a number because
`number?` returns true.

```racket
(if (number? x)
  x
  nil)
```

Is this a common idiom in Clojure? How do Multimethods and Protocols come into play?

Can we expand a multimethod call to refine the types of occurances?

# Resources

## Getting Started with Racket

* [Racket Docs](http://docs.racket-lang.org/)
* [Dr. Racket](http://racket-lang.org/download/)
* [Racket Blog](http://blog.racket-lang.org/)

## Getting Started with Typed Racket

* [Typed Racket Docs](http://docs.racket-lang.org/ts-guide/index.html?q=typed)

## Racket Macros

* Dr. Racket's Macro Stepper
* [The Racket Guide: Macros](http://docs.racket-lang.org/guide/macros.html)
* [Writing Hygienic Macros in Scheme with Syntax-Case (1992)](http://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0CCUQFjAA&url=http%3A%2F%2Fciteseerx.ist.psu.edu%2Fviewdoc%2Fdownload%3Fdoi%3D10.1.1.67.4504%26rep%3Drep1%26type%3Dpdf&ei=E7jNTrypDYytiQfHz_28Dg&usg=AFQjCNF297mxp5bwUuhTvAHzjd0jzsTlBQ&sig2=tJka0vmSYJzgsk6y2HSlZA)
* [Writing 'syntax-case' Macros (April 2011)](http://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html)
* [Advanced Macrology and the Implementation of Typed Scheme (2011)](http://www.ccs.neu.edu/racket/pubs/pldi11-thacff.pdf)

## Typed Racket

* [Logical Types for Untyped Languages](http://www.ccs.neu.edu/racket/pubs/icfp10-thf.pdf)
* [Typed Scheme: From Scripts to Programs](http://www.ccs.neu.edu/racket/pubs/dissertation-tobin-hochstadt.pdf)
* [Advanced Macrology and the Implementation of Typed Scheme (2011)](http://www.ccs.neu.edu/racket/pubs/pldi11-thacff.pdf)
* [Sam Tobin-Hochstadt's Home page](http://www.ccs.neu.edu/home/samth/)

## Typed Racket Milestones

Thanks to Sam Tobin-Hochstadt for breaking down the major hurdles of Typed Racket.

1. Devising an appropriate type system for Racket: [The Design and Implementation of Typed Scheme (2008)](http://www.ccs.neu.edu/racket/pubs/popl08-thf.pdf)
2. Arranging the Racket macro system to complement the type system: [Languages as Libraries (2011)](http://www.ccs.neu.edu/racket/pubs/pldi11-thacff.pdf)
3. Plugging the type system with the Racket ecosystem: [Interlanguage Migration: From Scripts to Programs (2006)](http://www.ccs.neu.edu/racket/pubs/dls06-thf.pdf)

## General

* [Logic Type Inference (2000)](http://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0CB8QFjAA&url=http%3A%2F%2Fciteseerx.ist.psu.edu%2Fviewdoc%2Fdownload%3Fdoi%3D10.1.1.105.833%26rep%3Drep1%26type%3Dpdf&ei=K7TNTvfJBuGUiAfJnr29Dg&usg=AFQjCNFdkcqKveaFZJTJbGudzUABvoxw0g&sig2=NgA9MCv_80HR53R-oY83Iw)



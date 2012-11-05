# Typed Clojure

Gradual typing in Clojure, as a library.

[A Practical Optional Type System for Clojure](http://cloud.github.com/downloads/frenchy64/papers/paper.pdf)

# License

Typed Clojure is released under the same license as Clojure: Eclipse Public License v 1.0.

See `LICENSE`.

# Download

Leiningen (Clojars):

`[typed "0.1.1"]`

# Changelog

0.1.2-SNAPSHOT
- TODO
  - Add tests for (if (seq a) (first a) 0) filter example.

- DONE
  - Fix objects and filters being lost during polymorphic and dotted function applications
  - Can annotate datatypes outside current namespace

0.1.1

- Ensure `ann-form` finally checks its expression is of the expected type
- Improve simplifying of intersections involving Java classes

# Quickstart

`(typed.core/ann v t)` gives var `v` the static type `t`.

`(typed.core/ann-form f t)` ensures form `f` is of the static type `t`.

`(typed.core/check-ns)` type checks the current namespace.

`(typed.core/cf t)` type checks the form `t`.

# Future work

* Equality filters for occurrence typing
* Type check multimethods
* Rest type checking in fn definition
* Type check defprotocol definitions
* Unify AST with ClojureScript
* Namespace dependency management
* Check [Asteriods](https://github.com/ztellman/penumbra/blob/master/test/example/game/asteroids.clj)

# Examples

(These don't completely type check yet)

* [typed.test.rbt](https://github.com/frenchy64/typed-clojure/blob/master/test/typed/test/rbt.clj) for examples of mutually recursive types and heterogenous maps
* [typed.test.core-logic](https://github.com/frenchy64/typed-clojure/blob/master/test/typed/test/core_logic.clj) for examples of typing (tightly coupled) datatypes and protocols
* [typed.test.example](https://github.com/frenchy64/typed-clojure/blob/master/test/typed/test/example.clj) for a few little examples of simple usage

# Limitations

## Clojure version

Only Clojure 1.5.0-beta1 is supported.

## Namespace management

Typed dependencies NYI.

## Destructuring

Only map destructuring *without* options is supported.

Other forms of destructuring require equality filters.

## Dotted Functions

A dotted function contains a dotted variable in its function type.

eg. map's type: 
     `(All [c a b ...]
           [[a b ... b -> c] (U nil (Seqable a)) (U nil (Seqable b)) ... b -> (Seqable c)]))`

Currently Typed Clojure does not support *any* checking of use or definition of
dotted functions, only syntax to define its type.

## Rest Arguments

Currently cannot check the definition of functions with rest arguments,
but usage checking should work.

## c.c/apply NYI

## Using `filter`

Not everything can be inferred from a `filter`. A common example is
`(filter identity coll)` does not work. The reason is `identity` only
gives negative information when its result is true: that the argument is *not*
`(U nil false)`.

This idiom must be converted to this syntax `(fn [a] a)` and then annotated with
positive propositions.

```clojure
;eg. 

(filter (ann-form (fn [a] a)
                  [(U nil Number) -> (U nil Number) :filters {:then (is Number 0)}])
        [1 nil 2])
; :- (Seqable Number)
```

Positive information infers just fine, like `(filter number? coll)`.
The above idiom is useful when you are filtering something like a `(Seqable (U nil x))` and there is no
predicate to test for `x`, so you can only test if something isn't `nil`.

# Usage

## Type Syntax

Rough grammar.

```
Type :=  nil
     |   true
     |   false
     |   (U Type*)
     |   (I Type+)
     |   FunctionIntersection
     |   (Value CONSTANT-VALUE)
     |   (Rec [Symbol] Type)
     |   (All [Symbol+] Type)
     |   (All [Symbol* Symbol ...] Type)
     |   (HMap {Keyword Type*})        ;eg (HMap {:a (Value 1), :b nil})
     |   (Vector* Type*)
     |   (Seq* Type*)
     |   (List* Type*)
     |   Symbol  ;class/protocol/free resolvable in current form

FunctionIntersection :=  ArityType
                     |   (Fn ArityType+)

ArityType :=  [FixedArgs -> Type]
           |   [FixedArgs RestArgs * -> Type]
           |   [FixedArgs DottedType ... Symbol -> Type]

FixedArgs := Type*
RestArgs := Type
DottedType := Type
```

### Special constants

`nil`, `true` and `false` resolve to the respective singleton types for those values

### Intersections

`(I Type+)` creates an intersection of types.

### Unions

`(U Type*)` creates a union of types.

### Functions

A function type is an ordered intersection of arity types.

There is a vector sugar for functions of one arity.

### Heterogeneous Maps

`(HMap {:a (Value 1)})` is a IPersistentMap type that contains at least an `:a`
key with value `(Value 1)`.

### Heterogeneous Vectors

`(Vector* (Value 1) (Value 2))` is a IPersistentVector of length 2, essentially 
representing the value `[1 2]`.

### Polymorphism

The binding form `All` introduces a number of free variables inside a scope.

Optionally scopes a dotted variable by adding `...` after the last symbol in the binder.

eg. The identity function: `(All [x] [x -> x])`
eg. Introducing dotted variables: `(All [x y ...] [x y ... y -> x])

### Recursive Types

`Rec` introduces a recursive type. It takes a vector of one symbol and a type.
The symbol is scoped to represent the entire type in the type argument.

```clojure
; Type for {:op :if
            :test {:op :var, :var #'A}
            :then {:op :nil}
            :else {:op :false}}
(Rec [x] 
     (U (HMap {:op (Value :if)
               :test x
               :then x
               :else x})
        (HMap {:op (Value :var)
               :var clojure.lang.Var})
        (HMap {:op (Value :nil)})
        (HMap {:op (Value :false)})))))
```

## Anonymous Functions

`typed.core/fn>` defines a typed anonymous function.

```clojure
eg. (fn [a b] (+ a b))
=>
(fn> [[a :- Number]
       [b :- Number]]
   (+ a b))
```

## Annotating vars

`typed.core/ann` annotates vars. Var does not have to exist at usage.

If definition isn't type checked, it is assumed correct anyway for checking usages.

All used vars must be annotated when type checking.

## Annotating datatypes

`typed.core/ann-datatype` annotates datatypes. 

Takes a name and a vector of fieldname/type type entries.

```clojure
(ann-datatype Pair [[lhs :- Term]
                    [rhs :- Term]])

(deftype Pair [lhs rhs]
  ...)
```

## Annotating Protocols

`typed.core/ann-protocol` annotates protocols.

Takes a name and a optionally a :methods keyword argument mapping
method names to expected types.


```clojure
(ann-protocol IUnifyWithLVar
              :methods
              {unify-with-lvar [Term LVar ISubstitutions -> (U ISubstitutions Fail)]})

(tc-ignore
(defprotocol IUnifyWithLVar
  (unify-with-lvar [v u s]))
)
```

## Type Aliases

`typed.core/def-alias` defines a type alias.

```clojure
(def-alias Term (I IUnifyTerms 
                   IUnifyWithNil
                   IUnifyWithObject
                   IUnifyWithLVar
                   IUnifyWithSequential
                   IUnifyWithMap
                   IUnifyWithSet
                   IReifyTerm
                   IWalkTerm
                   IOccursCheckTerm
                   IBuildTerm))
```

## Ignoring code

`typed.core/tc-ignore` tells the type checker to ignore any forms in the body.

```clojure
(tc-ignore
(defprotocol IUnifyTerms
  (unify-terms [u v s]))
)
```

## Declarations

`typed.core/declare-types`, `typed.core/declare-names` and `typed.core/declare-protocols` are similar
to `declare` in that they allow you to use types before they are defined.

```clojure
(declare-datatypes Substitutions)
(declare-protocols LVar)
(declare-names MyAlias)
```

## Checking typed namespaces

`typed.core/check-ns` checks the namespace that its symbol argument represents.

```clojure
(check-ns 'my.ns)
```

## Debugging

`typed.core/tc-pr-env` prints the environment at a particular point.

```clojure
(let [a 1]
  (tc-pr-env "Env:")
  a)
; Prints: "Env:" {:env {a (Value 1)},  ....}
```

`typed.core/cf` can be used at the REPL to return the type of a form.

```clojure
(cf 1)
;=> [(Value 1) {:then [top-filter], :else [bot-filter]} empty-object]
```

## Macros & Macro Definitions

Macro definitions are ignored. The type checker operates on the macroexpanded form from
the Compiler's analysis phase.

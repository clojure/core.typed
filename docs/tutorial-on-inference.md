# Tutorial on Type Inference

This tutorial is written for someone who might be interested in the
implementation of type inference in Typed Clojure.

This tutorial assume reader has some knowledge about types and
[syntax](https://github.com/clojure/core.typed/wiki/Types) in Typed Clojure.

## Why type inference is useful

So, why type inference is useful at all? Well, the most obvious usage of type
inference is it can save you from typing too much, for example, what type the
following expression has?

    (let [x 100] x)

it is too obvious, it has type `Number`, so Typed Clojure do not requires you
to annotate it as:

    (t/ann-form (let [x 100] x) Number)

although you can do that.

The second reason is type inference is necessary for type check, because many
functions have type variable that can be instantiated as any type, for example,
`identity` returns whatever argument it receives. So we just annotate it as:

    (All [x] [x -> x])

So how do we know if expression `(requires-number (identity 2))` can pass type
check or not? We have to do some type inference to infer expression
`(identity 2)` has type `Number`, and function `requires-number` requires a
`Number` to be its argument.

## Step by step tutorial on type inference

## Rest and drest function type

## Prest and pdot extends

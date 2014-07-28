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

Let us have an example to show what going on in Typed Clojure.

Assume we are checking type of expression `(identity 100)`, this expression is
function call, so we should know function type and type of each arguments, if
function type and types of arguments do not match, then it will get type error,
and if they match, we should get result type of this function application.

As we said earlier, `identity` function has type `(All [x] [x -> x])`, so this
type involve type variable `x`, we should first figure out what type value the
varibale `x` has, the process of finding out that value is called type
inference.

The process needs a data structure called cset(constraint set), we can
simplify that structure as `{:S xxx :T zzz}`, `S` means the bottom type, `T`
means the top type, it means a type variable should be type of `S` at least,
and type of `T` at most. You can read Local Type Inference
[paper](http://www.cis.upenn.edu/~bcpierce/papers/lti.pdf) for more
information. We should change the value of them during the process of type
inference. They are initialized as `{:S Bottom :T Any}`.

So if we do `(t/cf (identity 100) Number)`, we check our expected type `Number`
first, this will make cset of `x` to be `{:S Bottom :T Number}`, because `x`
should be at most type of `Number` to satisfy constraint. And then, we
check if the arguments satisfy constraint, `x` is type `100`, this is value
type in Typed Clojure, it help us specify type information more accurate, and
then we get cset of `x` to be `{:S 100 :T Any}`, attentition here, `T` of `x`
is still `Any` type, because we can only get one constraint at once, if we want
to combine the two result into one, we should do some sort of meet, this is
implemented in cset-meet of cs-gen.clj, so after we do cset-meet of two cset we
already have, we will get `{:S 100 :T Number}`, this cset-meet success because
type of `S` is subtype of type of `T`. If we do
`(cset-meet {:S String :T Any} {:S Bottom :T Number})`, it will get an error,
because `String` is not subtype of `Number`. And we treat this error as failure
of type inference process. This might caused by
`(t/cf (identity "aaa") Number)`, which has obvious type mismatch.

So cset of `x` we get is `{:S 100 :T Number}`, and no error happened during
this process, we consider this inference as success, so we will choose the most
accurate type to be the type of function, so we choose `100`. And type of
`identity` in this case becomes `[100 -> 100]`, it is a function accept a
argument must have type `100`, and returns type `100`. After we get this
function type, we check if all arguments is subtype of parameters of function,
and return type is subtype of expected type. In this case, we check if `100` is
subtype of `100`, and if `100` is subtype of `Number`. And we all get yes, so
function application `(identity 100)` is type correct, and this expression
itself has type `100`.

## Rest and drest function type

## Prest and pdot extends

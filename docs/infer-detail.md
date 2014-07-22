# Implementation detail of type inference

This documentation describes implementation detail of function type inference.

## infer-prest

`infer-prest` in cs_gen.clj is the function to infer function type with `<*`
syntax, like `hash-map` with following type:

    (All [k v]
      [(HSeq [k v] :repeat true) <* -> (Map k v)])

The signature of this function is `[X Y S T T-var R expected]`, this is very
similar to `infer-vararg`, except `T-var` is bound to type that on the left of
`<*`, which is `(HSeq [k v] :repeat true)` in `hash-map` example.

I implement it in a very understandable way: split `S` at count of T into
`short-S` and `rest-S`, wrap `rest-S` into `HeterogeneousVector`, and then
invoke `infer` to do the real work.

Because I have already implemented cs-gen for `HeterogeneousVector`, `HSeq` and
`HSequential` with `repeat` attribute. This should works fine.

The problem that concerns me is the explicit usage of `HeterogeneousVector`,
and the ability that people can use whatever type they want to be left of `<*`,
as long as that type has `repeat` attribute.

So if we support `repeat` in `HeterogeneousList` someday, and people use
`HList` as prest type, it will get type error in all cases.

## infer-pdot

`infer-pdot` in cs_gen.clj is the function to infer function type with `<...`
syntax, like `assoc` with following type:

    (All [m k v c ...]
       [m k v (HSeq [c c] :repeat true) <... c
        -> (Assoc m k v c ... c)])

syntax of `<...` is similar to `...` syntax, which requires type on its left,
and bounded symbol on its right.

The signature of `infer-pdot` is
`[X dotted-var dotted-bnd S T T-dotted R must-vars & {:keys [expected]}]`
which is similar to `infer-dots`. But we requires `T-dotted` must has `repeat`
and `type` attributes.

I treat all types in `types` of `T-dotted` as pre-type, so I first substitute
pre-type with fresh symbols generated by `var-store-take`.

Other part of `infer-pdot` is similar to `infer-dots`.

The main user of `<...` is `assoc` function, and `rng` is typically `Assoc`
type. The problem I faced is that it is very difficult to do cs-gen with
`Assoc` and `HMap` while checking with `expected`. Because `HMap` has so many
attributes like `optional`, `absent-keys`, and we can not constraint very
accurately on `target`, `entries` and `dentries` of `Assoc`, at this time.

I am wondering if we could construct expected type when we are done inference
of all arguments, and then check constructed type with `expected` type. This
will makes inference much easier to do, but I have not tried this way, and not
sure if this works.
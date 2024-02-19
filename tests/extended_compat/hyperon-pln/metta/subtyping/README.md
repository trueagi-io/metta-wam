# Subtyping

Define a [subtyping](https://en.wikipedia.org/wiki/Subtyping)
relationship `<:`.

Type `T1` is a subtype of `T2` is denoted as `T1 <: T2` or, in MeTTa
format

```
(<: T1 T2)
```

## Algebraic Laws

The subtyping relationship is:

- Reflexive:
  ```
  ⊢ T <: T
  ```
- Transitive:
  ```
  T1 <: T2
  T2 <: T3
  ⊢
  T1 <: T3
  ```
- Contravariant over inputs and covariant over outputs:
  ```
  T1 <: S1
  S2 <: T2
  ⊢
  S1 -> S2 <: T1 -> T2
  ```

## Relationships between Subtyping and Typing

There are two variants of relationship between subtyping and type, one
using explicit coercion, the other using implicit coercion.

### Explicit Coercion

For explicit coercion, the inference rule is the coercion function
itself, see `coerce` defined in `rule-base.metta`.

### Implicit Coercion

Because MeTTa supports non-determinism we can also do implicit type
coercion.  In that case the typing relationship is treated itself as
type, and its witness is an inference rule, not a coercion function,
see `STImplCoer` defined in `rule-base.metta`.

## Conclusion

From that experiment we can conclude the following:

1. Subtyping is possible with MeTTa.
2. Explicit coercion allows to treat programs (in that case coercion
   functions) on the same level as proofs, in a Curry-Howard
   correspondence fashion.
3. Implicit coercion has the advantage of not requiring a coercion
   function, but does not treat functions on the same level as proofs.

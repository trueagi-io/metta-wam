# Iterative Chaining Experiments

## Description

Experiments to keep intermediary knowledge while chaining.  For
instance, if the following inference tree has been derived

```
                --------(P2)     --------(Q2)
                (P 2) = ⊤        (Q 2) = ⊥
                --------------------------(IDIBase)
                P → Q ≞ ((:: 2 ∅), <0 0.5>)
                             |               -----(ZeroLTSucc)
                             |               0 ⍃ 5
                             |               -----(SuccMonotonicity)
                             |               1 ⍃ 6
--------(P7)  --------(Q7)   |               -----(SuccMonotonicity)
(P 7) = ⊤     (Q 7) = ⊤      |               2 ⍃ 7
--------------------------------------------------(IDIRecursive)
          P → Q ≞ ((:: 7 (:: 2 ∅)), <0.5 0.6667>)
```

Then all subtrees, such as

```
                --------(P2)     --------(Q2)
                (P 2) = ⊤        (Q 2) = ⊥
                --------------------------(IDIBase)
                P → Q ≞ ((:: 2 ∅), <0 0.5>)
```

will be stored in the knowledge base, or some auxiliary atomspace, so
that the next time around, deriving that inference tree will be
easier.

It's not clear how to decide what to store or not store in general,
that is what this experiment is for.  Ideally such decision should
user programmable.

There are at least two ways this could be done

1. With a callback embedded in the chainer.  So that at each step, if
   some condition is met the callback is called.
2. As an outer recursion around the chainer.  So that a large chaining
   is decomposed into a series of smaller chaining by an outer
   recursion, and whenever a smaller chaining is completed, a
   callback, such as storing the results, is called.

## Usage

### Iterative Forward Chaining

To run the Iterative Forward Chaining experiments, enter the following

```bash
metta ifc-xp.metta
```

It should outputs empty results indicating that all tests have passed.

### Iterative Backward Chaining

To run the Iterative Backward Chaining experiments, enter the following

```bash
metta ibc-xp.metta
```

It should outputs empty results indicating that all tests have passed.

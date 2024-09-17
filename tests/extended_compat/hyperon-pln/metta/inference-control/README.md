# Inference Control Experiments

This folder contains a number of experiments on backward chaining
inference control.

Each experiment is realized as a variation of the curried backward
chainer defined under the [../curried-chaining](../curried-chaining)
folder.  All variations follow the same basic idea, which is to place
termination or continuation conditionals at the start of each
non-deterministic branch.  Thus the branch can only continue if it is
evaluated to do so.

## Overview

A short overview of each experiment is provided below, followed by a
more detailed description of our last most successful experiment.

- [inf-ctl-xp.metta](inf-ctl-xp.metta) was the first attempt.  All
  places in the backward chainer code that create of a
  non-deterministic branch are wrapped around a termination
  conditional.  These places are the base case, the recursive step and
  the match query.  The termination condition is passed as a
  termination predicate to the backward chainer, alongside a context
  and a context updater.  Two tests are carried out:
  1. Reproduce the maximum depth behavior of the regular curried
     backward chainer.
  2. Provide a user programmed termination predicate to prune the
     non-deterministic evaluation and find proofs sooner.

- [inf-ctl-month-xp.metta](inf-ctl-month-xp.metta) was the second
  attempt.  It is almost identical to
  [inf-ctl-xp.metta](inf-ctl-xp.metta) but tested over another
  knowledge base, that of the chronological order of the months, with
  a shortcut for January, as it preceeds all other months.

- [inf-ctl-month-bc-xp.metta](inf-ctl-month-bc-xp.metta) was the third
  attempt.  It is almost identical to
  [inf-ctl-month-xp.metta](inf-ctl-month-xp.metta) but instead of
  providing a user defined termination predicate, it provides a user
  defined theory about inference control and calls the backward
  chainer to evaluate termination.  Thus in order to terminate a
  backward chaining branch, the termination condition calls yet
  another instance of the backward chainer, if it manages to find a
  proof of termination, then it terminates, otherwise continues.  That
  experiment failed because it is difficult in MeTTa to reason about
  terms with free variables without systematically altering them with
  unification and subtitution, which is required to reason about the
  state of a proof.  Since adding a proper quotation mechanism was
  beyond the scope of that experiment we decided to work around that
  limitation as described in the next and final experiment.

- [inf-ctl-month-bc-cont-xp.metta](inf-ctl-month-bc-cont-xp.metta) was
  the fourth and final attempt.  It is derived from
  [inf-ctl-month-bc-xp.metta](inf-ctl-month-bc-xp.metta) but with a
  number of differences.
  1. Termination conditionals are replaced by continuation
     conditionals.
  2. A dedicated control structure containing the continuation
     predicates and update functions is provided.
  3. Continuation predicates are different for each type of branches,
     base case, recursive step and match query.
  More information about that experiment can be found in the next
  Section.

## Description of `inf-ctl-month-bc-cont-xp.metta`

### Curried Backward Chainer

As explained above the controlled backward chainer is an altered
version of the curried backward chainer recalled below

```
;; Curried Backward Chainer
(: bc (-> $a    ; Knowledge base
          $b    ; Query
          Nat   ; Maximum depth
          $b))  ; Query result

;; Base case
(= (bc $kb (: $prf $ccln) $_) (match $kb (: $prf $ccln) (: $prf $ccln)))

;; Recursive step
(= (bc $kb (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) (bc $kb (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (bc $kb (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))
```

Given a knowledge base, the backward chainer takes a typing
relationship query of the form

```
(: <PROOF> <THEOREM>)
```

where `<PROOF>` and `<THEOREM>` are MeTTa terms, potentially
containing free variables interpreted as holes to be filled by the
backward chainer and returned as a superposition of grounded terms,
each representing a possible way to relate a proof to a theorem.

The algorithm works as follows:

1. Base case: the proof of a conclusion, expressed as `(: $prf $ccln)`
   is already present in the knowledge base.  A mere `match` query may
   suffice to retrieve such a proof and conclusion.

2. Recursive step: if no such proof and conclusion exist in the
   knowledge base then the problem is dividing into two sub-problems:

   2.1. finding a proof abstraction, `$prfabs`, of a unary rule
        leading a premise, `$prms`, to the target conclusion, `$ccln`.

   2.2. finding a proof argument, `$prfarg`, of the premise defined
        above.

Note that the curried backward chainer is given a maximum depth and
the recursive step is only allowed to be called when such maximum
depth is greater than zero.  This is a crude way to avoid infinite
recursion and can be improved by introducing finer inference control
as discussed further below.

Note also that for such backward chainer to operate, all rules must be
unary, and since MeTTa is not curried by default, n-ary rules must be
explicitely curried, such as

```
(: Trans (-> (=== $x $y) (-> (=== $y $z) (=== $x $z))))
```

instead of

```
(: Trans (-> (=== $x $y) (=== $y $z) (=== $x $z)))
```

This can however be circumvented either by preprocessing rules or
adding a currying inferrence rule that does that on the fly.

Finally, note that all unifications that are taking place during
backward chaining (via `match` or `let*`) may be further constrained
since `$prf` and `$ccln` be may partially grounded expressions instead
of mere variables.  Depending on how constrained they are, the
backward chainer may behave differently, such as

1. a regular backward chainer, if `$ccln` is partially grounded and
   `$prf` is a variable;

2. a proof checker, if both `$prf` and `$ccln` are fully grounded;

3. even a forward chainer, if `$prf` is partially grounded and `$ccln`
   is a variable;

4. or anything in between.

More variations of the curried backward chainer also exist, such as
one producing fully annotated proofs (which can be found under the
[../proof-tree](../proof-tree) folder), or one amenable to build
constructive proofs (which can be found under the [../hol](../hol)
folder).  However, we choose this particular variation to experiment
with inference control because it is the simplest.

### Controlled Backward Chainer

We identify 3 places where non-deterministism occurs in the curried
backward chainer:

1. The entry of the base case `(bc $kb (: $prf $ccln) $_)` which
   non-deterministically competes with the entry of the recursive step
   `(bc (: ($prfabs $prfarg) $ccln) (S $k))` when the depth is above
   zero.

2. The entry of the recursive step for the reason explained above.

3. The match query `(match &kb (: $prf $ccln) (: $prf $ccln))` in the
   base case, which, as per MeTTa semantics, returns the results as a
   non-deterministic superposition.

The idea is to wrap these 3 places with conditionals to prune branches
created at run-time by the backward chainer.  In addition, a context
used as input of the predicates inside these conditionals, is provided
as well as context updater functions.  In order to hold the predicates
and updater functions we defined the following control structure

```
(: Control (-> $b                   ; Query type
               $c                   ; Context type
               Type))
(: MkControl (-> (-> $b $c $c)      ; Abstraction context updater
                 (-> $b $c $c)      ; Argument context updater
                 (-> $b $c Bool)    ; Base case continuation predicate
                 (-> $b $c Bool)    ; Recursive step continuation predicate
                 (-> $b $c Bool)    ; Match continuation predicate
                 (Control $b $c)))  ; Control type
```

The `Control` type constructor defines a type parameterized by the
type of the query `$b` and the type of the context `$c`.

The `MkControl` data constructor defines a data structure respectively
holding:

1. The context updater applied before recursively calling the backward
   chainer on the proof abstraction.

2. The context updater applied before recursively calling the backward
   chainer on the proof argument.

3. The continuation predicate in charge of deciding whether to enter
   the base case depending on the current query and context.

4. The continuation predicate in charge of deciding whether to enter
   the recursive step depending the current query and context.

5. The continuation predicate in charge of deciding whether to
   continue the base case *after* getting the results of the `match`
   call.  This one is important because at the time of its calling,
   the `match` call has already unified the query with the knowledge
   base and thus much more information is available to make a decision
   about the continuation.

Given that control structure we can now present the controlled
backward chainer, starting with its type definition

```
(: bc (-> $a               ; Knowledge base
          (Control $b $c)  ; Control structure
          $c               ; Context
          $b               ; Query
          $b))             ; Query result
```

Meaning, it takes in arguments

1. A knowledge base containing the axioms and rules of the logic.

2. A control structure holding the continuation predicates and updater
   functions for inference control.

3. A user defined context.

4. A query of the form `(: <PROOF> <THEOREM>)`.

You may notice that the maximum depth argument has been removed.  It
is no longer required since it can be emulated with the proper control
structure and context, as described further below.

The base case of the controlled backward chainer becomes

```
(= (bc $kb                                               ; Knowledge base
       (MkControl $absupd $argupd $bcont $rcont $mcont)  ; Control
       $ctx                                              ; Context
       (: $prf $ccln))                                   ; Query
   ;; Base case continuation conditional
   (if ($bcont (: $prf $ccln) $ctx)
       ;; Continue by querying the kb
       (match $kb (: $prf $ccln)
              ;; Match continuation conditional
              (if ($mcont (: $prf $ccln) $ctx)
                  ;; Continue by returning the queried result
                  (: $prf $ccln)
                  ;; Terminate by pruning
                  (empty)))
       ;; Terminate by pruning
       (empty)))
```

replicating the regular curried backward chainer base case but with 2
conditionals, one at the entry of the function with continuation
predicate `$bcont`, and one as a post-filtering conditional applied to
the results of the `match` call, with continuation predicate `$mcont`.
Note the use of `(empty)` which has the effect of pruning the branch.

The recursive step of the controlled backward chainer becomes

```
(= (bc $kb                                              ; Knowledge base
       (MkControl $absupd $argupd $bcont $rcont $mcont) ; Control
       $ctx                                             ; Context
       (: ($prfabs $prfarg) $ccln))                     ; Query
   ;; Recursive step continuation conditional
   (if ($rcont (: ($prfabs $prfarg) $ccln) $ctx)
       ;; Continue by recursing
       (let* (;; Recurse on proof abstraction
              ((: $prfabs (-> $prms $ccln))
               (bc $kb                                         ; Knowledge base
                   (MkControl $absupd $argupd $bcont $rcont $mcont)   ; Control
                   ($absupd (: ($prfabs $prfarg) $ccln) $ctx) ; Updated context
                   (: $prfabs (-> $prms $ccln))))     ; Proof abstraction query
              ;; Recurse on proof argument
              ((: $prfarg $prms)
               (bc $kb                                         ; Knowledge base
                   (MkControl $absupd $argupd $bcont $rcont $mcont)   ; Control
                   ($argupd (: ($prfabs $prfarg) $ccln) $ctx) ; Updated context
                   (: $prfarg $prms))))                  ; Proof argument query
         ;; Output result
         (: ($prfabs $prfarg) $ccln))
       ;; Terminate by pruning
       (empty)))
```

replicating the regular curried backward chainer recursive step but
with a conditional at the entry of the function with the continuation
predicate `$rcont`.  Another addition is the application of the
abstraction and argument context updaters, via calling `$absupd` and
`argupd` before the recursive calls.

### Maximum Depth Controlled Backward Chainer

Once the controlled backward chainer is defined we still need to
provide continuation predicates and context updaters.  Our first test
is to use the maximum depth as context, thus the control structure is
defined as follows

```
(MkControl depth-updater    ; Abstraction context updater
           depth-updater    ; Argument context updater
           top-continuor    ; Base case continuor
           gtz-continuor    ; Recursive step continuor
           top-continuor))  ; Match continuor
```

where `depth-updater` is defined as follows

```
(: depth-updater (-> $a Nat Nat))
(= (depth-updater $query $depth) (dec $depth))
```

meaning it takes a query, a context which is a natural number, and
returns an updated context which is the natural number decreased by 1.

The `top-continuor` is merely a constant predicate that is always
`True`

```
(: top-continuor (-> $a Nat Bool))
(= (top-continuor $query $depth) True)
```

meaning that base case and match calls are always allowed to continue.

The maximum depth is then controlled by the continuation predicate
controlling the recursive step.

```
(: gtz-continuor (-> $a Nat Bool))
(= (gtz-continuor $query $depth)
   (not (is-zero $depth)))
```

which is only true if the depth is greater than zero.

The effect of running the controlled backward chainer with that
control structure is to reproduce the original backward chainer code
without inference control.

The corresponding code and tests can be found in
[inf-ctl-month-bc-cont-xp.metta](inf-ctl-month-bc-cont-xp.metta) after
the boxed comment

```
;;;;;;;;;;;;;;;;;;;;;;
;; Context is depth ;;
;;;;;;;;;;;;;;;;;;;;;;
```

### Reasoning-base Controlled Backward Chainer

This experiment brings us closer to a proper form of inference
control.  Indeed, the continuation predicates are not simply checking
whether a depth is above zero.  Instead, they consider a broader
context, specifically syntactic elements of the target theorem
expressed in the initial query, as well as the state of the current
query as it evolves alongside the inference trace.  Moreover, these
considerations are themselves expressed as a theory.  Thus, they are
two theories, one theory about the problem to be solved, the *problem
theory*, and another theory about the inference control that should be
adopted to efficiently solve that problem, the *control theory*.  A
continuation predicate then, in order to determine whether a
particular branch should be continued or not, calls the backward
chainer on the control theory.  If a proof of continuation is found,
then, the continuation predicate returns True, otherwise, if no such
proof is found then the continuation predicate returns False and the
branch gets pruned.  The backward chainer used to proof theorems in
the control theory has itself no proper inference control beside a
maximum depth, thus the decisional recursion stops here, in that
experiment anyway.

Before explaining how it all works, let us describe the problem and
control theories used in that experiment.

#### Problem Theory

The problem theory considered here merely describes the ordering
relationship between the months of the year.  Thus, the objects are
the 12 months of the year, `Jan` to `Dec` equipped with a non-strict
total order, `?`.  Theorems are statements like `(? Jun Nov)`
expressing that June non-strictly preceeds November.  Axioms are the
precedence relationships between every contiguous months, expressed as
a typing relationship such as

```
(: JF (? Jan Feb))
```

`JF` can be understood as the name of the axiom and `(? Jan Feb)` its
content.

Likewise, inference rules are expressed as typing relationship,
specifically the reflexivity of `?`

```
(: Refl (? $x $x))
```

Its transitivity

```
(: Trans (-> (? $x $y) (-> (? $y $z) (? $x $z))))
```

As well as an extra rule expressing that January non-strictly preceeds
every other month

```
(: JPA (? Jan $x))
```

This last rule can create a shortcut in the proofs for a subset of
theorems, all the ones involving comparing January to a following
month.  For other theorems, other than reflexive ones, the only way to
prove them is to apply the transitivity rule as many times as
necessary.

The idea is that the control theory should be able to express that
shortcut and therefor speed up reasoning for that particular subset of
theorems.

#### Control Theory

Before providing the control theory, let us define the context used by
the control structure.

```
(: Context Type)
(: MkContext (-> $a                     ; Target theorem
                 Nat                    ; Maximum depth
                 Context))
```

Thus the context captures the maximum depth as before, as well as the
target theorem.  For instance if the initial query provided to the
backward chainer is

```
(: $prf (? Jan Jan))
```

the target theorem is `(? Jan Jan)`.

As shown further above, the control structure contains three
continuation predicates, one for the base case, one for the recursive
step and one for the match operation.  The control theory here only
concerns the third one, the continuation predicate for the match
operation.  The two other predicates are controlled by the maximum
depth as before.  As for the third one, the control theory consists of
a single a parametric type, `Continue`, emulating a relationship
between the current query and the context.  If control rules can be
combined to construct such `Continue` type, then we have a proof of
continuation.

The control theory is formalized by three control rules.

The first control rule expresses that if the target theorem is of the
form `(? x x)` and the current proof is `Refl`, then it should
continue.  This is formalized by the following typing relationship

```
(: RS (Continue (: Refl $r) (MkContext (? $x $x) $k)))
```

The second control rule expresses that if the target theorem is of the
form `(? Jan x)` and the current proof is `JPA` then it should
continue.  This is formalized by the following typing relationship

```
(: JS (Continue (: JPA $r) (MkContext (? Jan $x) $k)))
```

The third control rule expresses that if the target theorem is of the
form `(? x y)` such that `x` is different than `Jan` and `x` is
different than `y`, and the current proof is `Trans` or any of the
axioms relating the precedence relationship between contiguous months,
`Jan` excluded, then it should continue.  This is formalized by the
following typing relationships

```
(: TS (-> (? Jan $x)
          (-> (? $x $y)
              (Continue (: $rn $rc) (MkControl (? $x $y) $k)))))
```

where `$rn` ranges `Trans` as well as other axiom names corresponding
to contiguous precedence, and `$rc` is left free.  In all three rules,
`$k`, the depth is left free as well, since inference control based on
maximum depth is left to the base case and recursive step continuation
predicates.

One may consider such `Continue` type somewhat artificial.  It would
be better to formalize what it means to carry inference efficiently,
and reason based on such formalization about the adequacy of
continuing.  Here, however for now, we are merely attempting to
produce a minimal viable way to realize the idea of carrying inference
control based on reasoning, even if that means greatly over
simplifying the task.

As far the context updaters are concerned, they only need to update
the depth, indeed the target theorem should be left unchanged, and the
current query needs no update because it is automatically passed to
the continuation predicates.

By running the same inference tests on the problem theory, first
solely based on maximum depth control, then based on the control
theory described above, we can observe subtantial speed-ups, ranging
from no speed-up when the theorem is easy to proof to many fold
speed-up when the theorem is hard to prove.

The corresponding code and tests can be found in
[inf-ctl-month-bc-cont-xp.metta](inf-ctl-month-bc-cont-xp.metta) after
the boxed comment

```
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context is depth and target theorem ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
```

## Conclusion

This work is only scratching the surface.  Below are a few suggestions
for improvements and futher exploration.

First, the problem and control theories should be more intricate.
Problem theories could cover more traditional mathematical theories,
or real world situations, such as controlling an agent in an
environment.  As for the control theory, it should capture more
broadly the notion of efficient inference control as to be reusable
across problem theories.

Second, the post-filtering conditional in the base case should be
replaced by a proper attentional focus mechanism.  This would save
computation spent by over-retreaving results which are then lost by
filtering.  Instead the `match` call should be able to restrict its
attention to reflect the predicate of that conditional.

Third, the code of the controlled backward chainer could be
automatically generated from the vanilla backward chainer code,
possibly applying some optimizations during the rewrite as to pay
up-front some of the cost instead of at run-time.  Note that this
potentially applies to any non-deterministic MeTTa program and relates
to the broader notion of program specialization, as per the work of
Alexey Potapov et al in other contexts.

Fourth, in the current design, the control mechanism is centralized
and systematic, which likely incurs a considerable overhead.  Each and
every step needs to go through the approval of the "global police" (in
Greg Meredith's words), also making such control not amenable to
concurrent processing.

Fifth, as previously discussed in various calls, the inference control
mechanics do not need to exist only as MeTTa code.  It could exist
below MeTTa, as Minimal MeTTa code.  Or even below Minimal MeTTa, as
foreign function code.  In fact there may be ways for these multiple
levels to co-exist as embodying particular implementations of the same
specification, maybe expressed with the help of type primitives over
abstract program traces, as suggested by Ben Goertzel.

Sixth, the control theory should incorporate evidence based aspects,
like PLN or NAL, as to be able to automatically discover control rules
via mining, evolutionary programming, or more generally abstract and
emprical reasoning.

Seventh, inference control may also exist at a higher level, going
beyond directly pruning or selecting the most likely branch.  For
instance by expressing the process of searching proofs in more
abstract or compositional ways.  Higher level inference control rules
could for instance describe how to decompose certain problems or when
to apply certain tatics.

Eighth, in these experiments, the problem and control theories have no
overlap.  In future experiments such theories may overlap, or may even
be the same.  What it would mean is that any knowledge gained about
solving problems would potentially be transferable to the problems of
inference control, allowing the possibility of a virtuous feedback
loop to emerge.  As such, as the backward chainer would become smarter
at solving problems in the outer world, it would also get smarter at
solving problems in the inner world.

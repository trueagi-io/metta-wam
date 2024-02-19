# Program Synthesis Experiments

Contains a number of experiments to implement program synthesis, and
thus by extension reasoning, in MeTTa.

- `synthesize-via-type-checking.metta`: experiment to implement a
  synthesizer from scratch attempting to use the type checker to check
  the validity of combinations.

- `synthesize-via-superpose.metta`: experiment similar to
  `synthesize-via-type-checking.metta` but simplified by using
  superposition.  It fails because the type checker is static.

- `unify-via-let.metta`: experiment to demonstrate that full syntactic
  unification can be achieved with the `let` operator.

- `unify-via-case.metta`: same as `unify-via-let.metta` but it uses
  `case` instead of `let`.

- `non-determinism.metta`: experiment with non-determinism.

- `synthesize-via-let.metta`: experiment to demonstrate that program
  synthesis can be achieved by combining unification via `let` and
  non-determinism.

- `synthesize-via-let-test.metta`: contains a dozen+ tests of program
  synthesis via let.  It shows that forward chaining, backward
  chaining, type inference and more can be accomplished with this
  simple technique.

- `synthesize-via-case.metta`: same as `synthesize-via-let.metta` but
  it uses `case` instead of `let`.  The advantage is that it ignores
  branches that are not fully reduced.

- `synthesize-via-case-test.metta`: same as
  `synthesize-via-let-test.metta` but it uses `case` instead of `let`.

- `synthesize-via-unify.metta`: same as `synthesize-via-let.metta`,
  but it uses `unify`/`unify*` instead of `let`/`let*`.  It ignores
  branches that are not fully reduced, however it is slower than
  `synthesize-via-case.metta`.

- `synthesize-via-unify-test.metta`: same as
  `synthesize-via-let-test.metta` but it is based on `unify` instead
  of `let`.  It currently does not work due to that
  [issue](https://github.com/trueagi-io/hyperon-experimental/issues/385).

- `self-contained-synthesize.metta`: Self-contained version of
  `sythesize-via-case.metta` and `synthesize-via-case-test.metta`,
  for demonstration purposes.

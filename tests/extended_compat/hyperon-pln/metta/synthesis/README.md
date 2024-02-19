# Program Synthesis

Contains a unifier and a generic program synthesizer.

- `Unify.metta`: define unify and unify* operators.  Like `let` and
  `let*` but rely on `case` to discard not fully reduced branches.

- `UnifyTest.metta`: test unify and unify* (not working yet).

- `Synthesize.metta`: Generic program synthesizer based on `case`.  It
  is meant to be using `unify` and `unify*`, however due to some
  issues it still relies on `let` and `let*`.  For an example of
  synthesizer based on `unify`, see
  `experiments/synthesize-via-unify.metta`.

- `SynthesizeTest.metta`: Tests for `Synthesize.metta`.

- `experiments`: folder containing a number of experiments leading to
  that particular implementation.

# Chaining Experiments in MeTTa

Different ways are explored to port PLN to MeTTa.  These ways are
organized into the following folders:


- match: rule is represented using MeTTa operator match;

- entail: rule is represented with the symbol ⊢;

- backward-chaining: backward chaining experiments, both using proof
  as program and proof as program execution trace.

- forward-chainer: forward chainer experiments.
- forward-chaining: forward chaining experiments, both using proof as
  program and proof as program execution trace.

- polyward-chaining: forward chaining experiments combined with
  backward chaining.

- hol: higher order logic, attempt to reason about programs, such as
  providing that 0 is the right identity of +.

- inference-control: inference control experiments, replace depth by
  control functions.

- iterative-chaining: save intermediary results while reasoning.

- synthesis: program synthesis experiments (backward chaining).

- subtyping: subtyping inference experiments.

- sumo: attempt to reason over sumo.

- common: some reusable type and function definitions.

- pln: collection of PLN port attempts.

- equal: rule is represented using MeTTa equality =;

- dependent-types: rule is represented as type constructor;

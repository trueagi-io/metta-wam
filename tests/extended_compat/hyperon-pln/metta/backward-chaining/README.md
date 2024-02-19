# Backward Chaining Experiments

## Description

Contains a number of experiments to realize backward chaining in
MeTTa.  Specifically:

* *Bare Entail*: represent rules with the `⊢` relationship.  Knowledge
  however is not wrapped around `⊢`.
* *Equality*: like bare entail but the rules are directly encoded in
  the backward chainer function.
* *Bare Entail Match*: like bare entail but both knowledge and rule
  bases are stored in their own spaces and `match` is used instead of
  `let`.  `let*` is still used to bind the and-branches of the
  inference trees together.  By and-branches we mean the multiple
  premises an inference rule must satify at once.
* *Equality Match*: like equality but the knowledge base is stored in
  its own space, the rules are directly encoded in the backward
  chainer function, and `match` is used instead of `let` to match
  premises with the knowledge base.  `let*` is still used to bind the
  and-branches of the inference trees together.
* *DTL Equality Match*: like DTL (Synthesizer) but the rules are hard
  wired in the backward chainer function and match used instead of let
  to match the premises with the knowledge base.  `let*` is still used
  to bind the and-branches of the inference trees together.

## Usage

To run the experiments, enter the following

```bash
metta bc-xp.metta
```

It should outputs empty results indicating that all tests have passed.

## Related

In the `iterative-chaining` experiments, a new implementation of the
backward chainer was discovered which recurses on rule abstraction
(not just rule premises) and uses currying to simplify the
implementation.  See `syn` in `ibc-xp.metta`.

Also, in the `hol` experiments, it was discovered that proof reduction
(simplifying a proof to reach a canonical form) can be achieved by
defining reduction rules as traditional function definitions, and thus
let MeTTa interper spontaneously reduce them.

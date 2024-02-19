# Forward Chaining Experiments

## Description

Contains a number of experiments to realize forward chaining in MeTTa.
Specifically:

* *Entail*: represent knowledge and rules with the `⊢` relationship.
* *Bare Entail*: like entail but only rules are represented with the
  `⊢` relationship.
* *Equality*: like bare entail but the rules are directly encoded in
  the forward chainer function.
* *Bare Entail Match*: like bare entail but both knowledge and rule
  bases are stored in their own spaces and `match` is used instead of
  `let`.
* *Equality Match*: like equality but the knowledge base is stored in
  its own space, the rules are directly encoded in the forward chainer
  function, and `match` is used instead of `let`.
* *DTL*: the knowledge and rule bases are represented with the typing
  relationship, proofs are carried along the chaining, as in a
  Dependently Typed Language.

## Usage

To run the experiments, enter the following

```bash
metta fc-xp.metta
```

It should outputs empty results indicating that all tests have passed.

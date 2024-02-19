# Reasoning on SUMO Experiments

Contains a number of reasoning experiments on SUMO, organized into
subfolders

* `orientation`
* `located`
* `john-carry-flower`

Each subfolder contains an experiment, with a particular snippet of
SUMO + a query to fulfill, i.e. a theorem to prove.

In addition it contains
1. `suo-kif-to-metta.sh`, a script to convert SUO-KIF (SUMO's file
   format) into MeTTa.
2. `load-suo-kif.metta`, an incomplete attempt to load SUO-KIF
   directly into MeTTa.
3. `rule-base.metta`, a set of rules to reason about SUMO.

## Import SUO-KIF

### Convert SUO-KIF to MeTTa

Due to the differences in variables between SUO-KIF and MeTTa, one may
apply the script `suo-kif-to-metta.sh` to convert SUO-KIF files into
MeTTa.  For now only SUO-KIF variables starting with `?` are
considered, thus the source file is assumed not to contain any
sequence variable.

For example

```bash
./suo-kif-to-metta.sh orientation.kif > orientation.kif.metta
```

will produce `orientation.kif.metta` where all SUO-KIF regular
variables have been replaced by MeTTa variables.

### Load SUO-KIF Directly

**WIP**

MeTTa is sufficiently rich to be able to load SUO-KIF file without any
prior conversion.  The challenge then is to apply MeTTa's pattern
matching capabilities to the loaded data.  An important difference
between SUO-KIF and MeTTa is the variable format.  SUO-KIF has two
types of variables, regular variable that starts with `?` and sequence
variable that starts with `@`.  MeTTa has only one type of variable
that starts with `$`.

To load SUO-KIF directly see `load-suo-kif.metta`.

## Dependent Types

The translation could be more Curry-Howard-ish, so that forall
quantifiers are dependent products, existential quantifiers are
dependent sums, implications are function applications, conjunctions
are products, disjunctions are Either, etc.

## Reasoning

### Rule Base

A set of rules have been specially crafted to reason over SUMO and
contained into `rules.metta`.

## Prerequisite

Make sure that hyperon-experimental has been compiled with
`variable_operator` disabled.  To check that, search for `[features]`
and make sure that `default` excludes `"variable_operation"` inside
the `<HYPERON-EXPERIMENTAL>/lib/Cargo.toml` file.

## Usage

See the README.md files under the subfolders of the experiments
for usage.

Note that all MeTTa files coming from SUO-KIF have already been
pre-generated.  If you need to regenerate them see Section [Convert
SUO-KIF to MeTTa](#convert-suo-kif-to-metta).

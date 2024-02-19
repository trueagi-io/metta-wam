# HOL

Higher Order Logic reasoning experiments, extracted from *Programming
and Proving in Isabelle/HOL* by Tobias Nipkow.

It contains the following experiments:

* `NatSimpleTest.metta`: prove really simple assertions such as
  2 + 2 = 4.

* `NatTest.metta`: prove more complex properties such x + Z = x.
  However, there is a little bit of cheating involved such as adding
  the inductive property in the axioms.

* `NatStandaloneTest.metta`: prove that x + Z = x without cheating.
  For that it uses its own chainer instead of relying on the
  synthesizer.

* `ListTest.metta`: prove theorems on lists such that list reversion
  is involutive.

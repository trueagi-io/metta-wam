# Program Verification

Program verification reasoning experiments.  Initially extracted from
*Programming and Proving in Isabelle/HOL* by Tobias Nipkow before it
became its own thing.

The folder contains the following experiments:

* [NatSimpleTest.metta](NatSimpleTest.metta): prove really simple
  assertions such as 2+2=4.

* [NatTest.metta](NatTest.metta): prove more complex properties such
  x+Z=x.  However, there is a bit of cheating involved such as adding
  the inductive property in the axioms.

* [NatStandaloneTest.metta](NatStandaloneTest.metta): prove that x+Z=x
  without cheating.  For that it uses its own chainer instead of
  relying on the synthesizer.

* [NatDTLTest.metta](NatDTLTest.metta): like
  [NatStandaloneTest.metta](NatStandaloneTest.metta) but rules are
  formatted as dependently typed.

* [ListTest.metta](ListTest.metta): initial attempt at proving
  theorems on lists such that list reversion is involutive.

* [NatParityTest.metta](NatParityTest.metta): almost successful
  attempt at proving a simple property, evenness, over a simple
  program, double.  The last bit to do in order to be completely
  successful is to reformulate the structural induction rule to be
  fully general.

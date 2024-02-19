; The script is designed to test a functional programming paradigm where variables
; have dependencies on each other, with changes to one variable automatically updating
; the values of others.  The key aspect of these tests is to ensure that these dependencies
; and updates are managed outside of the traditional call stack, allowing a dynamic,
; model of variable interaction.

; This function hides return values, taking an Atom and returning success
; Used to focus on side effects of a function rather than its output.
(: do (-> Atom %Undefined%))
(= (do $1) (case $1 ()))

; Executes multiple expressions in sequence.
(: sequential (-> Expression %Undefined%))
(= (sequential $1) (superpose $1))

; Define a complex-relationship function that calculates and returns three
; variables simultaneously. It establishes relationships between
; these likelihoods through a series of equations.
; $Likelihood1 is 30% of $Likelihood2.
; $Likelihood2 is 50% of $Likelihood3.
; $Likelihood3 is constrained to be between 0 and 1 (exclusive).
(= (complex-relationship $Likelihood1 $Likelihood2 $Likelihood3)
 (do
   (sequential
      ((= $Likelihood1 (* 0.3 $Likelihood2))
       (= $Likelihood2 (* 0.5 $Likelihood3))
       (< $Likelihood3 1.0)
       (> $Likelihood3 0.0)))))

; Testing relative values of L1, L2, L3 based on their defined relationships
; in complex-relationship. This section tests the relative Likelihoods
; without needing actual values.
!(sequential
  ((do (complex-relationship $L1 $L2 $L3))
   (assertEqual (< $L1 $L3) True)
   (assertEqual (< $L2 $L1) False)))

; Setting a specific value for L2 and checking if L1 and L3 update accordingly.
; This tests the dynamic calculation capability of the complex-relationship
; function based on input changes.
!(sequential
    (  (do (complex-relationship $L1 $L2 $L3))
       (do (= $L2 0.3))
       (assertEqual $L1 0.09)
       (assertEqual $L3 0.6)))

; Using 'let' to temporarily update the value of L2 and checking the effects
; within its scope. This part tests the functions handling of temporary value
; assignments and ensures the same variables outside the 'let' scope are not affected.
!(sequential
  ((do (complex-relationship $L1 $L2 $L3))
   (let $L2 0.3
     (sequential
       ((assertEqual $L1 0.09)
        (assertEqual $L3 0.6))))
   (assertNotEqual $L1 0.09)
   (assertNotEqual $L3 0.6)))

; Presently all these tests are passing in MeTTaLog

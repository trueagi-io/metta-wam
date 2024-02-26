;
;                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; 

;
;                       This is a rough approximation to the algorithm presented in:

;
; 

;
;                      	"An Algorithm for NAND Decomposition Under Network Constraints,"

;
;                      	IEEE Trans. Comp., vol C-18, no. 12, Dec. 1969, p. 1098

;
;                      	by E. S. Davidson.

;
; 

;
;                         Written by Bruce Holmer

;
; 

;
;                       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; 

;
;                         I have used the paper's terminology for names used in the program.

;
; 

;
;                          The data structure for representing functions and variables is

;
;                        		function(FunctionNumber, TrueSet, FalseSet,

;
;                        			ConceivableInputs,

;
;                        			ImmediatePredecessors, ImmediateSuccessors,

;
;                         			Predecessors, Successors)

;
; 

;
; 

;
;                           Common names used in the program:

;
; 

;
;                          	NumVars		number of variables (signal inputs)

;
;                          	NumGs		current number of variables and functions

;
;                          	Gs		list of variable and function data

;
;                          	Gi,Gj,Gk,Gl	individual variable or function--letter corresponds to

;
;                          			the subscript in the paper (most of the time)

;
;                           	Vector,V	vector from a function's true set

;
; 

;
;                           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  (= 
    (top) 
    (main 0))


  (= 
    (main $N) 
    ( (init-state $N $NumVars $NumGs $Gs) 
      (add-necessary-functions $NumVars $NumGs $Gs $NumGs2 $Gs2) 
      (test-bounds $NumVars $NumGs2 $Gs2) 
      (search $NumVars $NumGs2 $Gs2)))
  (= 
    (main  $_) True)
;
;                             	write('Search completed'), nl.


;
;                               Test input

;
;                               init_state(circuit(NumInputs, NumOutputs, FunctionList))


  (= 
    (init-state 0 2 3 
      (:: 
        (function 2 
          (:: 1 2) 
          (:: 0 3) Nil Nil Nil Nil Nil) 
        (function 1 
          (:: 2 3) 
          (:: 0 1) Nil Nil Nil Nil Nil) 
        (function 0 
          (:: 1 3) 
          (:: 0 2) Nil Nil Nil Nil Nil))) 
    (update-bounds $_ 100 $_))
;                               ; 2 input xor


  (= 
    (init-state 1 3 4 
      (:: 
        (function 3 
          (:: 3 5 6 7) 
          (:: 0 1 2 4) Nil Nil Nil Nil Nil) 
        (function 2 
          (:: 4 5 6 7) 
          (:: 0 1 2 3) Nil Nil Nil Nil Nil) 
        (function 1 
          (:: 2 3 6 7) 
          (:: 0 1 4 5) Nil Nil Nil Nil Nil) 
        (function 0 
          (:: 1 3 5 7) 
          (:: 0 2 4 6) Nil Nil Nil Nil Nil))) 
    (update-bounds $_ 100 $_))
;                               ; carry circuit


  (= 
    (init-state 2 3 4 
      (:: 
        (function 3 
          (:: 1 2 4 6 7) 
          (:: 0 3 5) Nil Nil Nil Nil Nil) 
        (function 2 
          (:: 4 5 6 7) 
          (:: 0 1 2 3) Nil Nil Nil Nil Nil) 
        (function 1 
          (:: 2 3 6 7) 
          (:: 0 1 4 5) Nil Nil Nil Nil Nil) 
        (function 0 
          (:: 1 3 5 7) 
          (:: 0 2 4 6) Nil Nil Nil Nil Nil))) 
    (update-bounds $_ 100 $_))
;                                ; example in paper


  (= 
    (init-state 3 3 4 
      (:: 
        (function 3 
          (:: 1 2 4 7) 
          (:: 0 3 5 6) Nil Nil Nil Nil Nil) 
        (function 2 
          (:: 4 5 6 7) 
          (:: 0 1 2 3) Nil Nil Nil Nil Nil) 
        (function 1 
          (:: 2 3 6 7) 
          (:: 0 1 4 5) Nil Nil Nil Nil Nil) 
        (function 0 
          (:: 1 3 5 7) 
          (:: 0 2 4 6) Nil Nil Nil Nil Nil))) 
    (update-bounds $_ 100 $_))
;                                ; sum (3 input xor)


  (= 
    (init-state 4 3 5 
      (:: 
        (function 4 
          (:: 3 5 6 7) 
          (:: 0 1 2 4) Nil Nil Nil Nil Nil) 
        (function 3 
          (:: 1 2 4 7) 
          (:: 0 3 5 6) Nil Nil Nil Nil Nil) 
        (function 2 
          (:: 4 5 6 7) 
          (:: 0 1 2 3) Nil Nil Nil Nil Nil) 
        (function 1 
          (:: 2 3 6 7) 
          (:: 0 1 4 5) Nil Nil Nil Nil Nil) 
        (function 0 
          (:: 1 3 5 7) 
          (:: 0 2 4 6) Nil Nil Nil Nil Nil))) 
    (update-bounds $_ 100 $_))
;                                ; do sum and carry together


  (= 
    (init-state 5 5 8 
      (:: 
        (function 7 
          (:: 1 3 4 6 9 11 12 14 16 18 21 23 24 26 29 31) 
          (:: 0 2 5 7 8 10 13 15 17 19 20 22 25 27 28 30) Nil Nil Nil Nil Nil) 
        (function 6 
          (:: 2 3 5 6 8 9 12 15 17 18 20 21 24 27 30 31) 
          (:: 0 1 4 7 10 11 13 14 16 19 22 23 25 26 28 29) Nil Nil Nil Nil Nil) 
        (function 5 
          (:: 7 10 11 13 14 15 19 22 23 25 26 27 28 29 30 31) 
          (:: 0 1 2 3 4 5 6 8 9 12 16 17 18 20 21 24) Nil Nil Nil Nil Nil) 
        (function 4 
          (:: 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31) 
          (:: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) Nil Nil Nil Nil Nil) 
        (function 3 
          (:: 8 9 10 11 12 13 14 15 24 25 26 27 28 29 30 31) 
          (:: 0 1 2 3 4 5 6 7 16 17 18 19 20 21 22 23) Nil Nil Nil Nil Nil) 
        (function 2 
          (:: 4 5 6 7 12 13 14 15 20 21 22 23 28 29 30 31) 
          (:: 0 1 2 3 8 9 10 11 16 17 18 19 24 25 26 27) Nil Nil Nil Nil Nil) 
        (function 1 
          (:: 2 3 6 7 10 11 14 15 18 19 22 23 26 27 30 31) 
          (:: 0 1 4 5 8 9 12 13 16 17 20 21 24 25 28 29) Nil Nil Nil Nil Nil) 
        (function 0 
          (:: 1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31) 
          (:: 0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30) Nil Nil Nil Nil Nil))) 
    (update-bounds $_ 21 $_))
;                                 ; 2 bit full adder

;                                 ; A2 (output)

;                                 ; B2 (output)

;                                 ; carry-out (output)

;                                  ; carry-in

;                                  ; B1 input

;                                  ; B0 input

;                                  ; A1 input

;                                  ; A0 input




;
;                                  Iterate over all the TRUE vectors that need to be covered.

;
;                                   If no vectors remain to be covered (select_vector fails), then

;
;                                   the circuit is complete (printout results, update bounds, and

;
;                                   continue search for a lower cost circuit).


  (= 
    (search $NumVars $NumGsIn $GsIn) 
    ( (select-vector $NumVars $NumGsIn $GsIn $Gj $Vector) 
      (set-det) 
      (cover-vector $NumVars $NumGsIn $GsIn $Gj $Vector $NumGs $Gs) 
      (add-necessary-functions $NumVars $NumGs $Gs $NumGsOut $GsOut) 
      (test-bounds $NumVars $NumGsOut $GsOut) 
      (search $NumVars $NumGsOut $GsOut)))
  (= 
    (search $NumVars $NumGs $Gs) 
    ( (update-bounds $NumVars $NumGs $Gs) (fail)))
;                                   ;	output_results(NumVars, NumGs, Gs),



;
;                                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                    Given the current solution, pick the best uncovered TRUE vector

;
;                                     for covering next.

;
;                                     The selected vector is specified by its vector number and function.

;
;                                     Select_vector fails if all TRUE vectors are covered.

;
;                                     Select_vector is determinant (gives only one solution).


  (= 
    (select-vector $NumVars $NumGs $Gs $Gj $Vector) 
    ( (select-vector $Gs $NumVars $NumGs $Gs dummy 0 nf 999 $Gj $Vector $Type $_) 
      (set-det) 
      (\= $Type cov) 
      (\= $Type nf)))

;
;                                     loop over functions

  (= 
    (select-vector 
      (Cons  $Gk $_) $NumVars $_ $_ $Gj $V $Type $N $Gj $V $Type $N) 
    ( (function-number $Gk $K) (< $K $NumVars)))
  (= 
    (select-vector 
      (Cons  $Gk $Gks) $NumVars $NumGs $Gs $GjIn $Vin $TypeIn $Nin $GjOut $Vout $TypeOut $Nout) 
    ( (function-number $Gk $K) 
      (>= $K $NumVars) 
      (true-set $Gk $Tk) 
      (select-vector $Tk $Gk $NumVars $NumGs $Gs $GjIn $Vin $TypeIn $Nin $Gj $V $Type $N) 
      (select-vector $Gks $NumVars $NumGs $Gs $Gj $V $Type $N $GjOut $Vout $TypeOut $Nout)))

;
;                                      loop over vectors

  (= 
    (select_vector  () $_ $_ $_ $_ $Gj $V $Type $N $Gj $V $Type $N) True)
  (= 
    (select-vector 
      (Cons  $V $Vs) $Gk $NumVars $NumGs $Gs $GjIn $Vin $TypeIn $Nin $GjOut $Vout $TypeOut $Nout) 
    ( (vector-cover-type $NumVars $Gs $Gk $V $Type $N) 
      (best-vector $GjIn $Vin $TypeIn $Nin $Gk $V $Type $N $Gj2 $V2 $Type2 $N2) 
      (select-vector $Vs $Gk $NumVars $NumGs $Gs $Gj2 $V2 $Type2 $N2 $GjOut $Vout $TypeOut $Nout)))


  (= 
    (vector-cover-type $NumVars $Gs $Gj $Vector $Type $NumCovers) 
    ( (immediate-predecessors $Gj $IPs) 
      (conceivable-inputs $Gj $CIs) 
      (false-set $Gj $Fj) 
      (cover-type1 $IPs $Gs $Vector nf 0 $T $N) 
      (cover-type2 $CIs $Gs $NumVars $Fj $Vector $T $N $Type $NumCovers)))


  (= 
    (cover_type1  () $_ $_ $T $N $T $N) True)
  (= 
    (cover-type1 
      (Cons  $I $IPs) $Gs $V $TypeIn $Nin $TypeOut $Nout) 
    ( (function $I $Gs $Gi) 
      (true-set $Gi $Ti) 
      (not (set-member $V $Ti)) 
      (set-det) 
      (false-set $Gi $Fi) 
      (det-if-then-else 
        (set-member $V $Fi) 
        (max-type $TypeIn cov $Type) 
        (max-type $TypeIn exp $Type)) 
      (is $N 
        (+ $Nin 1)) 
      (cover-type1 $IPs $Gs $V $Type $N $TypeOut $Nout)))
  (= 
    (cover-type1 
      (Cons  $_ $IPs) $Gs $V $TypeIn $Nin $TypeOut $Nout) 
    (cover-type1 $IPs $Gs $V $TypeIn $Nin $TypeOut $Nout))


  (= 
    (cover_type2  () $_ $_ $_ $_ $T $N $T $N) True)
  (= 
    (cover-type2 
      (Cons  $I $CIs) $Gs $NumVars $Fj $V $TypeIn $Nin $TypeOut $Nout) 
    ( (< $I $NumVars) 
      (function $I $Gs $Gi) 
      (false-set $Gi $Fi) 
      (set-member $V $Fi) 
      (set-det) 
      (max-type $TypeIn var $Type) 
      (is $N 
        (+ $Nin 1)) 
      (cover-type2 $CIs $Gs $NumVars $Fj $V $Type $N $TypeOut $Nout)))
  (= 
    (cover-type2 
      (Cons  $I $CIs) $Gs $NumVars $Fj $V $TypeIn $Nin $TypeOut $Nout) 
    ( (>= $I $NumVars) 
      (function $I $Gs $Gi) 
      (true-set $Gi $Ti) 
      (not (set-member $V $Ti)) 
      (set-det) 
      (false-set $Gi $Fi) 
      (det-if-then-else 
        (set-member $V $Fi) 
        (det-if-then-else 
          (set-subset $Fj $Ti) 
          (max-type $TypeIn fcn $Type) 
          (max-type $TypeIn mcf $Type)) 
        (det-if-then-else 
          (set-subset $Fj $Ti) 
          (max-type $TypeIn exf $Type) 
          (max-type $TypeIn exmcf $Type))) 
      (is $N 
        (+ $Nin 1)) 
      (cover-type2 $CIs $Gs $NumVars $Fj $V $Type $N $TypeOut $Nout)))
  (= 
    (cover-type2 
      (Cons  $_ $CIs) $Gs $NumVars $Fj $V $TypeIn $Nin $TypeOut $Nout) 
    (cover-type2 $CIs $Gs $NumVars $Fj $V $TypeIn $Nin $TypeOut $Nout))

;
;                                           The best vector to cover is the one with worst type, or, if types

;
;                                           are equal, with the least number of possible covers.


  (= 
    (best-vector dummy $_ $_ $_ $Gj2 $V2 $Type2 $N2 $Gj2 $V2 $Type2 $N2) 
    (set-det))
  (= 
    (best-vector $Gj1 $V1 $Type1 $N1 dummy $_ $_ $_ $Gj1 $V1 $Type1 $N1) 
    (set-det))
  (= 
    (best-vector $Gj1 $V1 $Type $N1 $Gj2 $_ $Type $N2 $Gj1 $V1 $Type $N1) 
    ( (function-number $Gj1 $J) 
      (function-number $Gj2 $J) 
      (< $N1 $N2) 
      (set-det)))
  (= 
    (best-vector $Gj1 $_ $Type $N1 $Gj2 $V2 $Type $N2 $Gj2 $V2 $Type $N2) 
    ( (function-number $Gj1 $J) 
      (function-number $Gj2 $J) 
      (>= $N1 $N2) 
      (set-det)))
  (= 
    (best-vector $Gj1 $V1 $Type $N1 $Gj2 $_ $Type $_ $Gj1 $V1 $Type $N1) 
    ( (or 
        (= $Type exp) 
        (= $Type var)) 
      (function-number $Gj1 $J1) 
      (function-number $Gj2 $J2) 
      (> $J1 $J2) 
      (set-det)))
  (= 
    (best-vector $Gj1 $_ $Type $_ $Gj2 $V2 $Type $N2 $Gj2 $V2 $Type $N2) 
    ( (or 
        (= $Type exp) 
        (= $Type var)) 
      (function-number $Gj1 $J1) 
      (function-number $Gj2 $J2) 
      (< $J1 $J2) 
      (set-det)))
  (= 
    (best-vector $Gj1 $V1 $Type $N1 $Gj2 $_ $Type $_ $Gj1 $V1 $Type $N1) 
    ( (not (or (= $Type exp) (= $Type var))) 
      (function-number $Gj1 $J1) 
      (function-number $Gj2 $J2) 
      (< $J1 $J2) 
      (set-det)))
  (= 
    (best-vector $Gj1 $_ $Type $_ $Gj2 $V2 $Type $N2 $Gj2 $V2 $Type $N2) 
    ( (not (or (= $Type exp) (= $Type var))) 
      (function-number $Gj1 $J1) 
      (function-number $Gj2 $J2) 
      (> $J1 $J2) 
      (set-det)))
  (= 
    (best-vector $Gj1 $V1 $Type1 $N1 $_ $_ $Type2 $_ $Gj1 $V1 $Type1 $N1) 
    ( (type-order $Type2 $Type1) (set-det)))
  (= 
    (best-vector $_ $_ $Type1 $_ $Gj2 $V2 $Type2 $N2 $Gj2 $V2 $Type2 $N2) 
    ( (type-order $Type1 $Type2) (set-det)))


  (= 
    (max-type $T1 $T2 $T1) 
    ( (type-order $T1 $T2) (set-det)))
  (= 
    (max-type $T1 $T2 $T2) 
    ( (not (type-order $T1 $T2)) (set-det)))

;
;                                           Order of types



  (= 
    (type_order  cov exp) True)
  (= 
    (type_order  cov var) True)
  (= 
    (type_order  cov fcn) True)
  (= 
    (type_order  cov mcf) True)
  (= 
    (type_order  cov exf) True)
  (= 
    (type_order  cov exmcf) True)
  (= 
    (type_order  cov nf) True)
  (= 
    (type_order  exp var) True)
  (= 
    (type_order  exp fcn) True)
  (= 
    (type_order  exp mcf) True)
  (= 
    (type_order  exp exf) True)
  (= 
    (type_order  exp exmcf) True)
  (= 
    (type_order  exp nf) True)
  (= 
    (type_order  var fcn) True)
  (= 
    (type_order  var mcf) True)
  (= 
    (type_order  var exf) True)
  (= 
    (type_order  var exmcf) True)
  (= 
    (type_order  var nf) True)
  (= 
    (type_order  fcn mcf) True)
  (= 
    (type_order  fcn exf) True)
  (= 
    (type_order  fcn exmcf) True)
  (= 
    (type_order  fcn nf) True)
  (= 
    (type_order  mcf exf) True)
  (= 
    (type_order  mcf exmcf) True)
  (= 
    (type_order  mcf nf) True)
  (= 
    (type_order  exf exmcf) True)
  (= 
    (type_order  exf nf) True)
  (= 
    (type_order  exmcf nf) True)

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                           Cover_vector will cover the specified vector and

;
;                                           generate new circuit information.

;
;                                           Using backtracking, all possible coverings are generated.

;
;                                           The ordering of the possible coverings is approximately that

;
;                                           given in Davidson's paper, but has been simplified.



  (= 
    (cover-vector $NumVars $NumGsIn $GsIn $Gj $Vector $NumGsOut $GsOut) 
    ( (immediate-predecessors $Gj $IPs) 
      (conceivable-inputs $Gj $CIs) 
      (vector-types $Type) 
      (cover-vector $Type $IPs $CIs $Gj $Vector $NumVars $NumGsIn $GsIn $NumGsOut $GsOut)))


  (= 
    (vector_types  var) True)
  (= 
    (vector_types  exp) True)
  (= 
    (vector_types  fcn) True)
  (= 
    (vector_types  mcf) True)
  (= 
    (vector_types  exf) True)
  (= 
    (vector_types  exmcf) True)
  (= 
    (vector_types  nf) True)


  (= 
    (cover-vector exp 
      (Cons  $I $_) $_ $Gj $V $_ $NumGs $GsIn $NumGs $GsOut) 
    ( (function $I $GsIn $Gi) 
      (true-set $Gi $Ti) 
      (not (set-member $V $Ti)) 
      (update-circuit $GsIn $Gi $Gj $V $GsIn $GsOut)))
  (= 
    (cover-vector exp 
      (Cons  $_ $IPs) $_ $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    (cover-vector exp $IPs $_ $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut))
  (= 
    (cover-vector var $_ 
      (Cons  $I $_) $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    ( (< $I $NumVars) 
      (function $I $GsIn $Gi) 
      (false-set $Gi $Fi) 
      (set-member $V $Fi) 
      (update-circuit $GsIn $Gi $Gj $V $GsIn $GsOut)))
  (= 
    (cover-vector var $_ 
      (Cons  $_ $CIs) $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    (cover-vector var $_ $CIs $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut))
  (= 
    (cover-vector fcn $_ 
      (Cons  $I $_) $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    ( (>= $I $NumVars) 
      (function $I $GsIn $Gi) 
      (false-set $Gi $Fi) 
      (set-member $V $Fi) 
      (true-set $Gi $Ti) 
      (false-set $Gj $Fj) 
      (set-subset $Fj $Ti) 
      (update-circuit $GsIn $Gi $Gj $V $GsIn $GsOut)))
  (= 
    (cover-vector fcn $_ 
      (Cons  $_ $CIs) $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    (cover-vector fcn $_ $CIs $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut))
  (= 
    (cover-vector mcf $_ 
      (Cons  $I $_) $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    ( (>= $I $NumVars) 
      (function $I $GsIn $Gi) 
      (false-set $Gi $Fi) 
      (set-member $V $Fi) 
      (true-set $Gi $Ti) 
      (false-set $Gj $Fj) 
      (not (set-subset $Fj $Ti)) 
      (update-circuit $GsIn $Gi $Gj $V $GsIn $GsOut)))
  (= 
    (cover-vector mcf $_ 
      (Cons  $_ $CIs) $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    (cover-vector mcf $_ $CIs $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut))
  (= 
    (cover-vector exf $_ 
      (Cons  $I $_) $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    ( (>= $I $NumVars) 
      (function $I $GsIn $Gi) 
      (false-set $Gi $Fi) 
      (not (set-member $V $Fi)) 
      (true-set $Gi $Ti) 
      (not (set-member $V $Ti)) 
      (false-set $Gj $Fj) 
      (set-subset $Fj $Ti) 
      (update-circuit $GsIn $Gi $Gj $V $GsIn $GsOut)))
  (= 
    (cover-vector exf $_ 
      (Cons  $_ $CIs) $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    (cover-vector exf $_ $CIs $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut))
  (= 
    (cover-vector exmcf $_ 
      (Cons  $I $_) $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    ( (>= $I $NumVars) 
      (function $I $GsIn $Gi) 
      (false-set $Gi $Fi) 
      (not (set-member $V $Fi)) 
      (true-set $Gi $Ti) 
      (not (set-member $V $Ti)) 
      (false-set $Gj $Fj) 
      (not (set-subset $Fj $Ti)) 
      (update-circuit $GsIn $Gi $Gj $V $GsIn $GsOut)))
  (= 
    (cover-vector exmcf $_ 
      (Cons  $_ $CIs) $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut) 
    (cover-vector exmcf $_ $CIs $Gj $V $NumVars $NumGs $GsIn $NumGs $GsOut))
  (= 
    (cover-vector nf $_ $_ $Gj $V $NumVars $NumGsIn $GsIn $NumGsOut $GsOut) 
    ( (is $NumGsOut 
        (+ $NumGsIn 1)) 
      (false-set $Gj $Fj) 
      (new-function-CIs $GsIn 
        (function $NumGsIn $Fj 
          (:: $V) Nil Nil Nil Nil Nil) $NumVars $Gs $Gi) 
      (update-circuit $Gs $Gi $Gj $V $Gs $GsOut)))


;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  (= 
    (update_circuit  () $_ $_ $_ $_ ()) True)
  (= 
    (update-circuit 
      (Cons  
        (function $K $Tk $Fk $CIk $IPk $ISk $Pk $Sk) $GsIn) $Gi $Gj $V $Gs 
      (Cons  
        (function $K $Tko $Fko $CIko $IPko $ISko $Pko $Sko) $GsOut)) 
    ( (= $Gi 
        (function $I $_ $Fi $_ $IPi $ISi $Pi $_)) 
      (= $Gj 
        (function $J $_ $Fj $_ $_ $_ $_ $Sj)) 
      (set-union 
        (:: $I) $Pi $PiI) 
      (set-union 
        (:: $J) $Sj $SjJ) 
      (det-if-then-else 
        (= $K $J) 
        (set-union $Tk $Fi $Tk2) 
        (= $Tk2 $Tk)) 
      (det-if-then-else 
        (= $K $I) 
        (set-union $Tk2 $Fj $Tk3) 
        (= $Tk3 $Tk2)) 
      (det-if-then-else 
        (or 
          (set-member $K $IPi) 
          (set-member $K $ISi)) 
        (set-union $Tk3 
          (:: $V) $Tko) 
        (= $Tko $Tk3)) 
      (det-if-then-else 
        (= $K $I) 
        (set-union $Fk 
          (:: $V) $Fko) 
        (= $Fko $Fk)) 
      (det-if-then-else 
        (or 
          (set-member $K $Pi) 
          (= $K $I)) 
        (set-difference $CIk $SjJ $CIk2) 
        (= $CIk2 $CIk)) 
      (det-if-then-else 
        (, 
          (set-member $I $CIk) 
          (set-member $V $Fk)) 
        (set-difference $CIk2 
          (:: $I) $CIk3) 
        (= $CIk3 $CIk2)) 
      (det-if-then-else 
        (= $K $I) 
        (exclude-if-vector-in-false-set $CIk3 $Gs $V $CIk4) 
        (= $CIk4 $CIk3)) 
      (det-if-then-else 
        (= $K $J) 
        (set-difference $CIk4 
          (:: $I) $CIko) 
        (= $CIko $CIk4)) 
      (det-if-then-else 
        (= $K $J) 
        (set-union $IPk 
          (:: $I) $IPko) 
        (= $IPko $IPk)) 
      (det-if-then-else 
        (= $K $I) 
        (set-union $ISk 
          (:: $J) $ISko) 
        (= $ISko $ISk)) 
      (det-if-then-else 
        (set-member $K $SjJ) 
        (set-union $Pk $PiI $Pko) 
        (= $Pko $Pk)) 
      (det-if-then-else 
        (set-member $K $PiI) 
        (set-union $Sk $SjJ $Sko) 
        (= $Sko $Sk)) 
      (update-circuit $GsIn $Gi $Gj $V $Gs $GsOut)))


  (= 
    (exclude_if_vector_in_false_set  () $_ $_ ()) True)
  (= 
    (exclude-if-vector-in-false-set 
      (Cons  $K $CIsIn) $Gs $V $CIsOut) 
    ( (function $K $Gs $Gk) 
      (false-set $Gk $Fk) 
      (set-member $V $Fk) 
      (set-det) 
      (exclude-if-vector-in-false-set $CIsIn $Gs $V $CIsOut)))
  (= 
    (exclude-if-vector-in-false-set 
      (Cons  $K $CIsIn) $Gs $V 
      (Cons  $K $CIsOut)) 
    (exclude-if-vector-in-false-set $CIsIn $Gs $V $CIsOut))


;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  (= 
    (add-necessary-functions $NumVars $NumGsIn $GsIn $NumGsOut $GsOut) 
    (add-necessary-functions $NumVars $NumVars $NumGsIn $GsIn $NumGsOut $GsOut))

  (= 
    (add-necessary-functions $NumGs $_ $NumGs $Gs $NumGs $Gs) 
    (set-det))
  (= 
    (add-necessary-functions $K $NumVars $NumGsIn $GsIn $NumGsOut $GsOut) 
    ( (function $K $GsIn $Gk) 
      (function-type $NumVars $NumGsIn $GsIn $Gk nf $V) 
      (set-det) 
      (false-set $Gk $Fk) 
      (new-function-CIs $GsIn 
        (function $NumGsIn $Fk 
          (:: $V) Nil Nil Nil Nil Nil) $NumVars $Gs $Gl) 
      (function $K $Gs $Gk1) 
      (update-circuit $Gs $Gl $Gk1 $V $Gs $Gs1) 
      (is $NumGs1 
        (+ $NumGsIn 1)) 
      (is $K1 
        (+ $K 1)) 
      (add-necessary-functions $K1 $NumVars $NumGs1 $Gs1 $NumGsOut $GsOut)))
  (= 
    (add-necessary-functions $K $NumVars $NumGsIn $GsIn $NumGsOut $GsOut) 
    ( (is $K1 
        (+ $K 1)) (add-necessary-functions $K1 $NumVars $NumGsIn $GsIn $NumGsOut $GsOut)))


  (= 
    (new-function-CIs $GsIn 
      (function $L $Tl $Fl $_ $IPl $ISl $Pl $Sl) $NumVars 
      (Cons  $GlOut $GsOut) $GlOut) 
    ( (new-function-CIs $GsIn $L $Fl $NumVars $GsOut Nil $CIlo) (= $GlOut (function $L $Tl $Fl $CIlo $IPl $ISl $Pl $Sl))))

  (= 
    (new_function_CIs  () $_ $_ $_ () $CIl $CIl) True)
  (= 
    (new-function-CIs 
      (Cons  
        (function $K $Tk $Fk $CIk $IPk $ISk $Pk $Sk) $GsIn) $L $Fl $NumVars 
      (Cons  
        (function $K $Tk $Fk $CIko $IPk $ISk $Pk $Sk) $GsOut) $CIlIn $CIlOut) 
    ( (set-intersection $Fl $Fk Nil) 
      (set-det) 
      (det-if-then-else 
        (>= $K $NumVars) 
        (set-union $CIk 
          (:: $L) $CIko) 
        (= $CIko $CIk)) 
      (new-function-CIs $GsIn $L $Fl $NumVars $GsOut 
        (Cons  $K $CIlIn) $CIlOut)))
  (= 
    (new-function-CIs 
      (Cons  $Gk $GsIn) $L $Fl $NumVars 
      (Cons  $Gk $GsOut) $CIlIn $CIlOut) 
    (new-function-CIs $GsIn $L $Fl $NumVars $GsOut $CIlIn $CIlOut))


  (= 
    (function-type $NumVars $NumGs $Gs $Gk $Type $Vector) 
    ( (true-set $Gk $Tk) (select-vector $Tk $Gk $NumVars $NumGs $Gs dummy 0 nf 999 $_ $Vector $Type $_)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                           Cost and constraint predicates:


;
;                                          very simple bound for now



  (= 
    (test-bounds $_ $NumGs $_) 
    ( (access bound $Bound) (< $NumGs $Bound)))


  (= 
    (update-bounds $_ $NumGs $_) 
    (set bound $NumGs))

;
;                                          set and access for systems that don't support them


  (= 
    (set $N $A) 
    ( (det-if-then-else 
        (recorded $N $_ $Ref) 
        (erase $Ref) True) (recorda $N $A $_)))


  (= 
    (access $N $A) 
    (recorded $N $A $_))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                           Output predicates:


;
;                                           for now just dump everything


;
;                                         output_results(NumVars, NumGs, Gs) :-

;
;                                         	NumGates is NumGs - NumVars,

;
;                                         	write(NumGates), write(' gates'), nl,

;
;                                         	write_gates(Gs), nl,

;
;                                         	write('searching for a better solution...'), nl, nl.



  (= 
    (write_gates  ()) True)
  (= 
    (write-gates (Cons  $Gi $Gs)) 
    ( (function-number $Gi $I) 
      (write 'gate #') 
      (write $I) 
      (write ' inputs:   ') 
      (immediate-predecessors $Gi $IPi) 
      (write $IPi) 
      (nl) 
      (write-gates $Gs)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                           Retrieve the specified function from the function list.

;
;                                           function(FunctionNumber, FunctionList, Function).


  (= 
    (function $I 
      (Cons  $Gi $_) $Gi) 
    ( (function-number $Gi $I) (set-det)))
  (= 
    (function $I 
      (Cons  $_ $Gs) $Gi) 
    (function $I $Gs $Gi))


  (= 
    (function_number  
      (function  $I $_ $_ $_ $_ $_ $_ $_) $I) True)

  (= 
    (true_set  
      (function  $_ $T $_ $_ $_ $_ $_ $_) $T) True)

  (= 
    (false_set  
      (function  $_ $_ $F $_ $_ $_ $_ $_) $F) True)

  (= 
    (conceivable_inputs  
      (function  $_ $_ $_ $CI $_ $_ $_ $_) $CI) True)

  (= 
    (immediate_predecessors  
      (function  $_ $_ $_ $_ $IP $_ $_ $_) $IP) True)

  (= 
    (immediate_successors  
      (function  $_ $_ $_ $_ $_ $IS $_ $_) $IS) True)

  (= 
    (predecessors  
      (function  $_ $_ $_ $_ $_ $_ $P $_) $P) True)

  (= 
    (successors  
      (function  $_ $_ $_ $_ $_ $_ $_ $S) $S) True)

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                                           Set operations assume that the sets are represented by an ordered list

;
;                                           of integers.



  (= 
    (set_union  () () ()) True)
  (= 
    (set_union  () 
      (Cons  $X $L2) 
      (Cons  $X $L2)) True)
  (= 
    (set_union  
      (Cons  $X $L1) () 
      (Cons  $X $L1)) True)
  (= 
    (set-union 
      (Cons  $X $L1) 
      (Cons  $X $L2) 
      (Cons  $X $L3)) 
    (set-union $L1 $L2 $L3))
  (= 
    (set-union 
      (Cons  $X $L1) 
      (Cons  $Y $L2) 
      (Cons  $X $L3)) 
    ( (< $X $Y) (set-union $L1 (Cons  $Y $L2) $L3)))
  (= 
    (set-union 
      (Cons  $X $L1) 
      (Cons  $Y $L2) 
      (Cons  $Y $L3)) 
    ( (> $X $Y) (set-union (Cons  $X $L1) $L2 $L3)))


  (= 
    (set_intersection  () () ()) True)
  (= 
    (set_intersection  () 
      (Cons  $_ $_) ()) True)
  (= 
    (set_intersection  
      (Cons  $_ $_) () ()) True)
  (= 
    (set-intersection 
      (Cons  $X $L1) 
      (Cons  $X $L2) 
      (Cons  $X $L3)) 
    (set-intersection $L1 $L2 $L3))
  (= 
    (set-intersection 
      (Cons  $X $L1) 
      (Cons  $Y $L2) $L3) 
    ( (< $X $Y) (set-intersection $L1 (Cons  $Y $L2) $L3)))
  (= 
    (set-intersection 
      (Cons  $X $L1) 
      (Cons  $Y $L2) $L3) 
    ( (> $X $Y) (set-intersection (Cons  $X $L1) $L2 $L3)))


  (= 
    (set_difference  () () ()) True)
  (= 
    (set_difference  () 
      (Cons  $_ $_) ()) True)
  (= 
    (set_difference  
      (Cons  $X $L1) () 
      (Cons  $X $L1)) True)
  (= 
    (set-difference 
      (Cons  $X $L1) 
      (Cons  $X $L2) $L3) 
    (set-difference $L1 $L2 $L3))
  (= 
    (set-difference 
      (Cons  $X $L1) 
      (Cons  $Y $L2) 
      (Cons  $X $L3)) 
    ( (< $X $Y) (set-difference $L1 (Cons  $Y $L2) $L3)))
  (= 
    (set-difference 
      (Cons  $X $L1) 
      (Cons  $Y $L2) $L3) 
    ( (> $X $Y) (set-difference (Cons  $X $L1) $L2 $L3)))


  (= 
    (set_subset  () $_) True)
  (= 
    (set-subset 
      (Cons  $X $L1) 
      (Cons  $X $L2)) 
    (set-subset $L1 $L2))
  (= 
    (set-subset 
      (Cons  $X $L1) 
      (Cons  $Y $L2)) 
    ( (> $X $Y) (set-subset (Cons  $X $L1) $L2)))


  (= 
    (set_member  $X 
      (Cons  $X $_)) True)
  (= 
    (set-member $X 
      (Cons  $Y $T)) 
    ( (> $X $Y) (set-member $X $T)))


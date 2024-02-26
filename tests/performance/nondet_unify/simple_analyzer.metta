;
;                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                         Copyright (C) 1990 Peter Van Roy and Regents of the University of California.

;
;                         All rights reserved.  This program may be freely used and modified for

;
;                         non-commercial purposes provided this copyright notice is kept unchanged.

;
;                         Written by Peter Van Roy as a part of the Aquarius project.

;
;                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                          Benchmark based on the Aquarius compiler flow analyzer version 1.

;
;                          This program does a dataflow analysis of quicksort using abstract

;
;                          interpretation.  The lattice has two useful values: uninit and ground.

;
;                           Analysis takes three passes (it prints three 'x' characters).

;
;                           Builtins used: compare/3, arg/3, functor/3, sort/2, keysort/2, ==/2, \==/2.



  (= 
    (top) 
    (main $_))
;
;                            main :- main(Table), write(Table), nl.



  (= 
    (main $Table) 
    (analyze-strees 
      (:: 
        (stree 
          (/ main 0) 
          (= 
            (main) 
            (or 
              (qsort 
                (:: 1 2) $L Nil) fail)) 
          (= main True) Nil 1) 
        (stree 
          (/ qsort 3) 
          (= 
            (qsort $U $P $Q) 
            (or 
              (, 
                (= $U 
                  (Cons  $N $O)) 
                (part $O $N $R $S) 
                (qsort $S $T $Q) 
                (qsort $R $P 
                  (Cons  $N $T))) 
              (or 
                (, 
                  (= $U Nil) 
                  (= $Q $P)) fail))) 
          (= 
            (qsort  $_ $_ $_) True) Nil 1) 
        (stree 
          (/ part 4) 
          (= 
            (part $W $X $Y $Z) 
            (or 
              (, 
                ($cut-load $A1) 
                ($cut-part/4-1 $W $X $Y $Z $A1)) fail)) 
          (= 
            (part  $_ $_ $_ $_) True) 
          (:: (stree (/ %cut-part/4-1 5) (= ($cut-part/4-1 $I1 $E1 $F1 $G1 $H1) (or (, (= $I1 (Cons  $C1 $D1)) ($fac-$cut-part/4-1/5-2 $D1 $E1 $F1 $G1 $H1 $C1)) (or (, (= $I1 Nil) (= $F1 Nil) (= $G1 Nil)) fail))) (= ($cut_part/4_1  $_ $_ $_ $_ $_) True) (:: (stree (/ %fac-%cut-part/4-1/5-2 6) (= ($fac-$cut-part/4-1/5-2 $K1 $L1 $Q1 $O1 $P1 $M1) (or (, (= $Q1 (Cons  $M1 $N1)) (=< $M1 $L1) ($cut-shallow $P1) (part $K1 $L1 $N1 $O1)) (or (, (= $O1 (Cons  $M1 $R1)) (part $K1 $L1 $Q1 $R1)) fail))) (= ($fac_$cut_part/4_1/5_2  $_ $_ $_ $_ $_ $_) True) Nil 1)) 1)) 1)) $Table))


  (= 
    (analyze-strees $Strees $OutTable) 
    ( (init-strees $Strees $_ $Table) 
      (seal $Table) 
      (analyze-closure $Strees $Table $OutTable)))

;
;                             Repeat traversal step until there are no more changes:


  (= 
    (analyze-closure $Strees $InTable $OutTable) 
    ( (traverse-strees $Strees $InTable $MidTable 0 $Changes) (analyze-closure $Strees $MidTable $OutTable $Changes)))
;                              ; Mark an analysis pass:

;                              ; put("x"), nl,



  (= 
    (analyze-closure $Strees $InTable $InTable $N) 
    ( (=< $N 0) (set-det)))
  (= 
    (analyze-closure $Strees $InTable $OutTable $N) 
    ( (> $N 0) 
      (set-det) 
      (analyze-closure $Strees $InTable $OutTable)))

;
;                              ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                               Initialize the table of call lattice values:



  (= 
    (init_strees  () $4 $4) True)
  (= 
    (init-strees 
      (Cons  $12 $13) $4 $5) 
    ( (= $12 
        (stree $14 
          (= $15 $16) $17 $18 $19)) 
      (bottom-call $14 $20) 
      (table-command 
        (get $14 $20) $4 $23) 
      (init-disj $16 $23 $24) 
      (init-strees $18 $24 $25) 
      (init-strees $13 $25 $5)))


  (= 
    (init_conj  true $4 $4) True)
  (= 
    (init-conj 
      (, $12 $13) $4 $5) 
    ( (init-goal $12 $4 $16) (init-conj $13 $16 $5)))


  (= 
    (init_disj  fail $4 $4) True)
  (= 
    (init-disj 
      (or $12 $13) $4 $5) 
    ( (init-conj $12 $4 $16) (init-disj $13 $16 $5)))


  (= 
    (init-goal $3 $4 $5) 
    ( (call-p $3) 
      (set-det) 
      (functor $3 $12 $13) 
      (bottom-call 
        (/ $12 $13) $14) 
      (table-command 
        (get 
          (/ $12 $13) $14) $4 $5)))
  (= 
    (init-goal $3 $4 $4) 
    ( (unify-p $3) (set-det)))

;
;                                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



  (= 
    (traverse_strees  () $4 $4 $6 $6) True)
  (= 
    (traverse-strees 
      (Cons  $14 $15) $4 $5 $6 $7) 
    ( (= $14 
        (stree $16 
          (= $17 $18) $19 $20 $21)) 
      (traverse-disj $17 $18 $4 $26 $6 $27) 
      (traverse-strees $20 $26 $28 $27 $29) 
      (traverse-strees $15 $28 $5 $29 $7)))


  (= 
    (traverse_disj  $3 fail $5 $5 $7 $7) True)
  (= 
    (traverse-disj $3 
      (or $15 $16) $5 $6 $7 $8) 
    ( (traverse-conj $3 $15 $5 $22 $7 $23) (traverse-disj $3 $16 $22 $6 $23 $8)))


  (= 
    (traverse-conj $3 $4 $5 $6 $7 $8) 
    ( (varset $3 $24) 
      (functor $3 $15 $16) 
      (table-command 
        (get 
          (/ $15 $16) $17) $5 $25) 
      (get-entry-modes uninit $3 $17 $26) 
      (get-entry-modes ground $3 $17 $27) 
      (traverse-conj $4 $25 $6 $7 $8 $27 $28 $26 $29 $24 $30)))

  (= 
    (traverse_conj  true $4 $4 $6 $6 $8 $8 $10 $10 $12 $12) True)
  (= 
    (traverse-conj 
      (, $20 $21) $4 $5 $6 $7 $8 $9 $10 $11 $12 $13) 
    ( (varset $20 $32) 
      (update-goal $20 $32 $4 $33 $6 $34 $8 $35 $10 $36 $12 $37) 
      (unionv $32 $37 $38) 
      (traverse-conj $21 $33 $5 $34 $7 $35 $9 $36 $11 $38 $13)))


  (= 
    (update-goal $3 $4 $5 $5 $7 $7 $9 $10 $11 $12 $13 $13) 
    ( (split-unify $3 $21 $27) 
      (var $21) 
      (nonvar $27) 
      (varset $27 $28) 
      (subsetv $28 $9) 
      (set-det) 
      (set-command 
        (add $21) $9 $10) 
      (set-command 
        (sub $21) $11 $12)))
  (= 
    (update-goal $3 $4 $5 $5 $7 $7 $9 $9 $11 $12 $13 $13) 
    ( (split-unify $3 $21 $30) 
      (var $21) 
      (nonvar $30) 
      (inv $21 $11) 
      (set-det) 
      (diffv $4 $13 $31) 
      (diffv $31 $9 $22) 
      (set-command 
        (add-set $22) $11 $32) 
      (set-command 
        (sub $21) $32 $33) 
      (intersectv $4 $13 $23) 
      (set-command 
        (sub-set $23) $33 $12)))
  (= 
    (update-goal $3 $4 $5 $5 $7 $7 $9 $10 $11 $12 $13 $13) 
    ( (split-unify $3 $27 $28) 
      (var $27) 
      (inv $27 $9) 
      (set-det) 
      (set-command 
        (add-set $4) $9 $10) 
      (set-command 
        (sub-set $4) $11 $12)))
  (= 
    (update-goal $3 $4 $5 $5 $7 $7 $9 $9 $11 $12 $13 $13) 
    ( (unify-p $3) 
      (set-det) 
      (set-command 
        (sub-set $4) $11 $12)))
  (= 
    (update-goal $3 $4 $5 $6 $7 $8 $9 $9 $11 $12 $13 $13) 
    ( (call-p $3) 
      (set-det) 
      (goal-dupset $3 $33) 
      (var-args $3 $34) 
      (functor $3 $22 $23) 
      (functor $35 $22 $23) 
      (create-new-call 1 $23 $9 $34 $33 $11 $13 $3 $35) 
      (update-table 
        (/ $22 $23) $35 $5 $6 $7 $8) 
      (set-command 
        (sub-set $4) $11 $12)))


  (= 
    (update-table 
      (/ $15 $16) $4 $5 $6 $7 $8) 
    ( (table-command 
        (get 
          (/ $15 $16) $18) $5 $24) 
      (lub-call $18 $4 $19) 
      (\== $18 $19) 
      (set-det) 
      (table-command 
        (set 
          (/ $15 $16) $19) $24 $6) 
      (is $8 
        (+ $7 1))))
  (= 
    (update_table  
      (/  $15 $16) $4 $5 $5 $7 $7) True)


  (= 
    (create-new-call $I $Ar $_ $_ $_ $_ $_ $_ $_) 
    ( (> $I $Ar) (set-det)))
  (= 
    (create-new-call $I $Ar $Gnd $VarArgs $DupVars $Uni $SoFar $Goal $Call) 
    ( (=< $I $Ar) 
      (set-det) 
      (arg $I $Goal $X) 
      (arg $I $Call $Y) 
      (ground-flag $X $Gnd $Gf) 
      (membership-flag $X $VarArgs $Vf) 
      (membership-flag $X $DupVars $Df) 
      (membership-flag $X $Uni $Uf) 
      (membership-flag $X $SoFar $Sf) 
      (create-argument $Gf $Vf $Df $Uf $Sf $Y) 
      (is $I1 
        (+ $I 1)) 
      (create-new-call $I1 $Ar $Gnd $VarArgs $DupVars $Uni $SoFar $Goal $Call)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                          Lattice utilities:



  (= 
    (lub unknown $X $X) 
    (set-det))
  (= 
    (lub $X unknown $X) 
    (set-det))
  (= 
    (lub any $_ any) 
    (set-det))
  (= 
    (lub $_ any any) 
    (set-det))
  (= 
    (lub uninit uninit uninit) 
    (set-det))
  (= 
    (lub ground ground ground) 
    (set-det))
  (= 
    (lub uninit ground any) 
    (set-det))
  (= 
    (lub ground uninit any) 
    (set-det))


  (= 
    (bottom  unknown) True)


  (= 
    (create-argument yes $_ $_ $_ $_ ground) 
    (set-det)) ;
;                                          Ground argument.

  (= 
    (create-argument no yes no yes $_ uninit) 
    (set-det)) ;
;                                          Non-duplicated uninit.

  (= 
    (create-argument no yes no $_ no uninit) 
    (set-det)) ;
;                                          First occurrence.

  (= 
    (create-argument no yes $_ no yes any) 
    (set-det))    ;
;                                          Already initialized.

  (= 
    (create-argument no yes yes $_ $_ any) 
    (set-det))    ;
;                                          Duplicated argument.

  (= 
    (create-argument no no $_ $_ $_ any) 
    (set-det))    ;
;                                          Non-variable argument.



  (= 
    (lub-call $Call1 $Call2 $Lub) 
    ( (functor $Call1 $Na $Ar) 
      (functor $Call2 $Na $Ar) 
      (functor $Lub $Na $Ar) 
      (lub-call 1 $Ar $Call1 $Call2 $Lub)))

  (= 
    (lub-call $I $Ar $_ $_ $_) 
    ( (> $I $Ar) (set-det)))
  (= 
    (lub-call $I $Ar $Call1 $Call2 $Lub) 
    ( (=< $I $Ar) 
      (set-det) 
      (arg $I $Call1 $X1) 
      (arg $I $Call2 $X2) 
      (arg $I $Lub $X) 
      (lub $X1 $X2 $X) 
      (is $I1 
        (+ $I 1)) 
      (lub-call $I1 $Ar $Call1 $Call2 $Lub)))


  (= 
    (bottom-call 
      (/ $Na $Ar) $Bottom) 
    ( (functor $Bottom $Na $Ar) (bottom-call 1 $Ar $Bottom)))

  (= 
    (bottom-call $I $Ar $Bottom) 
    ( (> $I $Ar) (set-det)))
  (= 
    (bottom-call $I $Ar $Bottom) 
    ( (=< $I $Ar) 
      (set-det) 
      (bottom $B) 
      (arg $I $Bottom $B) 
      (is $I1 
        (+ $I 1)) 
      (bottom-call $I1 $Ar $Bottom)))


  (= 
    (lattice-modes-call 
      (/ $Na $Ar) $Table 
      (= $Head $Formula)) 
    ( (functor $Head $Na $Ar) 
      (get $Table 
        (/ $Na $Ar) $Value) 
      (lattice-modes-call 1 $Ar $Value $Head $Formula True)))

  (= 
    (lattice-modes-call $I $Ar $_ $_ $Link $Link) 
    ( (> $I $Ar) (set-det)))
  (= 
    (lattice-modes-call $I $Ar $Value $Head $Formula $Link) 
    ( (=< $I $Ar) 
      (set-det) 
      (arg $I $Value $T) 
      (arg $I $Head $X) 
      (lattice-modes-arg $T $X $Formula $Mid) 
      (is $I1 
        (+ $I 1)) 
      (lattice-modes-call $I1 $Ar $Value $Head $Mid $Link)))


  (= 
    (lattice-modes-arg uninit $X 
      (, 
        (uninit $X) $Link) $Link) 
    (set-det))
  (= 
    (lattice-modes-arg ground $X 
      (, 
        (ground $X) $Link) $Link) 
    (set-det))
  (= 
    (lattice_modes_arg  $Other $X $Link $Link) True)

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                          Table utilities:


;
;                                          This code implements a mutable array, represented as a binary tree.


;
;                                          Access a value in logarithmic time and constant space:

;
;                                          This predicate can be used to create the array incrementally.


  (= 
    (get 
      (node $N $W $L $R) $I $V) 
    (get $N $W $L $R $I $V))

  (= 
    (get $N $V $_ $_ $I $V) 
    ( (= $I $N) (set-det)))
  (= 
    (get $N $_ $L $R $I $V) 
    ( (compare $Order $I $N) (get $Order $I $V $L $R)))

  (= 
    (get < $I $V $L $_) 
    (get $L $I $V))
  (= 
    (get > $I $V $_ $R) 
    (get $R $I $V))


  (= 
    (set  leaf $I $V 
      (node  $I $V leaf leaf)) True)
  (= 
    (set 
      (node $N $W $L $R) $I $V 
      (node $N $NW $NL $NR)) 
    ( (compare $Order $I $N) (set-2 $Order $I $V $W $L $R $NW $NL $NR)))


  (= 
    (set-2 < $I $V $W $L $R $W $NL $R) 
    (set $L $I $V $NL))
  (= 
    (set_2  = $I $V $_ $L $R $V $L $R) True)
  (= 
    (set-2 > $I $V $W $L $R $W $L $NR) 
    (set $R $I $V $NR))

;
;                                          Prevent any further insertions in the array:


  (= 
    (seal  leaf) True)
  (= 
    (seal (node $_ $_ $L $R)) 
    ( (seal $L) (seal $R)))

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                          General utilities:



  (= 
    (membership-flag $X $Set yes) 
    ( (inv $X $Set) (set-det)))
  (= 
    (membership_flag  $X $Set no) True)


  (= 
    (ground-flag $X $Ground yes) 
    ( (varset $X $Set) 
      (subsetv $Set $Ground) 
      (set-det)))
  (= 
    (ground_flag  $X $Ground no) True)


  (= 
    (get-entry-modes $Type $Head $Value $TypeSet) 
    ( (functor $Head $Na $Ar) 
      (get-entry-modes $Type 1 $Ar $Head $Value $Bag) 
      (sort $Bag $TypeSet)))

  (= 
    (get-entry-modes $_ $I $Ar $_ $_ Nil) 
    ( (> $I $Ar) (set-det)))
  (= 
    (get-entry-modes $T $I $Ar $Head $Value 
      (Cons  $X $Bag)) 
    ( (=< $I $Ar) 
      (arg $I $Value $T) 
      (set-det) 
      (arg $I $Head $X) 
      (is $I1 
        (+ $I 1)) 
      (get-entry-modes $T $I1 $Ar $Head $Value $Bag)))
  (= 
    (get-entry-modes $T $I $Ar $Head $Value $Bag) 
    ( (=< $I $Ar) 
      (set-det) 
      (is $I1 
        (+ $I 1)) 
      (get-entry-modes $T $I1 $Ar $Head $Value $Bag)))


  (= 
    (var-args $Goal $Set) 
    ( (functor $Goal $_ $Ar) 
      (filter-vars $Ar $Goal $Bag) 
      (sort $Bag $Set)))


  (= 
    (filter-vars $Ar $Goal $Vs) 
    (phrase 
      (filter-vars $Ar $Goal) $Vs))


  (= 
    (-->  
      (filter_vars  $N $Goal) 
      (,  
        { (=<  $N 0) } !)) True)
  (= 
    (-->  
      (filter_vars  $N $Goal) 
      (,  
        { (>  $N 0) } 
        (,  ! 
          (,  
            { (arg  $N $Goal $V) } 
            (filter_vars_arg  $N $Goal $V))))) True)

  (= 
    (-->  
      (filter_vars_arg  $N $Goal $V) 
      (,  
        { (var  $V) } 
        (,  ! 
          (,  
            ($V) 
            (,  
              { (is  $N1 
                  (-  $N 1)) } 
              (filter_vars  $N1 $Goal)))))) True)
  (= 
    (-->  
      (filter_vars_arg  $N $Goal $V) 
      (,  
        { (nonvar  $V) } 
        (,  ! 
          (,  
            { (is  $N1 
                (-  $N 1)) } 
            (filter_vars  $N1 $Goal))))) True)


  (= 
    (goal-dupset $Goal $DupSet) 
    (goal-dupset-varbag $Goal $DupSet $_))


  (= 
    (goal-dupset-varset $Goal $DupSet $VarSet) 
    ( (goal-dupset-varbag $Goal $DupSet $VarBag) (sort $VarBag $VarSet)))


  (= 
    (goal-dupset-varbag $Goal $DupSet $VarBag) 
    ( (varbag $Goal $VarBag) 
      (make-key $VarBag $KeyBag) 
      (keysort $KeyBag $KeySet) 
      (filter-dups $KeySet $DupSet)))


  (= 
    (make_key  () ()) True)
  (= 
    (make-key 
      (Cons  $V $Bag) 
      (Cons  
        (- $V dummy) $KeyBag)) 
    (make-key $Bag $KeyBag))


  (= 
    (filter-dups $KeySet $Set) 
    (phrase 
      (filter-dups $KeySet) $Set))


  (= 
    (-->  
      (filter_dups  ()) !) True)
  (= 
    (-->  
      (filter_dups  
        (Cons  
          (-  $V1 $_) 
          (Cons  
            (-  $V2 $_) 
            (Cons  
              (-  $V3 $_) $KeySet)))) 
      (,  
        { (,  
            (==  $V1 $V2) 
            (==  $V2 $V3)) } 
        (,  ! 
          (filter_dups  
            (Cons  
              (-  $V2 $_) 
              (Cons  
                (-  $V3 $_) $KeySet)))))) True)
  (= 
    (-->  
      (filter_dups  
        (Cons  
          (-  $V1 $_) 
          (Cons  
            (-  $V2 $_) $KeySet))) 
      (,  
        { (==  $V1 $V2) } 
        (,  ! 
          (,  
            ($V1) 
            (filter_dups  $KeySet))))) True)
  (= 
    (-->  
      (filter_dups  
        (Cons  
          (-  $V1 $_) $KeySet)) 
      (,  ! 
        (filter_dups  $KeySet))) True)

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                                          Low-level utilities:



  (= 
    (set-command 
      (sub $X) $In $Out) 
    (diffv $In 
      (:: $X) $Out))
  (= 
    (set-command 
      (add $X) $In $Out) 
    (includev $X $In $Out))
  (= 
    (set-command 
      (sub-set $X) $In $Out) 
    (diffv $In $X $Out))
  (= 
    (set-command 
      (add-set $X) $In $Out) 
    (unionv $X $In $Out))


  (= 
    (table-command 
      (get $I $Val) $In $In) 
    (get $In $I $Val))
  (= 
    (table-command 
      (set $I $Val) $In $Out) 
    (set $In $I $Val $Out))

;
;                                          Set utilities inspired by R. O'Keefe in Practical MeTTa:


  (= 
    (inv $A 
      (Cons  $B $S)) 
    ( (compare $Order $A $B) (inv-2 $Order $A $S)))


  (= 
    (inv_2  = $_ $_) True)
  (= 
    (inv-2 > $A $S) 
    (inv $A $S))


  (= 
    (intersectv  () $_ ()) True)
  (= 
    (intersectv 
      (Cons  $A $S1) $S2 $S) 
    (intersectv-2 $S2 $A $S1 $S))


  (= 
    (intersectv_2  () $A $S1 ()) True)
  (= 
    (intersectv-2 
      (Cons  $B $S2) $A $S1 $S) 
    ( (compare $Order $A $B) (intersectv-3 $Order $A $S1 $B $S2 $S)))


  (= 
    (intersectv-3 < $A $S1 $B $S2 $S) 
    (intersectv-2 $S1 $B $S2 $S))
  (= 
    (intersectv-3 = $A $S1 $_ $S2 
      (Cons  $A $S)) 
    (intersectv $S1 $S2 $S))
  (= 
    (intersectv-3 > $A $S1 $B $S2 $S) 
    (intersectv-2 $S2 $A $S1 $S))


  (= 
    (diffv  () $_ ()) True)
  (= 
    (diffv 
      (Cons  $A $S1) $S2 $S) 
    (diffv-2 $S2 $A $S1 $S))


  (= 
    (diffv_2  () $A $S1 
      ($A)) True)
  (= 
    (diffv-2 
      (Cons  $B $S2) $A $S1 $S) 
    ( (compare $Order $A $B) (diffv-3 $Order $A $S1 $B $S2 $S)))


  (= 
    (diffv-3 < $A $S1 $B $S2 
      (Cons  $A $S)) 
    (diffv $S1 
      (Cons  $B $S2) $S))
  (= 
    (diffv-3 = $A $S1 $_ $S2 $S) 
    (diffv $S1 $S2 $S))
  (= 
    (diffv-3 > $A $S1 $_ $S2 $S) 
    (diffv-2 $S2 $A $S1 $S))


  (= 
    (unionv  () $S2 $S2) True)
  (= 
    (unionv 
      (Cons  $A $S1) $S2 $S) 
    (unionv-2 $S2 $A $S1 $S))


  (= 
    (unionv_2  () $A $S1 
      (Cons  $A $S1)) True)
  (= 
    (unionv-2 
      (Cons  $B $S2) $A $S1 $S) 
    ( (compare $Order $A $B) (unionv-3 $Order $A $S1 $B $S2 $S)))


  (= 
    (unionv-3 < $A $S1 $B $S2 
      (Cons  $A $S)) 
    (unionv-2 $S1 $B $S2 $S))
  (= 
    (unionv-3 = $A $S1 $_ $S2 
      (Cons  $A $S)) 
    (unionv $S1 $S2 $S))
  (= 
    (unionv-3 > $A $S1 $B $S2 
      (Cons  $B $S)) 
    (unionv-2 $S2 $A $S1 $S))


  (= 
    (includev $A $S1 $S) 
    (includev-2 $S1 $A $S))


  (= 
    (includev_2  () $A 
      ($A)) True)
  (= 
    (includev-2 
      (Cons  $B $S1) $A $S) 
    ( (compare $Order $A $B) (includev-3 $Order $A $B $S1 $S)))


  (= 
    (includev_3  < $A $B $S1 
      (Cons  $A 
        (Cons  $B $S1))) True)
  (= 
    (includev_3  = $_ $B $S1 
      (Cons  $B $S1)) True)
  (= 
    (includev-3 > $A $B $S1 
      (Cons  $B $S)) 
    (includev-2 $S1 $A $S))


  (= 
    (subsetv  () $_) True)
  (= 
    (subsetv 
      (Cons  $A $S1) 
      (Cons  $B $S2)) 
    ( (compare $Order $A $B) (subsetv-2 $Order $A $S1 $S2)))


  (= 
    (subsetv-2 = $A $S1 $S2) 
    (subsetv $S1 $S2))
  (= 
    (subsetv-2 > $A $S1 $S2) 
    (subsetv 
      (Cons  $A $S1) $S2))


  (= 
    (varset $Term $VarSet) 
    ( (varbag $Term $VB) (sort $VB $VarSet)))

  (= 
    (varbag $Term $VarBag) 
    (phrase 
      (varbag $Term) $VarBag))


  (= 
    (-->  
      (varbag  $Var) 
      (,  
        { (var  $Var) } 
        (,  ! 
          ($Var)))) True)
  (= 
    (-->  
      (varbag  $Str) 
      (,  
        { (,  
            (nonvar  $Str) 
            (,  ! 
              (functor  $Str $_ $Arity))) } 
        (varbag  $Str 1 $Arity))) True)

  (= 
    (-->  
      (varbag  $Str $N $Arity) 
      (,  
        { (>  $N $Arity) } !)) True)
  (= 
    (-->  
      (varbag  $Str $N $Arity) 
      (,  
        { (=<  $N $Arity) } 
        (,  ! 
          (,  
            { (arg  $N $Str $Arg) } 
            (,  
              (varbag  $Arg) 
              (,  
                { (is  $N1 
                    (+  $N 1)) } 
                (varbag  $Str $N1 $Arity))))))) True)


  (= 
    (unify_p  
      (= $_ $_)) True)


  (= 
    (call-p $G) 
    (not (unify-p $G)))


  (= 
    (split_unify  
      (= $X $Y) $X $Y) True)
  (= 
    (split_unify  
      (= $Y $X) $X $Y) True)

;
;                                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



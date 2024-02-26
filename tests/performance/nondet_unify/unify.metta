;
;                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;                         Copyright (C) 1990 Regents of the University of California.

;
;                          All rights reserved.  This program may be freely used and modified for

;
;                          non-commercial purposes provided this copyright notice is kept unchanged.

;
;                          Written by Peter Van Roy as a part of the Aquarius project.

;
;                         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;
;                           Benchmark based on part of Aquarius MeTTa compiler

;
;                           Compiling unification into abstract machine code.



  (= 
    (top) 
    (main $X))
;
;                           , write(X), nl.


;
;                            PM: rewritten to use phrase/3

;
;                                the Start variable allows bypassing phrase/3 type-checking


  (= 
    (main $Size) 
    ( (u $X 
        (:: 1 $Y) 
        (:: $X) $Code) 
      (= $Start 0) 
      (phrase 
        (size $Code) $Start $Size)))

;
;                             Unify variable X with term T and write the result:


  (= 
    (u $X $T $In $Code) 
    (phrase 
      (unify $X $T $In $_) $Code))

;
;                              Unify the variable X with the term T, given that

;
;                              In = set of variables initialized before the unification.

;
;                              Returns the intermediate code for the unification and

;
;                              Out = set of variables initialized after the unification.


  (= 
    (-->  
      (unify  $X $T $In $Out) 
      (,  
        { (\+  
            (myin  $X $In)) } 
        (,  ! 
          (uninit  $X $T $In $Out)))) True)
  (= 
    (-->  
      (unify  $X $T $In $Out) 
      (,  
        { (myin  $X $In) } 
        (,  ! 
          (init  $X $T $In $Out nonlast $_)))) True)

;
;                               **** Uninit assumes X has not yet been initialized:

  (= 
    (-->  
      (uninit  $X $T $In $Out) 
      (,  
        { (my_compound  $T) } 
        (,  ! 
          (,  
            ( (move  
                (^  $Tag h) $X)) 
            (,  
              { (termtag  $T $Tag) } 
              (,  
                (unify_block  nonlast $T $_ $In $Mid $_) 
                { (incl  $X $Mid $Out) })))))) True)
  (= 
    (-->  
      (uninit  $X $T $In $Out) 
      (,  
        { (atomic  $T) } 
        (,  ! 
          (,  
            ( (move  
                (^  tatm $T) $X)) 
            { (incl  $X $In $Out) })))) True)
  (= 
    (-->  
      (uninit  $X $T $In $Out) 
      (,  
        { (var  $T) } 
        (,  ! 
          (unify_var  $X $T $In $Out)))) True)

;
;                                **** Init assumes X has already been initialized:

  (= 
    (-->  
      (init  $X $T $In $Out $Last $LLbls) 
      (,  
        { (nonvar  $T) } 
        (,  ! 
          (,  
            { (termtag  $T $Tag) } 
            (,  
              ( (deref  $X) (switch  $Tag $X (Cons  (trail  $X) $Write) $Read fail)) 
              (,  
                { (unify_writemode  $X $T $In $Last $LLbls $Write ()) } 
                { (unify_readmode  $X $T $In $Out $LLbls $Read ()) })))))) True)
  (= 
    (-->  
      (init  $X $T $In $Out $_ $_) 
      (,  
        { (var  $T) } 
        (,  ! 
          (unify_var  $X $T $In $Out)))) True)

;
;                                 **** Unifying two variables together:

  (= 
    (-->  
      (unify_var  $X $Y $In $In) 
      (,  
        { (,  
            (myin  $X $In) 
            (myin  $Y $In)) } 
        (,  ! 
          ( (unify  $X $Y fail))))) True)
  (= 
    (-->  
      (unify_var  $X $Y $In $Out) 
      (,  
        { (,  
            (myin  $X $In) 
            (\+  
              (myin  $Y $In))) } 
        (,  ! 
          (,  
            ( (move  $X $Y)) 
            { (incl  $Y $In $Out) })))) True)
  (= 
    (-->  
      (unify_var  $X $Y $In $Out) 
      (,  
        { (,  
            (\+  
              (myin  $X $In)) 
            (myin  $Y $In)) } 
        (,  ! 
          (,  
            ( (move  $Y $X)) 
            { (incl  $X $In $Out) })))) True)
  (= 
    (-->  
      (unify_var  $X $Y $In $Out) 
      (,  
        { (,  
            (\+  
              (myin  $X $In)) 
            (\+  
              (myin  $Y $In))) } 
        (,  ! 
          (,  
            ( (move  
                (^  tvar h) $X) 
              (move  
                (^  tvar h) $Y) 
              (add  1 h) 
              (move  $Y 
                ( (-  h 1)))) 
            { (,  
                (incl  $X $In $Mid) 
                (incl  $Y $Mid $Out)) })))) True)

;
;                                   **** Unify_readmode assumes X is a dereferenced nonvariable

;
;                                     at run-time and T is a nonvariable at compile-time.

  (= 
    (-->  
      (unify_readmode  $X $T $In $Out $LLbls) 
      (,  
        { (structure  $T) } 
        (,  ! 
          (,  
            ( (equal  
                ($X) 
                (^  tatm 
                  (/  $F $N)) fail)) 
            (,  
              { (functor  $T $F $N) } 
              (unify_args  1 $N $T $In $Out 0 $X $LLbls)))))) True)
  (= 
    (-->  
      (unify_readmode  $X $T $In $Out $LLbls) 
      (,  
        { (cons  $T) } 
        (,  ! 
          (unify_args  1 2 $T $In $Out -1 $X $LLbls)))) True)
  (= 
    (-->  
      (unify_readmode  $X $T $In $In $_) 
      (,  
        { (atomic  $T) } 
        (,  ! 
          ( (equal  $X 
              (^  tatm $T) fail))))) True)

  (= 
    (-->  
      (unify_args  $I $N $_ $In $In $_ $_ $_) 
      (,  
        { (>  $I $N) } !)) True)
  (= 
    (-->  
      (unify_args  $I $N $T $In $Out $D $X 
        (Cons  $_ $LLbls)) 
      (,  
        { (= $I $N) } 
        (,  ! 
          (unify_arg  $I $T $In $Out $D $X last $LLbls)))) True)
  (= 
    (-->  
      (unify_args  $I $N $T $In $Out $D $X $LLbls) 
      (,  
        { (<  $I $N) } 
        (,  ! 
          (,  
            (unify_arg  $I $T $In $Mid $D $X nonlast $_) 
            (,  
              { (is  $I1 
                  (+  $I 1)) } 
              (unify_args  $I1 $N $T $Mid $Out $D $X $LLbls)))))) True)

  (= 
    (-->  
      (unify_arg  $I $T $In $Out $D $X $Last $LLbls) 
      (,  
        ( (move  
            ( (+  $X $ID)) $Y)) 
        (,  
          { (,  
              (is  $ID 
                (+  $I $D)) 
              (,  
                (incl  $Y $In $Mid) 
                (arg  $I $T $A))) } 
          (init  $Y $A $Mid $Out $Last $LLbls)))) True)



;
;                                        **** Unify_writemode assumes X is a dereferenced unbound

;
;                                         variable at run-time and T is a nonvariable at compile-time.

  (= 
    (-->  
      (unify_writemode  $X $T $In $Last $LLbls) 
      (,  
        { (my_compound  $T) } 
        (,  ! 
          (,  
            ( (move  
                (^  $Tag h) 
                ($X))) 
            (,  
              { (termtag  $T $Tag) } 
              (unify_block  $Last $T $_ $In $_ $LLbls)))))) True)
  (= 
    (-->  
      (unify_writemode  $X $T $_ $_ $_) 
      (,  
        { (atomic  $T) } 
        (,  ! 
          ( (move  
              (^  tatm $T) 
              ($X)))))) True)


;
;                                         **** Generate a minimal sequence of moves to create T on the heap:

  (= 
    (-->  
      (unify_block  last $T $Size $In $In 
        (Cons  $Lbl $_)) 
      (,  ! 
        (,  
          ( (add  $Size h) (jump  $Lbl)) 
          { (size  $T 0 $Size) }))) True)
  (= 
    (-->  
      (unify_block  nonlast $T $Size $In $Out 
        (Cons  $_ $LLbls)) 
      (,  ! 
        (,  
          ( (add  $Size h)) 
          (,  
            { (,  
                (size  $T 0 $Size) 
                (is  $Offset 
                  (-  $Size))) } 
            (block  $T $Offset 0 $In $Out $LLbls))))) True)

  (= 
    (-->  
      (block  $T $Inf $Outf $In $Out $LLbls) 
      (,  
        { (structure  $T) } 
        (,  ! 
          (,  
            ( (move  
                (^  tatm 
                  (/  $F $N)) 
                ( (+  h $Inf)))) 
            (,  
              { (,  
                  (functor  $T $F $N) 
                  (,  
                    (is  $Midf 
                      (+  
                        (+  $Inf $N) 1)) 
                    (is  $S 
                      (+  $Inf 1)))) } 
              (,  
                (make_slots  1 $N $T $S $Offsets $In $Mid) 
                (block_args  1 $N $T $Midf $Outf $Offsets $Mid $Out $LLbls))))))) True)
  (= 
    (-->  
      (block  $T $Inf $Outf $In $Out $LLbls) 
      (,  
        { (cons  $T) } 
        (,  ! 
          (,  
            { (is  $Midf 
                (+  $Inf 2)) } 
            (,  
              (make_slots  1 2 $T $Inf $Offsets $In $Mid) 
              (block_args  1 2 $T $Midf $Outf $Offsets $Mid $Out $LLbls)))))) True)
  (= 
    (-->  
      (block  $T $Inf $Inf $In $In ()) 
      (,  
        { (atomic  $T) } !)) True)
  (= 
    (-->  
      (block  $T $Inf $Inf $In $In ()) 
      (,  
        { (var  $T) } !)) True)

  (= 
    (-->  
      (block_args  $I $N $_ $Inf $Inf () $In $In ()) 
      (,  
        { (>  $I $N) } !)) True)
  (= 
    (-->  
      (block_args  $I $N $T $Inf $Outf 
        ($Inf) $In $Out 
        (Cons  $Lbl $LLbls)) 
      (,  
        { (= $I $N) } 
        (,  ! 
          (,  
            ( (label  $Lbl)) 
            (,  
              { (arg  $I $T $A) } 
              (block  $A $Inf $Outf $In $Out $LLbls)))))) True)
  (= 
    (-->  
      (block_args  $I $N $T $Inf $Outf 
        (Cons  $Inf $Offsets) $In $Out $LLbls) 
      (,  
        { (<  $I $N) } 
        (,  ! 
          (,  
            { (arg  $I $T $A) } 
            (,  
              (block  $A $Inf $Midf $In $Mid $_) 
              (,  
                { (is  $I1 
                    (+  $I 1)) } 
                (block_args  $I1 $N $T $Midf $Outf $Offsets $Mid $Out $LLbls))))))) True)

  (= 
    (-->  
      (make_slots  $I $N $_ $_ () $In $In) 
      (,  
        { (>  $I $N) } !)) True)
  (= 
    (-->  
      (make_slots  $I $N $T $S 
        (Cons  $Off $Offsets) $In $Out) 
      (,  
        { (=<  $I $N) } 
        (,  ! 
          (,  
            { (arg  $I $T $A) } 
            (,  
              (init_var  $A $S $In) 
              (,  
                { (,  
                    (incl  $A $In $Mid) 
                    (make_word  $A $Off $Word)) } 
                (,  
                  ( (move  $Word 
                      ( (+  h $S)))) 
                  (,  
                    { (,  
                        (is  $S1 
                          (+  $S 1)) 
                        (is  $I1 
                          (+  $I 1))) } 
                    (make_slots  $I1 $N $T $S1 $Offsets $Mid $Out))))))))) True)


;
;                                          Initialize first-time variables in write mode:

  (= 
    (-->  
      (init_var  $V $I $In) 
      (,  
        { (,  
            (var  $V) 
            (\+  
              (myin  $V $In))) } 
        (,  ! 
          ( (move  
              (^  tvar 
                (+  h $I)) $V))))) True)
  (= 
    (-->  
      (init_var  $V $_ $In) 
      (,  
        { (,  
            (var  $V) 
            (myin  $V $In)) } !)) True)
  (= 
    (-->  
      (init_var  $V $_ $_) 
      (,  
        { (nonvar  $V) } !)) True)


  (= 
    (make-word $C $Off 
      (^ $Tag 
        (+ h $Off))) 
    ( (my-compound $C) 
      (set-det) 
      (termtag $C $Tag)))
  (= 
    (make-word $V $_ $V) 
    ( (var $V) (set-det)))
  (= 
    (make-word $A $_ 
      (^ tatm $A)) 
    ( (atomic $A) (set-det)))

;
;                                          Calculate the size of T on the heap:


  (= 
    (-->  
      (size  $T) 
      (,  
        { (structure  $T) } 
        (,  ! 
          (,  
            { (functor  $T $_ $N) } 
            (,  
              (call  
                (add  1)) 
              (,  
                (call  
                  (add  $N)) 
                (size_args  1 $N $T))))))) True)
  (= 
    (-->  
      (size  $T) 
      (,  
        { (cons  $T) } 
        (,  ! 
          (,  
            (call  
              (add  2)) 
            (size_args  1 2 $T))))) True)
  (= 
    (-->  
      (size  $T) 
      (,  
        { (atomic  $T) } !)) True)
  (= 
    (-->  
      (size  $T) 
      (,  
        { (var  $T) } !)) True)

  (= 
    (-->  
      (size_args  $I $N $_) 
      (,  
        { (>  $I $N) } !)) True)
  (= 
    (-->  
      (size_args  $I $N $T) 
      (,  
        { (=<  $I $N) } 
        (,  ! 
          (,  
            { (arg  $I $T $A) } 
            (,  
              (size  $A) 
              (,  
                { (is  $I1 
                    (+  $I 1)) } 
                (size_args  $I1 $N $T))))))) True)

;
;                                         **** Utility routines:



  (= 
    (add $I $X $Y) 
    (is $Y 
      (+ $X $I)))


  (= 
    (myin $A 
      (Cons  $B $S)) 
    ( (compare $Order $A $B) (in-2 $Order $A $S)))


  (= 
    (in_2  = $_ $_) True)
  (= 
    (in-2 > $A $S) 
    (myin $A $S))


  (= 
    (incl $A $S1 $S) 
    (incl-2 $S1 $A $S))


  (= 
    (incl_2  () $A 
      ($A)) True)
  (= 
    (incl-2 
      (Cons  $B $S1) $A $S) 
    ( (compare $Order $A $B) (incl-3 $Order $A $B $S1 $S)))


  (= 
    (incl_3  < $A $B $S1 
      (Cons  $A 
        (Cons  $B $S1))) True)
  (= 
    (incl_3  = $_ $B $S1 
      (Cons  $B $S1)) True)
  (= 
    (incl-3 > $A $B $S1 
      (Cons  $B $S)) 
    (incl-2 $S1 $A $S))


  (= 
    (my-compound $X) 
    ( (nonvar $X) (not (atomic $X))))

  (= 
    (cons $X) 
    ( (nonvar $X) (= $X (Cons  $_ $_))))

  (= 
    (structure $X) 
    ( (my-compound $X) (\= $X (Cons  $_ $_))))


  (= 
    (termtag $T tstr) 
    (structure $T))
  (= 
    (termtag $T tlst) 
    (cons $T))
  (= 
    (termtag $T tatm) 
    (atomic $T))
  (= 
    (termtag $T tvar) 
    (var $T))






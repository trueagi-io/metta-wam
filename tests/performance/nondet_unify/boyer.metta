;
;                    generated: 20 November 1989

;
;                    option(s):

;
; 

;
;                      boyer

;
; 

;
;                       Evan Tick (from Lisp version by R. P. Gabriel)

;
; 

;
;                       November 1985

;
; 

;
;                        prove arithmetic theorem



  (= 
    (go) 
    ( (statistics runtime 
        (:: $_ $_)) 
      (wff $Wff) 
      (rewrite $Wff $NewWff) 
      (tautology $NewWff Nil Nil) 
      (statistics runtime 
        (:: $_ $T)) 
      (write 'execution time is ') 
      (write $T) 
      (write ' milliseconds')))


  (= 
    (top) 
    ( (wff $Wff) 
      (rewrite $Wff $NewWff) 
      (tautology $NewWff Nil Nil)))


  (= 
    (wff (implies (and (implies $X $Y) (and (implies $Y $Z) (and (implies $Z $U) (implies $U $W)))) (implies $X $W))) 
    ( (= $X 
        (f (myplus (myplus a b) (myplus c zero)))) 
      (= $Y 
        (f (times (times a b) (myplus c d)))) 
      (= $Z 
        (f (reverse (append (append a b) Nil)))) 
      (= $U 
        (equal 
          (myplus a b) 
          (boyer-difference x y))) 
      (= $W 
        (lessp 
          (remainder a b) 
          (boyer-member a 
            (length b))))))


  (= 
    (tautology $Wff) 
    ( (rewrite $Wff $NewWff) (tautology $NewWff Nil Nil)))
;                        ;        write('rewriting...'),nl,

;                        ;        write('proving...'),nl,



  (= 
    (tautology $Wff $Tlist $Flist) 
    ( (det-if-then-else 
        (truep $Wff $Tlist) True 
        (det-if-then-else 
          (falsep $Wff $Flist) fail 
          (det-if-then-else 
            (= $Wff 
              (if $If $Then $Else)) 
            (det-if-then-else 
              (truep $If $Tlist) 
              (tautology $Then $Tlist $Flist) 
              (det-if-then-else 
                (falsep $If $Flist) 
                (tautology $Else $Tlist $Flist) 
                (, 
                  (tautology $Then 
                    (Cons  $If $Tlist) $Flist) 
                  (tautology $Else $Tlist 
                    (Cons  $If $Flist))))) False))) (set-det)))
;                         ; both must hold





  (= 
    (rewrite $Atom $Atom) 
    ( (atomic $Atom) (set-det)))
  (= 
    (rewrite $Old $New) 
    ( (functor $Old $F $N) 
      (functor $Mid $F $N) 
      (rewrite-args $N $Old $Mid) 
      (or 
        (, 
          (equal $Mid $Next) 
          (rewrite $Next $New)) 
        (= $New $Mid)) 
      (set-det)))
;                          ; should be ->, but is compiler smart

;                          ; enough to generate cut for -> ?




  (= 
    (rewrite-args 0 $_ $_) 
    (set-det))
  (= 
    (rewrite-args $N $Old $Mid) 
    ( (arg $N $Old $OldArg) 
      (arg $N $Mid $MidArg) 
      (rewrite $OldArg $MidArg) 
      (is $N1 
        (- $N 1)) 
      (rewrite-args $N1 $Old $Mid)))


  (= 
    (truep t $_) 
    (set-det))
  (= 
    (truep $Wff $Tlist) 
    (boyer-member $Wff $Tlist))


  (= 
    (falsep f $_) 
    (set-det))
  (= 
    (falsep $Wff $Flist) 
    (boyer-member $Wff $Flist))


  (= 
    (boyer-member $X 
      (Cons  $X $_)) 
    (set-det))
  (= 
    (boyer-member $X 
      (Cons  $_ $T)) 
    (boyer-member $X $T))



  (= 
    (equal  
      (and  $P $Q) 
      (if  $P 
        (if  $Q t f) f)) True)
  (= 
    (equal  
      (append  
        (append  $X $Y) $Z) 
      (append  $X 
        (append  $Y $Z))) True)
  (= 
    (equal  
      (assignment  $X 
        (append  $A $B)) 
      (if  
        (assignedp  $X $A) 
        (assignment  $X $A) 
        (assignment  $X $B))) True)
  (= 
    (equal  
      (assume_false  $Var $Alist) 
      (cons  
        (cons  $Var f) $Alist)) True)
  (= 
    (equal  
      (assume_true  $Var $Alist) 
      (cons  
        (cons  $Var t) $Alist)) True)
  (= 
    (equal  
      (boolean  $X) 
      (or  
        (equal  $X t) 
        (equal  $X f))) True)
  (= 
    (equal  
      (car  
        (gopher  $X)) 
      (if  
        (listp  $X) 
        (car  
          (flatten  $X)) zero)) True)
  (= 
    (equal  
      (compile  $Form) 
      (reverse  
        (codegen  
          (optimize  $Form) ()))) True)
  (= 
    (equal  
      (count_list  $Z 
        (sort_lp  $X $Y)) 
      (myplus  
        (count_list  $Z $X) 
        (count_list  $Z $Y))) True)
  (= 
    (equal  
      (countps_  $L $Pred) 
      (countps_loop  $L $Pred zero)) True)
  (= 
    (equal 
      (boyer-difference $A $B) $C) 
    (boyer-difference $A $B $C))
  (= 
    (equal  
      (divides  $X $Y) 
      (zerop  
        (remainder  $Y $X))) True)
  (= 
    (equal  
      (dsort  $X) 
      (sort2  $X)) True)
  (= 
    (equal  
      (eqp  $X $Y) 
      (equal  
        (fix  $X) 
        (fix  $Y))) True)
  (= 
    (equal 
      (equal $A $B) $C) 
    (eq $A $B $C))
  (= 
    (equal  
      (even1  $X) 
      (if  
        (zerop  $X) t 
        (odd  
          (decr  $X)))) True)
  (= 
    (equal  
      (exec  
        (append  $X $Y) $Pds $Envrn) 
      (exec  $Y 
        (exec  $X $Pds $Envrn) $Envrn)) True)
  (= 
    (equal 
      (exp $A $B) $C) 
    (exp $A $B $C))
  (= 
    (equal  
      (fact_  $I) 
      (fact_loop  $I 1)) True)
  (= 
    (equal  
      (falsify  $X) 
      (falsify1  
        (normalize  $X) ())) True)
  (= 
    (equal  
      (fix  $X) 
      (if  
        (numberp  $X) $X zero)) True)
  (= 
    (equal  
      (flatten  
        (cdr  
          (gopher  $X))) 
      (if  
        (listp  $X) 
        (cdr  
          (flatten  $X)) 
        (cons  zero ()))) True)
  (= 
    (equal 
      (gcd $A $B) $C) 
    (gcd $A $B $C))
  (= 
    (equal  
      (get  $J 
        (set  $I $Val $Mem)) 
      (if  
        (eqp  $J $I) $Val 
        (get  $J $Mem))) True)
  (= 
    (equal  
      (greatereqp  $X $Y) 
      (not  
        (lessp  $X $Y))) True)
  (= 
    (equal  
      (greatereqpr  $X $Y) 
      (not  
        (lessp  $X $Y))) True)
  (= 
    (equal  
      (greaterp  $X $Y) 
      (lessp  $Y $X)) True)
  (= 
    (equal  
      (if  
        (if  $A $B $C) $D $E) 
      (if  $A 
        (if  $B $D $E) 
        (if  $C $D $E))) True)
  (= 
    (equal  
      (iff  $X $Y) 
      (and  
        (implies  $X $Y) 
        (implies  $Y $X))) True)
  (= 
    (equal  
      (implies  $P $Q) 
      (if  $P 
        (if  $Q t f) t)) True)
  (= 
    (equal  
      (last  
        (append  $A $B)) 
      (if  
        (listp  $B) 
        (last  $B) 
        (if  
          (listp  $A) 
          (cons  
            (car  
              (last  $A))) $B))) True)
  (= 
    (equal 
      (length $A) $B) 
    (mylength $A $B))
  (= 
    (equal  
      (lesseqp  $X $Y) 
      (not  
        (lessp  $Y $X))) True)
  (= 
    (equal 
      (lessp $A $B) $C) 
    (lessp $A $B $C))
  (= 
    (equal  
      (listp  
        (gopher  $X)) 
      (listp  $X)) True)
  (= 
    (equal  
      (mc_flatten  $X $Y) 
      (append  
        (flatten  $X) $Y)) True)
  (= 
    (equal 
      (meaning $A $B) $C) 
    (meaning $A $B $C))
  (= 
    (equal 
      (boyer-member $A $B) $C) 
    (myboyer-member $A $B $C))
  (= 
    (equal  
      (not  $P) 
      (if  $P f t)) True)
  (= 
    (equal 
      (nth $A $B) $C) 
    (nth $A $B $C))
  (= 
    (equal  
      (numberp  
        (greatest_factor  $X $Y)) 
      (not  
        (and  
          (or  
            (zerop  $Y) 
            (equal  $Y 1)) 
          (not  
            (numberp  $X))))) True)
  (= 
    (equal  
      (or  $P $Q) 
      (if  $P t 
        (if  $Q t f) f)) True)
  (= 
    (equal 
      (myplus $A $B) $C) 
    (myplus $A $B $C))
  (= 
    (equal 
      (power-eval $A $B) $C) 
    (power-eval $A $B $C))
  (= 
    (equal  
      (prime  $X) 
      (and  
        (not  
          (zerop  $X)) 
        (and  
          (not  
            (equal  $X 
              (add1  zero))) 
          (prime1  $X 
            (decr  $X))))) True)
  (= 
    (equal  
      (prime_list  
        (append  $X $Y)) 
      (and  
        (prime_list  $X) 
        (prime_list  $Y))) True)
  (= 
    (equal 
      (quotient $A $B) $C) 
    (quotient $A $B $C))
  (= 
    (equal 
      (remainder $A $B) $C) 
    (remainder $A $B $C))
  (= 
    (equal  
      (reverse_  $X) 
      (reverse_loop  $X ())) True)
  (= 
    (equal  
      (reverse  
        (append  $A $B)) 
      (append  
        (reverse  $B) 
        (reverse  $A))) True)
  (= 
    (equal 
      (reverse-loop $A $B) $C) 
    (reverse-loop $A $B $C))
  (= 
    (equal  
      (samefringe  $X $Y) 
      (equal  
        (flatten  $X) 
        (flatten  $Y))) True)
  (= 
    (equal  
      (sigma  zero $I) 
      (quotient  
        (times  $I 
          (add1  $I)) 2)) True)
  (= 
    (equal  
      (sort2  
        (delete  $X $L)) 
      (delete  $X 
        (sort2  $L))) True)
  (= 
    (equal  
      (tautology_checker  $X) 
      (tautologyp  
        (normalize  $X) ())) True)
  (= 
    (equal 
      (times $A $B) $C) 
    (times $A $B $C))
  (= 
    (equal  
      (times_list  
        (append  $X $Y)) 
      (times  
        (times_list  $X) 
        (times_list  $Y))) True)
  (= 
    (equal  
      (value  
        (normalize  $X) $A) 
      (value  $X $A)) True)
  (= 
    (equal  
      (zerop  $X) 
      (or  
        (equal  $X zero) 
        (not  
          (numberp  $X)))) True)


  (= 
    (boyer-difference $X $X zero) 
    (set-det))
  (= 
    (boyer-difference 
      (myplus $X $Y) $X 
      (fix $Y)) 
    (set-det))
  (= 
    (boyer-difference 
      (myplus $Y $X) $X 
      (fix $Y)) 
    (set-det))
  (= 
    (boyer-difference 
      (myplus $X $Y) 
      (myplus $X $Z) 
      (boyer-difference $Y $Z)) 
    (set-det))
  (= 
    (boyer-difference 
      (myplus $B 
        (myplus $A $C)) $A 
      (myplus $B $C)) 
    (set-det))
  (= 
    (boyer-difference 
      (add1 (myplus $Y $Z)) $Z 
      (add1 $Y)) 
    (set-det))
  (= 
    (boyer_difference  
      (add1  
        (add1  $X)) 2 
      (fix  $X)) True)


  (= 
    (eq 
      (myplus $A $B) zero 
      (and 
        (zerop $A) 
        (zerop $B))) 
    (set-det))
  (= 
    (eq 
      (myplus $A $B) 
      (myplus $A $C) 
      (equal 
        (fix $B) 
        (fix $C))) 
    (set-det))
  (= 
    (eq zero 
      (boyer-difference $X $Y) 
      (not (lessp $Y $X))) 
    (set-det))
  (= 
    (eq $X 
      (boyer-difference $X $Y) 
      (and 
        (numberp $X) 
        (and (or (equal $X zero) (zerop $Y))))) 
    (set-det))
  (= 
    (eq 
      (times $X $Y) zero 
      (or 
        (zerop $X) 
        (zerop $Y))) 
    (set-det))
  (= 
    (eq 
      (append $A $B) 
      (append $A $C) 
      (equal $B $C)) 
    (set-det))
  (= 
    (eq 
      (flatten $X) 
      (cons $Y Nil) 
      (and 
        (nlistp $X) 
        (equal $X $Y))) 
    (set-det))
  (= 
    (eq 
      (greatest-factor $X $Y) zero 
      (and 
        (or 
          (zerop $Y) 
          (equal $Y 1)) 
        (equal $X zero))) 
    (set-det))
  (= 
    (eq 
      (greatest-factor $X $_) 1 
      (equal $X 1)) 
    (set-det))
  (= 
    (eq $Z 
      (times $W $Z) 
      (and 
        (numberp $Z) 
        (or 
          (equal $Z zero) 
          (equal $W 1)))) 
    (set-det))
  (= 
    (eq $X 
      (times $X $Y) 
      (or 
        (equal $X zero) 
        (and 
          (numberp $X) 
          (equal $Y 1)))) 
    (set-det))
  (= 
    (eq 
      (times $A $B) 1 
      (and 
        (not (equal $A zero)) 
        (and 
          (not (equal $B zero)) 
          (and 
            (numberp $A) 
            (and 
              (numberp $B) 
              (and 
                (equal 
                  (decr $A) zero) 
                (equal 
                  (decr $B) zero))))))) 
    (set-det))
  (= 
    (eq 
      (boyer-difference $X $Y) 
      (boyer-difference $Z $Y) 
      (if 
        (lessp $X $Y) 
        (not (lessp $Y $Z)) 
        (if 
          (lessp $Z $Y) 
          (not (lessp $Y $X)) 
          (equal 
            (fix $X) 
            (fix $Z))))) 
    (set-det))
  (= 
    (eq  
      (lessp  $X $Y) $Z 
      (if  
        (lessp  $X $Y) 
        (equal  t $Z) 
        (equal  f $Z))) True)


  (= 
    (exp $I 
      (myplus $J $K) 
      (times 
        (exp $I $J) 
        (exp $I $K))) 
    (set-det))
  (= 
    (exp  $I 
      (times  $J $K) 
      (exp  
        (exp  $I $J) $K)) True)


  (= 
    (gcd $X $Y 
      (gcd $Y $X)) 
    (set-det))
  (= 
    (gcd  
      (times  $X $Z) 
      (times  $Y $Z) 
      (times  $Z 
        (gcd  $X $Y))) True)


  (= 
    (mylength  
      (reverse  $X) 
      (length  $X)) True)
  (= 
    (mylength  
      (cons  $_ 
        (cons  $_ 
          (cons  $_ 
            (cons  $_ 
              (cons  $_ 
                (cons  $_ $X7)))))) 
      (myplus  6 
        (length  $X7))) True)


  (= 
    (lessp 
      (remainder $_ $Y) $Y 
      (not (zerop $Y))) 
    (set-det))
  (= 
    (lessp 
      (quotient $I $J) $I 
      (and 
        (not (zerop $I)) 
        (or 
          (zerop $J) 
          (not (equal $J 1))))) 
    (set-det))
  (= 
    (lessp 
      (remainder $X $Y) $X 
      (and 
        (not (zerop $Y)) 
        (and 
          (not (zerop $X)) 
          (not (lessp $X $Y))))) 
    (set-det))
  (= 
    (lessp 
      (myplus $X $Y) 
      (myplus $X $Z) 
      (lessp $Y $Z)) 
    (set-det))
  (= 
    (lessp 
      (times $X $Z) 
      (times $Y $Z) 
      (and 
        (not (zerop $Z)) 
        (lessp $X $Y))) 
    (set-det))
  (= 
    (lessp $Y 
      (myplus $X $Y) 
      (not (zerop $X))) 
    (set-det))
  (= 
    (lessp  
      (length  
        (delete  $X $L)) 
      (length  $L) 
      (boyer_member  $X $L)) True)


  (= 
    (meaning 
      (plus-tree (append $X $Y)) $A 
      (myplus 
        (meaning 
          (plus-tree $X) $A) 
        (meaning 
          (plus-tree $Y) $A))) 
    (set-det))
  (= 
    (meaning 
      (plus-tree (plus-fringe $X)) $A 
      (fix (meaning $X $A))) 
    (set-det))
  (= 
    (meaning  
      (plus_tree  
        (delete  $X $Y)) $A 
      (if  
        (boyer_member  $X $Y) 
        (boyer_difference  
          (meaning  
            (plus_tree  $Y) $A) 
          (meaning  $X $A)) 
        (meaning  
          (plus_tree  $Y) $A))) True)


  (= 
    (myboyer-member $X 
      (append $A $B) 
      (or 
        (boyer-member $X $A) 
        (boyer-member $X $B))) 
    (set-det))
  (= 
    (myboyer-member $X 
      (reverse $Y) 
      (boyer-member $X $Y)) 
    (set-det))
  (= 
    (myboyer_member  $A 
      (intersect  $B $C) 
      (and  
        (boyer_member  $A $B) 
        (boyer_member  $A $C))) True)


  (= 
    (nth  zero $_ zero) True)
  (= 
    (nth  () $I 
      (if  
        (zerop  $I) () zero)) True)
  (= 
    (nth  
      (append  $A $B) $I 
      (append  
        (nth  $A $I) 
        (nth  $B 
          (boyer_difference  $I 
            (length  $A))))) True)


  (= 
    (myplus 
      (myplus $X $Y) $Z 
      (myplus $X 
        (myplus $Y $Z))) 
    (set-det))
  (= 
    (myplus 
      (remainder $X $Y) 
      (times $Y 
        (quotient $X $Y)) 
      (fix $X)) 
    (set-det))
  (= 
    (myplus  $X 
      (add1  $Y) 
      (if  
        (numberp  $Y) 
        (add1  
          (myplus  $X $Y)) 
        (add1  $X))) True)


  (= 
    (power-eval 
      (big-plus1 $L $I $Base) $Base 
      (myplus 
        (power-eval $L $Base) $I)) 
    (set-det))
  (= 
    (power-eval 
      (power-rep $I $Base) $Base 
      (fix $I)) 
    (set-det))
  (= 
    (power-eval 
      (big-plus $X $Y $I $Base) $Base 
      (myplus $I 
        (myplus 
          (power-eval $X $Base) 
          (power-eval $Y $Base)))) 
    (set-det))
  (= 
    (power_eval  
      (big_plus  
        (power_rep  $I $Base) 
        (power_rep  $J $Base) zero $Base) $Base 
      (myplus  $I $J)) True)


  (= 
    (quotient  
      (myplus  $X 
        (myplus  $X $Y)) 2 
      (myplus  $X 
        (quotient  $Y 2))) True)
  (= 
    (quotient  
      (times  $Y $X) $Y 
      (if  
        (zerop  $Y) zero 
        (fix  $X))) True)


  (= 
    (remainder $_ 1 zero) 
    (set-det))
  (= 
    (remainder $X $X zero) 
    (set-det))
  (= 
    (remainder 
      (times $_ $Z) $Z zero) 
    (set-det))
  (= 
    (remainder  
      (times  $Y $_) $Y zero) True)


  (= 
    (reverse-loop $X $Y 
      (append 
        (reverse $X) $Y)) 
    (set-det))
  (= 
    (reverse_loop  $X () 
      (reverse  $X)) True)


  (= 
    (times $X 
      (myplus $Y $Z) 
      (myplus 
        (times $X $Y) 
        (times $X $Z))) 
    (set-det))
  (= 
    (times 
      (times $X $Y) $Z 
      (times $X 
        (times $Y $Z))) 
    (set-det))
  (= 
    (times $X 
      (boyer-difference $C $W) 
      (boyer-difference 
        (times $C $X) 
        (times $W $X))) 
    (set-det))
  (= 
    (times  $X 
      (add1  $Y) 
      (if  
        (numberp  $Y) 
        (myplus  $X 
          (times  $X $Y)) 
        (fix  $X))) True)


;; Standalone iterative backward chaining experiments.
;;
;; The particularity of iterative backward chaining is that the proof
;; trees should include proof abstractions, in the lambda calculus
;; sense.  I.e. we want the backward chaining part to produce macro
;; rules and re-use these macro rules iteratively.

;;;;;;;;;
;; Nat ;;
;;;;;;;;;

;; Define Nat
(: Nat Type)
(: Z Nat)
(: S (-> Nat Nat))

;; Define <=
(: <= (-> $a $a Bool))
(= (<= $x $y) (or (< $x $y) (== $x $y)))

;; Define cast functions between Nat and Number
(: fromNumber (-> Number Nat))
(= (fromNumber $n) (if (<= $n 0) Z (S (fromNumber (- $n 1)))))
(: fromNat (-> Nat Number))
(= (fromNat Z) 0)
(= (fromNat (S $k)) (+ 1 (fromNat $k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backward DTL Curried ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Similar to the traditional backward chaining DTL but rules are
;; curried.  This allows to partially apply rule which is useful for
;; inferring proof abstractions.

;; Knowledge and rule base
!(bind! &kb (new-space))

;; Knowledge base
!(add-atom &kb (: ab (→ A B)))
!(add-atom &kb (: bc (→ B C)))
!(add-atom &kb (: a A))

;; Rule base
!(add-atom &kb (: ModusPonens (-> (→ $p $q)  ; Premise 1
                                  (-> $p     ; Premise 2
                                      $q)))) ; Conclusion
!(add-atom &kb (: Deduction (-> (→ $q $r)         ; Premise 1
                                (-> (→ $p $q)     ; Premise 2
                                    (→ $p $r))))) ; Conclusion
!(add-atom &kb (: . (-> (-> $q $r)         ; Premise 1
                        (-> (-> $p $q)     ; Premise 2
                            (-> $p $r))))) ; Conclusion

;; Backward chainer/synthesizer
;; Base case
(: syn (-> $a Nat $a))
(= (syn (: $prf $ccln) $_) (match &kb (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (syn (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) (syn (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (syn (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))

;; Test backward chainer DTL curried
! "=== Test Backward Chainer DTL Curried ==="

;; Prove A (axiom)
!(assertEqual
  (syn (: a A) Z)
  (: a A))

;; Prove (-> (→ A B) (-> A B)) (axiom)
!(assertEqual
  (syn (: $prf (-> $prms (-> A B))) Z)
  (: ModusPonens (-> (→ A B) (-> A B))))

;; Prove (-> A B)
!(assertEqual
  (syn (: $prf (-> A B)) (fromNumber 1))
  (: (ModusPonens ab) (-> A B)))

;; Prove B (one modus ponens)
!(assertEqual
  (syn (: $prf B) (fromNumber 2))
  (: ((ModusPonens ab) a) B))

;; Prove C (two modus ponens)
!(assertEqual
  (syn (: $prf C) (fromNumber 3))
  (: ((ModusPonens bc) ((ModusPonens ab) a)) C))

;; Prove C (two modus ponens, or one modus ponens and one deduction)
!(assertEqualToResult
  (syn (: $prf C) (fromNumber 4))
  ((: ((((. ModusPonens) (Deduction bc)) ab) a) C)
   (: (((. (ModusPonens bc)) (ModusPonens ab)) a) C)
   (: ((ModusPonens ((Deduction bc) ab)) a) C)
   (: ((ModusPonens bc) ((ModusPonens ab) a)) C)))

;; Prove 3-premises version of Modus Ponens, that is
;;
;; (-> (→ $q $r) (-> (→ $p $q) (-> $p $r)))
!(assertEqualToResult
  (syn (: $prf (-> (→ $q $r) (-> (→ $p $q) (-> $p $r)))) (fromNumber 4))
  ((: ((((. .) .) ModusPonens) Deduction) (-> (→ $q $r) (-> (→ $p $q) (-> $p $r))))
   (: ((. (. ModusPonens)) Deduction) (-> (→ $q $r) (-> (→ $p $q) (-> $p $r))))))

;; Prove 3-premises version of Deduction, that is
;;
;; (-> (→ $r $s) (-> (→ $q $r) (-> (→ $p $q) (→ $p $s))))
!(assertEqualToResult
  (syn (: $prf (-> (→ $r $s) (-> (→ $q $r) (-> (→ $p $q) (→ $p $s))))) (fromNumber 4))
  ((: ((((. .) .) Deduction) Deduction) (-> (→ $r $s) (-> (→ $q $r) (-> (→ $p $q) (→ $p $s)))))
   (: ((. (. Deduction)) Deduction) (-> (→ $r $s) (-> (→ $q $r) (-> (→ $p $q) (→ $p $s)))))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Define ad-atom-nodup, that adds an atom only if it is not already
;; in the atomspace
(: add-atom-nodup (-> $st Atom ()))
(= (add-atom-nodup $space $atom)
   (case (match $space $atom $atom)
     (($atom ())
      (%void% (add-atom $space $atom)))))

;; Add all atoms from an expression to a given atomspace
(: add-atoms-nodup (-> $st Expression ()))
(= (add-atoms-nodup $space $atoms)
   (if (== $atoms ())
       ()
       (let* (($head (car-atom $atoms))
              ($tail (cdr-atom $atoms))
              ($dummy (add-atom-nodup $space $head)))
         (add-atoms-nodup $space $tail))))

;; Test utility functions
! "=== Test Utility Functions ==="

;; Test add-atom-nodup
!(bind! &space-test (new-space))
!(add-atom-nodup &space-test A)
!(add-atom-nodup &space-test A)
!(assertEqualToResult
  (get-atoms &space-test)
  (A))

;; Test add-atoms-nodup
!(add-atoms-nodup &space-test (A B C A))
!(assertEqualToResult
  (get-atoms &space-test)
  (A B C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterative Chainer Wrapped Around Backward DTL Curried (collapse) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call the backward DTL curried chainer iteratively, adding the
;; proofs, or proof abstractions to the knowledge space after each
;; iterations.  To avoid irreproducible behavior (due to the side
;; effects of modifying the atomspace), each call of the backward
;; chainer collapses between iterations.
(: isyn (-> $a                            ; Query
            Nat                           ; Depth
            Nat                           ; Number of iterations
            $a))                          ; Result

;; Base case.  For now it terminates at exactly iteration Z to avoid
;; collecting too many redundant results.
(= (isyn $query $depth Z) $query)

;; Iterative step
(= (isyn $query $depth (S $k))
   (let* (($cres (collapse (syn $query $depth)))
          ($dummy (add-atoms-nodup &kb $cres)))
     (isyn (superpose $cres) $depth $k)))

;; Test Iterative Chainer Wrapped Around Forward Revertant (collapse)
! "=== Test Iterative Chainer Wrapped Around Backward DTL Curried (collapse) ==="

;; No iteration
!(assertEqual
  (isyn (: a A) Z Z)
  (: a A))
!(assertEqualToResult
  (get-atoms &kb)
  ((: ab (→ A B))
   (: bc (→ B C))
   (: a A)
   (: ModusPonens (-> (→ $p $q) (-> $p $q)))
   (: Deduction (-> (→ $q $r) (-> (→ $p $q) (→ $p $r))))
   (: . (-> (-> $q $r) (-> (-> $p $q) (-> $p $r))))))

;; One iteration of two steps backward chainer.  We need two steps
;; of backward chaining because one step merely returns what is in the
;; knowledge/rule base.
!(assertEqual
  (isyn (: $prf B) (fromNumber 2) (fromNumber 1))
  (: ((ModusPonens ab) a) B))
!(assertEqualToResult
  (get-atoms &kb)
  ((: ab (→ A B))
   (: bc (→ B C))
   (: a A)
   (: ModusPonens (-> (→ $p $q) (-> $p $q)))
   (: Deduction (-> (→ $q $r) (-> (→ $p $q) (→ $p $r))))
   (: . (-> (-> $q $r) (-> (-> $p $q) (-> $p $r))))
   (: ((ModusPonens ab) a) B)))

;; One iteration of two steps backward chainer.  C can be proven with
;; just one iteration by re-using the proof of B.
!(assertEqual
  (isyn (: $prf C) (fromNumber 2) (fromNumber 1))
  (: ((ModusPonens bc) ((ModusPonens ab) a)) C))
!(assertEqualToResult
  (get-atoms &kb)
  ((: ab (→ A B))
   (: bc (→ B C))
   (: a A)
   (: ModusPonens (-> (→ $p $q) (-> $p $q)))
   (: Deduction (-> (→ $q $r) (-> (→ $p $q) (→ $p $r))))
   (: . (-> (-> $q $r) (-> (-> $p $q) (-> $p $r))))
   (: ((ModusPonens ab) a) B)
   (: ((ModusPonens bc) ((ModusPonens ab) a)) C)))

;; One iteration of four steps backward chainer.  With four steps,
;; proof abstractions such as 3-premises version of Modus Ponens can
;; be inferred as well.
!(assertEqualToResult
  (isyn (: $prf (-> (→ $q $r) (-> (→ $p $q) (-> $p $r))))
        (fromNumber 4) (fromNumber 1))
  ((: ((((. .) .) ModusPonens) Deduction)
      (-> (→ $q $r) (-> (→ $p $q) (-> $p $r))))
   (: ((. (. ModusPonens)) Deduction)
      (-> (→ $q $r) (-> (→ $p $q) (-> $p $r))))))

;; Let's add more premises to the knowledge base
!(add-atom &kb (: cd (→ C D)))
!(add-atom &kb (: de (→ D E)))
!(add-atom &kb (: ef (→ E F)))

;; Then F can be proven with one iteration and three steps (instead of
;; four) of backward chaining by re-using the proof abstraction
;; previously inferred.
;;
;; TODO: re-enable after introducing assert contain
;; !(assertEqualToResult
  !(isyn (: $prf F) (fromNumber 3) (fromNumber 1))
  ;; ((: (((((((. .) .) ModusPonens) Deduction) ef) de) ((ModusPonens cd) ((ModusPonens bc) ((ModusPonens ab) a)))) F)
  ;;  (: (((((. (. ModusPonens)) Deduction) ef) de) ((ModusPonens cd) ((ModusPonens bc) ((ModusPonens ab) a)))) F))

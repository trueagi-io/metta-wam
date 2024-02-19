
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inference control experiments.  Attempt to reproduce the OpenCog
;; Classic inference control meta learning
;; https://github.com/opencog/pln/tree/master/examples/pln/inference-control-meta-learning
;;
;; Letters are replaced by months.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knowledge and Rule Base ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; January precedes February, which precedes Mars, etc.
!(add-atom &kb (: JF (≼ Jan Feb)))
!(add-atom &kb (: FM (≼ Feb Mar)))
!(add-atom &kb (: MA (≼ Mar Apr)))
!(add-atom &kb (: AM (≼ Apr May)))
!(add-atom &kb (: MJ (≼ May Jun)))
!(add-atom &kb (: JJ (≼ Jun Jul)))
!(add-atom &kb (: JA (≼ Jul Aug)))
!(add-atom &kb (: AS (≼ Aug Sep)))
!(add-atom &kb (: SO (≼ Sep Oct)))
!(add-atom &kb (: ON (≼ Oct Nov)))
!(add-atom &kb (: ND (≼ Nov Dec)))

;; Precedence is non strict, i.e. reflexive
!(add-atom &kb (: Refl (≼ $x $x)))

;; Precedence is transitive
!(add-atom &kb (: Trans (-> (≼ $x $y)
                            (-> (≼ $y $z)
                                (≼ $x $z)))))

;; Shortcut rule: January precedes all months
!(add-atom &kb (: JPA (≼ Jan $x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backward Controlled Chainer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(: bc (-> (-> $a $ct $ct)      ; Context abstraction updater
          (-> $a $ct $ct)      ; Context argument updater
          (-> $a $ct Bool)     ; Termination predicate
          $ct                  ; Context
          $a                   ; Query
          $a))                 ; Query result

;; Base case.  Terminates no matter what, either by pruning the branch
;; or by querying the kb.  Thanks to non-determinism, terminating the
;; branch does not terminate alternative branches.
(= (bc $absupd $argupd $tmnpred $ctx (: $prf $ccln))
   ;; Base case termination conditional
   (if ($tmnpred (: $prf $ccln) $ctx)
       ;; Terminate by pruning
       (empty)
       ;; Continue by querying the kb
       (match &kb (: $prf $ccln)
              ;; Match termination conditional
              (if ($tmnpred (: $prf $ccln) $ctx)
                  ;; Terminate by pruning
                  (empty)
                  ;; Continue by returning the queried result
                  (: $prf $ccln)))))

;; Recursive step.  Recursion only happens if the termination
;; condition is false.  Otherwise, the branch is pruned.
(= (bc $absupd $argupd $tmnpred $ctx (: ($prfabs $prfarg) $ccln))
   ;; Recursive step termination conditional
   (if ($tmnpred (: ($prfabs $prfarg) $ccln) $ctx)
       ;; Terminate by pruning
       (empty)
       ;; Continue by recursing
       (let* (;; Recurse on proof abstraction
              ((: $prfabs (-> $prms $ccln))
               (bc ;; Context updaters and termination predicate
                   $absupd $argupd $tmnpred
                   ;; Updated context for proof abstraction
                   ($absupd (: ($prfabs $prfarg) $ccln) $ctx)
                   ;; Proof abstraction query
                   (: $prfabs (-> $prms $ccln))))
              ;; Recurse on proof argument
              ((: $prfarg $prms)
               (bc ;; Context updaters and termination predicate
                   $absupd $argupd $tmnpred
                   ;; Updated context for proof argument
                   ($argupd (: ($prfabs $prfarg) $ccln) $ctx)
                   ;; Proof argument query
                   (: $prfarg $prms))))
         ;; Output result
         (: ($prfabs $prfarg) $ccln))))

;;;;;;;;;;;;;;;;;;;;;;
;; Common functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Return True iff $lhs unifies with $rhs
(: ≐ (-> $a $a Bool))
(= (≐ $lhs $rhs) (case $rhs (($lhs True) ($_ False))))

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

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;
;; Context is depth ;;
;;;;;;;;;;;;;;;;;;;;;;

;; The context is the maximum depth, thus reproduces the experiments
;; done so far.  Note that a depth of 1, not 0, allows to query the
;; KB.

;; Define context updater, same for both for proof abstraction and
;; argument.  Decrement the depth.
(: dec (-> Nat Nat))
(= (dec Z) Z)
(= (dec (S $k)) $k)
(: depth-updater (-> $a Nat Nat))
(= (depth-updater $query $depth) (dec $depth))

;; Define termination predicate, called inside a conditional wrapping
;; the bc.  Terminates at 0.
(: is-zero (-> Nat Bool))
(= (is-zero Z) True)
(= (is-zero (S $k)) False)
(: depth-terminator (-> $a Nat Bool))
(= (depth-terminator $query $depth) (is-zero $depth))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context is depth and target theorem ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From the above experiment we make the following observations:
;;
;; 1. Refl is useful iff the theorem is (≼ x x).
;;
;; 2. For theorems (≼ Jan x), JPA is enough.
;;
;; 3. Otherwise, Trans is enough.

;; Context type and ctor
(: TD Type)
(: MkTD (-> $a                     ; Target theorem
            Nat                    ; Maximum depth
            TD))

;; Define context updater, same for both proof abstraction and proof
;; argument.  Decrement the depth, leave the target theorem unchanged.
(: td-updater (-> $a TD TD))
(= (td-updater $query (MkTD $trg-thm $depth))
   (MkTD $trg-thm (dec $depth)))

;; Define termination predicate.  Terminate at depth 0, and respect
;; the 3 observations above.
(: td-terminator (-> $a TD Bool))
(= (td-terminator (: $prf $thm) (MkTD $trg-thm $depth))
   (or ;; Terminate at depth 0
       (is-zero $depth)
       (if (≐ $trg-thm (≼ $x $x))
           ;; [1st observation] If the target theorem is (≼ x x) and
           ;; current proof is Trans or JPA then terminate.
           (or (== $prf Trans) (== $prf JPA))
           (if (≐ $trg-thm (≼ Jan $y))
               ;; [2nd observation] If the target theorem is (≼ Jan x)
               ;; and current proof is Trans or Refl then terminate.
               (or (== $prf Trans) (== $prf Refl))
               ;; [3th observation] Otherwise, Trans is enough.
               (or (== $prf JPA) (== $prf Refl))))))


;; Prove that Feb precedes Jul
!(assertEqualToResult
  (bc td-updater td-updater td-terminator
      (MkTD (≼ Feb Jul) (fromNumber 6))
      (: $prf (≼ Feb Jul)))
  ((: ((Trans ((Trans FM) ((Trans MA) AM))) ((Trans MJ) JJ)) (≼ Feb Jul))
   (: ((Trans ((Trans FM) MA)) ((Trans ((Trans AM) MJ)) JJ)) (≼ Feb Jul))
   (: ((Trans ((Trans FM) MA)) ((Trans AM) ((Trans MJ) JJ))) (≼ Feb Jul))
   (: ((Trans FM) ((Trans ((Trans MA) AM)) ((Trans MJ) JJ))) (≼ Feb Jul))
   (: ((Trans FM) ((Trans MA) ((Trans AM) ((Trans MJ) JJ)))) (≼ Feb Jul))))

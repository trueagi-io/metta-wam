;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inference control experiments.  Like inf-ctl-month-xp.metta but the
;; termination predicate is evaluated by using another instance of the
;; backward chainer as a controller.
;;
;; For now the idea is to have a Terminate dependent type
;;
;; (: Terminate (-> CONTEXT Type))
;;
;; In order to terminate (i.e. prune) a branch, the controller
;; attempts to find a proof of (Terminate CURRENT_CONTEXT), that is it
;; should terminate in the current context.  If it finds such a proof,
;; then it terminates, otherwise it continues.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Knowledge and Rule Base ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Knowledge and rule base
!(bind! &kb (new-space))

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

;; Backward Chainer.  The arguments of the backward chainer are:
;;
;; * Context abstraction updater.  Given the current query and
;;   context, update the context before recursively calling the
;;   backward chainer on the proof abstraction.
;;
;; * Context argument updater.  Given the current query and context,
;;   update the context before recursively calling the backward
;;   chainer on the proof argument.
;;
;; * Termination predicate.  Given the current query and context,
;;   provide the condition predicate of a conditional wrapping the
;;   base case and recursive step functions, as well its match
;;   results.  Terminating amounts to pruning the reduction (as in
;;   evaluation) branches.  For now there is only one termination
;;   predicate for all types of intersections, in the future we may
;;   want to split it.
;;
;; * Context: a context to be updated and passed to the recursive
;;   calls of the backward chainer.
;;
;; * Query: a metta term of the form (: <PROOF> <THEOREM>) where
;;   <PROOF> and <THEOREM> may contain free variables that may be
;;   filled by the backward chainer.
;;
;; The choice of the arguments of the context updaters and the
;; termination predicate is justified as follows.  Context updaters
;; take first the query, which can be viewed as an updater modulator,
;; which then takes the actual context to return the updated one.  An
;; alternative would have been to construct a contextualized query,
;; and update that contextualized query instead.  However, that
;; removes the guaranty that the user-programmed inference control
;; does not interfere with the correctness of the chainer.
(: bc (-> $kb                  ; Knowledge/rule base
          (-> $a $ct $ct)      ; Context abstraction updater
          (-> $a $ct $ct)      ; Context argument updater
          (-> $a $ct Bool)     ; Termination predicate
          $ct                  ; Context
          $a                   ; Query
          $a))                 ; Query result

;; Base case.  Terminates no matter what, either by pruning the branch
;; or by querying the kb.  Thanks to non-determinism, terminating the
;; branch does not terminate alternative branches.
(= (bc $kb $absupd $argupd $tmnpred $ctx (: $prf $ccln))
   ;; Base case termination conditional
   (if ($tmnpred (: $prf $ccln) $ctx)
       ;; Terminate by pruning
       (empty)
       ;; Continue by querying the kb
       (match $kb (: $prf $ccln)
              ;; Match termination conditional
              (if ($tmnpred (: $prf $ccln) $ctx)
                  ;; Terminate by pruning
                  (empty)
                  ;; Continue by returning the queried result
                  (: $prf $ccln)))))

;; Recursive step.  Recursion only happens if the termination
;; condition is false.  Otherwise, the branch is pruned.
(= (bc $kb $absupd $argupd $tmnpred $ctx (: ($prfabs $prfarg) $ccln))
   ;; Recursive step termination conditional
   (if ($tmnpred (: ($prfabs $prfarg) $ccln) $ctx)
       ;; Terminate by pruning
       (empty)
       ;; Continue by recursing
       (let* (;; Recurse on proof abstraction
              ((: $prfabs (-> $prms $ccln))
               (bc ;; Knowledge/rule base
                   $kb
                   ;; Context updaters and termination predicate
                   $absupd $argupd $tmnpred
                   ;; Updated context for proof abstraction
                   ($absupd (: ($prfabs $prfarg) $ccln) $ctx)
                   ;; Proof abstraction query
                   (: $prfabs (-> $prms $ccln))))
              ;; Recurse on proof argument
              ((: $prfarg $prms)
               (bc ;; Knowledge/rule base
                   $kb
                   ;; Context updaters and termination predicate
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

;; Return True iff $x is a variable
(: is-variable (-> Atom Bool))
(= (is-variable $x) (== (get-metatype $x) Variable))

;; Return True iff $x is closed
(: is-closed (-> Atom Bool))
(= (is-closed $x) (if (is-variable $x)
                      False
                      (if (is-expression $x)
                          (or (let $head (car-atom $y) (is-closed $head))
                              (let $tail (cdr-atom $y) (is-closed $tail)))
                          True)))

;; Return the proof of a query iff it is closed.  Otherwise prune the branch.
(: get-closed-proof (-> $a $a))
(= (get-closed-proof $query)
   (case $query (((: $prf $thm) (if (is-closed $prf) $prf (empty))))))

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
(= (depth-terminator $query $depth)
   (is-zero $depth))

;; Knowledge and rule base of the control space to evaluate, via
;; reasoning, the termination predicate, only using depth for now.
!(bind! &ctl-depth-kb (new-space))

;; ;; Define Terminate type constructor (actually not useful for now)
;; !(add-atom &ctl-depth-kb (: Terminate (-> Nat Type)))

;; There is only one way to terminate, the depth must be 0.
!(add-atom &ctl-depth-kb (: TerminateZ (Terminate Z)))

;; Define backward chainer based termination predicate evaluator.
;; Only terminates if it finds a proof that it should terminate.
(: depth-bc-terminator (-> $a Nat Bool))
(= (depth-bc-terminator $query $depth)
   (let $results (collapse
                  (get-closed-proof
                   (bc &ctl-depth-kb                  ; Knowledge base for control
                       depth-updater depth-updater    ; Depth updaters for control
                       depth-terminator               ; Depth terminators for control
                       (fromNumber 1)                 ; Depth for control
                       (: $prf (Terminate $depth))))) ; Termination query
     (not (== () $results))))

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

;; Prove that Z terminates
!(assertEqual
  (get-closed-proof
   (bc &ctl-depth-kb
       depth-updater depth-updater depth-terminator
       (fromNumber 1)
       (: $prf (Terminate Z))))
  TerminateZ)

;; Do not prove that (S Z) terminates
!(assertEqualToResult
  (get-closed-proof
   (bc &ctl-depth-kb
       depth-updater depth-updater depth-terminator
       (fromNumber 1)
       (: $prf (Terminate (fromNumber 1)))))
  ())

;; Prove that Jan non-strictly precedes Jan
!(assertEqualToResult
  (bc &kb
      depth-updater depth-updater depth-bc-terminator
      (fromNumber 2)
      (: $prf (≼ Jan Jan)))
  ((: JPA (≼ Jan Jan))
   (: Refl (≼ Jan Jan))))

;; Prove that Feb non-strictly precedes Feb
!(assertEqual
  (bc &kb
      depth-updater depth-updater depth-bc-terminator
      (fromNumber 2)
      (: $prf (≼ Feb Feb)))
  (: Refl (≼ Feb Feb)))

;; Prove that Jan precedes Feb
!(assertEqualToResult
  (bc &kb
      depth-updater depth-updater depth-bc-terminator
      (fromNumber 2)
      (: $prf (≼ Jan Feb)))
  ((: JF (≼ Jan Feb))
   (: JPA (≼ Jan Feb))))

;; Prove that Jan precedes Mar
!(assertEqualToResult
  (bc &kb
      depth-updater depth-updater depth-bc-terminator
      (fromNumber 3)
      (: $prf (≼ Jan Mar)))
  ((: ((Trans Refl) JPA) (≼ Jan Mar))
   (: ((Trans JPA) FM) (≼ Jan Mar))
   (: ((Trans JPA) Refl) (≼ Jan Mar))
   (: ((Trans JPA) JPA) (≼ Jan Mar))
   (: ((Trans JF) FM) (≼ Jan Mar))
   (: JPA (≼ Jan Mar))))

;; Prove that Feb precedes May
!(assertEqual
  (bc &kb
      depth-updater depth-updater depth-terminator
      (fromNumber 4)
      (: $prf (≼ Feb May)))
  (: ((Trans FM) ((Trans MA) AM)) (≼ Feb May)))

;; Prove that Feb precedes Jun

!(assertEqual
  !(bc &kb
      depth-updater depth-updater depth-terminator
      (fromNumber 5)
      (: $prf (≼ Feb Jun)))
 (: ((Trans ((Trans FM) MA)) ((Trans AM) MJ)) (≼ Feb Jun))

!(assertEqual
  (number-of (bc &kb
      depth-updater depth-updater depth-terminator
      (fromNumber 5)
      (: $prf (≼ Feb Jun)))) 
    1)
    

;; Prove that Feb precedes Jul (to long to list)
;; !(assertEqual
  !(number-of
     (bc &kb
      depth-updater depth-updater depth-terminator
      (fromNumber 6)
      (: $prf (≼ Feb Jul))))
  ;; (: ((Trans FM) ((Trans MA) ((Trans AM) ((Trans MJ) JJ)))) (≼ Feb Jul)))



;; Disabled because it takes 3h.
;;
;; ;; Prove that Feb precedes Aug
;; !(assertEqual
  !(number-of
     (bc &kb
       depth-updater depth-updater depth-terminator
       (fromNumber 7)
       (: $prf (≼ Feb Aug))))
;; (: ((Trans FM) ((Trans MA) ((Trans AM) ((Trans MJ) JJ)))) (≼ Feb Jul)))


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

;; Knowledge and rule base of the control space to evaluate, via
;; reasoning, the termination predicate, using depth, target theorem
;; and current query.
!(bind! &ctl-td-kb (new-space))

;; ;; Define Terminate type constructor (actually not useful for now)
;; !(add-atom &ctl-td-kb (: Terminate (-> $a ; Query
;;                                        TD ; Context
;;                                        Type)))

;; If the depth is zero then it terminates
!(add-atom &ctl-td-kb (: TerminateZ (Terminate $query (MkTD $trg-thm Z))))

;; 1st observation: if the target theorem is (≼ x x) and current proof
;; is Trans or JPA then terminate.
;;
;; TODO: the problem with the following rule, is that a control query
;; such as
;;
;; (: $tmn-prf (Terminate (: $prf $thm) (MkTD (≼ $x $x) $depth)))
;;
;; would unify with
;;
;; (: TerminateTrans (Terminate (: Trans $thm) (MkTD (≼ $x $x) $depth)))
;;
;; which would result in prematurely terminating the backward chainer.
;; It's unclear what is the proper way to address that. I can think of
;; three solutions:
;;
;; 1. Have the Terminate type takes quoted query and context, instead
;;    of open terms.  The problem is that there is no quotation
;;    mechanism in MeTTa capabable of preventing unification and
;;    substitution.
;;
;; 2. Use DeBruinj indices instead of variables to build a Terminate
;;    control query involving only closed terms.  This could be an
;;    alternative way to providing quoted terms.
;;
;; 3. Replace Terminate by Continue.  It could be that this way the
;;    super-type effect that unification provides (which leads to
;;    over-terminating in the case of a Terminate type), would lead to
;;    over-continuing in the case of a Continue type, which could be a
;;    feature rather than a bug.
!(add-atom &ctl-td-kb (: TerminateTrans (Terminate (: Trans $thm)
                                                   (MkTD (≼ $x $x) $depth))))

;; ;; Define termination predicate.  Terminate at depth 0, and respect
;; ;; the 3 observations above.
;; (: td-terminator (-> $a TD Bool))
;; (= (td-terminator (: $prf $thm) (MkTD $trg-thm $depth))
;;    (or ;; Terminate at depth 0
;;        (is-zero $depth)
;;        (if (≐ $trg-thm (≼ $x $x))
;;            ;; [1st observation] If the target theorem is (≼ x x) and
;;            ;; current proof is Trans or JPA then terminate.
;;            (or (== $prf Trans) (== $prf JPA))
;;            (if (≐ $trg-thm (≼ Jan $y))
;;                ;; [2nd observation] If the target theorem is (≼ Jan x)
;;                ;; and current proof is Trans or Refl then terminate.
;;                (or (== $prf Trans) (== $prf Refl))
;;                ;; [3th observation] Otherwise, Trans is enough.
;;                (or (== $prf JPA) (== $prf Refl))))))

;; ;; Prove that Jan non-strictly precedes Jan
;; !(assertEqual
;;   (bc &kb
;;       td-updater td-updater td-terminator
;;       (MkTD (≼ Jan Jan) (fromNumber 2))
;;       (: $prf (≼ Jan Jan)))
;;   (: Refl (≼ Jan Jan)))

;; ;; Prove that Feb non-strictly precedes Feb
;; !(assertEqual
;;   (bc &kb
;;       td-updater td-updater td-terminator
;;       (MkTD (≼ Feb Feb) (fromNumber 2))
;;       (: $prf (≼ Feb Feb)))
;;   (: Refl (≼ Feb Feb)))

;; ;; Prove that Jan precedes Feb
;; !(assertEqualToResult
;;   (bc &kb
;;       td-updater td-updater td-terminator
;;       (MkTD (≼ Jan Feb) (fromNumber 2))
;;       (: $prf (≼ Jan Feb)))
;;   ((: JF (≼ Jan Feb))
;;    (: JPA (≼ Jan Feb))))

;; ;; Prove that Jan precedes Mar
;; !(assertEqual
;;   (bc &kb
;;       td-updater td-updater td-terminator
;;       (MkTD (≼ Jan Mar) (fromNumber 4))
;;       (: $prf (≼ Jan Mar)))
;;   (: JPA (≼ Jan Mar)))

;; ;; Prove that Feb precedes May
;; !(assertEqual
;;   (bc &kb
;;       td-updater td-updater td-terminator
;;       (MkTD (≼ Feb May) (fromNumber 4))
;;       (: $prf (≼ Feb May)))
;;   (: ((Trans FM) ((Trans MA) AM)) (≼ Feb May)))

;; ;; Prove that Feb precedes Jun
;; !(assertEqualToResult
;;   (bc &kb
;;       td-updater td-updater td-terminator
;;       (MkTD (≼ Feb Jun) (fromNumber 5))
;;       (: $prf (≼ Feb Jun)))
;;   ((: ((Trans ((Trans FM) MA)) ((Trans AM) MJ)) (≼ Feb Jun))
;;    (: ((Trans FM) ((Trans MA) ((Trans AM) MJ))) (≼ Feb Jun))))

;; ;; Prove that Feb precedes Jul
;; !(assertEqualToResult
;;   (bc &kb
;;       td-updater td-updater td-terminator
;;       (MkTD (≼ Feb Jul) (fromNumber 6))
;;       (: $prf (≼ Feb Jul)))
;;   ((: ((Trans ((Trans FM) ((Trans MA) AM))) ((Trans MJ) JJ)) (≼ Feb Jul))
;;    (: ((Trans ((Trans FM) MA)) ((Trans ((Trans AM) MJ)) JJ)) (≼ Feb Jul))
;;    (: ((Trans ((Trans FM) MA)) ((Trans AM) ((Trans MJ) JJ))) (≼ Feb Jul))
;;    (: ((Trans FM) ((Trans ((Trans MA) AM)) ((Trans MJ) JJ))) (≼ Feb Jul))
;;    (: ((Trans FM) ((Trans MA) ((Trans AM) ((Trans MJ) JJ)))) (≼ Feb Jul))))


;;; To do all of the above in Rust it took 45 seconds.. In MeTTaLog 28.596 seconds
;;; Disabled because it takes 1h40 in MeTTa Rust
;;; Enabled because it only takes 23 seconds in MeTTaLog
;;; Prove that Feb precedes Aug

!(assertEqual
        (bc &kb
            td-updater td-updater td-terminator
            (MkTD (≼ Feb Aug) (fromNumber 7))
            (: $prf (≼ Feb Aug)))
         ((: ((Trans ((Trans ((Trans FM) MA)) ((Trans AM) ((Trans MJ) JJ)))) JA) (≼ Feb Aug))
          (: ((Trans ((Trans ((Trans FM) MA)) ((Trans AM) MJ))) ((Trans JJ) JA)) (≼ Feb Aug))
          (: ((Trans ((Trans ((Trans FM) MA)) AM)) ((Trans ((Trans MJ) JJ)) JA)) (≼ Feb Aug))
          (: ((Trans ((Trans ((Trans FM) MA)) AM)) ((Trans MJ) ((Trans JJ) JA))) (≼ Feb Aug))
          (: ((Trans ((Trans FM) ((Trans MA) ((Trans AM) MJ)))) ((Trans JJ) JA)) (≼ Feb Aug))
          (: ((Trans ((Trans FM) ((Trans MA) AM))) ((Trans ((Trans MJ) JJ)) JA)) (≼ Feb Aug))
          (: ((Trans ((Trans FM) ((Trans MA) AM))) ((Trans MJ) ((Trans JJ) JA))) (≼ Feb Aug))
          (: ((Trans ((Trans FM) MA)) ((Trans ((Trans AM) ((Trans MJ) JJ))) JA)) (≼ Feb Aug))
          (: ((Trans ((Trans FM) MA)) ((Trans ((Trans AM) MJ)) ((Trans JJ) JA))) (≼ Feb Aug))
          (: ((Trans ((Trans FM) MA)) ((Trans AM) ((Trans ((Trans MJ) JJ)) JA))) (≼ Feb Aug))
          (: ((Trans ((Trans FM) MA)) ((Trans AM) ((Trans MJ) ((Trans JJ) JA)))) (≼ Feb Aug))
          (: ((Trans FM) ((Trans ((Trans MA) ((Trans AM) MJ))) ((Trans JJ) JA))) (≼ Feb Aug))
          (: ((Trans FM) ((Trans ((Trans MA) AM)) ((Trans ((Trans MJ) JJ)) JA))) (≼ Feb Aug))
          (: ((Trans FM) ((Trans ((Trans MA) AM)) ((Trans MJ) ((Trans JJ) JA)))) (≼ Feb Aug))
          (: ((Trans FM) ((Trans MA) ((Trans ((Trans AM) MJ)) ((Trans JJ) JA)))) (≼ Feb Aug))
          (: ((Trans FM) ((Trans MA) ((Trans AM) ((Trans MJ) ((Trans JJ) JA))))) (≼ Feb Aug))))


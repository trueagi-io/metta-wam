;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inference control experiments.  Like inf-ctl-month-bc-xp.metta but
;; the termination predicate is replaced by a continuation predicate.
;;
;; So the idea is to have a Continue dependent type
;;
;; (: Continue (-> QUERY CONTEXT Type))
;;
;; At each intersection, the backward chainer needs to prove that it
;; is worth continuing in order to continue, i.e. not prune the
;; branch.
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

;; Control type definition.  Takes a query type and a context type.
;; Holds the functions involved in inference control.
(: Control (-> $b $c Type))

;; Control data type constructor.  Take various functions as inference
;; control guides, specifically to update an inference context and
;; decide whether to continue an inference.
;;
;; The functions are:
;;
;; * Abstraction context updater.  Given the current query and
;;   context, update the context before recursively calling the
;;   backward chainer on the proof abstraction.
;;
;; * Argument context updater.  Given the current query and context,
;;   update the context before recursively calling the backward
;;   chainer on the proof argument.
;;
;; * Base case continuation predicate.  Given the current query and
;;   context, provide the predicate of a conditional wrapping the base
;;   case.  Continuing amounts to not pruning the current reduction
;;   (as in evaluation) branches.
;;
;; * Recursive step continuation predicate.  Given the current query
;;   and context, provide the predicate of a conditional wrapping the
;;   recursive step functions, as well its match results.  Continuing
;;   amounts to not pruning the current reduction (as in evaluation)
;;   branches.
;;
;; * Match continuation predicate.  Given the current query and
;;   context, provide the predicate of a conditional wrapping the
;;   match results.  Continuing amounts to not pruning the current
;;   reduction (as in evaluation) branches.
(: MkControl (-> (-> $a $c $c)      ; Abstraction context updater
                 (-> $a $c $c)      ; Argument context updater
                 (-> $a $c Bool)    ; Base case continuation predicate
                 (-> $a $c Bool)    ; Recursive step continuation predicate
                 (-> $a $c Bool)    ; Match continuation predicate
                 (Control $a $c)))  ; Control type

;; Backward Chainer.  The arguments of the backward chainer are:
;;
;; * Knowledge base: pointer to a space containing axioms and rules in
;;   the format (: <NAME> <RULE>).
;;
;; * Control: a control structure containing the functions used for
;;   inference control.
;;
;; * Context: a context to be updated and passed to the recursive
;;   calls of the backward chainer.
;;
;; * Query: a metta term of the form (: <PROOF> <THEOREM>) where
;;   <PROOF> and <THEOREM> may contain free variables that may be
;;   filled by the backward chainer.
;;
;; The choice of the arguments of the context updaters and the
;; continuation predicate is justified as follows.  Context updaters
;; take first the query, which can be viewed as an updater modulator,
;; which then takes the actual context to return the updated one.  An
;; alternative would have been to construct a contextualized query,
;; and update that contextualized query instead.  However, that
;; removes the guaranty that the user-programmed inference control
;; does not interfere with the correctness of the chainer.
(: bc (-> $a               ; Knowledge base
          (Control $b $c)  ; Control structure
          $c               ; Context
          $b               ; Query
          $b))             ; Query result

;; Base case.  The call itself do not result into any recursion, no
;; matter what.  However, it may either terminate without pruning the
;; branch (thus leaving the interpreter keep that branch) or terminate
;; by pruning the branch (thus making the interpreter to drop that
;; branch).
(= (bc $kb                                               ; Knowledge base
       (MkControl $absupd $argupd $bcont $rcont $mcont)  ; Control
       $ctx                                              ; Context
       (: $prf $ccln))                                   ; Query
   ;; Base case continuation conditional
   (if ($bcont (: $prf $ccln) $ctx)
       ;; Continue by querying the kb
       (match $kb (: $prf $ccln)
              ;; Match continuation conditional
              (if ($mcont (: $prf $ccln) $ctx)
                  ;; Continue by returning the queried result
                  (: $prf $ccln)
                  ;; Terminate by pruning
                  (empty)))
       ;; Terminate by pruning
       (empty)))

;; Recursive step.  Recursion only happens if the continuation
;; condition is true.  Otherwise, the branch is pruned.
(= (bc $kb                                              ; Knowledge base
       (MkControl $absupd $argupd $bcont $rcont $mcont) ; Control
       $ctx                                             ; Context
       (: ($prfabs $prfarg) $ccln))                     ; Query
   ;; Recursive step continuation conditional
   (if ($rcont (: ($prfabs $prfarg) $ccln) $ctx)
       ;; Continue by recursing
       (let* (;; Recurse on proof abstraction
              ((: $prfabs (-> $prms $ccln))
               (bc $kb                                         ; Knowledge base
                   (MkControl $absupd $argupd $bcont $rcont $mcont)   ; Control
                   ($absupd (: ($prfabs $prfarg) $ccln) $ctx) ; Updated context
                   (: $prfabs (-> $prms $ccln))))     ; Proof abstraction query
              ;; Recurse on proof argument
              ((: $prfarg $prms)
               (bc $kb                                         ; Knowledge base
                   (MkControl $absupd $argupd $bcont $rcont $mcont)   ; Control
                   ($argupd (: ($prfabs $prfarg) $ccln) $ctx) ; Updated context
                   (: $prfarg $prms))))                  ; Proof argument query
         ;; Output result
         (: ($prfabs $prfarg) $ccln))
       ;; Terminate by pruning
       (empty)))

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

;; Return True iff $x is closed
(: is-closed (-> Atom Bool))
(= (is-closed $x)
   (case ($x (get-metatype $x))
     ((($_ Variable) False)
      ((($abs $arg) Expression) (and (is-closed $abs) (is-closed $arg)))
      ($_ True))))

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

;; Define recursive continuation predicate.  Only continue if depth is
;; greater than 0.
(: is-zero (-> Nat Bool))
(= (is-zero Z) True)
(= (is-zero (S $k)) False)
(: gtz-continuor (-> $a Nat Bool))
(= (gtz-continuor $query $depth)
   (not (is-zero $depth)))

;; Define base case and match continuation predicate.  Always
;; continue.
(: top-continuor (-> $a Nat Bool))
(= (top-continuor $query $depth) True)

;; Knowledge and rule base of the control space to evaluate, via
;; reasoning, the continuation predicate, only using depth for now.
!(bind! &md-ctl-kb (new-space))

;; To continue it is enough to prove that the depth is not 0.  MD
;; stands for Maximum Depth.  CS stands from Continue Successor.
!(add-atom &md-ctl-kb (: CS (MDContinue $query (S $k))))

;; Define maximum depth control structure.
!(bind! &md-ctl (MkControl depth-updater    ; Abstraction context updater
                           depth-updater    ; Argument context updater
                           top-continuor    ; Base case continuor
                           gtz-continuor    ; Recursive step continuor
                           top-continuor))  ; Match continuor

;; Define recursive step continuation predicate based on finding
;; continuation proof.
(: gtz-bc-continuor (-> $a Nat Bool))
(= (gtz-bc-continuor $query $depth)
   (let $results (collapse
                  (get-closed-proof
                   (bc &md-ctl-kb     ; Knowledge base for control
                       &md-ctl          ; Control structure
                       (fromNumber 0)  ; Context for control
                       (: $prf (MDContinue $query $depth))))) ; Continue query
     (not (== () $results))))

;; Define maximum depth control structure based on backward chaining
;; (i.e. using gtz-bc-continuor).
!(bind! &md-bc-ctl (MkControl depth-updater    ; Abstraction context updater
                              depth-updater    ; Argument context updater
                              top-continuor    ; Base case continuor
                              gtz-bc-continuor ; Recursive step continuor
                              top-continuor))  ; Match continuor

;;;;;;;;;;;
;; Tests ;;
;;;;;;;;;;;

;; Test greater than zero continuor
!(assertEqual
  (gtz-continuor (: $prf $thm) Z)
  False)
!(assertEqual
  (gtz-continuor (: $prf $thm) (S Z))
  True)

;; Test greater than zero backward chaining based continuor
!(assertEqual
  (gtz-bc-continuor (: $prf $thm) Z)
  False)
!(assertEqual
  (gtz-bc-continuor (: $prf $thm) (S Z))
  True)

;; Prove that Jan non-strictly precedes Jan
!(assertEqualToResult
  (bc &kb &md-bc-ctl (fromNumber 0) (: $prf (≼ Jan Jan)))
  ((: JPA (≼ Jan Jan))
   (: Refl (≼ Jan Jan))))

;; Prove that Feb non-strictly precedes Feb
!(assertEqual
  (bc &kb &md-bc-ctl (fromNumber 0) (: $prf (≼ Feb Feb)))
  (: Refl (≼ Feb Feb)))

;; Prove that Jan precedes Feb
!(assertEqualToResult
  (bc &kb &md-bc-ctl (fromNumber 0) (: $prf (≼ Jan Feb)))
  ((: JF (≼ Jan Feb))
   (: JPA (≼ Jan Feb))))

;; Verify a proof that Jan precedes Mar
!(assertEqual
  (bc &kb &md-bc-ctl (fromNumber 2) (: ((Trans JF) FM) (≼ Jan Mar)))
  (: ((Trans JF) FM) (≼ Jan Mar)))

;; Prove that Jan precedes Mar
!(assertEqualToResult
  (bc &kb &md-bc-ctl (fromNumber 2) (: $prf (≼ Jan Mar)))
  ((: JPA (≼ Jan Mar))
   (: ((Trans Refl) JPA) (≼ Jan Mar))
   (: ((Trans JF) FM) (≼ Jan Mar))
   (: ((Trans JPA) Refl) (≼ Jan Mar))
   (: ((Trans JPA) JPA) (≼ Jan Mar))
   (: ((Trans JPA) FM) (≼ Jan Mar))))

;; Prove that Feb precedes May
!(assertEqual
  (bc &kb &md-bc-ctl (fromNumber 3) (: $prf (≼ Feb May)))
  (: ((Trans FM) ((Trans MA) AM)) (≼ Feb May)))

;; Prove that Feb precedes Jun
!(assertEqualToResult
  (bc &kb &md-bc-ctl (fromNumber 4) (: $prf (≼ Feb Jun)))
  ((: ((Trans FM) ((Trans MA) ((Trans AM) MJ))) (≼ Feb Jun))
   (: ((Trans ((Trans FM) MA)) ((Trans AM) MJ)) (≼ Feb Jun))
   (: ((Trans ((Trans FM) MA)) ((Trans AM) ((Trans MJ) Refl))) (≼ Feb Jun))
   (: ((Trans ((Trans FM) MA)) ((Trans AM) ((Trans Refl) MJ))) (≼ Feb Jun))
   (: ((Trans ((Trans FM) MA)) ((Trans Refl) ((Trans AM) MJ))) (≼ Feb Jun))
   (: ((Trans ((Trans FM) Refl)) ((Trans MA) ((Trans AM) MJ))) (≼ Feb Jun))
   (: ((Trans ((Trans Refl) FM)) ((Trans MA) ((Trans AM) MJ))) (≼ Feb Jun))))

;; Prove that Feb precedes Jul (too long to run)
;; !(assertEqual
  !(bc &kb &md-bc-ctl (fromNumber 5) (: $prf (≼ Feb Jul)))
  ;; (: ((Trans FM) ((Trans MA) ((Trans AM) ((Trans MJ) JJ)))) (≼ Feb Jul))

;; Disabled because it takes Xh.

;; ;; Prove that Feb precedes Aug
;; ;; !(assertEqual
;;   !(bc &kb &md-bc-ctl (fromNumber 6) (: $prf (≼ Feb Aug)))
;;   ;; (: ((Trans FM) ((Trans MA) ((Trans AM) ((Trans MJ) JJ)))) (≼ Feb Jul))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context is depth and target theorem ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From the above experiment we make the following observations:
;;
;; 1. Refl is useful iff the theorem is (≼ x x).
;;
;; 2. For theorems (≼ Jan x), JPA is enough.
;;
;; 3. Otherwise, Trans, FM to ND are enough.

;; Context (type and ctor):
;; 1. Target theorem
;; 2. Maximum depth
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
;; and current query.  Used by the match continuor predicate.
!(bind! &td-ctl-kb (new-space))

;; ;; Define TDContinue type constructor (actually not useful for now)
;; !(add-atom &td-ctl-kb (: TDContinue (-> $a ; Query
;;                                         TD ; Context
;;                                         Type)))

;; 1st observation: if
;; - the target theorem is (≼ x x)
;; - the current proof is Refl
;; then continue.
!(add-atom &td-ctl-kb (: RS (TDContinue (: Refl $r) (MkTD (≼ $x $x) $k))))

;; 2nd observation: if
;; - the target theorem is (≼ Jan x)
;; - the current proof is JPA
;; then continue.
!(add-atom &td-ctl-kb (: JS (TDContinue (: JPA $r) (MkTD (≼ Jan $x) $k))))

;; 3rd observation: if
;; - the target theorem is (≼ $x $y) such that $x ≠ Jan and $x ≠ $y
;; - the current proof is Trans or FM to ND
;; then continue.
;;
;; In order to emulate a ≠ function we exhaustively add all
;; combinations of differences as axioms.  TODO: replace by a proper
;; function evaluation.
!(let* (($m1 (superpose (Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov)))
        ($m2 (superpose (Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov))))
   (if (== $m1 $m2)
       (empty)
       (add-atom &td-ctl-kb (: (WitnessOf (≠ $m1 $m2)) (≠ $m1 $m2)))))
!(let $rn (superpose (Trans FM MA AM MJ JJ JA AS SO ON ND))
   (add-atom &td-ctl-kb (: TS (-> (≠ Jan $x)
                                  (-> (≠ $x $y)
                                      (TDContinue (: $rn $rc)
                                                  (MkTD (≼ $x $y) $k)))))))

;; Always continue the base case
(: base-td-continuor (-> $a $ct Bool))
(= (base-td-continuor $query (MkTD $thm $depth)) True)

;; Only continue the recursive step if the depth is greater than 0
(: rec-td-continuor (-> $a $ct Bool))
(= (rec-td-continuor $query (MkTD $thm $depth)) (not (is-zero $depth)))

;; Match contination predicate for the TD (Target theorem, Maximum
;; depth) context.  The maximum depth of the backward chainer call is
;; 2 because axiom TS (the 3rd observation) has 2 premises, therefore
;; requires 2 recursions.
(: match-td-continuor (-> $a $ct Bool))
(= (match-td-continuor $query $ctx)
   (let $results (collapse
                  (get-closed-proof
                   (bc &td-ctl-kb                  ; Knowledge base for control
                       &md-ctl                              ; Control structure
                       (fromNumber 2)                     ; Context for control
                       (: $prf (TDContinue $query $ctx)))))    ; Continue query
     (not (== () $results))))

;; Inference control structure for TD.
!(bind! &td-ctl (MkControl td-updater            ; Abstraction context updater
                           td-updater            ; Argument context updater
                           base-td-continuor     ; Base case continuor
                           rec-td-continuor      ; Recursive step continuor
                           match-td-continuor))  ; Match continuor

;; Prove that Jan non-strictly precedes Jan
!(assertEqualToResult
  (let $thm (≼ Jan Jan)
    (bc &kb &td-ctl (MkTD $thm (fromNumber 0)) (: $prf $thm)))
  ((: Refl (≼ Jan Jan))
   (: JPA (≼ Jan Jan))))

;; Prove that Feb non-strictly precedes Feb
!(assertEqual
  (let $thm (≼ Feb Feb)
    (bc &kb &td-ctl (MkTD $thm (fromNumber 0)) (: $prf $thm)))
  (: Refl (≼ Feb Feb)))

!(assertEqual
  (let $thm (≼ Jan Feb)
    (bc &kb &td-ctl (MkTD $thm (fromNumber 0)) (: $prf $thm)))
  (: JPA (≼ Jan Feb)))

;; Prove that Jan precedes Mar
!(assertEqual
  (let $thm (≼ Jan Mar)
    (bc &kb &td-ctl (MkTD $thm (fromNumber 3)) (: $prf $thm)))
  (: JPA (≼ Jan Mar)))

;; Prove that Feb precedes May
!(assertEqual
  (let $thm (≼ Feb May)
    (bc &kb &td-ctl (MkTD $thm (fromNumber 3)) (: $prf $thm)))
  (: ((Trans FM) ((Trans MA) AM)) (≼ Feb May)))

;; Prove that Feb precedes Jun
!(assertEqualToResult
  (let $thm (≼ Feb Jun)
    (bc &kb &td-ctl (MkTD $thm (fromNumber 4)) (: $prf $thm)))
  ((: ((Trans ((Trans FM) MA)) ((Trans AM) MJ)) (≼ Feb Jun))
   (: ((Trans FM) ((Trans MA) ((Trans AM) MJ))) (≼ Feb Jun))))

;; Prove that Feb precedes Jul
!(assertEqualToResult
  (let $thm (≼ Feb Jul)
    (bc &kb &td-ctl (MkTD $thm (fromNumber 5)) (: $prf $thm)))
  ((: ((Trans ((Trans FM) ((Trans MA) AM))) ((Trans MJ) JJ)) (≼ Feb Jul))
   (: ((Trans ((Trans FM) MA)) ((Trans ((Trans AM) MJ)) JJ)) (≼ Feb Jul))
   (: ((Trans ((Trans FM) MA)) ((Trans AM) ((Trans MJ) JJ))) (≼ Feb Jul))
   (: ((Trans FM) ((Trans ((Trans MA) AM)) ((Trans MJ) JJ))) (≼ Feb Jul))
   (: ((Trans FM) ((Trans MA) ((Trans AM) ((Trans MJ) JJ)))) (≼ Feb Jul))))

;; Prove that Feb precedes Aug (takes 7h)
!(assertEqualToResult
  (let $thm (≼ Feb Aug)
    (bc &kb &td-ctl (MkTD $thm (fromNumber 6)) (: $prf $thm)))
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

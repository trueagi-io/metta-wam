;; Standalone iterative forward chaining experiments.

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forward Bare Entail ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reuse the Bare Entail forward chainer of
;; `../forward-chaining/fc-xp.metta`.  However it only returns results
;; at the exact provided depth, that way we avoid redundancy, which is
;; better in the context of testing the iterative chainer.

;; Foward chainer
(: fc_bare (-> Atom (-> Atom) (-> Atom) Nat Atom))
;; Base case
(= (fc_bare $premise $kb $rb Z) $premise)
;; Recursive steps
(= (fc_bare $premise1 $kb $rb (S $k))
   (let* (((⊢ $premise1 $premise2 $conclusion) ($rb))
          ($premise2 ($kb)))
     (fc_bare $conclusion $kb $rb $k)))
(= (fc_bare $premise2 $kb $rb (S $k))
   (let* (((⊢ $premise1 $premise2 $conclusion) ($rb))
          ($premise1 ($kb)))
     (fc_bare $conclusion $kb $rb $k)))

;; Knowledge base
(: kb_bare (-> Atom))
(= (kb_bare) (superpose ((→ A B)
                         (→ B C)
                         A)))

;; Rule base
(: rb_bare (-> Atom))
(= (rb_bare) (superpose ((⊢             ; Modus Ponens
                          ;; Premises
                          (→ $p $q)
                          $p
                          ;; Conclusion
                          $q)
                         (⊢             ; Deduction
                          ;; Premises
                          (→ $p $q)
                          (→ $q $r)
                          ;; Conclusion
                          (→ $p $r)))))

;; Test forward chainer
! "=== Test Forward Bare Entail ==="
!(assertEqual
  (fc_bare A kb_bare rb_bare (fromNumber 1))
  B)
!(assertEqual
  (fc_bare A kb_bare rb_bare (fromNumber 2))
  C)
!(assertEqualToResult
  (fc_bare (→ A B) kb_bare rb_bare (fromNumber 2))
  (C
   C))

;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Define quoted to prevent wrapped atom from being interpreted
(: quoted (-> Atom Atom))

;; Define nullary lambda
(: lambda0 (-> Atom (-> $t)))
(= ((lambda0 $body)) $body)

;; Define unary lambda
(: lambda1 (-> Variable Atom (-> $a $t)))
(= ((lambda1 $var $body) $val)
    (let (quoted $var) (quoted $val) $body))

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

;; Test lambda0
!(assertEqualToResult
  ((lambda0 "f"))
  ("f"))
!(assertEqualToResult
  ((let $s (superpose ("f" "g")) (lambda0 $s)))
  ("f" "g"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterative Chainer Wrapped Around Forward Bare Entail (no collapse) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call the forward chainer iteratively, adding the conclusions to the
;; knowledge base after each iterations.  That version does not
;; collapse the results of the forward chainer between iterations.
(: ic_bare (-> Atom                          ; Premise
               (-> Atom)                     ; Knowledge base
               (-> Atom)                     ; Rule base
               Nat                           ; Depth
               Nat                           ; Number of iterations
               (-> Atom)))                   ; Updated knowledge base
;; Base case.  For now it terminates at exactly iteration Z to avoid
;; having to deal with too many redundant results.  Ideally it should
;; terminate at any iteration an avoid redundancy by not inserting
;; knowledge already in the knowledge base.
(= (ic_bare $prms $kb $rb $depth Z) $kb)
;; Iterative step
(= (ic_bare $prms $kb $rb $depth (S $k))
   (case (fc_bare $prms $kb $rb $depth) ; call the forward chainer
     (($ccln
       ;; Iterate
       (ic_bare $ccln (superpose ($kb (lambda0 $ccln))) $rb $depth $k))
      (%void% (empty)))))

;; Test Iterative Chainer Wrapped Ardoun Forward Bare Entail (no collapse)
! "=== Test Iterative Chainer Wrapped Ardoun Forward Bare Entail (no collapse) ==="

;; No iteration
!(assertEqual
  (ic_bare A kb_bare rb_bare Z Z)
  kb_bare)

;; No iteration (apply result)
!(assertEqualToResult
  ((ic_bare A kb_bare rb_bare Z Z))
  ((→ A B)
   (→ B C)
   A))

;; One iteration of one step forward chainer (apply result)
!(assertEqualToResult
  (ic_bare A kb_bare rb_bare (fromNumber 1) (fromNumber 1))
  (kb_bare
   (lambda0 B)))

;; One iteration of one step forward chainer (apply result)
!(assertEqualToResult
  ((ic_bare A kb_bare rb_bare (fromNumber 1) (fromNumber 1)))
  ((→ A B)
   (→ B C)
   A
   B))

;; Two iterations of one step forward chainer
!(assertEqualToResult
  (ic_bare A kb_bare rb_bare (fromNumber 1) (fromNumber 2))
  (kb_bare
   (lambda0 C)))

;; Two iterations of one step forward chainer (apply result)
!(assertEqualToResult
  ((ic_bare A kb_bare rb_bare (fromNumber 1) (fromNumber 2)))
  ((→ A B)
   (→ B C)
   A
   C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterative Chainer Wrapped Around Forward Bare Entail (collapse) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call the forward chainer iteratively, adding the conclusions to the
;; knowledge after each iterations.  That version collapses the
;; results of the forward chainer between iterations.
(: ic_bare_collapse (-> Atom                          ; Premise
                        (-> Atom)                     ; Knowledge base
                        (-> Atom)                     ; Rule base
                        Nat                           ; Depth
                        Nat                           ; Number of iterations
                        (-> Atom)))                   ; Updated knowledge base
;; Base case.  For now it terminates at exactly iteration Z to avoid
;; having to deal with too many redundant results.  Ideally it should
;; terminate at any iteration and avoid redundancy by not inserting
;; knowledge already in the knowledge base.
(= (ic_bare_collapse $prms $kb $rb $depth Z) $kb)
;; Iterative step
(= (ic_bare_collapse $prms $kb $rb $depth (S $k))
   (let* (($cres (collapse (fc_bare $prms $kb $rb $depth)))
          ($scres (superpose $cres))
          ($kb_ext (lambda0 $scres))
          ($nu_kb (superpose ($kb $kb_ext))))
     (ic_bare_collapse $scres $nu_kb $rb $depth $k)))

;; Test Iterative Chainer Wrapped Around Forward Bare Entail (no collapse)
! "=== Test Iterative Chainer Wrapped Around Forward Bare Entail (collapse) ==="

;; No iteration
!(assertEqual
  (ic_bare_collapse A kb_bare rb_bare Z Z)
  kb_bare)

;; No iteration (apply result)
!(assertEqualToResult
  ((ic_bare_collapse A kb_bare rb_bare Z Z))
  ((→ A B)
   (→ B C)
   A))

;; One iteration of one step forward chainer
!(assertEqualToResult
  (ic_bare_collapse A kb_bare rb_bare (fromNumber 1) (fromNumber 1))
  (kb_bare
   (lambda0 B)))

;; One iteration of one step forward chainer (apply result)
!(assertEqualToResult
  ((ic_bare_collapse A kb_bare rb_bare (fromNumber 1) (fromNumber 1)))
  ((→ A B)
   (→ B C)
   A
   B))

;; Two iterations of one step forward chainer
;; TODO: can't use assertEqualToResult due to let* cruft
;; !(assertEqualToResult
  !(ic_bare_collapse A kb_bare rb_bare (fromNumber 1) (fromNumber 2))
  ;; (kb_bare
  ;;  (lambda0 C))

;; Two iterations of one step forward chainer (apply result);
;; TODO: can't use assertEqualToResult due to let* cruft
;; !(assertEqualToResult
  !((ic_bare_collapse A kb_bare rb_bare (fromNumber 1) (fromNumber 2)))
  ;; ((→ A B)
  ;;  (→ B C)
  ;;  A
  ;;  C

;;;;;;;;;;;;;;;;;;;;;;;
;; Forward DTL Match ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Forward revertant implementation, using spaces for knowledge and
;; rule bases.

;; Knowledge base
!(bind! &kb (new-space))
!(add-atom &kb (: ab (→ A B)))
!(add-atom &kb (: bc (→ B C)))
!(add-atom &kb (: a A))

;; Rule base
!(bind! &rb (new-space))
!(add-atom &rb (: ModusPonens (->
                               ;; Premises
                               (→ $p $q)
                               $p
                               ;; Conclusion
                               $q)))
!(add-atom &rb (: Deduction (->
                             ;; Premises
                             (→ $p $q)
                             (→ $q $r)
                             ;; Conclusion
                             (→ $p $r))))

;; Forward revertant chainer, based on the DTL version of
;; `../backward-chaining/bc-xp.metta` but matching premises is
;; replaced by calling the backward chainer defined above.
(: fc (-> Atom Nat Atom))
;; Base case
(= (fc (: $prf $prms) $depth) (: $prf $prms))
;; Recursive cases
(= (fc (: $prf1 $prms1) (S $k))
   (match &rb (: $ctor (-> $prms1 $prms2 $ccln))
          (match &kb (: $prf2 $prms2)
                 (fc (: ($ctor $prf1 $prf2) $ccln) $k))))
(= (fc (: $prf2 $prms2) (S $k))
   (match &rb (: $ctor (-> $prms1 $prms2 $ccln))
          (match &kb (: $prf1 $prms1)
                 (fc (: ($ctor $prf1 $prf2) $ccln) $k))))

;; Test forward chainer DTL match
! "=== Test Forward Chainer DTL Match ==="
!(assertEqual
  (fc (: a A) Z)
  (: a A))
!(assertEqualToResult
  (fc (: a A) (fromNumber 1))
  ((: a A)
   (: (ModusPonens ab a) B)))
;; Disable assertEqualToResult due to leftover
!(assertEqualToResult
  (fc (: a A) (fromNumber 2))
  ((: a A)
   (: (ModusPonens ab a) B)
   (: (ModusPonens bc (ModusPonens ab a)) C)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterative Chainer Wrapped Around Forward DTL Match (collapse) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Call the forward revertant chainer iteratively, adding the
;; conclusions to the knowledge space after each iterations.  To avoid
;; irreproducible behavior (due to the side effects of modifying the
;; atomspace), each call of the forward revertant chainer collapses
;; between iterations.
(: ifc (-> $a                            ; Premise
           Nat                           ; Depth
           Nat                           ; Number of iterations
           $a))                          ; Conclusion

;; Base case.  For now it terminates at exactly iteration Z to avoid
;; collecting too many redundant results.
(= (ifc $prms $depth Z) $prms)

;; Iterative step
(= (ifc $prms $depth (S $k))
   (let* (($cres (collapse (fc $prms $depth)))
          ($dummy (add-atoms-nodup &kb $cres)))
     (ifc (superpose $cres) $depth $k)))

;; Test Iterative Chainer Wrapped Around Forward Revertant (collapse)
! "=== Test Iterative Chainer Wrapped Around Forward Revertant (collapse) ==="

;; No iteration
!(assertEqual
  (ifc (: a A) Z Z)
  (: a A))
!(assertEqualToResult
  (get-atoms &kb)
  ((: ab (→ A B))
   (: bc (→ B C))
   (: a A)))

;; One iteration of one step forward chainer
!(assertEqualToResult
  (ifc (: a A) (fromNumber 1) (fromNumber 1))
  ((: a A)
   (: (ModusPonens ab a) B)))
!(assertEqualToResult
  (get-atoms &kb)
  ((: ab (→ A B))
   (: bc (→ B C))
   (: a A)
   (: (ModusPonens ab a) B)))

;; Two iterations of one step forward chainer
!(assertEqualToResult
  (ifc (: a A) (fromNumber 1) (fromNumber 2))
  ((: a A)
   (: (ModusPonens ab a) B)
   (: (ModusPonens ab a) B)
   (: (ModusPonens bc (ModusPonens ab a)) C)))
!(assertEqualToResult
  (get-atoms &kb)
  ((: ab (→ A B))
   (: bc (→ B C))
   (: a A)
   (: (ModusPonens ab a) B)
   (: (ModusPonens bc (ModusPonens ab a)) C)))

;; Standalone forward chaining experiments.

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

;;;;;;;;;;;;
;; Entail ;;
;;;;;;;;;;;;

! "=== Entail ==="

;; Foward chainer
(: fc_entail (-> Atom (-> Atom) (-> Atom) Nat Atom))
;; Base case
(= (fc_entail (⊢ $premise) $kb $rb $depth) (⊢ $premise))
;; Recursive steps
(= (fc_entail (⊢ $premise1) $kb $rb (S $k))
   (let* (((⊢ $premise1 $premise2 $conclusion) ($rb))
          ((⊢ $premise2) ($kb)))
     (fc_entail (⊢ $conclusion) $kb $rb $k)))
(= (fc_entail (⊢ $premise2) $kb $rb (S $k))
   (let* (((⊢ $premise1 $premise2 $conclusion) ($rb))
          ((⊢ $premise1) ($kb)))
     (fc_entail (⊢ $conclusion) $kb $rb $k)))

;; Knowledge base
(: kb_entail (-> Atom))
(= (kb_entail) (superpose ((⊢ (→ A B))
                           (⊢ (→ B C))
                           (⊢ A))))

;; Rule base
(: rb_entail (-> Atom))
(= (rb_entail) (⊢
                ;; Premises
                (→ $p $q)
                $p
                ;; Conclusion
                $q))

;; Test forward chainer
!(assertEqualToResult
  (fc_entail (⊢ A) kb_entail rb_entail (fromNumber 2))
  ((⊢ A) (⊢ B) (⊢ C)))
!(assertEqualToResult
  (fc_entail (⊢ (→ A B)) kb_entail rb_entail (fromNumber 2))
  ((⊢ (→ A B)) (⊢ B) (⊢ C)))
;; !(fc_entail (⊢ (→ $x $y)) kb_entail rb_entail (fromNumber 2))
!(assertEqualToResult
  (let (⊢ (→ $x $y)) (kb_entail)
       (fc_entail (⊢ (→ $x $y)) kb_entail rb_entail (fromNumber 2)))
  ((⊢ (→ A B)) (⊢ B) (⊢ C) (⊢ (→ B C))))

;;;;;;;;;;;;;;;;;
;; Bare Entail ;;
;;;;;;;;;;;;;;;;;

;; Variant of entail where ⊢ is not wrapped around the knowledge base.

! "=== Bare Entail ==="

;; Foward chainer
(: fc_bare (-> Atom (-> Atom) (-> Atom) Nat Atom))
;; Base case
(= (fc_bare $premise $kb $rb $depth) $premise)
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
(= (rb_bare) (⊢
              ;; Premises
              (→ $p $q)
              $p
              ;; Conclusion
              $q))

;; Test forward chainer
!(assertEqualToResult
  (fc_bare A kb_bare rb_bare (fromNumber 2))
  (A B C))
!(assertEqualToResult
  (fc_bare (→ A B) kb_bare rb_bare (fromNumber 2))
  ((→ A B) B C))
!(assertEqualToResult
  (let (→ $x $y) (kb_bare)
       (fc_bare (→ $x $y) kb_bare rb_bare (fromNumber 2)))
  ((→ A B) B C (→ B C)))

;;;;;;;;;;;;;;
;; Equality ;;
;;;;;;;;;;;;;;

! "=== Equality ==="

;; Knowledge base
(: kb_eq (-> Atom))
(= (kb_eq) (superpose ((→ A B)
                       (→ B C)
                       A)))

;; Forward chainer.  The rule based is directly embedded in the
;; forward chainer function.
(: fc_eq (-> Atom (-> Atom) Nat Atom))
;; Base case
(= (fc_eq $premise $kb $depth) $premise)
;; Recursive step (modus ponens)
(= (fc_eq (→ $p $q) $kb (S $k)) (let $p (kb_eq) (fc_eq $q $kb $k)))
(= (fc_eq $p $kb (S $k)) (let (→ $p $q) (kb_eq) (fc_eq $q $kb $k)))

;; Test forward chainer
!(assertEqualToResult
  (fc_eq A kb_eq (fromNumber 2))
  (A B C))
!(assertEqualToResult
  (fc_eq (→ A B) kb_eq (fromNumber 2))
  ((→ A B) B C))
!(assertEqualToResult
  (let (→ $x $y) (kb_eq)
       (fc_eq (→ $x $y) kb_eq (fromNumber 2)))
  ((→ A B) B C (→ B C)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Bare Entail Match ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Like bare entail but let is replaced by match

! "=== Bare Entail Match ==="

;; Knowledge base
!(bind! &kb_bem (new-space))
!(add-atom &kb_bem (→ A B))
!(add-atom &kb_bem (→ B C))
!(add-atom &kb_bem A)

;; Rule base
!(bind! &rb_bem (new-space))
!(add-atom &rb_bem (⊢
                    ;; Premises
                    (→ $p $q)
                    $p
                    ;; Conclusion
                    $q))

;; Forward chainer
(: fc_bem (-> Atom Nat Atom))
;; Base case
(= (fc_bem $premise $depth) $premise)
;; Recursive steps
(= (fc_bem $premise1 (S $k))
   (match &rb_bem (⊢ $premise1 $premise2 $conclusion)
          (match &kb_bem $premise2 (fc_bem $conclusion $k))))
(= (fc_bem $premise2 (S $k))
   (match &rb_bem (⊢ $premise1 $premise2 $conclusion)
          (match &kb_bem $premise1 (fc_bem $conclusion $k))))

;; Test forward chainer
!(assertEqualToResult
  (fc_bem A (fromNumber 2))
  (A B C))
!(assertEqualToResult
  (fc_bem (→ A B) (fromNumber 2))
  ((→ A B) B C))
!(assertEqualToResult
  (match &kb_bem (→ $x $y) (fc_bem (→ $x $y) (fromNumber 2)))
  ((→ B C) (→ A B) B C))

;;;;;;;;;;;;;;;;;;;;
;; Equality Match ;;
;;;;;;;;;;;;;;;;;;;;

;; Like equality but let is replaced by match

! "=== Equality Match ==="

;; Knowledge base
!(bind! &kb_em (new-space))
!(add-atom &kb_em (→ A B))
!(add-atom &kb_em (→ B C))
!(add-atom &kb_em A)

;; Forward chainer.  The rule based is directly embedded in the
;; forward chainer function.
(: fc_em (-> Atom Nat Atom))
;; Base case
(= (fc_em $premise $depth) $premise)
;; Recursive step (modus ponens)
(= (fc_em (→ $p $q) (S $k)) (match &kb_em $p (fc_em $q $k)))
(= (fc_em $p (S $k)) (match &kb_em (→ $p $q) (fc_em $q $k)))

;; Test forward chainer
!(assertEqualToResult
  (fc_em A (fromNumber 2))
  (A B C))
!(assertEqualToResult
  (fc_em (→ A B) (fromNumber 2))
  ((→ A B) B C))
!(assertEqualToResult
  (match &kb_em (→ $x $y) (fc_em (→ $x $y) (fromNumber 2)))
  ((→ A B) B C (→ B C)))

;;;;;;;;;
;; DTL ;;
;;;;;;;;;

! "=== DTL ==="

;; Foward chainer
(: fc_dtl (-> Atom (-> Atom) (-> Atom) Nat Atom))
;; Base case
(= (fc_dtl (: $proof $premise) $kb $rb $depth) (: $proof $premise))
;; Recursive steps
(= (fc_dtl (: $proof1 $premise1) $kb $rb (S $k))
   (let* (((: $ructor (-> $premise1 $premise2 $conclusion)) ($rb))
          ((: $proof2 $premise2) ($kb)))
     (fc_dtl (: ($ructor $proof1 $proof2) $conclusion) $kb $rb $k)))
(= (fc_dtl (: $proof2 $premise2) $kb $rb (S $k))
   (let* (((: $ructor (-> $premise1 $premise2 $conclusion)) ($rb))
          ((: $proof1 $premise1) ($kb)))
     (fc_dtl (: ($ructor $proof1 $proof2) $conclusion) $kb $rb $k)))

;; Knowledge base
(: kb_dtl (-> Atom))
(= (kb_dtl) (superpose ((: ab (→ A B))
                        (: bc (→ B C))
                        (: a A))))

;; Rule base
(: rb_dtl (-> Atom))
(= (rb_dtl) (: ModusPonens (->
                            ;; Premises
                            (→ $p $q)
                            $p
                            ;; Conclusion
                            $q)))

;; Test forward chainer
!(assertEqualToResult
  (fc_dtl (: a A) kb_dtl rb_dtl (fromNumber 2))
  ((: a A)
   (: (ModusPonens ab a) B)
   (: (ModusPonens bc (ModusPonens ab a)) C)))
!(assertEqualToResult
  (fc_dtl (: ab (→ A B)) kb_dtl rb_dtl (fromNumber 2))
  ((: ab (→ A B))
   (: (ModusPonens ab a) B)
   (: (ModusPonens bc (ModusPonens ab a)) C)))
!(assertEqualToResult
  (let (: $prf (→ $x $y)) (kb_dtl)
       (fc_dtl (: $prf (→ $x $y)) kb_dtl rb_dtl (fromNumber 2)))
  ((: ab (→ A B))
   (: (ModusPonens ab a) B)
   (: (ModusPonens bc (ModusPonens ab a)) C)
   (: bc (→ B C))))

;!(import! &self ../hyperon-pln/metta/curried-chaining/curried-chainer.metta)

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

;; Curried Backward Chainer
(: bc (-> $a Nat $a))
;; Base case
(= (bc (: $prf $ccln) $_) 
  (match &self (: $prf $ccln) (: $prf $ccln)))
;; Recursive step
(= (bc (: ($prfabs $prfarg) $ccln) (S $k))
   (let* (((: $prfabs (-> $prms $ccln)) (bc (: $prfabs (-> $prms $ccln)) $k))
          ((: $prfarg $prms) (bc (: $prfarg $prms) $k)))
     (: ($prfabs $prfarg) $ccln)))

;;;;;;;;;;;;;;;;;;;;;
;; Forward chainer ;;
;;;;;;;;;;;;;;;;;;;;;

;; Curried Forward Chainer
(: fc (-> $a Nat $a))
;; Base case.  Beware that the provided source is assumed to be true.
(= (fc (: $proof $premise) $_) (: $proof $premise))
;; Recursive step
(= (fc (: $prfarg $premise) (S $k))
   (let (: $prfabs (-> $premise $ccln)) (bc (: $prfabs (-> $premise $ccln)) $k)
     (fc (: ($prfabs $prfarg) $ccln) $k)))
(= (fc (: $prfabs (-> $prms $ccln)) (S $k))
    (let (: $prfarg $prms) (bc (: $prfarg $prms) $k)
     (fc (: ($prfabs $prfarg) $ccln) $k)))


;Rule 1: Gene G transcribes transcript T and transcript T is translated to protein P => gene G codes for protein P
(: gene-codes-protein 
    (-> (transcribed_to $g $t)
        (-> (translates_to $t $p)
                (codes_for $g $p))))  


;node definitions
(: gene-a (gene a))
(: gene-b (gene b))
(: transcript-t1 (transcript t1))
(: transcript-t2 (transcript t2))
(: transcript-t3 (transcript t3))
(: protein-p1 (protein p1))
(: protein-p2 (protein p2))
(: protein-p3 (protein p3))

;edge definitions
(: transcribed_to-a-t1 (transcribed_to (gene a) (transcript t1)))
(: transcribed_to-a-t2 (transcribed_to (gene a) (transcript t2)))
(: transcribed_to-b-t3 (transcribed_to (gene b) (transcript t3)))
(: translates_to-t1-p1 (translates_to (transcript t1) (protein p1)))
(: translates_to-t2-p2 (translates_to (transcript t2) (protein p2)))
(: translates_to-t3-p3 (translates_to (transcript t3) (protein p3)))

(= (find-atoms $self $pat) (match $self $pat $pat))


!(find-atoms &self
  (: $what 
    (-> (transcribed_to $g $t)
        (-> (translates_to $t $p)
                (codes_for $g $p)))))


 !(find-atoms &self (: $y (transcribed_to (gene a) $t)))


; !(fc (: gene-a (gene a)) (fromNumber 2))


; !(fc (: transcribed_to-a-t1 (transcribed_to (gene a) (transcript t1))) (fromNumber 2))

!(fc (: (gene-codes-protein transcribed_to-a-t1) 
          (-> (translates_to (transcript t1) $p) (codes_for (gene a) $p)))
          (fromNumber 1))
     
;;!(bc (: $prf (codes_for (gene a) (protein p1))) (fromNumber 3))

; [(: transcribed_to-a-t1 (transcribed_to (gene a) (transcript t1))), 
; (: (gene-codes-protein transcribed_to-a-t1) (-> (translates_to (transcript t1) $p#490) (codes_for (gene a) $p#490))),
;  (: ((gene-codes-protein transcribed_to-a-t1) translates_to-t1-p1) (codes_for (gene a) (protein p1)))]



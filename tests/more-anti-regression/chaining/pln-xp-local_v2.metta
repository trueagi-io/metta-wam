; !(bind! &kb2 (new-space))
!(import! &rb rules_v2)
!(bind! &kb (new-space))
; !(bind! &das (new-space))
!(load-ascii &kb sample_kb_v2.metta)
; !(load-ascii &das sample_kb_v2.metta)
;gencode


; !(match (superpose (&kb2 &kb3)) $x (add-atom &kb $x))

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

; (= (less_than $x $y) (if (< $x $y) (: (less_than $x $y) (≪ $x $y)) (empty)))
;; Curried Backward Chainer
(: bc (-> $a                            ; Knowledge base space
          $a                            ; Rule base space
          $b                            ; Query
          Nat                           ; Maximum depth
          $b))                          ; Result
;; Base case
(= (bc $kb $rb (: $prf $ccln) $_k)
     (case (get-metatype $ccln)
          ((Symbol (match $kb (: $prf $ccln) (: $prf $ccln)))
          (Variable (superpose ((match $kb (: $prf $ccln) (: $prf $ccln))
                              (match $rb (: $prf $ccln) (: $prf $ccln)))))
          (Grounded (match $kb (: $prf $ccln) (: $prf $ccln)))
          (Expression (case $ccln
                         (((-> $prm $ccl)
                                   (match $rb (: $prf (-> $prm $ccl)) 
                                        (: $prf (-> $prm $ccl))))
                         ($_ 
                                   (match $kb (: $prf $ccln) 
                                             (: $prf $ccln)))))))))
;; Recursive step
(= (bc $kb $rb (: ($prfabs $prfarg) $ccln) (S $k))
     (let* (((: $prfabs (-> $prms $ccln)) (bc $kb $rb (: $prfabs (-> $prms $ccln)) $k))
               ((: $prfarg $prms) (bc $kb $rb (: $prfarg $prms) $k)))
          (: ($prfabs $prfarg) $ccln)))




(= (get-proof $ccln $k) 
     (let (: $prf $ccln) (bc &kb &rb (: $prf $ccln) $k)
          (: $prf $ccln)))

; !(let (: $prf $ccln) (bc (: $prf (relevant_go (ontology_term GO_0000048) (sequence_variant rs15))) (fromNumber 9))
;      (: $prf $ccln))

; !(bc (: $prf (codes_for (gene L) (protein pL))) (fromNumber 2))

; !(bc (: $prf (relevant_go (ontology_term GO_0000048) (sequence_variant rs15))) (fromNumber 9))

; !(get-proof (translates_to (transcript tL) (protein pL)) (fromNumber 0))
; !(get-proof (member (gene L) (ontology_term GO_0000048) $k) (fromNumber 5))
; !(get-proof (member (gene L) (ontology_term GO_0000482) $k) (fromNumber 5))
; !(get-proof (member (gene L) (ontology_term GO_0004821) $k) (fromNumber 6))


!(get-proof (relevant_go (ontology_term GO_0000048) (sequence_variant rs15)) (fromNumber 9))
; !(get-proof (relevant_go (ontology_term GO_0000482) (sequence_variant rs15)) (fromNumber 9))
; !(bc (: $prf (relevant_go (ontology_term GO_0004821) (sequence_variant rs15))) (fromNumber 9))

; !(match &kb (: $id ($rel (ontology_term GO_0000048) (protein $p)))
;      (: $id ($rel (ontology_term GO_0000048) (protein $p))))
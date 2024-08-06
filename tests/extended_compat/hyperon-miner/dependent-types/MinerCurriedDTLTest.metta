!(register-module! ../../hyperon-miner)
!(import! &self hyperon-miner:utils:MinerUtils)
!(import! &self hyperon-miner:dependent-types:chainer)
;;!(load-ascii &self ../utils/MinerUtils.metta)
;;!(load-ascii &self chainer.metta)

;; sample data
;; commenting, match wouldn't work because
;; import adds stdlib into an imported space
; !(import! &db hyperon-miner:data:sample2)
!(bind! &db (new-space))
!(add-atom &db (Inheritance B A))
!(add-atom &db (Inheritance C A))
!(add-atom &db (Inheritance D E))
!(add-atom &db (Inheritance C E))

;; Parameters
(= (highsurp) 0.8)
(= (ms) 1)

;; Type definition for -> to prevent early reduction
;; of the conclusions before matching the correct premises.
(: -> (-> Atom Atom Type))

;;;;;;;;;;;;;;;;;;;;;;
;; KB and rule base ;;
;;;;;;;;;;;;;;;;;;;;;;

!(import! &kb MinerCurriedDTL)

;; Initialize miner with the given db, kb and parameters
!(init-miner &db &kb (ms) (highsurp))

;;;;;;;;;;
;; Test ;;
;;;;;;;;;;

;; Get all frequent patterns of link type Inheritance
;; given a fact that an abstract pattern has a support CNT:
;; e.g (supportOf (Inheritance (VarIdx Z) (VarIdx (S Z))) CNT)
;; do a,  
; !(let* (($aptrn (abstract-patterns))
;         ($cnt (count $aptrn))
;         ($fact (: (supportwitness $aptrn) (supportOf $aptrn $cnt)))
;         ($depth (fromNumber 8)))
;     (ifc $fact $depth (S Z) &kb))

!(println! (abstract-patterns))
!(println! (count (abstract-patterns)))

 !(let* (($aptrn (abstract-patterns))
         ($cnt (count $aptrn))
         ($fact (: (supportwitness $aptrn) (supportOf $aptrn $cnt)))
         ($depth (fromNumber 4)))
     (println! (ifc $fact $depth (S Z) &kb)))

;; or b,
; !(ifc (: ST (supportOf (Inheritance (VarIdx Z) (VarIdx (S Z))) 4)) (fromNumber 5) (fromNumber 2) &kb)

;; after a or b generates all patterns with minimum support,
;; This would get a conjunctions of patterns with minimum support
; !(ibc (: $prf (supportOf (, $ptrn1 $ptrn2) $cnt)) (fromNumber 4) (S Z) &kb)
; !(bc (: $prf (minsup $ptrn)) (fromNumber 4) &kb)

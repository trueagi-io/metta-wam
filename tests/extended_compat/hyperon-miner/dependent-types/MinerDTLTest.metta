; !(import! &self MinerDTL1.metta)
;;;;;;;;;;
;; Test ;;
;;;;;;;;;;
!(bind! &db (new-space))
!(add-atom &db (Inheritance B A))
!(add-atom &db (Inheritance C A))
!(add-atom &db (Inheritance D E))
!(add-atom &db (Inheritance C E))

;; Parameters
(= (highsurp) 0.8)
(= (ms) 1)

!(init-miner &db (ms) (highsurp))
;; Frequent patterns
!(let (: $proof (minsup $pattern)) (synthesize (: $proof $pattern) kb rb (fromNumber 5))
      (minsup $pattern))

;; Surprising patterns
; !(let (: $proof (surp $pattern true)) (synthesize (: $proof $pattern) kb rb (fromNumber 5))
;       (surp $pattern true))

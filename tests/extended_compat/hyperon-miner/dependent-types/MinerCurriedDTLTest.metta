;; Frequent single patterns
!(let* (($aptrn (Inheritance (VarIdx Z) (VarIdx (S Z))))
        ($depth (fromNumber 8)))
    (fc (: (supportwitness $aptrn) (supportOf $aptrn (countNat $aptrn))) $depth))

;; Frequent patterns, (single and conjunction)
; !(let* (($aptrn (Inheritance (VarIdx Z) (VarIdx (S Z))))
;         ($depth (fromNumber 8)))
;     (ifc (: (supportwitness $aptrn) (supportOf $aptrn (countNat $aptrn))) $depth (S Z)))

; !(let* (($aptrn (Inheritance (VarIdx Z) (VarIdx (S Z))))
;         ($depth (fromNumber 2)))
;     (fc (: (supportwitness $aptrn) (supportOf $aptrn (countNat $aptrn))) $depth))

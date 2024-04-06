;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MeTTa allows to use a separate knowledge base (Space) &kb
; to accumulate inferred knowledge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Bind &kb to a new empty Space
!(bind! &kb (new-space))

;Some knowledge
(= (frog $x)
    (and (croaks $x)
        (eat_flies $x)))

(= (croaks Fritz) True)
(= (eat_flies Fritz) False)
(= (croaks Sam) True)
(= (eat_flies Sam) True)
(= (green $x)
   (frog $x))

; Define conditional
(: ift (-> Bool Atom Atom))
(= (ift True $then) $then)

!(ift (green $x)
    (add-atom &kb (Green $x)))


!(match &kb (Green $x) $x)
!(or True False)
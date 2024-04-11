
(Evaluation (philosopher Plato))
(Evaluation (likes-to-wrestle Plato))

(Implication 
    (And (Evaluation (philosopher $x))
         (Evaluation (likes-to-wrestle $x)))
    (Evaluation (human $x)))

; Another implication rule
(Implication
   (Evaluation (human $x))
   (Evaluation (mortal $x)))

; Deduction case when the desired evaluation is present in
; the knowledge base
(= (deduce (Evaluation ($P $x)))
    (match &self (Evaluation ($P $x)) ($P $x)))

; Deduction case when the desired evaluation is the result
; of an implication, which implies a recursion
(= (deduce (Evaluation ($P $x)))
    (match &self 
        (Implication $a (Evaluation ($P $x)))
        (($P $x) proven by (deduce $a))))

(= (deduce (And $a $b))
   (And (deduce $a) (deduce $b)))

!(deduce (Evaluation (mortal $x)))

;;direct_metta

(= (And True True) True)
(= (frog $x)
    (And (croaks $x)
         (eat_flies $x)))

(= (croaks Fritz) False)
(= (eat_flies Fritz) True)

(= (green $x) (frog $x))

!(frog Fritz)
!(True)


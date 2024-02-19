;; Inference rules to operate on SUMO

;; Binary Conjunction Introduction
(: BinaryConjunctionIntroduction
   (->
    ;; Premises
    $x
    $y
    ;; Conclusion
    (∧ $x $y)))

;; Trinary Conjunction Introduction
(: TrinaryConjunctionIntroduction
   (->
    ;; Premises
    $x
    $y
    $z
    ;; Conclusion
    (∧ $x $y $z)))

;; Quaternary Conjunction Introduction
(: QuaternaryConjunctionIntroduction
   (->
    ;; Premises
    $x
    $y
    $z
    $w
    ;; Conclusion
    (∧ $x $y $z $w)))

;; Modus Ponens
(: ModusPonens
   (->
    ;; Premises
    (⟹ $x $y)
    $x
    ;; Conclusion
    $y))

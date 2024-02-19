; DeductionMatch and ImplicationDirectionIntroductionMatch

!(import! &self ImplicationDirectIntroductionMatch.metta)
!(import! &self DeductionMatch.metta)

; Use IDI rule to infer P->Q based on the following,
(≞ (P 42) (Bl True))
(≞ (Q 42) (Bl False))
(∉ 42 Empty)

; Q->R is already in the atomspace
(≞ (→ Q R) (STV 1 0.5))

; Predicates
(≞ P (STV 1 0.1))
(≞ Q (STV 1 0.5))
(≞ R (STV 1 0.1))

; Matching query to do IDI and deduction rules
!(match &self
    ;; Premises
    (, (≞ $p $ptv)
       (≞ $q $qtv)
       (≞ $r $rtv)
       (≞ ($p $a) $patv)
       (≞ ($q $a) $qatv)
       (∉ $a $ev)
       (≞ (→ $q $r) $qrtv))
    ;; Conclusion
    (deduction (≞ $p $ptv)
               (≞ $q $qtv)
               (≞ $r $rtv)
               (idi_induction (≞ ($p $a) $patv) (≞ ($q $a) $qatv) (idi_axiom) (∉ $a $ev))
               (≞ (→ $q $r) $qrtv)))

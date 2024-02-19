;; Test Implication Direct Introduction Match Rule
!(import! &self ImplicationDirectIntroductionMatch.metta)

(≞ (P 42) (Bl True))
(≞ (Q 42) (Bl False))
(≞ (→ P Q) (ETV Empty (STV 1 0)))
(∉ 42 Empty)

!(idi_induction_match &self)

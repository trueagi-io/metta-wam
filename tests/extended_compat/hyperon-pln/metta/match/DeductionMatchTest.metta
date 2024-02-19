;; Test Deduction match Rule
!(import! &self DeductionMatch.metta)

(≞ P (STV 1 0.1))
(≞ Q (STV 1 0.1))
(≞ R (STV 1 0.1))
(≞ (→ P Q) (STV 1 0.5))
(≞ (→ Q R) (STV 1 0.5))

!(deduction_match &self)


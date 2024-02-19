;; Test Deduction equality Rule
!(import! &self DeductionEqual.metta)

;; Test 1:
!(, (≞ P (STV 1 0.1))
    (≞ Q (STV 1 0.1))
    (≞ R (STV 1 0.1))
    (≞ (→ P Q) (STV 1 0.5))
    (≞ (→ Q R) (STV 1 0.5)))

;; Test 2: Apply deduction rule over the result
;; we get from test 1 as an input.
!(let $PR
    (,  (≞ P (STV 1 0.1))
        (≞ Q (STV 1 0.1))
        (≞ R (STV 1 0.1))
        (≞ (→ P Q) (STV 1 0.5))
        (≞ (→ Q R) (STV 1 0.5)))
    (,
    (≞ P (STV 1 0.1))
    (≞ R (STV 1 0.1))
    (≞ S (STV 1 0.1))
    $PR
    (≞ (→ R S) (STV 1 0.5))))

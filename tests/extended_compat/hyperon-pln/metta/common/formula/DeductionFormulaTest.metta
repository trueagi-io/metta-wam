!(import! &self DeductionFormula.metta)

;; Test
!(assertEqualToResult (max 3 4) (4))
!(assertEqualToResult (min 3 4) (3))
!(assertEqualToResult (and True True) (True))
!(assertEqualToResult (or True False) (True))
!(assertEqualToResult (<= 5 4) (False))

!(clamp 2 0 1)
!(smallest-intersection-probability 0.9 0.6)
!(conditional-probability-consistency 0.9 0.6 0.5)
!(simple-deduction-strength-formula 0.9 0.6 0.5 0.3 0.4)
!(conditional-probability-consistency 0.2 0.3 1)
!(conditional-probability-consistency 0.3 0.4 0.9)
!(deduction-formula (STV 0.2 0.3) (STV 0.3 0.2) (STV 0.4 0.1) (STV 1 0.5) (STV 0.9 0.7))

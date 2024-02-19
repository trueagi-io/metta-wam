;; Import modules
!(import! &self Num.metta)
!(import! &self ../synthesis/Synthesize.metta)

;; Test elementary functions
!(assertEqual (fromNumber 1) (S Z))
!(assertEqual (fromNat (S (S Z))) 2)
!(assertEqual (ceil 2.4) 3)
!(assertEqual (abs -10) 10)
!(assertEqual (approxEq 10 10.0001 1e-3) True)
!(assertEqual (approxEq 10 10.0001 1e-9) False)
!(assertEqual (⩻ Z (S Z)) True)

;; Define knowledge and rule bases to reason about Natural order
(= (kb) (zero-lt-succ-axiom))
(= (rb) (succ-monotonicity-rule))

;; Test reasoning about Natural order
!(assertEqual                           ; 0 < 1
  (synthesize (: $proof (⍃ Z (S Z))) kb rb Z)
  (: ZeroLTSucc (⍃ Z (S Z))))
!(assertEqual                           ; 1 < 3
  (synthesize (: $proof (⍃ (fromNumber 1) (fromNumber 3))) kb rb (S Z))
  (: (SuccMonotonicity ZeroLTSucc) (⍃ (fromNumber 1) (fromNumber 3))))
!(assertEqual                           ; 2 < 7
  (synthesize (: $proof (⍃ (fromNumber 2) (fromNumber 7))) kb rb (fromNumber 5))
  (: (SuccMonotonicity (SuccMonotonicity ZeroLTSucc)) (⍃ (fromNumber 2) (fromNumber 7))))

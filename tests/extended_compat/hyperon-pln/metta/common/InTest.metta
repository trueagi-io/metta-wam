;; Test ∉

;; Import modules
!(import! &self In.metta)
!(import! &self Num.metta)
!(import! &self ../synthesis/Synthesize.metta)

;; Define knowledge and rule bases to reason about ∉.  This requires
;; reasoning about order as well (Natural for now).
(= (kb) (superpose ((zero-lt-succ-axiom)
                    (notin-empty-axiom))))
(= (rb) (superpose ((succ-monotonicity-rule)
                    (lt-notin-recursive-left-rule)
                    (lt-notin-recursive-right-rule))))

;; Test reasoning about ∉
!(assertEqual                           ; 2 ∉ ∅
  (synthesize (: $proof (∉ Z ∅)) kb rb Z)
  (: NotInEmpty (∉ Z ∅)))
;; TODO: re-enabled once unify* can be used
;; !(assertEqual                           ; 0 ∉ {1}
;;   (synthesize (: $proof (∉ Z (insert (S Z) ∅))) (S Z) kb rb)
;;   (: (LTNotInRecursiveLeft NotInEmpty NotInEmpty ZeroLTSucc) (∉ Z (insert (S Z) ∅))))

;; NEXT: fix
;; !(synthesize (: $proof (∉ Z (insert (S Z) ∅))) (S Z) kb rb)
!(synthesize
  (: (LTNotInRecursiveLeft NotInEmpty NotInEmpty ZeroLTSucc)
     (∉ Z (insert (S Z) ∅)))
  kb
  rb
  (S Z))

;; Formula for the inductive Implication Direct Introduction rule.

;; Import modules
!(import! &self ../truthvalue/TruthValue.metta)

;; Alternate implication direct introduction formula assuming Boolean
;; evidence.  Base case, meaning there is only one piece of evidence.
(: direct-introduction-base-formula (-> Bool TruthValue))
(= (direct-introduction-base-formula $qa)
   (STV (bool->number $qa) (count->confidence 1)))

;; Alternate implication direct introduction formula.  Inductive case,
;; meaning the piece of evidence gets aggregated to another truth
;; value.
(: direct-introduction-recursive-formula (-> Bool TruthValue TruthValue))
(= (direct-introduction-recursive-formula False (STV $PQs $PQc))
   (inc-neg-count (STV $PQs $PQc)))
(= (direct-introduction-recursive-formula True (STV $PQs $PQc))
   (inc-pos-count (STV $PQs $PQc)))

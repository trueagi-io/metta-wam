;; MeTTa port of pattern miner, using match.

;; Given a collection of data trees db, a minimum support ms and an
;; initial collection of patterns Candidate (containing at least the identity
;; pattern), the pattern mining algorithm works as follows

;; 1. Select a pattern P from C
;; 2. Extract the valuation set of P over db, called V
;; 3. Determine the shallow abstractions of V, called A
;; 4. Specialize P by composing it with elements in A
;; 5. Add the resulting specializations with enough support in C,
;;    discard the others
;; 6. Repeat till termination

;; Import utils
; !(import! &self ../utils/MinerUtils.metta)

;; Get candidate patterns depth zero
(= (get-candidate Z $linkType $ms)
    (let $sp (specialize $linkType)
      (support $sp $ms)))

;; Get candidate patterns depth n. (maximum is three)
(= (get-candidate (S $k) $linkType $ms)
    (let $comb (, (get-pattern (get-candidate $k $linkType $ms))
                  (get-pattern (get-candidate Z $linkType $ms)))
      (support (flatten $comb) $ms)))

;; Frequent patterns Miner
(= (miner $db $ms $depth)
    (superpose ((init-miner $db $ms $highsurp)
                (let $link (get-links)
                  (get-candidate $depth $link $ms)))))

;; Surprising patterns miner
(= (miner-surprising $db $ms $depth $highsurp)
  (let* (($cptrn (miner $db $ms $depth))
         ($isurp (iSurprisingness $cptrn)))
         (if (> $isurp $highsurp) (surp (get-pattern $cptrn) $isurp) (superpose ()))))

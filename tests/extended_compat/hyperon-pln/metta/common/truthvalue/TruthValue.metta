;; Truth value type definition

;; Import modules
!(import! &self ../Num.metta)

;;;;;;;;;;
;; Type ;;
;;;;;;;;;;

(: TruthValue Type)

;;;;;;;;;;;;;;;;;;
;; Constructors ;;
;;;;;;;;;;;;;;;;;;

;; Boolean TV constructor
;; TODO: alternatively we could have (: ⊤ TruthValue) and (: ⊥ TruthValue)
(: Bl (-> Bool TruthValue))

;; First order probability TV constructor, i.e. mere probability.
(: Pr (-> Number TruthValue))

;; Simple Truth Value.  A Second order probability TV constructor,
;; i.e. probability and confidence.  The probability is in fact the
;; mode of the corresponding beta distribution.
(: STV (-> Number Number TruthValue))

;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;

;; For now the underlying beta distributions have a Jeffreys prior,
;; i.e. the prior alpha and beta are 0.5.
(: prior-alpha (-> Number))
(= (prior-alpha) 0.5)
(: prior-beta (-> Number))
(= (prior-beta) 0.5)

;; Lookahead
(: lookahead (-> Number))
(= (lookahead) 1.0)

;; Maximum supported count (till +inf is supported, possibly).
(: max-count (-> Number))
(= (max-count) 1e9)

;;;;;;;;;;;;;
;; Methods ;;
;;;;;;;;;;;;;

;; Convert count to confidence using the formula
;;
;; confidence = count / (count + lookahead)
(: count->confidence (-> Number Number))
(= (count->confidence $cnt) (/ $cnt (+ $cnt (lookahead))))

;; Convert confidence to count using the formula
;;
;; count = (confidence * lookahead) / (1 - confidence)
(: confidence->count (-> Number Number))
(= (confidence->count $conf) (if (approxEq 1.0 $conf 1e-9)
                                 (max-count)
                                 (/ (* $conf (lookahead)) (- 1.0 $conf))))

;; Increment the negative count of a given truth value, by
;; incrementing its total count without incrementing the positive
;; count.
(: inc-neg-count (-> TruthValue TruthValue))
(= (inc-neg-count (STV $s $c)) (let* (($tot_cnt (confidence->count $c))
                                      ($pos_cnt (* $s $tot_cnt))
                                      ($new_tot_cnt (+ $tot_cnt 1)))
                                 (STV (/ $pos_cnt $new_tot_cnt)
                                      (count->confidence $new_tot_cnt))))

;; Increment the positive count of a given truth value, by
;; incrementing its total count and its positive count.
(: inc-pos-count (-> TruthValue TruthValue))
(= (inc-pos-count (STV $s $c)) (let* (($tot_cnt (confidence->count $c))
                                      ($pos_cnt (* $s $tot_cnt))
                                      ($new_pos_cnt (+ $pos_cnt 1))
                                      ($new_tot_cnt (+ $tot_cnt 1)))
                                 (STV (/ $new_pos_cnt $new_tot_cnt)
                                      (count->confidence $new_tot_cnt))))

;; Return the first order probability mode of the second order
;; distribution associated to a truth value.
(: mode (-> TruthValue Number))
(= (mode (Bl True)) 1.0)
(= (mode (Bl False)) 0.0)
(= (mode (Pr $pr)) $pr)
(= (mode (STV $pr $_)) $pr)

;; Return the total count of a truth value.  For truth values not
;; capturing a notion of confidence, such as Bl or Pr then the count
;; is assumed to be a very large number (cause +inf does not seem to
;; be supported at the moment).
(: count (-> TruthValue Number))
(= (count (Bl $_)) (max-count))
(= (count (Pr $_)) (max-count))
(= (count (STV $_ $conf)) (confidence->count $conf))

;; Return the confidence of a truth value.  For truth values not
;; capturing a notion of confidence, such as Bl or Pr then the
;; confidence is assumed to be 1.0.  For truth values capturing a
;; notion of confidence, such as STV, the formula to convert a count
;; into confidence is as follows
;;
;; confidence = count / (count + lookahead)
(: confidence (-> TruthValue Number))
(= (confidence (Bl $_)) 1.0)
(= (confidence (Pr $_)) 1.0)
(= (confidence (STV $_ $conf)) $conf)

;; Return the positive count of a truth value.
(: pos-count (-> TruthValue Number))
(= (pos-count $tv) (* (mode $tv) (count $tv)))

;; Return the negative count of a truth value.
(: neg-count (-> TruthValue Number))
(= (neg-count $tv) (* (- 1 (mode $tv)) (count $tv)))

;; Return the posterior alpha of a truth value
(: post-alpha (-> TruthValue Number))
(= (post-alpha $tv) (+ (prior-alpha) (pos-count $tv)))

;; Return the posterior beta of a truth value
(: post-beta (-> TruthValue Number))
(= (post-beta $tv) (+ (prior-beta) (neg-count $tv)))

;; Return the first order probability mean of the second order
;; distribution associated to a truth value.  For truth values not
;; capturing a notion of confidence, such as Bl or Pr then the
;; confidence is assumed to be 1.0.  For truth values capturing a
;; notion of confidence, such as STV, a beta distribution is assumed.
(: mean (-> TruthValue Number))
(= (mean (Bl True)) 1.0)
(= (mean (Bl False)) 0.0)
(= (mean (Pr $pr)) $pr)
(= (mean (STV $pr $conf))
   (let* (($a (post-alpha (STV $pr $conf)))
          ($b (post-beta (STV $pr $conf))))
     (/ $a (+ $a $b))))

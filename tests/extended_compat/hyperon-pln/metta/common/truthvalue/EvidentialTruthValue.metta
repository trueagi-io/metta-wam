;; Evidential truth value type. Represent a truth value alongside its
;; evidence.

!(import! &self TruthValue.metta)
!(import! &self ../OrderedSet.metta)

;;;;;;;;;;
;; Type ;;
;;;;;;;;;;

(: EvidentialTruthValue Type)

;;;;;;;;;;;;;;;;;;
;; Constructor ;;
;;;;;;;;;;;;;;;;;;

;; Takes an evidence set ascribed to a truth value to form an
;; evidential truth value.
(: ETV (-> (OrderedSet $a) TruthValue EvidentialTruthValue))

;;;;;;;;;;;;;
;; Methods ;;
;;;;;;;;;;;;;

;; Access the truth value of an evidential truth value.
(: tv (-> EvidentialTruthValue TruthValue))
(= (tv (ETV $ev $tv)) $tv)

;; Return the first order probability mode of the second order
;; distribution associated to an evidential truth value.
(: mode (-> EvidentialTruthValue Number))
(= (mode (ETV $ev $tv)) (mode $tv))

;; Return the total count of an evidential truth value.
(: count (-> EvidentialTruthValue Number))
(= (count (ETV $ev $tv)) (count $tv))

;; Return the positive count of a truth value.
(: pos_count (-> EvidentialTruthValue Number))
(= (pos_count (ETV $ev $tv)) (* (mode $tv) (count $tv)))

;; Return the negative count of a truth value.
(: neg_count (-> EvidentialTruthValue Number))
(= (neg_count (ETV $ev $tv)) (* (- 1 (mode $tv)) (count $tv)))

;; Return the posterior alpha of a truth value
(: post_alpha (-> EvidentialTruthValue Number))
(= (post_alpha (ETV $ev $tv)) (+ (prior_alpha) (pos_count $tv)))

;; Return the posterior beta of a truth value
(: post_beta (-> EvidentialTruthValue Number))
(= (post_beta (ETV $ev $tv)) (+ (prior_beta) (neg_count $tv)))

;; Return the first order probability mean of the second order
;; distribution associated to an evidential truth value.
(: mean (-> EvidentialTruthValue Number))
(= (mean (ETV $ev $tv)) (mean $tv))

;; Return the confidence of an evidential truth value.
(: confidence (-> EvidentialTruthValue Number))
(= (confidence (ETV $ev $tv)) (confidence $tv))

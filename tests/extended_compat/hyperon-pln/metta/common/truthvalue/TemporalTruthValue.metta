;; Temporal [evidential] truth value.  A truth value indexed by time.
;;
;; It is implemeted as a map from time to truth value.  In a way a
;; temporal truth value is a second order fuzzy temporal predicate.
;;
;; Time is discrete for now.

!(import! &self EvidentialTruthValue.metta)

;;;;;;;;;;;
;; Types ;;
;;;;;;;;;;;

(: TemporalTruthValue Type)
(: TemporalEvidentialTruthValue Type)

;;;;;;;;;;;;;;;;;;
;; Constructors ;;
;;;;;;;;;;;;;;;;;;

;; Temporal truth value constructor.  For now time is discrete.
(: TTV (-> (-> Number TruthValue) TemporalTruthValue))

;; Temporal evidential truth value constructor.  For now time is
;; discrete.
(: TETV (-> (-> Number EvidentialTruthValue) TemporalEvidentialTruthValue))

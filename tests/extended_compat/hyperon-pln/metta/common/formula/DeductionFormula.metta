;; Formula and other functions used for the deduction rule

;; Import modules
!(import! &self ../truthvalue/TruthValue.metta)
!(import! &self ../Num.metta)

; Consistency Conditions
(: smallest-intersection-probability (-> Number Number Number))
(= (smallest-intersection-probability $As $Bs)
  (clamp (/ (- (+ $As $Bs) 1) $As) 0 1))

(: largest-intersection-probability (-> Number Number Number))
(= (largest-intersection-probability $As $Bs)
  (clamp (/ $Bs $As) 0 1))

(: conditional-probability-consistency (-> Number Number Number Bool))
(= (conditional-probability-consistency $As $Bs $ABs)
   (and (< 0 $As)
        (and (<= (smallest-intersection-probability $As $Bs) $ABs)
             (<= $ABs (largest-intersection-probability $As $Bs)))))

;; Main Formula
(: simple-deduction-strength-formula (-> Number Number Number Number Number Number))
(= (simple-deduction-strength-formula $As $Bs $Cs $ABs $BCs)
  (if
     (and
        (conditional-probability-consistency $As $Bs $ABs)
        (conditional-probability-consistency $Bs $Cs $BCs))
     ;; Preconditions are met
     (if (< 0.99 $Bs)
        ;; sB tends to 1
        $Cs
        ;; otherwise
        (+ (* $ABs $BCs) (/ (* (- 1 $ABs) (- $Cs (* $Bs $BCs))) (- 1 $Bs))))
     ;; Preconditions are not met
     0))

;; Alternate deduction formula hardwired for STV to make it faster.
(= (deduction-formula (STV $Ps $Pc)
                      (STV $Qs $Qc)
                      (STV $Rs $Rc)
                      (STV $PQs $PQc)
                      (STV $QRs $QRc))
   (if (and (conditional-probability-consistency $Ps $Qs $PQs)
            (conditional-probability-consistency $Qs $Rs $QRs))
       ;; Preconditions are met
       (STV (if (< 0.9999 $Qs)                  ; avoid division by 0
                ;; Qs tends to 1
                $Rs
                ;; Otherwise
                (+ (* $PQs $QRs) (/ (* (- 1 $PQs) (- $Rs (* $Qs $QRs))) (- 1 $Qs))))
            (min $Pc (min $Qc (min $Rc (min $PQc $QRc)))))
       ;; Preconditions are not met
       (STV 1 0)))

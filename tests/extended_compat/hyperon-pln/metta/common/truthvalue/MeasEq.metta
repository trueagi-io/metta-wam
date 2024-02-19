;; The symbol ≞, called Measured By in Unicode, and \measeq in LaTeX,
;; is used to represent the measure of an event.  This measure may be
;; boolean, probabilistic first or second order, evidential or
;; otherwise.  The idea of using a dedicated equality, as opposed to
;; regular definitional equality =, is to not conflate event and
;; measure which can lead to contradictions.

;; Define ≞ as a dependent type, assigning a truth value to an event.
(: ≞ (-> $event $tv Type))

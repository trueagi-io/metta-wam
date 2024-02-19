; from https://github.com/trueagi-io/hyperon-experimental/issues/530
; What is the problem?
; Unification such as (A $a $a) ≐ (A $x $y) fails to recognize that $x = $y.

; How to reproduce the problem?
; Run the following metta code


(A $a $a)
!(match &self (A $x $y) (A $x $y))

;;What should be normally output?
;;It should output (A $x $x) or (A $y $y) (or even (A $a $a) if the variables present in the query are not prioritized).

;;What is output instead?
;;(A $x $y)

!(repl!)


;What else can you tell?
;When variable $a is replaced by a, as in

(AA a a)
!(match &self (AA $x $y) (AA $x $y))



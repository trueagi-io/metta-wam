(: traced-eval (-> Atom Atom))
(= (traced-eval $arg) (traced-eval-q (quote $arg)))
(: traced-eval-q (-> Atom Atom))
(= (traced-eval-q (quote $arg))
   (let* (
	(() (println! (quote (====>EVAL $arg))))
	($r $arg)
	(() (println! (quote (RET<=== $r)))))
   $r))

(: progn (-> Atom Atom))
(= (progn $body) (progn-q (quote $body)))
(: progn-q (-> Atom Atom))
(= (progn-q (quote $body))
	(if (== (quote $body) (quote ())) ()
		(let* (
			(($head $rest) (decons-atom $body)) ; Deconstruct body
			($r (traced-eval-q (quote $head))))
			(if (== (quote $rest) (quote ())) 
			   $r (progn-q (quote $rest))))))


(: If (-> Bool Atom Atom Atom))
(= (If $cond $then $else) (if $cond $then $else))

(= (facF $n)
   (If (== $n 0)
	   1
	   (* $n (facF (- $n 1)))))

(= (facf $n)
   (if (== $n 0)
	   1
	   (* $n (facf (- $n 1)))))


!(facF 10)

!(facf 10)

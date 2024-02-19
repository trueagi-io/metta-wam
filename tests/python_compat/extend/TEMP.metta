
(= (facF $n)
   (If (== $n 0)
       1
       (* $n (facF (- $n 1)))))

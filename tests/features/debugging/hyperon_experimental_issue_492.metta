

; https://github.com/trueagi-io/hyperon-experimental/issues/492


(= (memb $X Nil) False)
(= (memb $X (Cons $H $Tail))
   (memb $X $Tail))
(= (memb $X (Cons $X $Tail))
   True)


!(let $res
    (and (memb $X (Cons 1 (Cons 2 Nil)))
             (memb $X (Cons 2 (Cons 3 Nil))))
	     (if $res $X None))


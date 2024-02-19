!(extend-py! mettalog)

!(use_module (library clpfd))

!(and
	(= $vars ($S $E $N $D $M $O $R $Y))
	(ins $vars 0 9)
	(/\= $S 0)
	(/\= $M 0)
	(all-different $vars)
	(#= $send               (+ (* 1000 $S) (* 100 $E) (* 10 $N) $D))
	(#= $more               (+ (* 1000 $M) (* 100 $O) (* 10 $R) $E))
	(#= $money (+ (* 10000 $M) (* 1000 $O) (* 100 $N) (* 10 $E) $Y))
	(#= (+ $send $more) $money)

        (label $vars) ;; compute the answer
	(format " SEND =               S(~d)*1000 + E(~d)*100 + N(~d)*10 + D(~d)" $S $E $N $D)
	(format " MORE =               M(~d)*1000 + O(~d)*100 + R(~d)*10 + E(~d)" $M $O $R $E)
	(format "MONEY = M(~d)*10000 + O(~d)*1000 + N(~d)*100 + E(~d)*10 + Y(~d)" $M $O $N $E $Y)
	(format "~d + ~d = ~d~%" $send $more $money)
)

!(mettalog::vspace-main)




(: is-even (-> Number Bool))
(= (is-even $X) (== (% $X 2) 0))

(= (fail-not-even $v) (if (is-even $v) () (empty)))

!(superpose
   ; using nop to hide results
   (nop (freeze! $even fail-not-even))  ; freeze (terible name chosen by Furtwidth))
       (assertEqual                                    ;  nondetermisticaly implants coide that will accept or reject bindings
             (let $even (superpose (1 2 3 4 5 6)) $even)
             (superpose (2 4 6))))


(= (append $x $y)
    (if (== $x ())
        $y
        (let* (($h (car-atom $x)) ($t (cdr-atom $x)))
            (let $rest (append $t $y)
                (cons-atom $h $rest)))))

(= (nth $n $x)
    (if (== $n 0)
        (car-atom $x)
        (let* (($rest (cdr-atom $x)) ($nn (- $n 1)))
            (nth $nn $rest))))


(= (format-args-aux $format-chars $args $n)
    (if (== $format-chars ())
        ()
        (let* (($first (car-atom $format-chars)) ($rest (cdr-atom $format-chars)))
            (if (== $first '{')
                (if (== $rest ())
                    ('{')
                    (if (== (car-atom $rest) '}')
                        (let* (($arg (nth $n $args)) ($restrest (cdr-atom $rest)) ($nn (+ $n 1)))
                            (let* (($argchars (stringToChars $arg)) ($rest2 (format-args-aux $restrest $args $nn)))
                                (append $argchars $rest2)
                            )
                        )
                        (let $rest2 (format-args-aux $rest $args $n)
                            (cons-atom $first $rest2)
                        )
                    )
                )
                (let $rest2 (format-args-aux $rest $args $n)
                    (cons-atom $first $rest2)
                )
            )
        )
    )
)

(@doc format-argsx
  (@desc "Fills {} symbols in the input expression with atoms from the second expression. E.g. (format-args (Probability of {} is {}%) (head 50)) gives [(Probability of head is 50%)]. Atoms in the second input value could be variables")
  (@params (
    (@param "Expression with {} symbols to be replaced")
    (@param "Atoms to be placed inside expression instead of {}")))
  (@return "Expression with replaced {} with atoms"))

(: format-argsx (-> String Atom String))
(= (format-argsx $format $args)
	(let $format-chars (stringToChars $format)
        (let $formatted (format-args-aux $format-chars $args 0)
            (charsToString $formatted))))

; Tests
!(format-argsx "{}ab" ("XX" "ZZ"))
!(append (1 2) (3 5))
!(nth 3 (10 11 12 13 14 15))
!(format-argsx "ab" ("XX" "ZZ"))
!(format-argsx "{ab" ("XX" "ZZ"))
!(format-argsx "a{b" ("XX" "ZZ"))
!(format-argsx "{}a{b" ("XX" "ZZ"))
!(format-argsx "{}a{}b" ("XX" "ZZ"))
!(format-argsx "a{}b{}" ("XX" "ZZ"))




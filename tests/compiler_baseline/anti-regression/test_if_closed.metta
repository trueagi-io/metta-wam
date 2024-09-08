(: is-variable (-> Atom Bool))
(= (is-variable $x) (== (get-metatype $x) Variable))

(: lazy-or (-> Bool Atom Bool))
(= (lazy-or False $x) $x)
(= (lazy-or True $x) True)

(: is-expression (-> Atom Bool))
(= (is-expression $x) (== (get-metatype $x) Expression))

(: is-closed (-> Atom Bool))

(= (is-closed $x) (if (is-variable $x)
					  False
					  (if (== () $x) True
					  (if (is-expression $x)
						  (and (let $head (car-atom $x) (is-closed $head))
							   (let $tail (cdr-atom $x) (is-closed $tail)))
						  True))))



;!(pragma! eval trace)


!(assertEqual (is-expression $x) False) ; rust/mettalog False
!(assertEqual (is-expression (⍃ 4 3)) True)  ; rust/mettalog True
!(assertEqual (is-expression (⍃ 4 $x)) True) ; rust/mettalog True
!(assertEqual (is-expression 4) False) ; rust/mettalog False

!(assertEqual (is-closed $x) False) ; rust/mettalog False
!(assertEqual (is-closed 4) True) ; rust/mettalog True
!(assertEqual (is-closed (⍃ $y 3)) False) ; rust gets stuck / mettalog false
!(assertEqual (is-closed (⍃ 4 $x)) False) ; rust gets stuck / mettalog false
!(assertEqual (is-closed (⍃ 4 3)) True) ; rust gets stuck / mettalog true




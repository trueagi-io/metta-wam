!(extend-py! metta_learner)

;! metta_vspace ; hyperon.runner.MeTTa object (`metta_vspace` is registered 'manually' now)
;! metta_learner ; just a symbol, because `metta_learner` extension doesn't 'register itself', but it could be possible to automate this

!(metta_learner::fact 5) ; 120

!(get-by-key &my-dict "A")

(Recipe Morning-shot (With-ingredient Ginger))
(Recipe Morning-shot (With-ingredient Orange))

(In-store Apple)
(In-store Banana)
(In-store Orange)

(Hate Apple)
(Hate Grape)
(Love Banana)
(Love Orange)
(Love Strawberry)

!(transform (Love $fruit) (transform (In-store $fruit)
    (join
        (Buy $fruit)
        (transform (Recipe $recipe (With-ingredient $fruit))
            (transform (Recipe $recipe (With-ingredient $ingredient))
                (Buy $ingredient)
            )
        )
    )
))


!(print "=====================================")
!(print "Starting Shell !!!!!!!!!!!!!!!!!!!!!!")
!(print "=====================================")


!(metta_learner::vspace-main)



!(metta_learner::self-from-self) ; content

; !(match &self ($x content) $x) ; []

; this returns `some` meaning that `content` from this scripts
; is not distinguished from `content` symbol in `metta_learner`
; !(match metta_learner::&self ($x content) $x)

! metta_learner::content ; it is turned in OperationAtom, but it is not executed
! (metta_learner::content) ; its execution results in unreduced `(content)`
; the following doesn't work (it might be ok, but semantics of :: is weird overall)
;!(match metta_learner::&self ($x metta_learner::content) $x)

; both work, so :: doesn't separate "namespaces" precisely
!(metta_learner::call_func fact 6)
!(metta_learner::call_func metta_learner::fact 6)

; bind a bunch of numpy objects to tokens for convenience
!(bind! &x (np.vector 1 2 3))
!(bind! &y (np.array (3 2 3)))
!(bind! &m (np.array ((6 1) (7 2) (8 3))))
!(bind! &mt (np.array ((1 0 0) (0 1 0))))

; FIXME: no exception?
!(np.vector 1 "S")
; !(np.array (1 (1 2))) ; exception because of bad shape

!(print "HALF WAY")

; wrapping into a function should work
; also works automatically with python numbers
; (no need to turn 1 into np.array)
(= (np-add-1 $x) (np.add $x 1))

!(print "1 HALF WAY")

!(np-add-1 (np-add-1 &x)) ; [3 4 5] ; not a pattern

!(print "2 HALF WAY")
; could work with untyped np objects,
; although with some problems (the resulting type would be Number),
; but doesn't work, because + expects Number
;!(+ &x &y)

!(print "3 HALF WAY")

!(== (np.add &y &x) (np.add &x &y)) ; True
!(== &x &y) ; False
!(== &x (np.vector 1 2)) ; False

! (np.add &m (np.vector 1 2)) ; ((7 3) (8 4) (9 5))
! (np.matmul &mt &m) ; ((6 1) (7 2))

! (np.add &m $x) ; will be turned into pattern

! (case (np.array (((1 1) (2 2) (3 3)) ((2 3) (3 4) (4 5))))
    (
        (($a $b $c) $b)
        (($x ($y1 $y2 $y3)) (np.add $x $y2))
    )
  )

! (case (np.vector 1 2 3)
    (
        (&x "X")
    )
  )


!(metta_learner::vspace-main)

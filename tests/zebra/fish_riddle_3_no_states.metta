






!(include dv-pl) ; why does include work but not import in H-E?
;!(import! &self dv-pl)
;!(test_normal_queries)
;!(test_normal_queries)


;;;;;;;;;;;;;;;;;;
;; TEST Queries ;;
;;;;;;;;;;;;;;;;;;

((because $color house keeps $other the $nationality is the $type owner) :- 
  ((same $Houses
			(Cons ( 1 $_1 $_2 $_3 $_4 $_5) ; First house
			(Cons ( 2 $_6 $_7 $_8 $_9 $_10) ; Second house
			 $Open)))

	;(same $Open Nil)

	; Clue 1: The Norwegian lives in the first house
	(member ( 1 norwegian $_11 $_12 $_13 $_14) $Houses)
	; Clue 2: The Brit lives in the red house
	(member ( $_15 brit red $_16 $_17 $_18) $Houses)
	; Clue 3: The owner of the blue house drinks tea
	(member ( $_19 $_20 blue tea $_21 $_22) $Houses)
	; Clue 4: The owner of the red house keeps $other
	(member ( $_23 $_24 $color $_25 $other $_26) $Houses)
	; Clue 5: The owner of the tea-drinking house smokes Prince
	(member ( $_27 $_28 $_29 tea $_30 prince) $Houses)
	; Determine the $nationality of owns the $type
	(member ( $_31 $nationality $_32 $_33 $type $_34) $Houses)))

;;;;;;;;;;;;;;;;;;
;; TEST Queries ;;
;;;;;;;;;;;;;;;;;;

!(query &self (because red house keeps dogs the $norwegian is the fish owner))

!(query &self (because blue house keeps dogs the $brit is the fish owner))

!(query &self (because red house keeps cats the $norwegian is the fish owner))




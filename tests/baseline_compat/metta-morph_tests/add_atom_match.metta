
(: sequential (-> Expression %Undefined%))
(= (sequential $1) (superpose $1))

; Used to work
;!(assertEqualToResult
;  (sequential
;	( (add-atom &self (man mortal))
;	  (match &self ($A mortal) (mortal $A))))
;  ((mortal man)))

!(sequential
	( (add-atom &self (man mortal))
	  (match &self ($A mortal) (mortal $A))))
  



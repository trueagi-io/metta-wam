
(: sequential (-> Expression %Undefined%))
(= (sequential $1) (superpose $1))

!(assertEqualToResult
  (sequential
    ( (add-atom &self (man mortal))
      (match &self ($A mortal) (mortal $A))))
  ((mortal man)))















(: sequential (-> Expression %Undefined%))
(= (sequential $1) (superpose $1))


(= (c1)
 (superpose
  ( (add-atom &self (man1 mortal1))
	(match &self ($M mortal1) (mortal1 $M)))))


(= (c2)
 (first_of (superpose
  ( (add-atom &self (man2 mortal2))
	(match &self ($M mortal1) (mortal1 $M))))))

(= (c3)
 (first_of (sequential
  ( (add-atom &self (man3 mortal3))
	(match &self ($M mortal3) (mortal3 $M))))))

(= (c4)
 (sequential
  ( (add-atom &self (man4 mortal4))
	(match &self ($M mortal4) (mortal4 $M)))))


!(assertEqualToResult
  (sequential
    ( (add-atom &self (man mortal))
      (match &self ($M mortal) (mortal $M))))
  ((mortal man)))


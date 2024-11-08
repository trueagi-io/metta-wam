
(= (conclude (Parent Bob $y))
   $result)
(= (conclude       $A)
   (match &self (Implies $A $B)
          (match &self $A $B)))


;(Implies (Parent Bob $y)      $B      )
;(Implies (Parent $x  $y) (Child $y $x))
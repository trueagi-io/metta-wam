!(bind! &attentional_focus (new-space))
!(add-atom &attentional_focus 1)
!(bind! &concepts (new-space))
!(add-atom &concepts 2)

;; check they are added
!(assertEqualToResult
  (match &attentional_focus $1 $1)
   (1))

!(assertEqualToResult
  (match &concepts $1 $1)
   (2))

;; check superposing spaces
!(assertEqualToResult
   (match (superpose (&attentional_focus &concepts)) $1 $1)
   (1 2))


!(bind! &FIFO (new-state 42))
!(bind! &WU (new-space))
!(change-state! &FIFO 45)
!(get-state &FIFO)
!(add-atom &WU a)
!(match &WU $1 $1)
!(add-atom &WU b)
!(remove-atom &WU a)

!(assertEqual
  (match &WU $1 $1)
  (get-atoms &WU))

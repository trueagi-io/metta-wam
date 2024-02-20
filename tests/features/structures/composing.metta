

; insert an argument into a compound term..  0= functor, 1 = arg1 etc
(= (insert-at $nth $item $list)
    (collapse 
         (limit $nth (superpose $list)) 
         $item 
         (ofset $nth (superpose $list))))
                               



; call a relation making $nth the return value
(= (renth $nth $list)
  (let $call (insert-at $nth $retval $list)
    (match &self (call! $call) $ret)))


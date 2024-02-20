
;; Captures stdout and sends to a file
(: with-output-to-file (-> Uri (-> $Ret) $Ret))
(= (with-output-to-file $name $atom)
 (let $out 
  (open! $name append)
  (call-cleanup!
    (with-output-to! $out (eval $atom)
    (close $out))))


;; Writes results to a stream
(: with-result-to (-> Writable (-> $Ret) $Ret))
(= (with-result-to $stream $eval)
    (let $res (eval $eval)
     (write-src! $stream $res)))

;; Captures results of function places them in file
(: with-result-to-file (-> Uri (-> $Ret) $Ret))
(= (with-result-to-file $name $atom)
 (let $out 
  (open! $name append)
  (call-cleanup!
    (with-result-to $out (collapse (nop $atom)))
    (close $out))))




;; Captures stdout and sends to a file
(: with-output-to-file (-> Uri (-> $Ret) $Ret))
(= (with-output-to-file $name $atom)
 (let $out 
  (open! $name write)
  (call-cleanup!
    (with-output-to! $out (eval $atom))
    (close! $out))))


;; Writes results to a stream
(: with-results-to (-> Writable (-> $Ret) $Ret))
(= (with-results-to $stream $eval)
    (let $res (eval $eval)
     (, (write-src! $stream $res) 
            (nl! $stream))))

;; Captures results of function places them in file
(: with-results-to-file (-> Uri (-> $Ret) $Ret))
(= (with-results-to-file $name $atom)
 (let $out 
  (open! $name write)
  (call-cleanup!
    (with-results-to $out $atom)
    (close! $out))))



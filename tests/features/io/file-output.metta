
;; Captures stdout and sends to a file
(: with-output-to-file (-> Uri (-> $Ret) $Ret))
(= (with-output-to-file $name $atom)
 (let $out 
  (open! "output.txt" append)
  (setup-cleanup!
    (with-output-to $out (eval $atom)
    (close $out))))


;; Writes results to a stream
(: with-result-to (-> Writable (-> $Ret) $Ret))
(= (with-result-to $stream $eval)
 (let $null (null-stream-fn)
   (with-output-to $null 
    (let $res (eval $eval)
     (write-src! $stream $res)))))



;; Captures results of function places them in file
(: with-result-to-file (-> Uri (-> $Ret) $Ret))
(= (with-result-to-file $name $atom)
 (let $out 
  (open! $name append)
  (setup-cleanup!
    (with-result-to $out (collapse (nop $atom)))
    (close $out))))



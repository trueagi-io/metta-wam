; https://github.com/trueagi-io/hyperon-experimental/issues/481

;patham9 commented on Nov 1, 2023 •
;Clearly there is no way for the following collapse to have any item:

!(== () (collapse (let* (($L ())
                         ($x (superpose $L)))
                        $x)))
;Yet:
;Output: [False]
;Expected: [True]

;MeTTa leaves the let* expression unevaluated in this case, which leaves the collapse with a "pseudo-item", an expression with variables, which leads to the comparison to return false even though there cannot be any variable assignment that actually satisfies the let* expression.


; vsbogd commented on Nov 2, 2023


;Regarding minimal MeTTa, we have Empty symbol there to represent an empty result. Using it we could potentially properly fix superpose to return Empty from (superpose Empty) call. Thus the example above will turn into:
!(collapse (let $L Empty (let $x (superpose $L) $x)))


;And collapse will return () in this case. One can "simulate" this on a current version of minimal MeTTa using additional parenthesis:
!(collapse (let $L (Empty) (let $x (superpose $L) $x)))

; vsbogd commented on Nov 2, 2023
; Thus if (superpose $T) is called with $T == Empty it will work. We should also fix collapse to return Empty instead of () to make it complete and allow collapse/superpose chains on Empty results.

; patham9 commented on Nov 6, 2023
; I didn't yet get why "Empty" needs to be an explicit value. Either there is a value, which can be an empty tuple () or 42 as a special case, or there is no value, in which case backtracking should occur. What is it for?




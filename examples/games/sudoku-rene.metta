

(sudoku_puzzle_state 1 1 1 5  )
(sudoku_puzzle_state 1 2 1 3  )
(sudoku_puzzle_state 1 3 1 0  )
(sudoku_puzzle_state 1 4 2 0  )
(sudoku_puzzle_state 1 5 2 7  )
(sudoku_puzzle_state 1 6 2 0  )
(sudoku_puzzle_state 1 7 3 0  )
(sudoku_puzzle_state 1 8 3 0  )
(sudoku_puzzle_state 1 9 3 0  )
(sudoku_puzzle_state 2 1 1 6  )
(sudoku_puzzle_state 2 2 1 0  )
(sudoku_puzzle_state 2 3 1 0  )
(sudoku_puzzle_state 2 4 2 1  )
(sudoku_puzzle_state 2 5 2 9  )
(sudoku_puzzle_state 2 6 2 5  )
(sudoku_puzzle_state 2 7 3 0  )
(sudoku_puzzle_state 2 8 3 0  )
(sudoku_puzzle_state 2 9 3 0  )
(sudoku_puzzle_state 3 1 1 0  )
(sudoku_puzzle_state 3 2 1 9  )
(sudoku_puzzle_state 3 3 1 8  )
(sudoku_puzzle_state 3 4 2 0  )
(sudoku_puzzle_state 3 5 2 0  )
(sudoku_puzzle_state 3 6 2 0  )
(sudoku_puzzle_state 3 7 3 0  )
(sudoku_puzzle_state 3 8 3 6  )
(sudoku_puzzle_state 3 9 3 0  )
(sudoku_puzzle_state 4 1 4 8  )
(sudoku_puzzle_state 4 2 4 0  )
(sudoku_puzzle_state 4 3 4 0  )
(sudoku_puzzle_state 4 4 5 0  )
(sudoku_puzzle_state 4 5 5 6  )
(sudoku_puzzle_state 4 6 5 0  )
(sudoku_puzzle_state 4 7 6 0  )
(sudoku_puzzle_state 4 8 6 0  )
(sudoku_puzzle_state 4 9 6 3  )
(sudoku_puzzle_state 5 1 4 4  )
(sudoku_puzzle_state 5 2 4 0  )
(sudoku_puzzle_state 5 3 4 0  )
(sudoku_puzzle_state 5 4 5 8  )
(sudoku_puzzle_state 5 5 5 0  )
(sudoku_puzzle_state 5 6 5 3  )
(sudoku_puzzle_state 5 7 6 0  )
(sudoku_puzzle_state 5 8 6 0  )
(sudoku_puzzle_state 5 9 6 1  )
(sudoku_puzzle_state 6 1 4 7  )
(sudoku_puzzle_state 6 2 4 0  )
(sudoku_puzzle_state 6 3 4 0  )
(sudoku_puzzle_state 6 4 5 0  )
(sudoku_puzzle_state 6 5 5 2  )
(sudoku_puzzle_state 6 6 5 0  )
(sudoku_puzzle_state 6 7 6 0  )
(sudoku_puzzle_state 6 8 6 0  )
(sudoku_puzzle_state 6 9 6 6  )
(sudoku_puzzle_state 7 1 7 0  )
(sudoku_puzzle_state 7 2 7 6  )
(sudoku_puzzle_state 7 3 7 0  )
(sudoku_puzzle_state 7 4 8 0  )
(sudoku_puzzle_state 7 5 8 0  )
(sudoku_puzzle_state 7 6 8 0  )
(sudoku_puzzle_state 7 7 9 2  )
(sudoku_puzzle_state 7 8 9 8  )
(sudoku_puzzle_state 7 9 9 0  )
(sudoku_puzzle_state 8 1 7 0  )
(sudoku_puzzle_state 8 2 7 0  )
(sudoku_puzzle_state 8 3 7 0  )
(sudoku_puzzle_state 8 4 8 4  )
(sudoku_puzzle_state 8 5 8 1  )
(sudoku_puzzle_state 8 6 8 9  )
(sudoku_puzzle_state 8 7 9 0  )
(sudoku_puzzle_state 8 8 9 0  )
(sudoku_puzzle_state 8 9 9 5  )
(sudoku_puzzle_state 9 1 7 0  )
(sudoku_puzzle_state 9 2 7 0  )
(sudoku_puzzle_state 9 3 7 0  )
(sudoku_puzzle_state 9 4 8 0  )
(sudoku_puzzle_state 9 5 8 8  )
(sudoku_puzzle_state 9 6 8 0  )
(sudoku_puzzle_state 9 7 9 0  )
(sudoku_puzzle_state 9 8 9 7  )
(sudoku_puzzle_state 9 9 9 9  )



(row 1 )
(row 2 )
(row 3 )
(row 4 )
(row 5 )
(row 6 )
(row 7 )
(row 8 )
(row 9 )
(column 1 )
(column 2 )
(column 3 )
(column 4 )
(column 5 )
(column 6 )
(column 7 )
(column 8 )
(column 9 )
(quad 1 )
(quad 2 )
(quad 3 )
(quad 4 )
(quad 5 )
(quad 6 )
(quad 7 )
(quad 8 )
(quad 9 )
(sudoku_number 1 )
(sudoku_number 2 )
(sudoku_number 3 )
(sudoku_number 4 )
(sudoku_number 5 )
(sudoku_number 6 )
(sudoku_number 7 )
(sudoku_number 8 )
(sudoku_number 9 )


(tmp_candidate 1 )
(tmp_candidate 2 )
(tmp_candidate 3 )
(tmp_candidate 4 )
(tmp_candidate 5 )
(tmp_candidate 6 )
(tmp_candidate 7 )
(tmp_candidate 8 )
(tmp_candidate 9 )


(tempnum 1)
(tempnum 2)
(tempnum 3)

(xcount 0)

(ycount 0)

(act_rc 1 1)

(violated 0 0 )

; i need a match statement that can return 1 item 
(act_trace 0 0 0 0)
(false_candidate 0 0 0 )

; backtrack_to_level
; (act_trace $e20 $e21 )


;(= (increment $atom) (change-state! $atom (+ 1 (get-state $atom))))


(= (exist_temp_num $num)   
        (let $m (collapse (match &self (tempnum $num) True))
        (if (== $m ())
            False
            True
        )
    )
             )


;(= (add_tmp_candidate (cell_candidate $num ) )  (add-atom &self (tmp_candidate $num) )  )
;(: map-expr (-> (-> $t $t) Expression Expression))
;tmp_candidate
;(= (add_tmp_candidates $expr)
; remove_tmp_candidates

(= (init_a_stack )    (match &self (sudoku_puzzle_state $r $c $quad $state)    
   (if (== $state 0)    (action_rc $r $c)     (empty)    ) 
      )    )   



(= (init_candidates )   
  ( match &self   (sudoku_number $num   )      (cell_candidate $num )  )
  )



(= (get_remaining_possible_candidates )   
     (   match &self   (sudoku_number $num   )     (
      (if (== (exist_temp_num $num) False )
;      (if (temp_num $num) 
         empty
       ($num)    )  

;   ($num)
      )       
       )  
  )



(= (add_xnum (cell_xstate $r $c $sta ) )  (add-atom &self (tempnum $sta) )  )

(= (add_tmp_candidate (cell_candidate $num ) )  (add-atom &self (tmp_candidate $num) )  )

(= (add_act_stack (action_rc $r $c ) )  (add-atom &self (act_rc $r $c) )  )


; candidates
; TODO:  apply_first_candidate


; vio
; (= (apply_f_candidate (action_rc $r $c ) )  

; OR THIS

;!(remove_violations)
;!(init_action_stack)
;!(apply_action_from_stack (collapse  (get_all_actions)  ) )
;!(score_cells  (collapse (all_empty_cells)  ) ) 
;!(if_exist_violation_backtrack_stack )
;!(puzzle_is_complete)


(= (loop_sudoku_until_complete (coninu $yesno ) )
 (collapse 
    (let*  (
 ($e0 (println $yesno) )
 ($e1 (remove_violations) )
 ($tr (add-atom &self (violated 0 0 ) ) )
 ($e2 (init_action_stack) )
 ($e3 (apply_action_from_stack (collapse  (get_all_actions)  ) ) )
 ($e4 (score_cells  (collapse (all_empty_cells)  ) )  )
 ($e5 (if_exist_violation_backtrack_stack ) )
 ($e6 (puzzle_is_complete) )   
 ($e7 (+ $yesno 1) )   
   )   
;    (True)  
     (if (== $e6 True) () (loop_sudoku_until_complete (coninu $e7 )) )  
    ) 
    )  
 )

; !(exist_violation )
; !(update_cell_state 1 2 8)
; !(get_row_stateyz 1 3)   
;!(get_all_actions)
; continue here 
; 1.1.1.1


(= (apply_action (action_rc $r $c ) )  
 (collapse (let*  (
  ($e3 (remove-atom &self (act_rc $r $c) )  )
  ($e4 (init_check_cell  $r $c )  )
  ($e5 ( check_cell  $r $c  ) )
  ($e20 (increment_ycount ) )
;  ($e7 (: (tmp_candidate $num) $num ) )
;  ($e8 ( print "take num"  ) )
;  ($e9 ( print $r  ) )
;  ($e11 ( print $c  ) )
   ($e10 ( get_first_candidate  (collapse (get_tmp_candidates) )  ) )
   ($e21 (add-atom $self (act_trace $e20 $r $c $e10)  ) )
;   ($e8 ( print "take num"  ) )
;   ($e9 ( print $r $c $e10  ) )
   )
   ( 
  ;  True   ( tmp_candidate $num  )
  ;  (apply_first_candidate  (collapse (get_tmp_candidates_rc) ) )  
      update_cell_state $r $c $e10
     ; apply_action
    )
    ) )
    )
 


(= (remove_tmp_candidate (cell_candidate $num ) )  (remove-atom &self (tmp_candidate $num) )  )


;(: map-expr (-> (-> $t $t) Expression Expression))

;tmp_candidate
  
  

;(= (apply_first_candidate $expr)
;   (if (== $expr ()) ()
;       (let* (($head (car-atom $expr))
;              ($tail (cdr-atom $expr))
;
;              ($head-new (apply_f_candidate $head))
;             )
;         (cons-atom $head-new $tail)
;       )
;   )
;)

;              ($head-new (apply_f_candidate $head))
 ;        (cons-atom $head-new $tail)
;          (collapse $head)

(= (get_first_candidate $expr)
   (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))
                     ($headnew (car-atom $head))

             )
            $headnew
       )
   )
 )


;(= (get_first_candidate $expr)
;   (if (== $expr ()) ()
;       (collapse  (let* (($head (car-atom $expr))
;              ($tail (cdr-atom $expr))
;             )
;            $head
;       ) )
;   )
; )

  



(= (apply_action_from_stack $expr)
   (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))

              ($head-new (apply_action $head))
;              ($tail-new (add_a_stack $tail))
             )
         (cons-atom $head-new $tail)
       )
   )
)
 

(= (add_a_stack $expr)
   (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))

              ($head-new (add_act_stack $head))
              ($tail-new (add_a_stack $tail))
             )
         (cons-atom $head-new $tail-new)
       )
   )
)



(= (add_tmp_candidates $expr)
   (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))

              ($head-new (add_tmp_candidate $head))
              ($tail-new (add_tmp_candidates $tail))
             )
         (cons-atom $head-new $tail-new)
       )
   )
)

(= (remove_tmp_candidates $expr)
   (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))

              ($head-new (remove_tmp_candidate $head))
              ($tail-new (remove_tmp_candidates $tail))
             )
         (cons-atom $head-new $tail-new)
       )
   )
)


(= (add_temp_nums $expr)
   (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))
;              ($head-new ($f $head))
              ($head-new (add_xnum $head))
              ($tail-new (add_temp_nums $tail))
             )
         (cons-atom $head-new $tail-new)
       )
   )
)


(= (score_cells $expr)
   (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))
              ($head-new (score_cell $head))
              ($tail-new (score_cells $tail))
             )
         (cons-atom $head-new $tail-new)
       )
   )
)



(= (check_violations $expr)
   (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))
              ($head-new (check_violation $head))
              ($tail-new (check_violations $tail))
             )
         (cons-atom $head-new $tail-new)
       )
   )
)




;(= (set_connected_cell_values  $row $col )  (        
; (let*      (
;    ($e1 (add_temp_nums    (get_column_stateyz $row $col )    )  )
;  ($e2 (add_temp_nums    (get_row_stateyz $row $col )    )  )
;  ($e3  (add_temp_nums    (get_quad_stateyz (get_cell_quadn $row $col ) )  )  )   )  True   )         )         )

;(= (set_connected_cell_values  $row $col )  (        
; (let*      (
;    ($e1    (get_column_stateyz_add_nums $row $col )      )
;  ($e2     (get_row_stateyz_add_nums $row $col )      )
;  ($e3     (get_quad_stateyz_add_nums (get_cell_quadn $row $col )   )  )   )  (empty)   )         )         )

(= (set_connected_cell_values  cell_rc($row $col) )  (        
 (let*      (
    
    ($e1    (get_column_stateyz_add_nums $row $col )      )
  ($e2     (get_row_stateyz_add_nums $row $col )      )
  ($e3     (get_quad_stateyz_add_nums (get_cell_quadn $row $col )   )  )   )  True   )         )         )


;(= (set_connected_cell_values  $row $col )  (        
; 
;        (get_column_stateyz_add_nums $row $col )      
;       (get_row_stateyz_add_nums $row $col )      
;       (get_quad_stateyz_add_nums (get_cell_quadn $row $col )   )       ) )



;(= (set_connected_cell_values  $row $col ) 
; (let* 

;  ($e1 (add_temp_nums (get_quad_stateyz (get_cell_quadn $row $col ) ) ) ) 
;  ($e2 (add_temp_nums (get_column_stateyz $row $col ) ) )
;  ($e3 (add_temp_nums (get_row_stateyz $row $col )  ) )
 ;              )
 ;  True
;         )






(= (process_list $expr)
   (if (== $expr ()) ()
       (let* (($head (car-atom $expr))
              ($tail (cdr-atom $expr))
;              ($head-new ($f $head))
              ($head-new (increment_count))

              ($tail-new (process_list $tail))
             )
        (
             (cons-atom $head-new $tail-new)
;    (println "jfjfjfj")
             )
       )
   )
)



(= (length_list $expr)
   (let* 
    ( ($g init_count)
      ($ty (process_list $expr)   ) 
      
    )
; True
  ()
 ;  (get_count )
    )

  )

; (xcount 0)
(= (get_count )  (match &self (xcount $co)    $co    )   )
(= (get_ycount )  (match &self (ycount $co)    $co    )   )

;(= (get_count )  ( get-state xcount )  )   

(= (init_count )   ( 
 (let*  (
      ($was (get_count ) )
      ($we  (remove-atom &self (xcount $was)) )
      ($re (add-atom &self (xcount 0 ))   )
;       (change-state &self (xcount 0 )   )
     )
     True 
  )  ) )

(= (set_ycount $va)   ( 
 (let*  (
      ($was (get_ycount ) )
      ($we  (remove-atom &self (ycount $was)) )
      ($re (add-atom &self (ycount $va ))   )

     )
     True 
  )  ) )



(= (init_ycount )   ( 
 (let*  (
      ($was (get_ycount ) )
      ($we  (remove-atom &self (ycount $was)) )
      ($re (add-atom &self (ycount 0 ))   )
;       (change-state &self (xcount 0 )   )
     )
     True 
  )  ) )


;(= (increment $atom) (change-state! $atom (+ 1 (get-state $atom))))
; (get-state (xcount $was))
(= (increment_count )   ( 
 (let*  (
      ($was (get_count ) )
      ($we  (remove-atom &self (xcount $was)) )
      ($cx  (+ $was 1) )
      ($re (add-atom &self (xcount $cx ))   )
;      ($re (change-state &self (xcount $cx ))   )
     )
     $cx 
  )  ) )


(= (increment_ycount )   ( 
 (let*  (
      ($was (get_ycount ) )
      ($we  (remove-atom &self (ycount $was)) )
      ($cx  (+ $was 1) )
      ($re (add-atom &self (ycount $cx ))   )

     )
     $cx 
  )  ) )



(= (update_cell_state $row $column $state)   ( 
 (let*  (
  	 	($was (get_cell_state $row $column) )
        ($quan (get_cell_quadn $row $column) )
    	($we  (remove-atom &self (sudoku_puzzle_state $row $column $quan $was)) )
      ($re (add-atom &self (sudoku_puzzle_state $row $column $quan $state  ))   )
 ;      ($was (change-state (sudoku_puzzle_state $row $column $state) ) )
     )
     True 
	)  ) )

  


 

; DEBUG
(= (all_empty_cells )    (match &self (sudoku_puzzle_state $r $c $quad $state)    
;   (if (== $state 0)    (cell_rc $r $c)     (empty)    ) 
   (if (== $state 9)    (cell_rc $r $c)     (empty)    ) 
      )    )   



(= (get_cell_state $row $column)    (match &self (sudoku_puzzle_state $row $column $quad $state)    $state    )   )
(= (get_cell_quadn $row $column)    (match &self (sudoku_puzzle_state $row $column $quadn $state)    $quadn    )   )

 

;(= (get_row_state $row)   (collapse (match &self  (sudoku_puzzle_state $row $num $state)    $state    )  ) )
;(= (get_row_statex $row)  (collapse (match &self (, (column $w) (sudoku_puzzle_row_state $row $w $state)  )    $state    )  ) )
;(= (get_row_statey $row)   (collapse (match &self  (sudoku_puzzle_state $row $colx $state)    (cell_xstate $row $colx $state)    )  ) )

 
(= (get_tmp_candidates)   (match &self   ( tmp_candidate $num  )      ($num    )          )  )

(= (get_temp_nums)   (match &self   ( tempnum $num  )      ($num    )          )  )
(= (get_temp_nums_as_candidates)   (match &self   ( tempnum $num  )      (cell_candidate $num    )          )  )


;(act_trace 0 0 0 0)
;(false_candidate 0 0 0 0)
;update_cell
;    (remove-atom &self (tempnum $num )  )    


(= (backtrack_to_level $levelx ) (match &self   ( act_trace $level $row $col $state )   
    (if (< $level $levelx )
    (update_cell_state $row $col 0)
    (True)
    ) ) )

(= (set_false_candidates $levelx ) (match &self   ( act_trace $level $row $col $state )   
    (if (< $level $levelx )
    (add-atom &self (false_candidate $level $row $col $state )  )
    (True)
    ) ) )

(= (remove_trace $levelx ) (match &self   ( act_trace $level $row $col $state )   
    (if (< $level $levelx )
    (remove-atom &self (act_trace $level $row $col $state )  )
    (True)
    ) ) )


; set_ycount
; (= (set_false_candidates $levelx ) () )
 




(= (remove_temp_nums)   (match &self   ( tempnum $num  )   (remove-atom &self (tempnum $num )  )    )  )





(= (remove_a_stack)   (match &self   ( act_rc $r $c  )   (remove-atom &self (act_rc $r $c )  )    )  )

(= (get_all_actions)   (match &self   ( act_rc $r $c  )    (action_rc $r $c )     )  )
 

(= (remove_violations)   (match &self   ( violated $r $c  )   (remove-atom &self (violated $r $c )  )    )  )

(= (remove_tmp_candidates)   (match &self   ( tmp_candidate $num  )   (remove-atom &self (tmp_candidate $num )  )    )  )

; (cell_candidate $num )



(= (get_column_stateyz $rowx $colx)   
     (match &self   ( sudoku_puzzle_state $row $colx $quad $state  )     (
      (if (> $state 0)
       (cell_xstate $row $colx $state)
       (empty)
    )  )     )  
  )

(= (get_column_stateyz_add_nums $rowx $colx)   
     (match &self   ( sudoku_puzzle_state $row $colx $quad $state  )     (
      (if (> $state 0)
;       (cell_xstate $row $colx $state)
       (add-atom &self (tempnum $state ) )

       (empty)
    )  )     )  
  )



; probably collapse add 
; (sudoku_board 9 7 9 7  )
; (sudoku_puzzle_state 1 1 5  )

;(= (sudoku_quad_num $rowx $colx)   
;       (match &self   ( sudoku_board $rowx $colx $quadn $nothing  )     
;      $quadn           )          )


(= (get_quad_stateyz $quadn)   
     (match &self   ( sudoku_puzzle_state $row $colx $quadx $state  )     (
      (if (and (> $state 0) (== $quadx $quadn ) )
       (cell_xstate $row $colx $state)
       (empty)
    )  )     )  
  )


(= (get_quad_stateyz_add_nums $quadn)   
     (match &self   ( sudoku_puzzle_state $row $colx $quadx $state  )     (
      (if (and (> $state 0) (== $quadx $quadn ) )
;       (cell_xstate $row $colx $state)
        (add-atom &self (tempnum $state ) )
       (empty)
    )  )     )  
  )






(= (get_row_stateyz $row $xcol)   
     (match &self   ( sudoku_puzzle_state $row $colx $qua $state  )     (
      (if (> $state 0)
       (cell_xstate $row $colx $state)
       (empty)
    )  )     )  
	)


(= (get_row_stateyz_add_nums $row $xcol)   
     (match &self   ( sudoku_puzzle_state $row $colx $qua $state  )     (
      (if (> $state 0)
;       (cell_xstate $row $colx $state)
       (add-atom &self (tempnum $state ) )
       (empty)
    )  )     )  
    )




;!(set_connected_cell_values  2 9 ) 
;!(get_temp_nums)
;!(get_remaining_possible_candidates)
;!(length_list (get_remaining_possible_candidates ))

 ;(let*  (
 ;  ($e  (set_connected_cell_values  $r $c )  )
  ;    ($re (length_list (get_remaining_possible_candidates ) )   )
  ;   )
  ;   True 
  ; )


(= (count_list $x)
   (if (== $x ())
     0
     (+ (count_list (cdr-atom $x)) 1)))


;(= (check_cell $r $c )  
;(let*  ( 
;    ( $ty (remove_temp_nums) )
; ($ze (set_connected_cell_values  $r $c ) )
;   
;    )
;  True
;    )
;  )

(= (progn $body )
    (if (== $body ()) 
        ()
        
         (let* (
            (($head $rest) (decons-atom $body))
            
            ($r $head)
            (if (== $rest ())
                $r
                 (progn  $rest ))))  

        ))            


;    (get_column_stateyz_add_nums $r $c )      
;     (get_row_stateyz_add_nums $r $c )      
;    (get_quad_stateyz_add_nums (get_cell_quadn $r $c )   ) 

; (progn    (get_column_stateyz_add_nums $r $c )      
;     (get_row_stateyz_add_nums $r $c )      
;    (get_quad_stateyz_add_nums (get_cell_quadn $r $c )   )  )



;(= (check_cell $r $c )  
;(progn  
;(println "Check cell function   ")
;(remove_tmp_candidates)
;(add_tmp_candidates (collapse (init_candidates) ) )
;(remove_temp_nums)
; (set_connected_cell_values  cell_rc($r $c) ) 
; (remove_tmp_candidates (collapse  ( get_temp_nums_as_candidates ) ) )
; (count_list  (collapse (get_tmp_candidates) ) )
;  ) )

; action_rc


(= (init_action_stack  )  (
 
 (collapse (let*  (
  ($e1 (remove_a_stack) )
  ($e2 (add_a_stack (collapse (init_a_stack) ) ) )
;  ($e2 (add_a_stack  (init_a_stack)  ) )
;  ($e3 (remove_temp_nums) )
;  ($e4 (set_connected_cell_values  cell_rc($r $c) )  )
;  ($e5 (remove_tmp_candidates (collapse  ( get_temp_nums_as_candidates ) ) ) )
; (count_list  (collapse (get_tmp_candidates) ) )  
  ) 
 
;  (count_list  (collapse (get_tmp_candidates) ) )  
  ( True )

  )  )  ) )




(= (init_check_cell $r $c )  (
 
 (collapse (let*  (
  ($e1 (remove_tmp_candidates) )
  ($e2 (add_tmp_candidates (collapse (init_candidates) ) ) )
  ($e3 (remove_temp_nums) )
;  ($e4 (set_connected_cell_values  cell_rc($r $c) )  )
;  ($e5 (remove_tmp_candidates (collapse  ( get_temp_nums_as_candidates ) ) ) )
; (count_list  (collapse (get_tmp_candidates) ) )  
  ) 
 
;  (count_list  (collapse (get_tmp_candidates) ) )  
  True

  )  )  ) )



(= (check_cell $r $c )  (
 
 (collapse (let*  (
;  ($e1 (remove_tmp_candidates) )
;  ($e2 (add_tmp_candidates (collapse (init_candidates) ) ) )
;  ($e3 (remove_temp_nums) )
  ($e4 (set_connected_cell_values  cell_rc($r $c) )  )
  ($e5 (remove_tmp_candidates (collapse  ( get_temp_nums_as_candidates ) ) ) )
; (count_list  (collapse (get_tmp_candidates) ) )  
  ) 
 
;  (count_list  (collapse (get_tmp_candidates) ) )  
  True

  )  )  ) )




(= (check_violation (num_of_candidates $r $c $numx ) )  
    (if (== $numx 0 )
     ( is_violated )
     ( is_not_violated )
    )
 )


;  True
; (count_list  (collapse (get_tmp_candidates) ) )



; original 
;(= (score_cell (cell_rc $r $c) )  (
; (collapse (let*  (
;  ($e4 (init_check_cell  $r $c )  )
;  ($e5 ( check_cell  $r $c  ) )
;  ) 
; (num_of_candidates $r $c (count_list  (collapse (get_tmp_candidates) ) ) )
;  )  )  ) )


(= (score_cell (cell_rc $r $c) )  (
 (collapse (let*  (
  ($e4 (init_check_cell  $r $c )  )
  ($e5 ( check_cell  $r $c  ) )
  ($e6 (count_list  (collapse (get_tmp_candidates) ) ) )
; DEBUG  
  ($e7 (if  (== $e6 2 )    (add-atom &self (violated $r $c) )    True   ) )
;  ($e7 (if  (== $e6 0 )    (add-atom &self (violated $r $c) )    True   ) )
  ) 
; (num_of_candidates $r $c $e6 )
; DEBUG  
  (if  (== $e6 2 )   (violate_candidates $r $c $e6 )    (num_of_candidates $r $c $e6 )   )
;  (if  (== $e6 0 )   (violate_candidates $r $c $e6 )    (num_of_candidates $r $c $e6 )   ) 

  )  )  ) )

; TODO implement 
; use match  walk puzzle state row column , get candidates for that cell -score_cell (cell_rc $r $c), 
; subtract false_candidates

(= (find_level_where_there_were_alternatives)  (- (get_ycount) 1 )   )

; set_ycount
(= (backtrack_to_level_where_there_were_alternatives) 
 (collapse (let*  (
  ($ew (remove_violations) )
  ($tr (add-atom &self (violated 0 0 ) ) )
  ($e4 (find_level_where_there_were_alternatives )  )
  ($ex5 (set_ycount $e4 ) )

  ($e6 (backtrack_to_level $e4 ) )
  ($e7 (set_false_candidates $e4 ) )
  ($e9 (remove_trace $e4 ) )
    
  ) 
  True
  )  )
    )

; puzzle_state
; (= (all_empty_cells )    (match &self (sudoku_puzzle_state $r $c $quad $state)    
;   
;   (if (== $state 9)    (cell_rc $r $c)     (empty)    ) 
;      )    )   

(= (puzzle_is_complete )   
        (let $m (collapse (match &self (sudoku_puzzle_state $r $c $quad $state) 
;            True  
            (if (== $state 0)    (cell_rc $r $c)     (empty)    ) 
            )    )
        (if (== $m ())
            True
            False
            
        )    )         )



(= (exist_violation )   
        (let $m (collapse (match &self (violated $r $c)  (if (> $r 0) True False )  ))
        (if (== $m ())
            False
            True
        )
    )
             )



(= (if_exist_violation_backtrack_stack )  
    (if (== exist_violation True )
            (backtrack_to_level_where_there_were_alternatives)
            (print "Not exist violation" )
         

        )  

    )


; cons-atom
; add-atom

;!(init_check_cell 2 9)
;!(check_cell 2 9 )

;!(init_a_stack)

;!(collapse (all_empty_cells)  )
;!(get_row_stateyz 1 3)   


!(init_ycount)

; Make a Loop here 

;* !(update_cell_state 1 2 8)
;* !(get_row_stateyz 1 3)   
;*!(get_all_actions)
;* continue here 
;* 1.1.1.1


; THIS


!(loop_sudoku_until_complete (coninu 1 ) )

; OR THIS

;!(remove_violations)
;!(init_action_stack)
;!(apply_action_from_stack (collapse  (get_all_actions)  ) )
;!(score_cells  (collapse (all_empty_cells)  ) ) 
;!(if_exist_violation_backtrack_stack )
;!(puzzle_is_complete)


; !(exist_violation )

;!( check_violations  ( score_cells  (collapse (all_empty_cells)  )  )  )  
; !( match &self   (violated $r $c )   (violated $r $c )   )

; match violated


; !(update_cell_state 2 1 7)
;!(score_cells   (all_empty_cells)  ) 
;!(score_cell (cell_rc 2 9 ) )
;!(score_cell (cell_rc 2 3 ) )
;!(score_cell (cell_rc 1 4 ) )



; !(count_list  (collapse (get_tmp_candidates) ) )




;!(process_list (get_remaining_possible_candidates ))
;!(size-atom (collapse (get_remaining_possible_candidates ) ) )  
; !(increment_count )
;!(exist_temp_num 4) 



;!(update_cell_state 2 1 7)
; apply_action

;!(get_remaining_possible_candidates)
;!(get_temp_nums)


; OR THIS



;!(println "REMAINING candidates test for 0 0 ")
; !(remove_temp_nums) 
;!(check_cell 2 9 0  )
;!(length_list (get_remaining_possible_candidates )) 
;!(get_count)

;!(println "Quad ")
;!(get_quad_stateyz 5)

;!(println "Column ")
;!(get_column_stateyz 5 6 )  

;!(println "Row ")
;!(get_row_stateyz 5 6 )   

;(= (add_tmp_candidate (cell_candidate $num ) )  (add-atom &self (tmp_candidate $num) )  )
;(: map-expr (-> (-> $t $t) Expression Expression))
;tmp_candidate
;(= (add_tmp_candidates $expr)

;!(check_cell 2 9   )


;!(get_column_stateyz_add_nums 2 9 )      
;!(get_row_stateyz_add_nums 2 9 )      
;!(get_quad_stateyz_add_nums (get_cell_quadn 2 9 ) )    


; USE THIS -- does not work  This is the crucial  task  we want the code at: *1.0*   check_cell should be implemented
; above its been tried  in multple variation 

;!(remove_tmp_candidates) 
;!(add_tmp_candidates (collapse (init_candidates) ) ) 
;!(remove_temp_nums) 

;!(init_check_cell 2 9)
;!(check_cell 2 9 )
;!(count_list  (collapse (get_tmp_candidates) ) )



;*********
; match 
; !($cellqw (superpose (collapse (all_empty_cells) ) ) )
;!(println  (collapse (all_empty_cells) ) )
;!(println  $cellqw )


; *1.0*    THIS  works well   they should be grouped in a new function which can call it with row and column (check cell )
; *************
;!(remove_tmp_candidates)
;!(add_tmp_candidates (collapse (init_candidates) ) )
;!(println  (get_tmp_candidates) )

;!(println "REMAINING candidates test for 2 9 ")
;!(println (remove_temp_nums) )

;!(set_connected_cell_values  cell_rc( 2 9 ) ) 
       ;!(set_connected_cell_values  $cellqw ) 

;!(println "Sudoku values for cell:  2 9 ")
;!(println  (get_temp_nums )  )


;!(println  (get_tmp_candidates) )
;!(println "START REmove candidates1 ")
;!(remove_tmp_candidates (collapse  ( get_temp_nums_as_candidates ) ) )
;!(println "AFTER REmove candidates2 ")
;!(println  (get_tmp_candidates) )
;!(count_list  (collapse (get_tmp_candidates) ) )




;*******************************






;!(collapse (get_temp_nums_as_candidates ) )
;!(remove_tmp_candidates ( (cell_candidate 2) (cell_candidate 3) ) )
;!(remove_tmp_candidates (collapse  ( get_temp_nums_as_candidates ) ) )
;!(get_tmp_candidates)



;OR THIS  LASTTT

;!(println "REMAINING candidates test for 2 9 ")
;!(remove_temp_nums)
;!(set_connected_cell_values  2 9 ) 
;!(get_temp_nums)

;!(length_list (get_remaining_possible_candidates ))
;!(get_remaining_possible_candidates )
;!(println "get count1")
;!(get_count)








;!($qn (get_cell_quadn 2 9 ) )
;!($stx  (get_quad_stateyz $qn)  )
;!(println $stx) 
;!($e1 (add_temp_nums (get_quad_stateyz $qn) ) ) 
;!(get_temp_nums)

;!(get_temp_nums)
;!(get_remaining_possible_candidates)
;!($lex (get_remaining_possible_candidates ) )
;!(length_list  (collapse  $lex )  )



;!(length_list  $we   )
;!(count_list ( $we )  )

;!(let* ( ($we get_remaining_possible_candidates  )  ) (  $we  ) )   
;!(print $we )


;!(println "REMAINING candidates test for 3 4 ")
;!(init_count)
;!(remove_temp_nums)
;!(set_connected_cell_values  3 4 ) 
;!(get_temp_nums)
;!(get_remaining_possible_candidates)
;!(length_list (get_remaining_possible_candidates ))
;!(println "get count2")
;!(get_count)

;!(change-state &self (xcount 0 )   )

;!(try_cell_value (take_action_from_action_stack) )
;!(if_violation_backtrack_else_continue)

;!(println "count ")
;!(countlist (get_remaining_possible_candidates ))
   

; !(get_count)

;!(process_list (get_row_statey 2 ))
;!(println "xx 46-2 ")

;!(zipx (vx A (vx B ())) (vx 1 (vx 2 ())) ) 
;!(println "xx 47 ")
;!(zipy (vp A B C) (vu 1 2 3))  

;!(assertEqualToResult ( get_row_state 2 )  ( 4 4 4 )  ) 
;!(collapse  (  match &self (sudoku_puzzle_state $row 1 $state)    $state   ) )

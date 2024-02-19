; eval_80/6: Evaluates a Python function call within MeTTa.
; Parameters:
; - Eq: denotes get-type, match, or interpret call.
; - RetType: Expected return type of the MeTTa function.
; - Depth: Recursion depth or complexity control.
; - Self: Context or environment for the evaluation.
; - [MyFun|More]: List with MeTTa function and additional arguments.
; - RetVal: Variable to store the result of the Python function call.

  (= 
    (eval-80 $Eq $RetType $Depth $Self 
      (Cons  $MyFun $More) $RetVal) 
    (, 
      (metta-atom $Self 
        (:: registered-python-function $PyModule $PyFun $MyFun)) 
      (if-then-else 
        (, 
          (get-operator-typedef $Self $MyFun $Params $RetType) 
          (try-adjust-arg-types $RetType $Depth $Self 
            (Cons  $RetType $Params) 
            (Cons  $RetVal $More) 
            (Cons  $MVal $Adjusted))) True 
        (, 
          (maplist as-prolog $More $Adjusted) 
          (= $MVal $RetVal))) 
      (compound-name-arguments $Call $PyFun $Adjusted) 
      (if-trace 
        (or host python) 
        (print-tree (py-call (with_self  $PyModule $Call) $RetVal))) 
      (py-call 
        (with_self  $PyModule $Call) $MVal) 
      (check-returnval $Eq $RetType $RetVal))); MyFun as a registered Python function with its module and function name.
; Tries to fetch the type definition for MyFun, ignoring failures.
; Constructs a compound term for the Python function call with adjusted arguments.
; Optionally prints a debug tree of the Python call if tracing is enabled.
; Executes the Python function call and captures the result in MVal which propagates to RetVal.
; Checks the return value against the expected type and criteria.




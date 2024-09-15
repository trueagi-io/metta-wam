; This will be executed during import! (not `llm` call)
; We cannot use parent functions like (user-query) under !
; We may avoid ! if there was an import version that returns the result
; instead of binding it to a token

; This will actually be evaluated during `llm` call.
; `task` will be resolved and `if` will be reduced to one of the
; imported spaces. This space will be considered as a prompt
; template, and its messages will be in the common list of messages.
(if (== (task) ORDER)
    (Script nested_script_order.mps)
    (Script nested_script_greet.mps))

; Just add the user query to the list of messages
(user (user-query))

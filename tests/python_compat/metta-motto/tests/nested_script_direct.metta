!(import! &self motto)

(= (user-query) "Hi there")
(= (task) ORDER)

; user-query and task are not accessible during import!
; and are kept as symbols

; they are resolved by llm-gate in the call
!(assertEqual
   ((echo-agent) (Script nested_script_main.mps))
"system Help the user with lunch order
user Hi there")

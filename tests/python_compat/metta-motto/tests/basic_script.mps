; This is an example of passive script - it doesn't execute
; any code, but only stores the parameters for the llm call.
; This is an ordinary MeTTa code, but it is supposed to be passed
; to other agents, thus we use a different extension, mps (MeTTa prompt script)

; `user-query` is not defined in the script, but it will be evaluated
; in the context of the parent script, where it is expected to be defined
(user (user-query))

; The content of messages will be evaluated at each llm call, so
; they can contain some MeTTa code
; TODO: unfortunately, atm, it cannot contain functions defined in the script itself,
;       because the script is evaluated in the different context
(assistant
  (if (== (user-query) "Hello world.")
      "Hi there"
      "Bye"))


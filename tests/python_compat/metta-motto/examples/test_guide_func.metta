!(import! &self motto)

; Function for calls still cannot be put into the prompt space, because
; they will not be evaluated, when the functional call is evoked here
(= (doc calc_math)
   (Doc
     (description "You should call this function with a mathematical expression in Scheme")
     (parameters
      ((: expression Atom) "Mathematical expression in Scheme")
     ))
)
(: calc_math (-> Atom Expression Expression))
(= (calc_math ($expr) (Messages $1 (user $msg)))
   (As per user request $msg (quote $expr) = $expr))

(= (user-query) "What is the result of 111102 + 18333?")

!((chat-gpt-agent) (Script test_guide_prompt.mps))

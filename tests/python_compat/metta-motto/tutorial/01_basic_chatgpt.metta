!(import! &self motto)

; We can call ChatGPT Agent from MeTTa
!((chat-gpt-agent) (user "Hello"))
; With additional messages emulating the dialog history,
; the output will be different
!((chat-gpt-agent)
  (Messages (assistant "Hello") (user "Hello")))

; We can also pass function call to ChatGPT from MeTTa.
; It is done via providing a function doc in a special format.
(= (doc func)
   (Doc
     (description "Call this function to respond every user question")
     (parameters
       (user_query "user utterance")
     ))
)
; A dummy function just to show it is called
(= (func ("What is my name?") $msgs)
   "I don't know!")

; The result of llm will be a MeTTa expression, which will be evaluated futher
; The output "I don't know!" indicates that `func` was called
!((chat-gpt-agent)
  (Messages (system "Always perform the function call")
            (user "What is my name?"))
  (Functions func))

; The result can remain unreduced as well meaning that
; reduction happens on the MeTTa side (not LLM)
!((chat-gpt-agent)
  (Messages (system "Always perform the function call")
            (user "What is my surname?"))
  (Functions func))

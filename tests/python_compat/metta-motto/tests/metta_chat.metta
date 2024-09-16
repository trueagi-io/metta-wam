!(import! &self motto)

!(bind! &chat (dialog-agent chat_echo_history.msa))

; the metta_chat script outputs "Nice to meet you"
; if the history is empty
!(assertEqual (&chat (user "Ping"))
  "Nice to meet you")
; Now, the response will contain the history after
; the first call together with the present user message
!(assertEqual (&chat (user "Pong"))
"user Ping
assistant Nice to meet you
user Pong")

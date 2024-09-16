!(import! &self motto)

; A dialog agent, which calls another agent with the additional system message,
; which should not appear in the history
!(bind! &chat (dialog-agent nested_dialog_agent.msa))

!(assertEqual
  (&chat (user "Hello"))
  "Hello")

!(assertEqual
  (&chat (user "Who are you?"))
  "I am a test bot")

!(assertEqual
  (&chat (user "History"))
"user Hello
assistant Hello
user Who are you?
assistant I am a test bot")
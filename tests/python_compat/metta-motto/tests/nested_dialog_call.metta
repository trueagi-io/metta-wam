!(import! &self motto)

; A dialog agent, which calls another agent with the additional system message,
; which should not appear in the history
!(bind! &chat (Agent (metta-chat nested_dialog_agent.msa)))

!(assertEqual
  (llm &chat (user "Hello"))
  "Hello")

!(assertEqual
  (llm &chat (user "Who are you?"))
  "I am a test bot")

!(assertEqual
  (llm &chat (user "History"))
"user Hello
assistant Hello
user Who are you?
assistant I am a test bot")
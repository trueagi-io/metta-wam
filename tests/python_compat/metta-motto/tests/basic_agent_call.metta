!(import! &self motto)

(= (no-new-expression) True)

!(assertEqual
  ((metta-agent "basic_agent.msa") (user "Ping"))
  "assistant Pong")

; agent is recreated here, although it doesn't matter too much
; for this agent
!(assertEqual
  ((metta-agent "basic_agent.msa") (system "Ping"))
  "assistant gniP")

!(assertEqual (no-new-expression) True)

; metta script agent can also be used, although there is no
; difference for these agents
!(assertEqual
  ((metta-script-agent "basic_agentsc.msa") (user "Hello world."))
"user Hello world.
assistant Hi there")

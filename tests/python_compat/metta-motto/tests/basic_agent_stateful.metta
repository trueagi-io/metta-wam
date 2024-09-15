!(import! &self motto)

!(bind! &agent (metta-agent "basic_agent_stateful.msa"))

; The agent should change the response on the second call
!(assertEqual
  (&agent (Messages (user hello)))
  "Hello")
!(assertEqual
  (&agent (Messages (user hello)))
  "How can I assist you today?")

; Another agent should be created
!(assertEqual
  ((metta-agent "basic_agent_stateful.msa")
   (Messages (user hello)))
  "Hello")

!(bind! &agent-sc (metta-script-agent "basic_agent_stateful.msa"))

; metta-script-agent should not be used this way
; its code is executed after messages are received
!(assertEqual
  (&agent-sc (Messages (user hello)))
  "Hello")
!(assertEqual
  (&agent-sc (Messages (user hello)))
  "Hello")

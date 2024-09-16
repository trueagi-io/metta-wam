!(import! &self motto)

!(bind! &agent (metta-script-agent "basic_agent_script.msa"))

!(assertEqual
  (&agent (Messages (user "Hello")))
  "Hi there")

!(assertEqual
  (&agent (Messages (user "Hi")))
  "Hi")

!(assertEqual
  ((metta-script-agent "basic_agent_script.msa")
   (Messages (user "Hello")))
  "Hi there")

; metta-agent executes its code on creation - not on call
; while basic_agent_script processes messages at load time
!(assertEqual
  ((metta-agent "basic_agent_script.msa")
   (Messages (user "Hello")))
  "Incorrect call")

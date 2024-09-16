!(import! &self motto)

; Prompt templates can be provided via Script parameter to agents.
; This way, motto extention is automatically enabled for the script,
; and the script should not contain !(import! &self motto)
!((chat-gpt-agent) (Script 04_prompt_template.mps)
      (user "Compose an adventure about Flatland"))

; Since prompt templates are just spaces treated as parameters to agents,
; they can be created and filled in directly, but this is rarely needed.
!(bind! &space (new-space))
!(add-atom &space (system "Output the user utterance backward"))
!(add-atom &space (user "Window"))
!((chat-gpt-agent) (Script &space))

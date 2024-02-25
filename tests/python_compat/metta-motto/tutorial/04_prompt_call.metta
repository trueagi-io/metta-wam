!(import! &self motto)

; Prompt templates can be provides via Script parameter to llm.
; This way, motto extention is automatically enabled for the script,
; and it should not contain !(import! &self mott) to know agents,
; e.g., (chat-gpt), (EchoAgent), etc.
!(llm (Script 04_prompt_template.mps)
      (user "Compose an adventure about Flatland"))

; Since prompt templates are just spaces treated as parameters to llm,
; they can be created and filled in directly, but this is rarely needed.
!(bind! &space (new-space))
!(add-atom &space (system "Output the user utterance backward"))
!(add-atom &space (user "Window"))
!(llm (Script &space))

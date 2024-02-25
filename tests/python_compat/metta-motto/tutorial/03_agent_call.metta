!(import! &self motto)

(= (user-msg)
   (user "Can you calculate (109 plus 3) times 11?"))

; We can call both Metta and ChatGPT agents using generic llm function
! (llm (Agent (chat-gpt))
    (Messages
      (system ("Respond to the user taking into account that the answer is"
                (llm (Agent "02_metta_agent.msa") (user-msg))))
      (user-mgs)))

; Run this with and without system message.
(llm (Agent (chat-gpt))
  (Messages (system "Provide only a short answer")
            (user-msg)))

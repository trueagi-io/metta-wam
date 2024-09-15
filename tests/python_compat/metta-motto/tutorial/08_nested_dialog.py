from motto.agents import DialogAgent

def reply_and_print(agent, msg):
    print("USER: ", msg)
    print("AI: ", agent(msg).content)

agent = DialogAgent(code='''
    !(bind! &internal
       (dialog-agent
         ; inline definitions should be a quoted expression for response function
         (quote (= (response) ((chat-gpt-agent) (history) (messages))))
       )
     )
    ; Checking if the agent is required to return its history
    (= (response)
       (if (== (messages) (system HISTORY))
         (history)
         (superpose (
           (println! "======== PRELIMINARY ANSWER ========")
           ; We first execute the agent with the system and user message.
           (println! (&internal
              (system "Try to break down the problem into parts and perform the first step of reasoning. Do not provide the final answer - only the breakdown.")
              (messages)))
           ; This agent call relies on the history, because the original message is not passed
           (&internal (user "Continue your reasoning and provide the final answer"))
         ))
    )
    )
''')

#reply_and_print(agent, '''(user "
#     On the island there is a city of liars who always lie,
#     and a city of knights who always tell the truth.
#     The traveler met one of them at a fork in the road.
#     What should he ask to find out which road leads to the city of knights?")''')
reply_and_print(agent, '(user "Calculate (100 + 3) * (100 - 3)")')

# The reply will be the history containing only `user` and `assistant` messages
# of the top level DialogAgent
reply_and_print(agent, '(system HISTORY)')


!(let $model (new-state ((py-atom langchain_openai.ChatOpenAI) (Kwargs (model "gpt-3.5-turbo-0125") (temperature 0))))
    (let () (add-atom &self (= (langchain-model) $model)) (empty))
)

(= (get-langchain-model)
   (get-state (langchain-model))
)

(= (set-langchain-model $model)
   (let () (change-state! (langchain-model) $model) (empty))
)

; functions for langchain tools
(= (get-chat-prompt)
    ((py-dot (py-atom langchain_core.prompts.ChatPromptTemplate) from_messages) (py-list
        (
            (py-tuple ("system" "You are a helpful assistant."))
            (py-tuple ("placeholder" "{chat_history}"))
            (py-tuple ("user" "{input}"))
            ((py-atom langchain_core.prompts.MessagesPlaceholder) (Kwargs (variable_name "agent_scratchpad")))
        )
    ))
)
( = (get-langchain-agent $langchain-tools)
     ((py-atom langchain.agents.create_tool_calling_agent)
        (Kwargs ("llm" (get-langchain-model)) (tools  $langchain-tools) (prompt (get-chat-prompt)))
     )
)

;create agent executor and save
!(let $executor (new-state None)
   (let ()  (add-atom &self (= (langchain-agent-executor) $executor)) (empty))
 )

(= (get-langchain-agent-executor)
   (get-state (langchain-agent-executor))
)

(= (set-langchain-agent-executor $langchain-tools)
    (let ()
        (
            (change-state! (langchain-agent-executor)
                (
                    (py-atom langchain.agents.AgentExecutor)
                    (Kwargs (agent (get-langchain-agent $langchain-tools)) (tools $langchain-tools))
                )
            )
        )
    (empty))
)

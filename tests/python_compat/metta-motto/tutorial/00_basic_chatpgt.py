from motto.agents import ChatGPTAgent

# One can simply run a chatGPT agent directly, but it will provide
# no additional value from motto
agent = ChatGPTAgent()
print(agent([{'role': 'user', 'content': 'Hello'}]).content)
print(agent([{'role': 'assistant', 'content': 'Hello'},
             {'role': 'user', 'content': 'Hello'}
            ]).content)

# An example with a function call
r = agent([{'role': 'system', 'content': 'Always perform the function call'},
           {'role': 'user', 'content': 'What is my name?'}],
          [{"name": "func",
              "description": "Call this function to respond every user question",
              "parameters": {
                  "type": "object",
                  "properties": {
                      "user_query": {
                          "type": "string",
                          "description": "user utterance"
                      }
                  }}
              }]
          ).tool_calls[0].function
import json
# The function call is just an object
print(r.name, "(", json.loads(r.arguments), ")")

from motto.agents import MettaAgent

# There is MettaAgent, which can be used from Python.
# It should contain `response` function which will called.
# It can do anything (e.g. implement a rule-based system with intent recognition, etc.)
# `(messages)` is used by MettaAgent to get the input messages.
agent = MettaAgent(code='''
    (= (respond (user Ping)) "Pong")
    (= (response) (respond (messages)))
''')
print(agent('(user Ping)').content)

# But it can also call another agent including ChatGPT,
# which can include a function call, which will be evaluated in
# the MeTTa agent right away.
agent = MettaAgent(path='02_metta_agent.msa')

print(agent('(user "Calculate 109 times 11")').content)

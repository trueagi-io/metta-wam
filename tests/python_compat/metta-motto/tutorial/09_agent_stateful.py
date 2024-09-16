from motto.agents import DialogAgent

# Here, we check that the state of the agent is changed
# after the first call. The right answer is 'Liz',
# which is not known by LLM, and the agent can retrieve
# it, only if its internal state of the user name is changed.
agent = DialogAgent(path='09_agent_stateful.msa')
agent('(user "My name is Sam")')
print(agent('(user "Do you remember my friend?")').content)

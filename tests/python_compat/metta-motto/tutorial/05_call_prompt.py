from motto.agents import MettaScriptAgent

# We should use MettaScriptAgent agent here.
# See explanations in 05_prompt_agent.msa
agent = MettaScriptAgent(path='05_prompt_agent.msa')

agent('(user "kids studying programming")')

# We can call the agent for the second time.
# Its whole code will be rerun from scratch.
# agent('(user "futuristic techological singularity world")')

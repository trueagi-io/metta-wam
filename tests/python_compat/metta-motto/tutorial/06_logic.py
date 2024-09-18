from motto.agents import ChatGPTAgent, MettaAgent
from hyperon import ValueAtom
import re

# NOTE: This is a PoC example of LLM-MeTTa interoperability.
# It can be changed in the future and become a core agent.
# It's implementation can be quite different: adding a message or function call,
# inheriting from ChatGPT or MeTTa agents, doing postprocessing by itself
# or relying on eval by the parent agent, etc.

class LogicAgent(ChatGPTAgent):

    def __call__(self, messages, functions=[]):
        instruct = '''
Any user question should always be represented as a logical query in Prolog,
but converted into Scheme/Lisp syntax. Output only the query expression itself without quoting.
'''
        ext_msgs = messages[:-1] + \
           [{'role': 'system', 'content': instruct}] + \
           [messages[-1]]
        result = super().__call__(ext_msgs, functions)
        pattern = r"\?\w+"
        matches = re.findall(pattern, result.content)
        result.content = result.content.replace('?', '$')
        vars = ''
        for m in matches:
            vars += m.replace('?', '$') + ' '
        if len(matches) == 0:
            vars = 'True'
        elif len(matches) > 1:
            vars = '(' + vars + ')'
        result.content = f"(if {result.content} {vars} False)"
        return result

agentL = LogicAgent()
# Without MeTTa support, this agent will simply return an expression as a string
print(agentL([{'role': 'user', 'content': "Who is mortal?"}]).content)

# Let's add some MeTTa code
agent_code = '''
(= (human Socrates) True)
(= (human Plato) True)
(= (mortal $x) (human $x))
(= (response) (_eval (llm (Agent LogicAgent) (messages))))
'''
# When the agent doesn't change its space and needs the changes
# to preserve between calls and doesn't execute commands at
# load time, we can use any of MettaAgent and MettaScriptAgent
# However, MettaAgent is more efficient, since its code will
# be loaded only once
agentM = MettaAgent(code=agent_code,
                    atoms={"LogicAgent": ValueAtom(agentL)})

print(agentM('(user "Who is mortal?")').content)
print(agentM('(user "Who is human?")').content)

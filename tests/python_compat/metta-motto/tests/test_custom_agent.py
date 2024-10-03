from hyperon import MeTTa, ValueAtom
from motto.agents import Agent, Response

class CustomSplitAgent(Agent):
    def __call__(self, msgs, functions, word=None):
        if word is None:
            return Response("I need a word to search")
        if word in msgs[0]['content']:
            return Response(msgs[0]['content'].split(word)[-1])
        return Response("Fail")


def test_custom_agent():
    agent = CustomSplitAgent()
    m = MeTTa()
    m.register_atom('&agent', ValueAtom(agent))
    m.run("!(import! &self motto)")
    assert m.run('''
        !(llm (Agent &agent) (user "Hello"))
        !(llm (Agent &agent (word "name is ")) (user "My name is Name"))
        !(llm (Agent &agent (word "abracadabra")) (user "My name is Name"))
    ''') == [
        [ValueAtom("I need a word to search")],
        [ValueAtom("Name")],
        [ValueAtom("Fail")]]

if __name__ == '__main__':
    test_custom_agent()


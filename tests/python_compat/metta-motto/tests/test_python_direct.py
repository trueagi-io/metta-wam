from hyperon import MeTTa, ValueAtom, E, S
from motto.llm_gate import llm
from motto.agents import EchoAgent, MettaScriptAgent, DialogAgent

def test_python_metta_direct():
    m = MeTTa()
    # we can run metta code from python directly and motto works
    m.run('!(import! &self motto)')
    assert m.run('!((metta-agent basic_agent.msa) (user "Ping"))') == \
        [[ValueAtom("assistant Pong")]]

def test_python_echo_agent():
    # calling agents from Python should be enough
    a = EchoAgent()
    assert a([{'role': 'user', 'content': 'Ping'}]).content == 'user Ping'

def test_python_metta_agent():
    # we can run metta agent directly (also from code string)
    a = MettaScriptAgent(code = '''
    (= (proc-messages (user "Ping")) (assistant "Pong"))
    (= (response) ((echo-agent) (proc-messages (messages))))
    ''')
    # MeTTa agents return atoms for better composability with other agents
    assert a('(user "Ping")').content == [ValueAtom("assistant Pong")]
    # we can also call llm directly, but the main purpose of llm is to unwrap atoms
    # for the agent call, so it usually makes more sense to call the agent directly
    # but we do this here for the testing purpose
    m = MeTTa()
    msgs_atom = m.parse_single('(user "Ping")')
    assert llm(m, msgs_atom, E(S('Agent'), ValueAtom(a))) == \
        [ValueAtom("assistant Pong")]

def test_python_metta_dialog():
    a = DialogAgent(code = '''
    (= (proc-messages (user "Recall")) (history))
    (= (proc-messages (user "Echo")) (messages))
    (= (response) ((echo-agent) (proc-messages (messages))))
    ''')
    assert a('(user "Echo")').content == [ValueAtom('user Echo')]
    assert a('(user "Recall")').content == [ValueAtom('user Echo\nassistant user Echo')]

def test_python_metta_dialog_clear_histoy():
    a = DialogAgent(code = '''
    (= (proc-messages (user "Recall")) (history))
    (= (proc-messages (user "Echo")) (messages))
    (= (response) ((echo-agent) (proc-messages (messages))))
    ''')
    assert a('(user "Echo")').content == [ValueAtom('user Echo')]
    assert a('(user "Recall")').content == [ValueAtom('user Echo\nassistant user Echo')]
    a.clear_history()
    assert a('(user "Recall")').content == [ValueAtom('')]

if __name__ == '__main__':
    test_python_metta_direct()
    test_python_metta_agent()
    test_python_metta_dialog()
    test_python_metta_dialog_clear_histoy()


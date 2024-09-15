from motto.agents import DialogAgent
from hyperon import ExpressionAtom, OperationAtom, ValueAtom, E, S
from hyperon.ext import register_atoms
from motto.llm_gate import AgentCaller

# from hyperon.stdlib import get_py_atom
# from hyperon import G, AtomType, OperationObject, ValueObject
# default_model = E(E(OperationAtom("py-atom", get_py_atom, unwrap=False), S("langchain_openai.ChatOpenAI")),
#                   E(S("Kwargs"), E(S("model"), G(ValueObject("gpt-3.5-turbo-0125"))),
#                     E(S("temperature"), G(ValueObject(0)))))


class LangchainAgent(DialogAgent):

    def __init__(self, model, path=None, code=None, atoms={}, include_paths=None):

        self.history = []
        self.model = model
        super().__init__(path, code, atoms, include_paths)
        self._metta.space().add_atom(E(S('='), E(S('langchain-model')),
                                 self.model))


@register_atoms(pass_metta=True)
def langchaingate_atoms(metta):
    langchainAtom = OperationAtom('langchain-agent',
      lambda *args: [
          OperationAtom('langchain', AgentCaller(metta, LangchainAgent, unwrap=False, *args),  unwrap=False)],
      type_names=['Atom', '%Undefined%', 'Atom'], unwrap=False)
    return {
        r"langchain-agent": langchainAtom,

    }

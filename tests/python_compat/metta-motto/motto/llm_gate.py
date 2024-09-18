from hyperon import *
from hyperon.ext import register_atoms
from .agents import *
import json
from .utils import *
import importlib.util


import logging
logger = logging.getLogger(__name__)

__default_agent = None

def to_nested_expr(xs):
    if isinstance(xs, list):
        return E(*list(map(to_nested_expr, xs)))
    if isinstance(xs, dict):
        return E(*[E(to_nested_expr(k), to_nested_expr(v)) for k, v in xs.items()])
    return xs if isinstance(xs, Atom) else ValueAtom(xs)

def atom2msg(atom):
    if isinstance(atom, ExpressionAtom):
        # Avoid () in Expression representation
        txt = ""
        for ch in atom.get_children():
            txt += atom2msg(ch) + " "
        return txt[:-1] + "\n"
    if isinstance(atom, GroundedAtom):
        if isinstance(atom.get_grounded_type(), ExpressionAtom):
            return repr(atom)
        if isinstance(atom.get_object(), ValueObject):
            # Parse String separately to avoid "" in its repr
            v = atom.get_object().value
            if isinstance(v, str):
                return v.replace("\\n", "\n")
    return repr(atom)

def get_func_def(fn, metta, prompt_space):
    doc = None
    if prompt_space is not None:
        # TODO: Querying for a function description in prompt_space works well,
        # but it is useless, because this function cannot be called
        # from the main script, so the functional call is not reduced.
        # Fixing this requires in general better library management in MeTTa,
        # although it can be managed here by interpreting the functional call expression.
        # Another approach would be to have load-template, which will import all functions to &self
        # (or just to declare function in separate files and load to self, since we may want them
        # to be reusable between templates)
        r = prompt_space.query(E(S('='), E(S('doc'), fn), V('r')))
        if not r.is_empty():
            doc = r[0]['r']
    if doc is None:
        # We use `match` here instead of direct `doc` evaluation
        # to evoid non-reduced `doc`
        doc = metta.run(f"! (match &self (= (doc {fn}) $r) $r)")
        if len(doc) == 0 or len(doc[0]) == 0:
            raise RuntimeError(f"No {fn} function description")
        doc = doc[0][0]
    # TODO: format is not checked
    doc = doc.get_children()
    properties = {}
    for par in doc[2].get_children()[1:]:
        p = par.get_children()
        # NOTE: metta-type is passed to an LLM as well, which is tolerated atm
        prop = {"type": "string",
                "description": p[1].get_object().value,
                "metta-type": 'String',
               }
        if isinstance(p[0], ExpressionAtom):
            par_typ = p[0].get_children()
            if repr(par_typ[0]) != ':':
                raise TypeError(f"{p[0]} can only be a typing expression")
            par_name = repr(par_typ[1])
            prop["metta-type"] = repr(par_typ[2])
        else:
            par_name = p[0].get_name()
        if len(p) > 2:
            # FIXME? atom2msg or repr or ...?
            prop["enum"] = list(map(lambda x: atom2msg(x), p[2].get_children()))
        properties.update({par_name: prop})
    # FIXME: This function call format is due to ChatGPT. It seems like an excessive
    # wrapper here and might be reduced (and extended in the gpt-agent itself).
    return {
        "name": fn.get_name(),
        "description": doc[1].get_children()[1].get_object().value,
        "parameters": {
            "type": "object",
            "properties": properties
        }
    }

def is_space(atom):
    return isinstance(atom, GroundedAtom) and \
           isinstance(atom.get_object(), SpaceRef)


def get_llm_args(metta: MeTTa, prompt_space: SpaceRef, *args):
    agent = None
    messages = []
    functions = []
    msg_atoms = []
    def __msg_update(ag, m, f, a):
        nonlocal agent, messages, functions, msg_atoms
        if ag is not None:
            agent = ag
        messages += m
        functions += f
        msg_atoms += [a]

    for atom in args:
        # We first interpret the atom argument in the context of the main metta space.
        # If the prompt template is in a separate file and contains some external
        # symbols like (user-query), they will be resolved here.
        # It is useful for messages, agents, as well as arbitrary code, which relies
        # on information from the agent.
        # TODO: we may want to do something special with equalities
        #arg = interpret(metta.space(), atom)
        arg = metta.evaluate_atom(atom)
        # NOTE: doesn't work now since Error inside other expressions is not passed through them
        #       but it can be needed in the future
        #if isinstance(arg, ExpressionAtom) and repr(arg.get_children()[0]) == 'Error':
        #    raise RuntimeError(repr(arg.get_children()[1]))
        arg = atom if len(arg) == 0 else arg[0]
        if is_space(arg):
            # Spaces as prompt templates should be wrapped into Script argument
            continue
        elif isinstance(arg, ExpressionAtom):
            ch = arg.get_children()
            if len(ch) > 1:
                name = ch[0].get_name()
                if name == 'Messages':
                    __msg_update(*get_llm_args(metta, prompt_space, *ch[1:]))
                elif name in ['system', 'user', 'assistant', 'media']:
                    messages += [{'role': name, 'content': atom2msg(ch[1])}]
                    msg_atoms += [arg]
                elif name in ['Functions', 'Function']:
                    functions += [get_func_def(fn, metta, prompt_space)
                                  for fn in ch[1:]]
                elif name == 'Script':
                    if is_space(ch[1]):
                        # FIXME? This will overwrites the current prompt_space if it is set.
                        # It is convenient to have it here to successfully execute
                        # (llm &prompt (Functions fn)), when fn is defined in &prompt.
                        # But (Function fn) can also be put in &prompt directly.
                        # Depending on what is more convenient, this overriding can be changed.
                        prompt_space = ch[1].get_object()
                    else:
                        # TODO: a better way to load a script?
                        m = MeTTa()
                        # TODO: asserts
                        m.run("!(import! &self motto)")
                        with open(atom2msg(ch[1])) as f:
                            m.run(f.read())
                        prompt_space = m.space()
                    __msg_update(*get_llm_args(metta, prompt_space, *prompt_space.get_atoms()))
                elif name == 'Agent':
                    agent = ch[1]
                    # The agent can be a Python object or a string (filename)
                    if isinstance(agent, GroundedAtom):
                        agent = agent.get_object().content
                    elif isinstance(agent, SymbolAtom):
                        agent = agent.get_name()
                    else:
                        raise TypeError(f"Agent {agent} is not identified")
                    params = {}
                    for param in ch[2:]:
                        ps = param.get_children()
                        params[repr(ps[0])] = ps[1]
                    agent = (agent, params)
                elif name == 'Kwargs':
                    params = {}
                    for param in ch[1:]:
                        ps = param.get_children()
                        params[repr(ps[0])] = ps[1]
                    if not isinstance(agent, tuple):
                        agent = (agent, params)
                    else:
                        agent[1].update(params)
                elif name == '=':
                    # We ignore equalities here: if a space is used to store messages,
                    # it can contain equalities as well (another approach would be to
                    # ignore everythins except valid roles)
                    continue
                else:
                    raise RuntimeError("Unrecognized argument: " + repr(arg))
            else:
                # Ignore an empty expression () for convenience, but we need
                # to put it back into msg_atoms to keep the structure
                msg_atoms += [arg]
        else:
            raise RuntimeError("Unrecognized argument: " + repr(arg))
    # Do not wrap a single message into Message (necessary to avoid double
    # wrapping of single Message argument)
    return agent, messages, functions, \
        msg_atoms[0] if len(msg_atoms) == 1 else E(S('Messages'), *msg_atoms)

def get_response(metta,agent,  response, functions, msgs_atom):
    if not hasattr(response, "tool_calls"):
        # if response is stream
        return [ValueAtom(response)]
    if response.tool_calls is not None:
        result = []
        for tool_call in response.tool_calls:
            fname = tool_call.function.name
            fs = S(fname)
            args = tool_call.function.arguments if isinstance(tool_call.function.arguments, dict) else json.loads(
                tool_call.function.arguments)
            args = {} if args is None else \
                json.loads(args) if isinstance(args, str) else args
            # Here, we check if the arguments should be parsed to MeTTa
            for func in functions:
                if func["name"] != fname:
                    continue
                for k, v in args.items():
                    if func["parameters"]["properties"][k]['metta-type'] == 'Atom':
                        args[k] = metta.parse_single(v)
            result.append(repr(E(fs, to_nested_expr(list(args.values())), msgs_atom)))
        res = f"({' '.join(result)})" if len(result) > 1 else result[0]
        val = metta.parse_single(res)
        return [val]
    return response.content if isinstance(agent, MettaAgent) else \
        [ValueAtom(response.content)]

def llm(metta: MeTTa, *args):
    try:
        agent, messages, functions, msgs_atom= get_llm_args(metta, None, *args)
    except Exception as e:
        # NOTE: we put the error into the log since it can be ignored by the caller
        logger.error(e)
        # return [E(S("Error"), ValueAtom(str(e)))]
        raise e
    if agent is None:
        agent = __default_agent
        params = {}
    else:
        (agent, params) = agent
    if isinstance(agent, str):
        # NOTE: We could pass metta here, but it is of no use atm
        agent = MettaScriptAgent(agent)
    if not isinstance(agent, Agent):
        raise TypeError(f"Agent {agent} should be of Agent type. Got {type(agent)}")
    if not isinstance(agent, MettaAgent):
        for p in params.keys():
            if not isinstance(params[p], GroundedAtom):
                raise TypeError(f"GroundedAtom is expected as input to a non-MeTTa agent. Got type({params[p]})={type(params[p])}")
            params[p] = params[p].get_object().value
    try:
        response = agent(msgs_atom if isinstance(agent, MettaAgent) else messages,
                        functions, **params)
    except Exception as e:
        logger.error(e)
        raise e
    return get_response(metta, agent, response, functions, msgs_atom )

class AgentCaller:

    def __init__(self, metta, agent_class, *args, unwrap=True):
        self.metta = metta
        if unwrap:
            self.agent_atom = OperationAtom("_", agent_class).get_object().execute(*args)[0]
        else:
            self.agent_atom = OperationAtom(str(agent_class), agent_class(*args))

    def __call__(self, *args):
        return llm(self.metta, E(S('Agent'), self.agent_atom), *args)


@register_atoms(pass_metta=True)
def llmgate_atoms(metta):
    global __default_agent
    __default_agent = ChatGPTAgent()
    llmAtom = OperationAtom('llm', lambda *args: llm(metta, *args), unwrap=False)
    # Just a helper function if one needs to print from a metta-script
    # the message converted from expression to text
    msgAtom = OperationAtom('atom2msg',
                    lambda atom: [ValueAtom(atom2msg(atom))], unwrap=False)
    containsStrAtom = OperationAtom('contains-str', lambda a, b: [ValueAtom(contains_str(a, b))], unwrap=False)
    concatStrAtom = OperationAtom('concat-str', lambda a, b: [ValueAtom(concat_str(a, b))], unwrap=False)
    message2tupleAtom = OperationAtom('message2tuple', lambda a: [ValueAtom(message2tuple(a))], unwrap=False)
    result = {
        r"llm": llmAtom,
        r"atom2msg": msgAtom,
        # FIXME: We add this function here, so we can explicitly evaluate results of LLMs, but
        # we may either expect that this function appear in core MeTTa or need a special safe eval
        r"_eval": OperationAtom("_eval",
                                lambda atom: metta.run("! " + atom.get_object().value)[0],
                                unwrap=False),
        r"contains-str": containsStrAtom,
        r"concat-str": concatStrAtom,
        r"message2tuple": message2tupleAtom

    }
    if importlib.util.find_spec('anthropic') is not None:
        result[r"anthropic-agent"] = OperationAtom('anthropic',
        lambda *args: [OperationAtom('anthropic-agent', AgentCaller(metta, AnthropicAgent, *args), unwrap=False)],
        unwrap=False)
    if importlib.util.find_spec('tiktoken') is not None:
        if (importlib.util.find_spec('bs4') is not None) \
                and (importlib.util.find_spec('markdown') is not None):
            result[r"retrieval-agent"] = OperationAtom('retrieval-agent',
        lambda *args:  [OperationAtom('retrieval', AgentCaller(metta, RetrievalAgent,  unwrap=True,*args), unwrap=False)], unwrap=False)

    chatGPTAgentAtom = OperationAtom('chat-gpt-agent',
        lambda *args: [OperationAtom('chat-gpt', AgentCaller(metta, ChatGPTAgent, *args), unwrap=False)],
        unwrap=False)
    result[r"chat-gpt-agent"] = chatGPTAgentAtom
    meTTaScriptAtom = OperationAtom('metta-script-agent',
        lambda *args: [OperationAtom('msa', AgentCaller(metta, MettaScriptAgent, unwrap=False, *args), unwrap=False)],
        unwrap=False)
    result[r"metta-script-agent"] = meTTaScriptAtom
    meTTaAgentAtom = OperationAtom('metta-agent',
        lambda *args: [OperationAtom('ma', AgentCaller(metta, MettaAgent, unwrap=False, *args), unwrap=False)],
        unwrap=False)
    result[r"metta-agent"] = meTTaAgentAtom
    dialogAgentAtom = OperationAtom('dialog-agent',
        lambda *args: [OperationAtom('mda', AgentCaller(metta, DialogAgent, unwrap=False, *args), unwrap=False)],
        unwrap=False)
    result[r"dialog-agent"] = dialogAgentAtom
    echoAgentAtom = OperationAtom('echo-agent',
        lambda *args: [OperationAtom('EchoAgent', AgentCaller(metta, EchoAgent, *args), unwrap=False)],
        unwrap=False)
    result[r"echo-agent"] = echoAgentAtom
    result[r"open-router-agent"] = OperationAtom('open-router-agent',
                        lambda *args: [OperationAtom('ora', AgentCaller(metta, OpenRouterAgent, *args), unwrap=False)],
                        unwrap=False)

    return result

def str_find_all(str, values):
    return list(filter(lambda v: v in str, values))


@register_atoms
def postproc_atoms():
    strfindAtom = OperationAtom('str-find-all', str_find_all)
    return {
        r"str-find-all": strfindAtom,
    }

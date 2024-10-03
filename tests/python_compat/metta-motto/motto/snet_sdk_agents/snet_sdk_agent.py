from motto import get_string_value

from motto.agents import Agent
from hyperon import ExpressionAtom, OperationAtom, ValueAtom, E, S, V, MeTTa, Environment, GroundingSpaceRef, G
from hyperon.ext import register_atoms
from hyperon.exts import snet_io
from motto.llm_gate import AgentCaller
from motto.agents import Response


class SnetSDKAgent(Agent):

    def _init_metta(self):
        ### =========== Creating MeTTa runner ===========
        # NOTE: each MeTTa agent uses its own space and runner,
        # which are not inherited from the caller agent. Thus,
        # the caller space is not directly accessible as a context.
        if self._include_paths is not None:
            env_builder = Environment.custom_env(include_paths=self._include_paths)
            metta = MeTTa(env_builder=env_builder)
        else:
            metta = MeTTa()
        # TODO: assert
        metta.run("!(import! &self motto) \n !(import! &self  snet_io)")
        # Externally passed atoms for registrations

        self._metta = metta

    def __init__(self, org_id, service_id, method_args, kwargs=None, include_paths=None):

        self.history = []
        self.org_id = get_string_value(org_id)
        self.service_id = get_string_value(service_id)
        self._include_paths = include_paths
        self._init_metta()
        self._context_space = None
        self.method_args = method_args.get_object().value
        # create create_service_client and space with methods generated for given service
        wrapper = snet_io.SNetSDKWrapper()
        wrapper.init_sdk()
        sp = wrapper.create_service_space(self.org_id, self.service_id, **kwargs.get_object().value) if kwargs is not None else wrapper.create_service_space(self.org_id, self.service_id)
        self.service_space = sp[0]
        self._metta.space().add_atom(self.service_space)



    def _prepare(self, msgs_atom):
        # The context space is recreated on each call
        if self._context_space is not None:
            self._metta.space().remove_atom(self._context_space)
        self._context_space = G(GroundingSpaceRef())
        self._metta.space().add_atom(self._context_space)
        context_space = self._context_space.get_object()
        message = ""
        #add user message to "(query)"
        for msg in msgs_atom:
            if 'content' in msg:
                message += msg['content'] + " "
        context_space.add_atom(E(S('='), E(S('query')), ValueAtom(message)))

    def __call__(self, msgs_atom, functions=[]):
        # TODO: support {'role': , 'content': } dict input
        if isinstance(msgs_atom, str):
            msgs_atom = self._metta.parse_single(msgs_atom)
        self._prepare(msgs_atom)
        args = []
        for k, v in self.method_args.items():
            args.append(S(get_string_value(v)))
        # call the method of service with initialised (query)
        response = self._metta.run(f'!{E(*args)}')
        return self._postproc(response[0])

    def _postproc(self, response):
        # No postprocessing is needed here
        return Response(response, None)


@register_atoms(pass_metta=True)
def snet_sdk_atoms(metta):
    sdkAtom= OperationAtom('snet-sdk-agent',
      lambda *args: [
          OperationAtom('snet-sdk-agnt', AgentCaller(metta, SnetSDKAgent, unwrap=False, *args),  unwrap=False)], unwrap=False)
    return {
        r"snet-sdk-agent": sdkAtom,

    }

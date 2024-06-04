from .agent import Response, Agent, EchoAgent
from .metta_agent import MettaAgent, DialogAgent
from .gpt_agent import ChatGPTAgent
import importlib.util
if (importlib.util.find_spec('bs4') is not None) \
        and (importlib.util.find_spec('tiktoken') is not None)\
        and (importlib.util.find_spec('markdown') is not None):
    from .retrieval_agent import RetrievalAgent

if importlib.util.find_spec('anthropic') is not None:
    from .anthropic_agent import AnthropicAgent

from .agent import Response, Agent, EchoAgent
from .metta_agent import MettaScriptAgent, MettaAgent, DialogAgent
from .gpt_agent import ChatGPTAgent
from .openrouter_agent import OpenRouterAgent
import importlib.util
if importlib.util.find_spec('tiktoken') is not None:
    if (importlib.util.find_spec('bs4') is not None) \
            and (importlib.util.find_spec('markdown') is not None):
        from .retrieval_agent import RetrievalAgent

if importlib.util.find_spec('anthropic') is not None:
    from .anthropic_agent import AnthropicAgent

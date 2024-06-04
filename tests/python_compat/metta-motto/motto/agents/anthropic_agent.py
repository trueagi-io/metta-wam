from .agent import Agent, Response
import httpx
import os
import logging
import time
import importlib.util

# FIXME: A more flexible was to setup proxy?
proxy = os.environ.get('OPENAI_PROXY')
if importlib.util.find_spec('anthropic') is not None:
    import anthropic

    client = anthropic.Anthropic() if proxy is None else \
        anthropic.Anthropic(http_client=httpx.Client(proxies=proxy))


    class AnthropicAgent(Agent):

        def __init__(self, model="claude-3-opus-20240229"):
            self._model = model
            self.log = logging.getLogger(__name__ + '.' + type(self).__name__)

        def run_insists(self, **kwargs):
            response = None
            while response is None:
                try:
                    response = client.messages.create(**kwargs)
                except anthropic.RateLimitError as e:
                    self.log.debug(f"Error: {e}")
                    self.log.debug(f"Error: {type(e)}")
                    self.log.debug("RETRY!")
                    time.sleep(10)
            return response

        def __call__(self, messages, functions=[]):
            if functions == []:
                response = self.run_insists(model=self._model,
                                            messages=get_messages_no_system(messages),
                                            system=get_system(messages),
                                            max_tokens=1024,
                                            temperature=0,
                                            timeout=15)
            else:
                raise Exception("We do not support functional calls with Anthropic models")
            return Response(response.content[0].text)



def get_system(messages):
    return "\n".join(m["content"] for m in messages if m["role"] == "system")


def get_messages_no_system(messages):
    return [m for m in messages if m["role"] != "system"]

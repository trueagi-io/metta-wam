from openai import OpenAI
from .metta_agent import Agent
import httpx
import os
import json
from .messages_processor import MessagesProcessor

# FIXME: A more flexible was to setup proxy?
proxy = os.environ.get('OPENAI_PROXY')
key = os.environ.get('OPENAI_API_KEY')
if key is not None or True:
    client = OpenAI() if proxy is None else \
        OpenAI(http_client=httpx.Client(proxies=proxy))

class ChatGPTAgent(Agent):
    '''
    GPT agent with a cut_history parameter to ensure the length of the current context window stays within the model's allowed limit
    '''

    def __init__(self, model="gpt-3.5-turbo", stream=False, cut_history=False,  timeout=15, max_response_tokens=512):
        self._model = model
        self.stream_response = stream
        self.timeout = timeout
        self.max_response_tokens = max_response_tokens
        self.messages_processor = MessagesProcessor(self._model, self.max_response_tokens, cut_history)

    def __call__(self, messages, functions=[]):
        messages = self.messages_processor.process_messages(messages)

        if functions == []:
            response = client.chat.completions.create(model=self._model,
                                                      messages=messages,
                                                      temperature=0,
                                                      timeout=self.timeout,
                                                      stream=self.stream_response,
                                                      max_tokens=self.max_response_tokens)
            return response.choices[0].message if not self.stream_response else response
        tools = []
        for func in functions:
            dict_values = {}
            dict_values["type"] = "function"
            dict_values["function"] = func
            tools.append(dict_values)
        response = client.chat.completions.create(model=self._model,
                                                  messages=messages,
                                                  tools=tools,
                                                  temperature=0,
                                                  tool_choice="auto",
                                                  timeout=self.timeout,
                                                  max_tokens=self.max_response_tokens)
        # FIXME? Only one result is supposed now. API can be changed later if it turns out to be needed.
        return response.choices[0].message

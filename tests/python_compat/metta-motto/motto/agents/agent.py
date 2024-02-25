class FunctionCall:
    def __init__(self, name, arguments):
        self.name = name
        self.arguments = arguments

class Response:
    def __init__(self, content, function_call=None):
        self.content = content
        self.function_call = function_call
    def __repr__(self):
        return f"Response(content: {self.content}, function_call: {self.function_call})"


class Agent:

    def __init__(self):
        pass

    def __call__(self, messages, functions):
        raise NotImplementedError(
            f"__call__(self, messages, functions) for {self.__class__.__name__} should be defined"
        )


class EchoAgent(Agent):

    def __call__(self, messages, functions=[]):
        msg = list(map(lambda m: m['role'] + ' ' + m['content'], messages))
        msg = '\n'.join(msg)
        fcall = None
        # A mock function call processing for testing purposes
        for f in functions:
            vcall = None
            if f['description'] in msg:
                prop = f['parameters']['properties']
                for k in prop:
                    if 'enum' in prop[k]:
                        for v in prop[k]['enum']:
                            if prop[k]['description'] + v in msg:
                                vcall = {k: v}
                fcall = FunctionCall(f['name'], vcall)
        return Response(msg, fcall)

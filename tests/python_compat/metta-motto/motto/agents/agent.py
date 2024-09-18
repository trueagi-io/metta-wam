class Function:
    def __init__(self, name, arguments, format=""):
        self.name = name
        self.forma = format
        self.arguments = arguments

class ToolCall:
    def __init__(self, id, function, type="function"):
        self.id = id
        self.function = function
        self.type = type

class Response:
    def __init__(self, content, functions=None):
        self.content = content
        self.tool_calls = None
        if functions is not None:
            k = 1
            self.tool_calls = []
            for f in functions:
                self.tool_calls.append(ToolCall(k, f))
                k =+ 1

    def __repr__(self):
        return f"Response(content: {self.content}, tool_calls: {self.tool_calls})"


class Agent:

    def __init__(self):
        pass

    def __call__(self, messages, functions):
        raise NotImplementedError(
            f"__call__(self, messages, functions) for {self.__class__.__name__} should be defined"
        )


class EchoAgent(Agent):

    def __call__(self, messages, functions=[], user_name=[]):
        msg = list(map(lambda m: m['role'] + ' ' + m['content'], messages))
        if user_name:
            msg.append(f'your name is {user_name}')
        msg = '\n'.join(msg)
        fcall = [] if len(functions) > 0 else None
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
                fcall.append(Function(f['name'], vcall))
        return Response(msg, fcall)

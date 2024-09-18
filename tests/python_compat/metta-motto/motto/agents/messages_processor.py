import json


def get_max_tokens(model_name):
    if model_name == "gpt-3.5-turbo":
        return 10000  # real limit 16385
    if model_name == "gpt-4-turbo-preview":
        return 10000  # Limit max number of tokens to reduce cost (real limit 128000)
    if model_name == "gpt-4o" or model_name == "gpt-4-turbo":
        return 10000  # Set max number of tokens to reduce cost (real limit 128000)
    raise Exception("Unknown model name")


class MessagesProcessor:
    def __init__(self, model_name, max_response_tokens, cut_history):
        self.model_name = model_name
        self.max_tokens = get_max_tokens(self.model_name) - max_response_tokens
        # we need tokenizer only to calculate number of tokens and cut dialog history if needed
        self.cut_history = cut_history
        if self.cut_history:
            import tiktoken
            self.encoder = tiktoken.encoding_for_model(self.model_name)

    def process_messages(self, messages):
        if self.cut_history:
            messages = self.cut_dialog_history(messages)
        new_messages = []
        for m in messages:
            # append media files to messages
            if m['role'] == 'media':
                try:
                    value = json.loads(m['content'])
                    if isinstance(value, list):
                        new_messages.extend(value)
                    else:
                        new_messages.append(value)
                except:
                    continue
            else:
                new_messages.append(m)

        return new_messages

    def num_tokens_for_single_message(self, m):
        tokens_per_message = 3  # every message follows <|start|>{role/name}\n{content}<|end|>\n
        num_tokens = tokens_per_message
        if ('role' in m) and m['role'] == 'media':
            return num_tokens
        for key, value in m.items():
            num_tokens += len(self.encoder.encode(value))
        return num_tokens

    def cut_dialog_history(self, messages):
        # remove old history in order to fit into the prompt
        lines_tokens = [self.num_tokens_for_single_message(m) for m in messages]
        sum_tokens = 0
        i_cut = 0
        for i in reversed(range(len(lines_tokens))):
            sum_tokens += lines_tokens[i]
            if sum_tokens > self.max_tokens:
                i_cut = i + 1
                break
        if i_cut > 0:
            new_messages = messages[i_cut:]
            # do not cut media files
            for m in messages[:i_cut]:
                # do not cut media information
                if ('role' in m) and m['role'] == 'media':
                    new_messages.append(m)
            return new_messages
        return messages

from typing import List

import markdown
import tiktoken
from bs4 import BeautifulSoup

CHUNK_SIZE = 200
MIN_CHUNK_SIZE_CHARS = 350  # The minimum size of each text chunk in characters
MIN_CHUNK_LENGTH_TO_EMBED = 5  # Discard chunks shorter than this

class DocProcessor:
    encoding = tiktoken.encoding_for_model("gpt-3.5-turbo")

    @staticmethod
    def clear_text(file: str) -> str:
        """Saves the contents of the given markdown file to a string.

        Args:
            link (str): Path to the markdown file.

        Returns:
            str: Cleaned markdown content.
        """
        with open(file, "r", encoding="utf-8") as input_file:
            text = input_file.read()

        html = markdown.markdown(text)
        soup = BeautifulSoup(html, "html.parser")

        text = soup.get_text().strip()
        if text.startswith("Page settings") and "\n\n" in text:
            text = text[str(text).find("\n\n") + 2:]
        return text

    @staticmethod
    def get_text_chunks(text: str, chunk_token_size: int = CHUNK_SIZE) -> List[str]:
        """Splits a text into chunks of ~CHUNK_SIZE tokens, based on punctuation and newline boundaries.

        Args:
            text (str): Text content to split into chunks.
            chunk_token_size (int, optional): The target size of each chunk in tokens. Defaults to CHUNK_SIZE.

        Returns:
            list: List of text chunks.
        """
        tokens = DocProcessor.encoding.encode(text)

        chunks = []
        chunk_size = chunk_token_size
        num_chunks = 0

        while tokens:
            chunk = tokens[:chunk_size]

            chunk_text = DocProcessor.encoding.decode(chunk)

            if not chunk_text or chunk_text.isspace():
                tokens = tokens[len(chunk):]
                continue

            last_punctuation = max(chunk_text.rfind(
                "."), chunk_text.rfind("\n"), chunk_text.rfind("\n\n"))

            if last_punctuation != -1 and last_punctuation > MIN_CHUNK_SIZE_CHARS:
                chunk_text = chunk_text[: last_punctuation + 1]

            chunk_text_to_append = chunk_text.replace("\n", " ").strip()

            if len(chunk_text_to_append) > MIN_CHUNK_LENGTH_TO_EMBED:
                chunks.append(chunk_text_to_append)

            tokens = tokens[len(DocProcessor.encoding.encode(chunk_text)):]
            num_chunks += 1

        if tokens:
            remaining_text = DocProcessor.encoding.decode(tokens).replace("\n", " ").strip()
            if len(remaining_text) > MIN_CHUNK_LENGTH_TO_EMBED:
                chunks.append(remaining_text)

        return chunks



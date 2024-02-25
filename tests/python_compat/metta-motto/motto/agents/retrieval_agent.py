import shutil
import os
import pathlib
import concurrent.futures

from .agent import Agent
from .agent import Response
from .data_processors import OpenAIEmbeddings, DocProcessor


def fix_for_chromabd(ex):
    if "sqlite3" in str(ex):
        __import__('pysqlite3')
        import sys
        sys.modules['sqlite3'] = sys.modules.pop('pysqlite3')

class RetrievalAgent(Agent):
    max_length = 2270

    def __init__(self, data_source, chunk_token_size, docs_count, data_dir):

        if not (os.path.isfile(data_source) or os.path.isdir(data_source)):
            raise AttributeError("data_source should be file or folder")

        if not os.path.exists(data_source):
            return

        self.docs_count = docs_count
        self.chunk_token_size = chunk_token_size
        self.embeddings_getter = OpenAIEmbeddings()
        # data source can be a single file or a folder with files
        self.data_source = data_source
        # the database is named as data_source
        db_name = pathlib.Path(self.data_source).stem
        self.db = os.path.join(data_dir, f"{db_name}_embedings")
        self.collection_name = f"{db_name}_collection"
        need_load_docs = not os.path.exists(self.db)

        try:
            import chromadb
        except Exception as ex:
            fix_for_chromabd(ex)
            import chromadb

        self.chroma_client = chromadb.PersistentClient(self.db)
        self.collection = self.chroma_client.get_or_create_collection(name=self.collection_name,
                                                                      metadata={"hnsw:space": "cosine"})

        if not need_load_docs:
            if self.collection.count() == 0:
                need_load_docs = True
                shutil.rmtree(self.db)

        if need_load_docs:
            self._load_docs()

    def _process_doc(self, file):
        chunks = {}
        if os.path.exists(file):
            text = DocProcessor.clear_text(file)
            chunks['texts'] = DocProcessor.get_text_chunks(text, self.chunk_token_size)
            chunks['embeddings'] = self.embeddings_getter.get_chunks_embeddings(chunks['texts'])
            length = len(chunks['texts'])
            chunks['source'] = [{'source': file}] * length if length > 0 else []
        return chunks

    def _load_docs(self):
        try:
            with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
                futures = []
                if os.path.isfile(self.data_source):
                    files = [self.data_source]
                else:
                    files = [os.path.join(self.data_source, file) for file in os.listdir(self.data_source)]
                for file in files:
                    futures.append(executor.submit(self._process_doc, file=file))
                i = 0
                for future in concurrent.futures.as_completed(futures):
                    chunks = future.result()
                    length = len(chunks['texts']) if ('texts' in chunks) else 0
                    if length > 0:
                        chunks['ids'] = [f"id{i + j}" for j in range(length)]
                        i += length
                        self.collection.add(embeddings=chunks['embeddings'], documents=chunks['texts'],
                                            metadatas=chunks['source'], ids=chunks['ids'])
        except Exception as ex:
            if os.path.exists(self.db):
                shutil.rmtree(self.db)
            raise RuntimeError(f"RetrievalAgent.__load_docs error: {ex}")

    def __call__(self, messages, functions=[], doc_name=[]):
        if isinstance(messages, str):
            text = messages
        else:
            try:
                text = list(map(lambda m: m['content'], messages))
                text = '\n'.join(text)
            except Exception as ex:
                raise TypeError(f"Incorrect argument for retrieval-agent: {ex}")
        embeddings_values = self.embeddings_getter.get_embeddings(text)
        if not doc_name:
            context = self.collection.query(query_embeddings=embeddings_values, n_results=self.docs_count)
        else:
            if self.data_source not in doc_name:
                doc_name = os.path.join(self.data_source, doc_name)
            context = self.collection.query(query_embeddings=embeddings_values, n_results=self.docs_count,
                                            where={"source": doc_name})
        docs = context["documents"][0]
        res = ""
        for doc in docs:
            next = doc.replace('"', "'")
            if next not in res:
                res += next + "\n"

        return Response(f"\"{res}\"", None)

!(import! &self motto)
!(bind! &retrieval (Agent (retrieval-agent "text_for_retrieval.txt" 200 2 "data")))
(= (get_information $question)
   (llm &retrieval (user $question)))

!(bind! &retrieval_folder (retrieval-agent "./data/texts_for_retrieval" 200 1 "data"))
!(llm (Agent &retrieval_folder)
      (user "Who made significant advancements in the fields of electromagnetism?"))
!(llm (Agent &retrieval_folder (doc_name "story1.txt"))
      (user  "Who made significant advancements in the fields of electromagnetism?"))

!(let $question "Can I publish service without using grpc?"
  (llm
    (Agent (chat-gpt "gpt-3.5-turbo-0613"))
    (Messages (system ("Taking this information into account, answer the user question"
                      (get_information $question)))
              (user $question))
  )
)

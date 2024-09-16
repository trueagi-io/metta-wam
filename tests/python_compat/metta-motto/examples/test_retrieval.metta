!(import! &self motto)
!(bind! &retrieval (retrieval-agent "text_for_retrieval.txt" 200 2 "data"))
(= (get_information $question)
   (&retrieval (user $question)))

!(bind! &retrieval_agent (retrieval-agent "./data/texts_for_retrieval" 200 1 "data"))
!(&retrieval_agent
      (user "Who made significant advancements in the fields of electromagnetism?"))
!((&retrieval_agent (Kwargs (doc_name "story1.txt")))
      (user  "Who made significant advancements in the fields of electromagnetism?"))

!(let $question "Can I publish service without using grpc?"
  (
    (chat-gpt-agent "gpt-3.5-turbo")
    (Messages (system ("Taking this information into account, answer the user question"
                      (get_information $question)))
              (user $question))
  )
)

(get_information "Can I publish service without using grpc?")
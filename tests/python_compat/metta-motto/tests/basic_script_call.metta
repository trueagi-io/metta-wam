!(import! &self motto)

(= (user-query) "Hello world.")

!(assertEqual (llm (Script basic_script.mps))
"user Hello world.
assistant Hi there")

!(remove-atom &self (= (user-query) "Hello world."))
(= (user-query) "Bye")
!(assertEqual (llm (Script basic_script.mps))
"user Bye
assistant Bye")

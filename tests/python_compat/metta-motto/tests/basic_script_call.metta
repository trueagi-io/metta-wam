!(import! &self motto)

(= (user-query) "Hello world.")

!(assertEqual ((echo-agent) (Script basic_script.mps))
"user Hello world.
assistant Hi there")

!(remove-atom &self (= (user-query) "Hello world."))
(= (user-query) "Bye")
!(assertEqual ((echo-agent) (Script basic_script.mps))
"user Bye
assistant Bye")

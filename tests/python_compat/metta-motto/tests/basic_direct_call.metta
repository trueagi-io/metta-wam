!(import! &self motto)

; Testing parsing of arguments of `llm` using EchoAgent,
; which is passed explicitly as one of the arguments

; Messages can be wrapped into `Messages` constructor
!(assertEqual
   (llm
     (Agent EchoAgent)
     (Messages (user "Test")))
"user Test")

; Messages can be recognized by their roles without `Messages` constructor
!(assertEqual
   (llm
     (Agent EchoAgent)
     (system "Ping") (user "Pong"))
"system Ping
user Pong")

; Messages can be wrapped recursively
!(assertEqual
   (llm
     (Agent EchoAgent)
     (Messages (system "Ping")
               (Messages (user "Pong") (assistant "Catch"))))
"system Ping
user Pong
assistant Catch")

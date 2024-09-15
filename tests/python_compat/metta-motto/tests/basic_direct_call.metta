!(import! &self motto)

; Testing parsing of arguments of `llm` using EchoAgent,
; which is passed explicitly as one of the arguments

; Messages can be wrapped into `Messages` constructor
!(assertEqual
   ((echo-agent)
    (Messages (user "Test")))
"user Test")

; Messages can be recognized by their roles without `Messages` constructor
!(assertEqual
   ((echo-agent)
    (system "Ping") (user "Pong"))
"system Ping
user Pong")

; Messages can be wrapped recursively
!(assertEqual
   ((echo-agent)
    (Messages (system "Ping")
              (Messages (user "Pong") (assistant "Catch"))))
"system Ping
user Pong
assistant Catch")

!(assertEqual
  ((echo-agent) (Kwargs (user_name "John")) (user "Test"))
"user Test
your name is John")

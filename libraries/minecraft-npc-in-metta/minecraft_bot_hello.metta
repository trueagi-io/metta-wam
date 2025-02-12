
!(import! &self minecraft_bot_driver.metta )

(event-trigger (on_chat "hello bot") (say "Hello, human!"))
(event-trigger (on_chat "who are you?") (say "I am a MeTTa-powered AI bot!"))
(event-trigger (on_chat "what can you do?") (say "I can move, chat, and explore using voxel data!"))
(event-trigger (on_chat "move forward") (move 1 0 0))
(event-trigger (on_chat "move up") (move 0 1 0))
(event-trigger (on_chat "scan area") (enumerate_voxel_space))
(event-trigger (on_chat "goodbye") (say "Goodbye, see you soon!"))

!(println! "loaded minecraft_bot_hello.metta")





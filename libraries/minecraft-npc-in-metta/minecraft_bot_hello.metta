
!(import! &self minecraft_bot_hello)

(: on_chat "hello bot" (say "Hello, human!"))
(: on_chat "who are you?" (say "I am a MeTTa-powered AI bot!"))
(: on_chat "what can you do?" (say "I can move, chat, and explore using voxel data!"))
(: on_chat "move forward" (move 1 0 0))
(: on_chat "move up" (move 0 1 0))
(: on_chat "scan area" (enumerate_voxel_space))
(: on_chat "goodbye" (say "Goodbye, see you soon!"))


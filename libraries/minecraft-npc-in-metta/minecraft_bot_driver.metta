
; !(call-p use_module [ library jpl ] )

; !(call-p use_module [ library jpl ] )

!(call-p ensure_loaded prolog/minecraft_bot_driver.pl )

!(call-p ensure_loaded prolog/minecraft_bot_hello.pl ) 

;; Move the bot in a given direction (X, Y, Z)
(= (move $X $Y $Z)
   (call-p! send_command move $X $Y $Z))

;; Scan and enumerate nearby voxel data
(= (enumerate_voxel_space)
   (call-p! send_command "enumerate_voxel_space"))

;; Handle received voxel data (from Java/Prolog)
(= (on_voxel_data $X $Y $Z $BlockID)
   (println "Voxel at" $X $Y $Z "is Block ID:" $BlockID))

;; Make the bot say something in chat
(= (say $Message)
   (call-p! send_command "chat" $Message))

;; Example: If player says "hello bot", respond with chat

(= (on_chat "hello bot")
   (say "Hello, human!"))

;; Example: If player says "scan area", trigger voxel scanning
(= (on_chat "scan area")
   (enumerate_voxel_space))

(= (start-bot) (call-p logon0))

!(println! "loaded minecraft_bot_driver.metta")

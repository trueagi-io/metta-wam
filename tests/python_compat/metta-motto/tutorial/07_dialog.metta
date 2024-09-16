!(import! &self motto)

; In order to keep the dialog history, we need to use the same agent
!(bind! &chat (dialog-agent 07_dialog.msa))

(= (print-respond $msg)
   (nop (println! $msg)
        (println! (&chat $msg))
   )
)

!(print-respond (user "Hello!"))
!(print-respond (user "May I sell the ring?"))
!(print-respond (user "Sorry, I am confused. What I wanted to sell?"))

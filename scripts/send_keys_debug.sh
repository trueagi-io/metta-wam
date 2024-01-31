#!/usr/bin/expect

# Start your command
spawn sh -c "cls; MeTTa --eval=debug --repl"

# Send "prolog.\r"
send "prolog.\r"

# Send Control-D
send "repl.\r"

# Send up arrows
#send "\033\[A"
#send "\033\[A"
#send "\033\[A"
send "!(import! &self examples/extended_compat/nars/new/tests1.metta)"

# Hand control over to the user
interact


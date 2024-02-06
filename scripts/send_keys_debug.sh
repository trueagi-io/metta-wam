#!/usr/bin/expect

# Configure timeout for expect commands to prevent hanging
set timeout 10

# Initialize variables to hold the file argument and the modified argv
set file_arg ""
set modified_argv []

# Loop through each argument in $argv
foreach arg $argv {
    # Check if the argument is a file that exists and file_arg is not yet set
    if {[file exists $arg] && $file_arg eq ""} {
        # If it exists and no file_arg has been set, use this as the file_arg
        set file_arg $arg
    } elseif {$arg ne "--debugable"} {  # Exclude the --breakable argument
        # Otherwise, if the argument is not --breakable, add it to the modified_argv list
        lappend modified_argv $arg
    }
}


# Convert modified_argv list back to a string with spaces
set args_for_metta [join $modified_argv " "]

# Spawn the MeTTa REPL with debug and REPL mode, omitting the file path from argv
# clear; 
spawn sh -c "MeTTa $args_for_metta --repl "

# Send initial commands to MeTTa REPL. Use \r to simulate ENTER key.
send "prolog.\r(repl,maybe_halt(7)).\r"


# Continue with REPL interaction
#send "repl.\r"
#expect "metta &self +>"



# Proceed only if a file argument has been found
if {$file_arg ne ""} {
    
    # Sending a command to import a file, using the found file argument
    send "!(import! &self $file_arg)\r"
} else {
    
}
# Hand over control to the user for interactive use
interact
#!/usr/bin/expect



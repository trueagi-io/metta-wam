@echo off 
swipl -l src/canary/metta_interp -g do_loon,repl -- %* --repl

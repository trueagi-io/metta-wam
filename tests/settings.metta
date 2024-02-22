;; MeTTaLog Configuration Guide for Command-Line Options
;; This section outlines how to translate various command-line options into MeTTaLog script configurations.
;; Commented pragma!s can be uncommented and modified as needed to customize the MeTTaLog environment.

;; Customizing Your Configuration:
;; Uncomment and modify any of the provided pragma statements to tailor the MeTTaLog environment to your project's needs.
;; The flexibility of script-based configuration allows for fine-tuned control over the behavior and output of your MeTTaLog scripts, enhancing the development and debugging experience.

;; MeTTaLog Verbosity Configuration Guide
;; This document outlines various configurations to tailor verbosity settings for different development scenarios.
;; Each setting is designed to optimize the logging output according to the specific needs of the task at hand.

;; Operations:
;; load: Controls logging for loading operations. Example: (pragma! load silent)
;; transpiler: Adjusts logging for compilation. Example: (pragma! transpiler verbose)
;; eval: Sets verbosity for expression evaluation. Example: (pragma! eval show)
;; result: Dictates how results are logged. Example: (pragma! result debug)

;; Verbosity Levels:
;; silent: No output for the operation. Suppresses all logs. Example: (pragma! load silent)
;; show: Logs basic operation information. Essential details only. Example: (pragma! eval show)
;; verbose: Provides detailed operation logs, including context. In-depth information. Example: (pragma! transpiler verbose)
;; debug: Outputs exhaustive detail for deep debugging. Maximum verbosity. Example: (pragma! result debug)

;; Preset Configurations
;; Like MeTTa in Rust - Minimal Output Configuration
(= (like-metta-in-rust)
  (pragma! load silent)    ;; Silences all load operation logs.
  (pragma! eval silent)    ;; Disables verbose evaluation logs.
  (pragma! result show))   ;; Only displays the final results of script execution.

;; Development and Debugging - Verbose Configuration
(= (development-debugging)
  (pragma! load verbose)   ;; Logs detailed information on resource loading.
  (pragma! eval verbose)   ;; Offers detailed logs during expression evaluations.
  (pragma! result verbose));; Includes verbose details in the output of operation results.

;; Unit Testing - Comprehensive Logging Configuration
(= (unit-testing)
  (pragma! load show)      ;; Logs resource loading actions at a basic level.
  (pragma! eval debug)     ;; Applies the highest level of verbosity for evaluations.
  (pragma! result verbose));; Emphasizes verbose result logging for clear failure insights.

;; Customized Balanced Configuration
(= (on-repl)
  (pragma! load show)      ;; Shows loading operations for contextual awareness.
  (pragma! eval show)      ;; Keeps evaluation logs informative yet not overwhelming.
  (pragma! result debug))  ;; Delivers verbose results but waits for key presses if --tty=true

;; Dynamic Setting Adjustment
!(pragma! result show) ;; Updates result verbosity to show, focusing on essential output.

;; Default Configuration
!(like-metta-in-rust)


;; Testing Options
;; !(pragma! test True)        ; Use test options (equivalent to --test)
;; !(pragma! html True)        ; Save terminal output as HTML (equivalent to --html[=bool])

;; Debugging Options
;; !(pragma! debug_on_interrupt True)        ; Trap the debugger on interrupt (equivalent to --debug-on-interrupt[=bool])
;; !(pragma! case   debug)         ; Show extra debug info about case statements (equivalent to --case=debug)
;; !(pragma! signals True)         ; Modify signal handling (equivalent to --signals[=bool])
;; !(pragma! threads True)         ; Allow for threads (equivalent to --threads[=bool])
;; !(pragma! repl True)            ; Start the REPL after processing files (equivalent to --repl)
;; !(pragma! prolog True)          ; Drop to the host system debugger (equivalent to --prolog)
;; !(pragma! exec skip)            ; Skip over !exec directives when loading files (equivalent to --exec=skip)
                                   ; used when you want to load file contents and debug in the repl

;; Compilation Options
;; !(pragma! compile False)  ; use only the interpeter
;; !(pragma! compile True)   ; use the compiler in some cercomstance 
;; !(pragma! compile full)   ; force the use of the compiler
;; !(pragma! tabling True)   ; Memoize compiled functions
!(pragma! transpiler silent)    ;; Prevents logging of compilation details.
;; !(pragma! transpiler show)      ;; Shows basic compilation information.
;; !(pragma! transpiler verbose)   ;; Provides verbose output for compilation steps.
;; !(pragma! transpiler debug)   ;; Captures comprehensive details on compilation.
;; !(pragma! optimised True)   ; Optimised compilation (equivalent to -O)
;; !(pragma! debug True)       ; Generate debug info (equivalent to --debug)

;; Running Options
;; !(pragma! stack_limit "size")  ; Specify maximum size of stacks (equivalent to --stack-limit=size)
;; !(pragma! pce True)             ; Make the XPCE GUI available (equivalent to --pce[=bool])
;; !(pragma! packs True)           ; Attach add-ons (equivalent to --packs[=bool])
;; !(pragma! p2p-port 3023)     ; Start a Peer-to-Peer Distributed MeTTa server on the specified port (equivalent to --pldoc[=port])
;; !(pragma! facilitator "localhost:3023") ; Set the P2P facilitator address to "localhost:3023"
;; This configuration means that the first MeTTaLog instance becomes the facilitator by using port 3023. If unavailable, it increments the port by 100. Subsequent instances register themselves accordingly.
;; The facilitator notifies all nodes upon leaving the network and requests one to bind its port. Typically, the one closest to the port is selected.
;; !(pragma! pldoc 8088)           ; Start PlDoc server at specified port (equivalent to --pldoc[=port])
;; !(pragma! python True)          ; Enable Python support (equivalent to --python[=bool])
;; !(pragma! tty False)            ; Disallow tty control (equivalent to --tty[=bool])
;; !(pragma! quiet True)           ; Suppress informational messages (equivalent to --quiet[=bool] or -q)

;; Stack and Trace Options
;; !(pragma! stack-max 10_000)          ; Set maximum stack size to 10,000 (equivalent to --stack-max=10_000)
;; !(pragma! trace-length 10_000)       ; Set trace length to 10,000 (equivalent to --trace-length=10_000)
;; !(pragma! trace-on-overtime 20.0)    ; Set threshold for trace on overtime to 20.0 (equivalent to --trace-on-overtime=20.0)
;; !(pragma! trace-on-overflow False)   ; Disable tracing on overflow (equivalent to --trace-on-overflow=False)
;; !(pragma! trace-on-exec True)        ; Enable tracing on execution (equivalent to --trace-on-exec=True)
;; !(pragma! trace-on-eval True)        ; Enable tracing on evaluation (equivalent to --trace-on-eval=True)
;; !(pragma! trace-on-fail False)       ; Disable tracing on failures (equivalent to --trace-on-fail=False)
;; !(pragma! trace-on-pass False)       ; Disable tracing on passes (equivalent to --trace-on-pass=False)

;; Miscellaneous Options
;; !(pragma! doing_repl False)          ; Indicate not being in REPL mode (equivalent to --doing_repl=False)
;; !(pragma! test-retval False)         ; Disable returning test values (equivalent to --test-retval=False)
;; !(pragma! load debug)                ; Set load verbosity to debug (equivalent to --load=debug)
;; !(pragma! exeout './Sav.gitlab.MeTTaLog') ; Set path for executable output (equivalent to --exeout='./Sav.gitlab.MeTTaLog')


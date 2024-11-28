;; LSP Server Configuration and Implementation
;; This file configures hooks and behaviors for an LSP server, including hover functionality,
;; code lenses, and code actions.

;; sometimes something like this would be nice:
;; !(bind! file-context &lsp-server)
;; However, the file is loaded via `!(import! &lsp-server lsp-callbacks)` so it doesn't matter. 
;; We still use !(add-atom &lsp-server ..) in case the system needs individual hooks.

;; Hook for displaying hover information.
;; Displays details when hovering over a term in the editor.
;; Parameters:
;; - $path: File path of the hovered term.
;; - $loc: Location details of the hovered term (line/character position).
;; - $term: The term being hovered over (e.g., a symbol or keyword).
;; - $arity: If the term is a symbol, the number of arguments it is used with.
!(add-atom &lsp-server
    (= (hook-hover-string $path $_loc $term $arity)
       ;; Format and display hover information.
       (format-args "hovering over '{}', arity: {}, in file: {}" ($term $arity $path))))

;; Hook for logging hover information for debugging purposes.
;; Logs details of the hovered term for further analysis.
!(add-atom &lsp-server
    (= (hook-hover-print $path $_loc $term $arity)
       ;; Log hover information to the console or output channel.
       (println! (format-args "hovering over '{}', arity: {}, in file: {}" ($term $arity $path)))))

;; Code lenses provide inline suggestions and actions based on source file analysis.
;; Parameters:
;; - $uri: URI of the file being analyzed.
;; - $lvl: Depth of the S-expressions for the term or expression ($what).
;; - $ord: File order of these terms or expressions.
;; - $kind: The type of the target object (e.g., 'string', 'symbol', 'variable', 'type', etc.).
;;          For details, see: https://github.com/trueagi-io/metta-wam/blob/main/src/packs/lsp_server_metta/prolog/lsp_metta_outline.pl#L101-L139
;; - $what: The term or expression being analyzed.
;; - $vl: If the expression uses variables, their original names.
;; - $path: Absolute file path of the analyzed code (may differ from $uri for includes).
;; - $range: Range of code where the lens applies.
!(add-atom &lsp-server
    (= (compute-each-code-lens $uri $lvl $ord $kind $what $vl $path $range)
       ;; Debugging the lens computation.
       (sequential
         (lsp-debug! todo (quote (compute-each-code-lens $uri $lvl $ord $kind $what $vl $path $range))))))

;; Computes actions available at a specific code position upon user request.
;; Dynamically determines actions based on the type of the code object.
;; Parameters:
;; - $objecttype: Specifies the scope of the code action (e.g., 'term', 'block', 'file').
;; - $uri: URI of the file where the action is computed.
;; - $range: Code range applicable for the action.
;; - $object: The target code object for which the action applies.
!(add-atom &lsp-server
    (= (compute-typed-code-action $objecttype $uri $range $object)
       (sequential
         ;; Debugging the typed-code-action computation.
         (lsp-debug! todo (quote (compute-typed-code-action $objecttype $uri $range $object))))))

;; === Individual Hooks for compute-typed-code-action ===

;; Hook for handling variables.
;; Provides a "Rename Variable" action.
!(add-atom &lsp-server
    (= (compute-typed-code-action term $uri $range $object)
       (if (== Variable (get-type $object))
           ((range $range)
            (command
               ((title "Rename Variable")
                (command rename-variable)
                (arguments ($object))))))))

;; Hook for handling symbols.
;; Provides a "Rename Symbol" action.
!(add-atom &lsp-server
    (= (compute-typed-code-action term $uri $range $object)
       (if (== Symbol (get-type $object))
           ((range $range)
            (command
               ((title "Rename Symbol")
                (command rename-symbol)
                (arguments ($object))))))))

;; Hook for handling expressions.
;; Provides an "Evaluate Expression" action, running the code and displaying results in the UI.
!(add-atom &lsp-server
    (= (compute-typed-code-action term $uri $range $object)
       (if (== $object (type-cast Expression $object))
           ((range $range)
            (command
               ((title "Evaluate Expression")
                (command lsp-eval-into-ui)
                (arguments (eval $object))))))))

;; Hook for handling types.
;; Provides a "Check Type" action to validate the type of the object.
!(add-atom &lsp-server
    (= (compute-typed-code-action term $uri $range $object)
       (if (== Type (get-type $object))
           ((range $range)
            (command
               ((title "Check Type")
                (command lsp-eval-into-ui)
                (arguments ((check-type $object))))))))))

;; Hook for handling enums.
;; Provides a "Show Instances of Type" action to display instances matching the given type.
!(add-atom &lsp-server
    (= (compute-typed-code-action term $uri $range $object)
       (if (== Type (get-type $object))
           ((range $range)
            (command
               ((title "Show Instances of Type")
                (command lsp-eval-into-ui)
                (arguments ((match &self (: $inst $object) $inst))))))))))

;; Hook for handling strings.
;; Provides a "Transform String to ProperCase" action.
!(add-atom &lsp-server
    (= (compute-typed-code-action term $uri $range $object)
       (if (== String (get-type $object))
           ((range $range)
            (command
               ((title "Transform String to ProperCase")
                (command lsp-eval-into-ui)
                (arguments ((toProperCase! $object))))))))))

;; Hook for handling blocks.
;; Provides an "Extract Block as Function" action.
!(add-atom &lsp-server
    (= (compute-typed-code-action block $uri $range $code)
       ((range $range)
        (command
           ((title "Extract Block as Function")
            (command lsp-eval-into-ui)
            (arguments ((quote (= <function-name> $code)))))))))

;; Hook for handling top-level forms.
;; Provides a "Format Toplevel Form" action using the editor's built-in formatter.
!(add-atom &lsp-server
    (= (compute-typed-code-action toplevel $uri $range $object)
       ((range $range)
        (command
           ((title "Format Toplevel Form")
            (command format-block)
            (arguments ($uri $range)))))))

;; Hook for handling entire files.
;; Provides two actions: "Run This Entire File" and "Open File".
!(add-atom &lsp-server
    (= (compute-typed-code-action file $uri $range $object)
       (if (== $uri $object)
           ;; Action: Run the entire file.
           ((range $range)
            (command
               ((title "Run This Entire File")
                (command lsp-eval-into-ui)
                (arguments ((run-file $uri)))))))
       (if (not (== $uri $object))
           ;; Action: Open a different file.
           (let $title (format-args "Open File: {}" ($object))
               ((range $range)
                (command
                   ((title $title)
                    (command lsp-eval-into-ui)
					;; see https://github.com/trueagi-io/metta-wam/blob/main/src/packs/lsp_server_metta/prolog/lsp_server_requests.pl#L379
                    (arguments ((show-document! $object "true" null $_success))))))))))

;; Example of a custom action to include and execute a file.
;; Loads the specified file into the runtime environment.
!(add-atom &lsp-server
    (= (run-file $path)
       (include $path)))

;; Utility for sending evaluation results to the UI.
(= (lsp-eval-into-ui $code)
  ;; https://github.com/trueagi-io/metta-wam/blob/main/src/packs/lsp_server_metta/prolog/lsp_server_requests.pl#L148
   (send_feedback_message $code info))


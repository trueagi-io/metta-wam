/** <module> LSP Server Requests Implementation

This module provides implementations of various Language Server Protocol (LSP) server requests in Prolog, including enum handling, diagnostics publishing, code lens support, and command execution.

Assumptions:
- `send_client_message/1`: Predicate to send messages to the client. You need to implement this predicate according to your setup. It should serialize the message dictionary to JSON and send it to the client.
- `receive_response/2`: Predicate to receive responses from the client. You need to implement this predicate to handle incoming responses corresponding to message IDs.
- `create_msg_id/1`: Predicate to generate unique message IDs.
- `resolve_enum/3`: Predicate to resolve enum values to their protocol equivalents.

*/

:- module(lsp_server_requests, [
    save_json/2,
    save_json_value/3,
    into_message_string/2,
    % Simplified predicates
    send_feedback_message/2,
    resolve_diagnostic_enum/2,
    resolve_enum/3,
    report_diagnostics/3,
    % Sample predicates
    test_show_message_request/0,
    test_feedback_message/0,
    sample_report_diagnostics/0,
    test_log_message/0,
    test_telemetry_event/0,
    test_work_done_progress/0,
    sample_ws_edit/0,
    fetch_workspace_configuration/0,
    fetch_workspace_folders/0,
    sample_show_document/0,
    % LSP methods
    send_show_message_request/4,
    create_work_done_progress/1,
    register_client_capabilities/1,
    unregister_client_capabilities/1,
    apply_workspace_edit/3,
    fetch_workspace_configuration/2,
    fetch_workspace_folders/1,
    show_document/4,
    refresh_code_lens/0,
    refresh_inlay_hints/0,
    refresh_semantic_tokens/0,
    send_log_message/2,
    send_telemetry_event/1,
    report_progress/2,
    cancel_work_done_progress/1,
    publish_diagnostics/4,
    create_msg_id/1,
    resolve_enum/3,
    sample_code_lens/3
]).

:- include(lsp_metta_include).


receive_response(MsgId, MsgBody):-
  debug_lsp(requests,begin_receive_response(MsgId)),
  repeat, sleep(0.1),
   user:last_request(Method, MsgBody),
   get_dict(id,MsgBody,Id), Id==MsgId,!,
   retract(user:last_request(Method, MsgBody)),!,
   debug_lsp(requests,received_response(MsgId, MsgBody)).


:- use_module(library(uuid)).  % For generating unique IDs.

%% Enum Declarations

:- multifile(enum_predicate/1).
enum_predicate(message_type_value).
enum_predicate(diagnostic_severity_value).

% MessageType enum
message_type_value(error, 1).
message_type_value(warning, 2).
message_type_value(info, 3).
message_type_value(log, 4).
message_type_value(N,V):- diagnostic_severity_value(N, V).

% DiagnosticSeverity enum
diagnostic_severity_value(error, 1).
diagnostic_severity_value(warning, 2).
diagnostic_severity_value(warn, 2).
diagnostic_severity_value(information, 3).
diagnostic_severity_value(info, 3).
diagnostic_severity_value(hint, 4).
diagnostic_severity_value(log, 4).

/** resolve_enum(+EnumName, +InputValue, -ProtocolValue) is det.

Resolves an enum value to its protocol-specified numeric value.

@param EnumName The name of the enum predicate (e.g., message_type_value).
@param InputValue The input value (atom, string, or number).
@param ProtocolValue The corresponding numeric value used in the protocol.

@example
    % Resolve a message type.
    resolve_enum(message_type_value, error, Value), writeln(Value). % Outputs 1
    resolve_enum(message_type_value, "warning", Value), writeln(Value). % Outputs 2
    resolve_enum(message_type_value, 3, Value), writeln(Value). % Outputs 3
@end_example

*/
resolve_enum(EnumName, InputValue, ProtocolValue) :-
    % Convert InputValue to atom if it's a string
    (string(InputValue) -> atom_string(AtomValue, InputValue); AtomValue = InputValue),
    (number(AtomValue) ->
        % Input is already a number; assume it's the correct protocol value
        ProtocolValue = AtomValue
    ;
        % Look up the enum value
        (call(EnumName, AtomValue, ProtocolValue) ->
            true
        ;
            % Try to parse numeric atom (e.g., '1' as 1)
            atom_number(AtomValue, ProtocolValue)
        )
    ).

/** create_msg_id(-MsgId) is det.

Generates a unique message ID for use in JSON-RPC messages.

@param MsgId The generated unique message ID as a string.

@example
    % Generate a unique message ID.
    create_msg_id(MsgId),
    writeln(MsgId).
@end_example
*/
create_msg_id(MsgId) :-
    uuid(UUID, [version(4)]),
    atom_string(UUID, MsgId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simplified Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% send_feedback_message(+Message, +MessageType) is det.
%
% Sends a message to the client without requiring feedback.
%
% @param Message The message string to display to the user.
% @param MessageType The type of message (atom, string, or number).
%
% @example
%    % Send an info message to the user.
%    send_feedback_message("Operation completed successfully.", info).
% @end_example
%
send_feedback_message(Message, MessageType) :-
  ignore(( Message \== null, Message \== "", Message \== "null",
    into_message_string(Message, SMessage),
    resolve_enum(message_type_value, MessageType, MessageTypeValue),
    Msg = _{

        method: "window/showMessage",
        params: _{
            type: MessageTypeValue,
            message: SMessage
        }
    },
    send_client_message(Msg))).

into_message_string(Message, SMessage):- string(Message),!,SMessage=Message.
into_message_string(Message, SMessage):- \+ compound(Message),!,sformat(SMessage,'NC: ~w',[Message]),!.
into_message_string(format(F,A), SMessage):- !, into_message_string((F-A), SMessage).
into_message_string(F-A, SMessage):- is_list(A),!,sformat(SMessage,F,A),!.
into_message_string(F-A, SMessage):- !,sformat(SMessage,F,[A]),!.
into_message_string(Message, SMessage):- sformat(SMessage,'~q',[Message]),!.


%% report_diagnostics(+Uri, +Range, +Message) is det.
%
% Reports a diagnostic with severity 'info' for a specific range in a document.
%
% @param Uri The document URI.
% @param Range The range in the document where the diagnostic applies.
% @param Message The diagnostic message.
%
% @example
%    % Report an info diagnostic at a specific range.
%    Uri = "file:///path/to/file.pl",
%    Range = _{
%        start: _{line: 5, character: 0},
%        end: _{line: 5, character: 10}
%    },
%    report_diagnostics(Uri, Range, "Informational message about this code segment.").
% @end_example
%
report_diagnostics(Uri, Range, Message) :-
    must_succeed1((into_message_string(Message, SMessage),
    Diagnostic = _{
        range: Range,
        severity: info,
        message: SMessage,
        source: "metta-lsp"
    },
    publish_diagnostics(Uri, [Diagnostic], _Version, _Options))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Code Lens Handlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Compute Code Lenses for the given document
lsp_hooks:compute_code_lens(Uri, Path, CodeLens) :-
    % Check if the URI ends with '.lsp_test'
    %sub_atom(Uri, _, 9, 0, '.lsp_test'),
    once((user:metta_file_buffer(_Lvl, _Ord, _Kind, end_of_file, _VL, Path, BRange),
    BRange = range(line_char(SL,_),line_char(_,_)))),
    %succ(SL0,SL),
    sample_code_lens(Uri, SL, CodeLens).

sample_code_lens(Uri, SL0, CodeLens):-
    ARange = range(line_char(SL0,1), line_char(SL0,10)),
    into_json_range(ARange, _Range),
    % List of sample predicates
    SamplePredicates = [
        test_show_message_request,
        test_feedback_message,
        sample_report_diagnostics,
        test_log_message,
        test_telemetry_event,
        test_work_done_progress,
        sample_ws_edit,
        fetch_workspace_configuration,
        fetch_workspace_folders,
        sample_show_document
    ],
    member(Predicate, SamplePredicates),
    % Create a code lens for each sample predicate
    sformat(PredicateName, '~w', [Predicate]),
    sformat(Title, ' ~w ', [PredicateName]),
    CodeLens = _{
        %range: Range,
        range: _{start: _{line: 0, character: 0}, end: _{line: 0, character: 1}} ,
        command: _{
            title: Title,
            command: "system_call_zero_args",
            arguments: [Uri, PredicateName]
        }
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sample Predicate for Code Lens Action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% test_show_message_request is det.
%
% Sample predicate that sends a Yes/No message to the user and handles the response.
%
% @example
%    % Execute the sample action.
%    test_show_message_request.
% @end_example
%
test_show_message_request :-
    Actions = [
        _{title: "Yes"},
        _{title: "No"}
    ],
    send_show_message_request("Do you want to proceed?", info, Actions, Response),
    first_dict_key(result;response, Response,UserResponse),
    ( UserResponse = _{title: "Yes"} ->
        send_feedback_message("You chose Yes.", info)
    ; UserResponse = _{title: "No"} ->
        send_feedback_message("You chose No.", info)
    ;
        send_feedback_message("No response received.", warning)
    ).

% Sample predicate for 'send_feedback_message/2'
test_feedback_message :-
    send_feedback_message("This is a feedback message.", info).

% Sample predicate for 'report_diagnostics/3'
sample_report_diagnostics :-
    sample_uri(Uri),
    Range = _{
        start: _{line: 5, character: 0},
        end: _{line: 5, character: 10}
    },
    report_diagnostics(Uri, Range, "Sample diagnostic message.").

% Sample predicate for 'send_log_message/2'
test_log_message :-
    send_log_message("This is a log message.", info).

% Sample predicate for 'send_telemetry_event/1'
test_telemetry_event :-
    TelemetryData = _{
        event: "sampleEvent",
        properties: _{
            detail: "Sample telemetry data"
        }
    },
    send_telemetry_event(TelemetryData).

% Sample predicate for 'create_work_done_progress/1' and 'report_progress/2'
test_work_done_progress :-
    Token = "sample_progress_token",
    create_work_done_progress(Token),
    ProgressValue = _{
        kind: "begin",
        title: "Sample Progress",
        percentage: 0,
        cancellable: true
    },
    report_progress(Token, ProgressValue),
    % Simulate progress updates
    sleep(1),
    report_progress(Token, _{kind: "report", message: "Halfway there...", percentage: 50}),
    sleep(1),
    report_progress(Token, _{kind: "end", message: "Completed", percentage: 100}).

% Sample predicate for 'apply_workspace_edit/3'
sample_ws_edit :-
    Label = "Sample Edit",
    sample_uri(Uri),
    Edit = _{
        uri: Uri,
        changes:
               [
                _{
                    uri: Uri,
                    range: _{
                        start: _{line: 0, character: 0},
                        end: _{line: 0, character: 0}
                    },
                    newText: "% Sample code inserted\n"
                }
            ]
        },
    apply_workspace_edit(Label, Edit, Response),
    format('Workspace edit response: ~w', [Response]).

% Sample predicate for 'fetch_workspace_configuration/2'
fetch_workspace_configuration :-
    ConfigurationItems = [
        _{section: "metta-lsp"}
    ],
    fetch_workspace_configuration(ConfigurationItems, Configurations),
    format('Configurations: ~w', [Configurations]),
    first_dict_key( result, Configurations, ClientConfig),
    save_json(client_configuration,ClientConfig),
    resync_debug_values.
resync_debug_values:-
   ignore((stored_json_value(client_configuration, [options], List),
      resync_debug_values(List))).
resync_debug_values(List):- is_list(List), !,
   nodebug(lsp(_)), % reset
   maplist(resync_debug_values,List).
%   ["debug_main", "debug_errors", "debug_threads", "debug_high", "debug_todo", "debug_xref"]
resync_debug_values(Str):- ignore((string(Str), atom_concat("debug_", Atom, Str), debug(lsp(Atom)))).
% resync_debug_values(Str):- string(Str), atom_string(Atom,Str), !, resync_debug_values(Atom).


% Sample predicate for 'fetch_workspace_folders/1'
fetch_workspace_folders :-
    fetch_workspace_folders(Folders),
    format('Workspace folders: ~w', [Folders]),
    send_feedback_message("Retrieved workspace folders.", info).


sample_uri("file:///home/deb12user/metta-wam/tests/baseline_compat/hyperon-mettalog_sanity/flip_test.metta").

% Sample predicate for 'show_document/4'
sample_show_document :-
    sample_uri(Uri),
    TakeFocus = true,
    Selection = _{
        start: _{line: 10, character: 0},
        end: _{line: 10, character: 10}
    },
    show_document(Uri, TakeFocus, Selection, Success),
    ( is_lsp_success(Success) ->
        send_feedback_message("Document shown successfully.", info)
    ;
        send_feedback_message("Failed to show document.", error)
    ).

is_lsp_success(Success):-
  \+ ((first_dict_key(success,Success,Value), Value==false)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Command Handler for 'system_call_zero_args'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Command execution logic
lsp_hooks:exec_code_action("system_call_zero_args", [_Uri, PredicateName], null) :-
    atom_string(PredicateAtom, PredicateName),
    (   catch(call(PredicateAtom), Error,
          ( format('Error executing predicate ~w: ~w', [PredicateAtom, Error]),
            send_feedback_message('Error executing predicate', error),
            fail))
    ->  true
    ;   send_feedback_message('Predicate execution failed', error)
    ),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Existing Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% send_show_message_request(+Message, +MessageType, +Actions, -Response) is det.
%
% Sends a message to the client and awaits a user response.
%
% @param Message The message string to display to the user.
% @param MessageType The type of message (atom, string, or number).
% @param Actions A list of action dictionaries, each with a "title" key.
% @param Response The user's response received from the client.
%
% @example
%    % Send a message to the user and await a response.
%    Actions = [
%        _{title: "Yes"},
%        _{title: "No"}
%    ],
%    send_show_message_request("Do you want to proceed?", error, Actions, UserResponse),
%    writeln(UserResponse).
% @end_example
%
send_show_message_request(Message, MessageType, Actions, Response) :-
    into_message_string(Message, SMessage),
    create_msg_id(MsgId),
    resolve_enum(message_type_value, MessageType, MessageTypeValue),
    Msg = _{

        id: MsgId,
        method: "window/showMessageRequest",
        params: _{
            type: MessageTypeValue,
            message: SMessage,
            actions: Actions
        }
    },
    send_client_message(Msg),
    receive_response(MsgId, Response).

%% create_work_done_progress(+Token) is det.
%
% Requests the client to create a progress indicator.
%
% @param Token A unique progress token.
%
% @example
%    % Request the client to create a progress indicator.
%    ProgressToken = "progress_token_123",
%    create_work_done_progress(ProgressToken).
% @end_example
%
create_work_done_progress(Token) :-
    create_msg_id(MsgId),
    Msg = _{

        id: MsgId,
        method: "window/workDoneProgress/create",
        params: _{
            token: Token
        }
    },
    send_client_message(Msg),
    receive_response(MsgId, _Response).

%% register_client_capabilities(+Registrations) is det.
%
% Dynamically registers new capabilities on the client.
%
% @param Registrations A list of capability registration dictionaries.
%
% @example
%    % Dynamically register new capabilities on the client.
%    Registrations = [
%        _{
%            id: "fileWatcher",
%            method: "workspace/didChangeWatchedFiles",
%            registerOptions: _{
%                watchers: [
%                    _{globPattern: "**/*.metta"}
%                ]
%            }
%        }
%    ],
%    register_client_capabilities(Registrations).
% @end_example
%
register_client_capabilities(Registrations) :-
    create_msg_id(MsgId),
    Msg = _{

        id: MsgId,
        method: "client/registerCapability",
        params: _{
            registrations: Registrations
        }
    },
    send_client_message(Msg),
    receive_response(MsgId, _Response).

%% unregister_client_capabilities(+Unregistrations) is det.
%
% Unregisters previously registered capabilities.
%
% @param Unregistrations A list of capability unregistration dictionaries.
%
% @example
%    % Unregister previously registered capabilities.
%    Unregistrations = [
%        _{
%            id: "fileWatcher",
%            method: "workspace/didChangeWatchedFiles"
%        }
%    ],
%    unregister_client_capabilities(Unregistrations).
% @end_example
%
unregister_client_capabilities(Unregistrations) :-
    create_msg_id(MsgId),
    Msg = _{

        id: MsgId,
        method: "client/unregisterCapability",
        params: _{
            unregisterations: Unregistrations
        }
    },
    send_client_message(Msg),
    receive_response(MsgId, _Response).

%% apply_workspace_edit(+Label, +Edit, -Response) is det.
%
% Requests the client to apply a workspace edit.
%
% @param Label A label describing the edit.
% @param Edit The workspace edit dictionary.
% @param Response The client's response to the apply edit request.
%
% @example
%    % Request the client to apply workspace edits.
%    Edit = _{
%        changes: _{
%            "file:///path/to/file.pl": [
%                _{
%                    range: _{
%                        start: _{line: 10, character: 0},
%                        end: _{line: 20, character: 0}
%                    },
%                    newText: "% Refactored code here\nnew_code_predicate :- true."
%                }
%            ]
%        }
%    },
%    Label = "Refactor MeTTa Code",
%    apply_workspace_edit(Label, Edit, ApplyEditResponse),
%    writeln(ApplyEditResponse).
% @end_example
%
apply_workspace_edit(Label, Edit, Response) :-
    create_msg_id(MsgId),
    Msg = _{

        id: MsgId,
        method: "workspace/applyEdit",
        params: _{
            label: Label,
            edit: Edit
        }
    },
    send_client_message(Msg),
    receive_response(MsgId, Response).

%% fetch_workspace_configuration(+ConfigurationItems, -Configurations) is det.
%
% Retrieves configuration settings from the client.
%
% @param ConfigurationItems A list of configuration item dictionaries.
% @param Configurations The configurations retrieved from the client.
%
% @example
%    % Retrieve configuration settings from the client.
%    ConfigurationItems = [
%        _{scopeUri: "file:///path/to/workspace", section: "metta-lsp"}
%    ],
%    fetch_workspace_configuration(ConfigurationItems, Configurations),
%    writeln(Configurations).
% @end_example
%
fetch_workspace_configuration(ConfigurationItems, Configurations) :-
    create_msg_id(MsgId),
    Msg = _{

        id: MsgId,
        method: "workspace/configuration",
        params: _{
            items: ConfigurationItems
        }
    },
    send_client_message(Msg),
    receive_response(MsgId, Configurations).


%% fetch_workspace_folders(-Folders) is det.
%
% Retrieves the list of workspace folders from the client.
%
% @param Folders The list of workspace folders.
%
% @example
%    % Retrieve the list of workspace folders.
%    fetch_workspace_folders(WorkspaceFolders),
%    writeln(WorkspaceFolders).
% @end_example
%
fetch_workspace_folders(Folders) :-
    create_msg_id(MsgId),
    Msg = _{

        id: MsgId,
        method: "workspace/workspaceFolders",
        params: _{}
    },
    send_client_message(Msg),
    receive_response(MsgId, Folders).

%% show_document(+Uri, +TakeFocus, +Selection, -Success) is det.
%
% Requests the client to open or reveal a document.
%
% @param Uri The URI of the document to show.
% @param TakeFocus Whether to bring the document into focus (true or false).
% @param Selection An optional range to select within the document.
% @param Success True if the document was successfully shown.
%
% @example
%    % Request the client to open or reveal a document.
%    Uri = "file:///path/to/file.pl",
%    TakeFocus = true,
%    Selection = _{
%        start: _{line: 5, character: 0},
%        end: _{line: 5, character: 10}
%    },
%    show_document(Uri, TakeFocus, Selection, Success),
%    writeln(Success).
% @end_example
%
show_document(Uri, TakeFocus, Selection, Success) :-
    create_msg_id(MsgId),
    Msg = _{

        id: MsgId,
        method: "window/showDocument",
        params: _{
            uri: Uri,
            external: false,
            takeFocus: TakeFocus,
            selection: Selection
        }
    },
    send_client_message(Msg),
    receive_response(MsgId, Response),
    ((first_dict_key(success,Response,Success), Success\==none)->true;Success=true).

%% refresh_code_lens is det.
%
% Requests the client to refresh all code lenses.
%
% @example
%    % Request the client to refresh all code lenses.
%    refresh_code_lens.
% @end_example
%
refresh_code_lens :-
    Msg = _{

        method: "workspace/codeLens/refresh",
        params: _{}
    },
    send_client_message(Msg).

%% refresh_inlay_hints is det.
%
% Requests the client to refresh all inlay hints.
%
% @example
%    % Request the client to refresh all inlay hints.
%    refresh_inlay_hints.
% @end_example
%
refresh_inlay_hints :-
    Msg = _{

        method: "workspace/inlayHint/refresh",
        params: _{}
    },
    send_client_message(Msg).

%% refresh_semantic_tokens is det.
%
% Requests the client to refresh all semantic tokens.
%
% @example
%    % Request the client to refresh all semantic tokens.
%    refresh_semantic_tokens.
% @end_example
%
refresh_semantic_tokens :-
    Msg = _{

        method: "workspace/semanticTokens/refresh",
        params: _{}
    },
    send_client_message(Msg).

%% send_log_message(+Message, +MessageType) is det.
%
% Sends a log message to the client.
%
% @param Message The message string to log.
% @param MessageType The type of message (atom, string, or number).
%
% @example
%    % Send a log message to the client.
%    Message = "Indexing completed successfully.",
%    MessageType = info,
%    send_log_message(Message, MessageType).
% @end_example
%
send_log_message(Message, MessageType) :-
    into_message_string(Message, SMessage),
    resolve_enum(message_type_value, MessageType, MessageTypeValue),
    Msg = _{

        method: "window/logMessage",
        params: _{
            type: MessageTypeValue,
            message: SMessage
        }
    },
    send_client_message(Msg).

%% send_telemetry_event(+Data) is det.
%
% Sends telemetry data to the client.
%
% @param Data A dictionary containing the telemetry event data.
%
% @example
%    % Send telemetry data to the client.
%    TelemetryData = _{
%        event: "startup",
%        timestamp: 1638316800,
%        details: _{
%            version: "1.0.0",
%            os: "Linux"
%        }
%    },
%    send_telemetry_event(TelemetryData).
% @end_example
%
send_telemetry_event(Data) :-
    Msg = _{

        method: "telemetry/event",
        params: Data
    },
    send_client_message(Msg).

%% report_progress(+Token, +Value) is det.
%
% Reports progress for a long-running operation.
%
% @param Token The progress token used when creating the progress.
% @param Value A dictionary containing the progress value to report.
%
% @example
%    % Report progress for a long-running operation.
%    Token = "progress_token_123",
%    ProgressValue = _{
%        kind: "report",
%        message: "Processing files...",
%        percentage: 50
%    },
%    report_progress(Token, ProgressValue).
% @end_example
%
report_progress(Token, Value) :-
    Msg = _{

        method: "$/progress",
        params: _{
            token: Token,
            value: Value
        }
    },
    send_client_message(Msg).

%% cancel_work_done_progress(+Token) is det.
%
% Informs the client that a progress operation has been canceled.
%
% @param Token The progress token identifying the operation to cancel.
%
% @example
%    % Inform the client that a progress operation has been canceled.
%    Token = "progress_token_123",
%    cancel_work_done_progress(Token).
% @end_example
%
cancel_work_done_progress(Token) :-
    Msg = _{

        method: "window/workDoneProgress/cancel",
        params: _{
            token: Token
        }
    },
    send_client_message(Msg).

%% publish_diagnostics(+Uri, +Diagnostics, +Version, +Options) is det.
%
% Publishes diagnostics for a document to the client.
%
% @param Uri The document URI for which diagnostics are reported.
% @param Diagnostics A list of diagnostic dictionaries.
% @param Version (Optional) The version number of the document.
% @param Options (Optional) Additional options as a dictionary.
%
% @example
%    % Publish diagnostics for a document.
%    Uri = "file:///path/to/file.pl",
%    Diagnostics = [
%        _{
%            range: _{
%                start: _{line: 10, character: 5},
%                end: _{line: 10, character: 15}
%            },
%            severity: error,
%            code: "syntax_error",
%            source: "metta-lsp",
%            message: "Syntax error: unexpected token."
%        }
%    ],
%    publish_diagnostics(Uri, Diagnostics, _Version, _Options).
% @end_example
%
publish_diagnostics(Uri, Diagnostics, Version, Options) :-
    % Resolve enum values in diagnostics
  ((
    flatten([Diagnostics],DiagnosticsL),
    must_succeed1(maplist(resolve_diagnostic_enum, DiagnosticsL, ResolvedDiagnostics)),
    % Build the params dictionary
    ParamsBase = _{
        uri: Uri,
        diagnostics: ResolvedDiagnostics
    },
    % Include version if provided
    (nonvar(Version) -> Params1 = ParamsBase.put(version, Version); Params1 = ParamsBase),
    % Include additional options if provided
    (nonvar(Options) -> Params = Params1.put(Options); Params = Params1),
    % Construct the message
    Msg = _{

        method: "textDocument/publishDiagnostics",
        params: Params
    },
    send_client_message(Msg))).

% Helper predicate to resolve enums in diagnostics
resolve_diagnostic_enum(Diagnostic, ResolvedDiagnostic) :-
    % Resolve severity enum if present
    ( get_dict(severity, Diagnostic, SeverityInput) ->
      ( must_succeed1(resolve_enum(diagnostic_severity_value, SeverityInput, SeverityValue)),
        ResolvedDiagnostic = Diagnostic.put(severity, SeverityValue))
    ;
        ResolvedDiagnostic = Diagnostic
    ).


% Predicate to handle the entry point where JSON is the input dictionary.
:- dynamic(user:stored_json_value/3).

% Save JSON as stored_json_value facts with reversed keys.
save_json(Pred, JSON) :-
    save_json_value(Pred, [], JSON).

% Handle values that could be nested dictionaries or lists with dictionaries.
save_json_value(Pred, Path, Dict) :-
    is_dict(Dict), !,
    dict_pairs(Dict, _, Pairs),
    maplist({Pred, Path}/[Key-Value]>>(
        append(Path, [Key], NewPath),
        save_json_value(Pred, NewPath, Value)
    ), Pairs).
save_json_value(Pred, Path, List) :-
    % Check if a list contains any dictionaries (nested or directly).
    is_list(List), sub_term(Sub, List), is_dict(Sub), !,
    maplist(save_json_value(Pred, Path), List).
save_json_value(Pred, Path, Value) :-
    reverse(Path, RevPath),
    retractall(user:stored_json_value(Pred, RevPath, _)),
    asserta(user:stored_json_value(Pred, RevPath, Value)).

end_of_file.

3 ?- listing(stored_json_value).
:- dynamic stored_json_value/3.
:- module_transparent stored_json_value/3.

stored_json_value(client_configuration, [temperature, chatgpt, xtras], 0.7).
stored_json_value(client_configuration, [model, chatgpt, xtras], "gpt-3.5-turbo").
stored_json_value(client_configuration, [maxTokens, chatgpt, xtras], 500).
stored_json_value(client_configuration, [enabled, chatgpt, xtras], true).
stored_json_value(client_configuration, [apiKey, chatgpt, xtras], "").
stored_json_value(client_configuration, [showDeveloperHoverDebug, trace], "true").
stored_json_value(client_configuration, [server, trace], "off").
stored_json_value(client_configuration, [sendDiagnostics], "false").
stored_json_value(client_configuration, [options], ["debug_main", "debug_errors", "debug_threads", "debug_high", "debug_todo", "debug_xref"]).
stored_json_value(client_configuration, [maxNumberOfProblems], 1000).
stored_json_value(client_configuration, [features], ["codeExecution", "systemCodeIndexing", "codeCommenting", "testRunning"]).
stored_json_value(client_configuration, [showIncompleteFeatures, debug], false).
stored_json_value(client_configuration, [options, debug], ["show_thread_monitor", "lsp_todo"]).
stored_json_value(client_configuration, [model, chatgpt], "gpt-3.5-turbo").
stored_json_value(client_configuration, [apiKey, chatgpt], "666").
stored_json_value(client_capabilities, [workspaceFolders, workspace], true).
stored_json_value(client_capabilities, [resourceOperations, workspaceEdit, workspace], ["create", "rename", "delete"]).
stored_json_value(client_capabilities, [normalizesLineEndings, workspaceEdit, workspace], true).
stored_json_value(client_capabilities, [failureHandling, workspaceEdit, workspace], "textOnlyTransactional").
stored_json_value(client_capabilities, [documentChanges, workspaceEdit, workspace], true).
stored_json_value(client_capabilities, [groupsOnLabel, changeAnnotationSupport, workspaceEdit, workspace], true).
stored_json_value(client_capabilities, [valueSet, tagSupport, symbol, workspace], [1]).
stored_json_value(client_capabilities, [valueSet, symbolKind, symbol, workspace], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]).
stored_json_value(client_capabilities, [properties, resolveSupport, symbol, workspace], ["location.range"]).
stored_json_value(client_capabilities, [dynamicRegistration, symbol, workspace], true).
stored_json_value(client_capabilities, [refreshSupport, semanticTokens, workspace], true).
stored_json_value(client_capabilities, [refreshSupport, inlineValue, workspace], true).
stored_json_value(client_capabilities, [refreshSupport, inlayHint, workspace], true).
stored_json_value(client_capabilities, [refreshSupport, foldingRange, workspace], true).
stored_json_value(client_capabilities, [willRename, fileOperations, workspace], true).
stored_json_value(client_capabilities, [willDelete, fileOperations, workspace], true).
stored_json_value(client_capabilities, [willCreate, fileOperations, workspace], true).
stored_json_value(client_capabilities, [dynamicRegistration, fileOperations, workspace], true).
stored_json_value(client_capabilities, [didRename, fileOperations, workspace], true).
stored_json_value(client_capabilities, [didDelete, fileOperations, workspace], true).
stored_json_value(client_capabilities, [didCreate, fileOperations, workspace], true).
stored_json_value(client_capabilities, [dynamicRegistration, executeCommand, workspace], true).
stored_json_value(client_capabilities, [relativePatternSupport, didChangeWatchedFiles, workspace], true).
stored_json_value(client_capabilities, [dynamicRegistration, didChangeWatchedFiles, workspace], true).
stored_json_value(client_capabilities, [dynamicRegistration, didChangeConfiguration, workspace], true).
stored_json_value(client_capabilities, [refreshSupport, diagnostics, workspace], true).
stored_json_value(client_capabilities, [configuration, workspace], true).
stored_json_value(client_capabilities, [refreshSupport, codeLens, workspace], true).
stored_json_value(client_capabilities, [applyEdit, workspace], true).
stored_json_value(client_capabilities, [workDoneProgress, window], true).
stored_json_value(client_capabilities, [additionalPropertiesSupport, messageActionItem, showMessage, window], true).
stored_json_value(client_capabilities, [support, showDocument, window], true).
stored_json_value(client_capabilities, [dynamicRegistration, typeHierarchy, textDocument], true).
stored_json_value(client_capabilities, [linkSupport, typeDefinition, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, typeDefinition, textDocument], true).
stored_json_value(client_capabilities, [willSaveWaitUntil, synchronization, textDocument], true).
stored_json_value(client_capabilities, [willSave, synchronization, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, synchronization, textDocument], true).
stored_json_value(client_capabilities, [didSave, synchronization, textDocument], true).
stored_json_value(client_capabilities, [labelOffsetSupport, parameterInformation, signatureInformation, signatureHelp, textDocument], true).
stored_json_value(client_capabilities, [documentationFormat, signatureInformation, signatureHelp, textDocument], ["markdown", "plaintext"]).
stored_json_value(client_capabilities, [activeParameterSupport, signatureInformation, signatureHelp, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, signatureHelp, textDocument], true).
stored_json_value(client_capabilities, [contextSupport, signatureHelp, textDocument], true).
stored_json_value(client_capabilities, [tokenTypes, semanticTokens, textDocument], ["namespace", "type", "class", "enum", "interface", "struct", "typeParameter", "parameter", "variable", "property", "enumMember", "event", "function", "method", "macro", "keyword", "modifier", "comment", "string", "number", "regexp", "operator", "decorator"]).
stored_json_value(client_capabilities, [tokenModifiers, semanticTokens, textDocument], ["declaration", "definition", "readonly", "static", "deprecated", "abstract", "async", "modification", "documentation", "defaultLibrary"]).
stored_json_value(client_capabilities, [serverCancelSupport, semanticTokens, textDocument], true).
stored_json_value(client_capabilities, [range, requests, semanticTokens, textDocument], true).
stored_json_value(client_capabilities, [delta, full, requests, semanticTokens, textDocument], true).
stored_json_value(client_capabilities, [overlappingTokenSupport, semanticTokens, textDocument], false).
stored_json_value(client_capabilities, [multilineTokenSupport, semanticTokens, textDocument], false).
stored_json_value(client_capabilities, [formats, semanticTokens, textDocument], ["relative"]).
stored_json_value(client_capabilities, [dynamicRegistration, semanticTokens, textDocument], true).
stored_json_value(client_capabilities, [augmentsSyntaxTokens, semanticTokens, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, selectionRange, textDocument], true).
stored_json_value(client_capabilities, [prepareSupportDefaultBehavior, rename, textDocument], 1).
stored_json_value(client_capabilities, [prepareSupport, rename, textDocument], true).
stored_json_value(client_capabilities, [honorsChangeAnnotations, rename, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, rename, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, references, textDocument], true).
stored_json_value(client_capabilities, [rangesSupport, rangeFormatting, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, rangeFormatting, textDocument], true).
stored_json_value(client_capabilities, [versionSupport, publishDiagnostics, textDocument], false).
stored_json_value(client_capabilities, [valueSet, tagSupport, publishDiagnostics, textDocument], [1, 2]).
stored_json_value(client_capabilities, [relatedInformation, publishDiagnostics, textDocument], true).
stored_json_value(client_capabilities, [dataSupport, publishDiagnostics, textDocument], true).
stored_json_value(client_capabilities, [codeDescriptionSupport, publishDiagnostics, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, onTypeFormatting, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, linkedEditingRange, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, inlineValue, textDocument], true).
stored_json_value(client_capabilities, [properties, resolveSupport, inlayHint, textDocument], ["tooltip", "textEdits", "label.tooltip", "label.location", "label.command"]).
stored_json_value(client_capabilities, [dynamicRegistration, inlayHint, textDocument], true).
stored_json_value(client_capabilities, [linkSupport, implementation, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, implementation, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, hover, textDocument], true).
stored_json_value(client_capabilities, [contentFormat, hover, textDocument], ["markdown", "plaintext"]).
stored_json_value(client_capabilities, [dynamicRegistration, formatting, textDocument], true).
stored_json_value(client_capabilities, [rangeLimit, foldingRange, textDocument], 5000).
stored_json_value(client_capabilities, [lineFoldingOnly, foldingRange, textDocument], true).
stored_json_value(client_capabilities, [valueSet, foldingRangeKind, foldingRange, textDocument], ["comment", "imports", "region"]).
stored_json_value(client_capabilities, [collapsedText, foldingRange, foldingRange, textDocument], false).
stored_json_value(client_capabilities, [dynamicRegistration, foldingRange, textDocument], true).
stored_json_value(client_capabilities, [valueSet, tagSupport, documentSymbol, textDocument], [1]).
stored_json_value(client_capabilities, [valueSet, symbolKind, documentSymbol, textDocument], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]).
stored_json_value(client_capabilities, [labelSupport, documentSymbol, textDocument], true).
stored_json_value(client_capabilities, [hierarchicalDocumentSymbolSupport, documentSymbol, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, documentSymbol, textDocument], true).
stored_json_value(client_capabilities, [tooltipSupport, documentLink, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, documentLink, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, documentHighlight, textDocument], true).
stored_json_value(client_capabilities, [relatedDocumentSupport, diagnostic, textDocument], false).
stored_json_value(client_capabilities, [dynamicRegistration, diagnostic, textDocument], true).
stored_json_value(client_capabilities, [linkSupport, definition, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, definition, textDocument], true).
stored_json_value(client_capabilities, [linkSupport, declaration, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, declaration, textDocument], true).
stored_json_value(client_capabilities, [insertTextMode, completion, textDocument], 2).
stored_json_value(client_capabilities, [dynamicRegistration, completion, textDocument], true).
stored_json_value(client_capabilities, [contextSupport, completion, textDocument], true).
stored_json_value(client_capabilities, [itemDefaults, completionList, completion, textDocument], ["commitCharacters", "editRange", "insertTextFormat", "insertTextMode", "data"]).
stored_json_value(client_capabilities, [valueSet, completionItemKind, completion, textDocument], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]).
stored_json_value(client_capabilities, [valueSet, tagSupport, completionItem, completion, textDocument], [1]).
stored_json_value(client_capabilities, [snippetSupport, completionItem, completion, textDocument], true).
stored_json_value(client_capabilities, [properties, resolveSupport, completionItem, completion, textDocument], ["documentation", "detail", "additionalTextEdits"]).
stored_json_value(client_capabilities, [preselectSupport, completionItem, completion, textDocument], true).
stored_json_value(client_capabilities, [labelDetailsSupport, completionItem, completion, textDocument], true).
stored_json_value(client_capabilities, [valueSet, insertTextModeSupport, completionItem, completion, textDocument], [1, 2]).
stored_json_value(client_capabilities, [insertReplaceSupport, completionItem, completion, textDocument], true).
stored_json_value(client_capabilities, [documentationFormat, completionItem, completion, textDocument], ["markdown", "plaintext"]).
stored_json_value(client_capabilities, [deprecatedSupport, completionItem, completion, textDocument], true).
stored_json_value(client_capabilities, [commitCharactersSupport, completionItem, completion, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, colorProvider, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, codeLens, textDocument], true).
stored_json_value(client_capabilities, [properties, resolveSupport, codeAction, textDocument], ["edit"]).
stored_json_value(client_capabilities, [isPreferredSupport, codeAction, textDocument], true).
stored_json_value(client_capabilities, [honorsChangeAnnotations, codeAction, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, codeAction, textDocument], true).
stored_json_value(client_capabilities, [disabledSupport, codeAction, textDocument], true).
stored_json_value(client_capabilities, [dataSupport, codeAction, textDocument], true).
stored_json_value(client_capabilities, [valueSet, codeActionKind, codeActionLiteralSupport, codeAction, textDocument], ["", "quickfix", "refactor", "refactor.extract", "refactor.inline", "refactor.rewrite", "source", "source.organizeImports"]).
stored_json_value(client_capabilities, [dynamicRegistration, callHierarchy, textDocument], true).
stored_json_value(client_capabilities, [executionSummarySupport, synchronization, notebookDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, synchronization, notebookDocument], true).
stored_json_value(client_capabilities, [retryOnContentModified, staleRequestSupport, general], ["textDocument/semanticTokens/full", "textDocument/semanticTokens/range", "textDocument/semanticTokens/full/delta"]).
stored_json_value(client_capabilities, [cancel, staleRequestSupport, general], true).
stored_json_value(client_capabilities, [version, regularExpressions, general], "ES2020").
stored_json_value(client_capabilities, [engine, regularExpressions, general], "ECMAScript").
stored_json_value(client_capabilities, [positionEncodings, general], ["utf-16"]).
stored_json_value(client_capabilities, [version, markdown, general], "1.1.0").
stored_json_value(client_capabilities, [parser, markdown, general], "marked").

true.


emacs


1 ?- listing(stored_json_value).
:- dynamic stored_json_value/3.
:- module_transparent stored_json_value/3.

stored_json_value(client_capabilities, [workspaceFolders, workspace], true).
stored_json_value(client_capabilities, [resourceOperations, workspaceEdit, workspace], ["create", "rename", "delete"]).
stored_json_value(client_capabilities, [documentChanges, workspaceEdit, workspace], true).
stored_json_value(client_capabilities, [valueSet, symbolKind, symbol, workspace], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]).
stored_json_value(client_capabilities, [willRename, fileOperations, workspace], true).
stored_json_value(client_capabilities, [willDelete, fileOperations, workspace], false).
stored_json_value(client_capabilities, [willCreate, fileOperations, workspace], false).
stored_json_value(client_capabilities, [didRename, fileOperations, workspace], true).
stored_json_value(client_capabilities, [didDelete, fileOperations, workspace], false).
stored_json_value(client_capabilities, [didCreate, fileOperations, workspace], false).
stored_json_value(client_capabilities, [dynamicRegistration, executeCommand, workspace], false).
stored_json_value(client_capabilities, [dynamicRegistration, didChangeWatchedFiles, workspace], true).
stored_json_value(client_capabilities, [configuration, workspace], true).
stored_json_value(client_capabilities, [refreshSupport, codeLens, workspace], true).
stored_json_value(client_capabilities, [applyEdit, workspace], true).
stored_json_value(client_capabilities, [workDoneProgress, window], true).
stored_json_value(client_capabilities, [support, showDocument, window], true).
stored_json_value(client_capabilities, [dynamicRegistration, typeHierarchy, textDocument], true).
stored_json_value(client_capabilities, [linkSupport, typeDefinition, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, typeDefinition, textDocument], true).
stored_json_value(client_capabilities, [willSaveWaitUntil, synchronization, textDocument], true).
stored_json_value(client_capabilities, [willSave, synchronization, textDocument], true).
stored_json_value(client_capabilities, [didSave, synchronization, textDocument], true).
stored_json_value(client_capabilities, [labelOffsetSupport, parameterInformation, signatureInformation, signatureHelp, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, signatureHelp, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, selectionRange, textDocument], true).
stored_json_value(client_capabilities, [prepareSupport, rename, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, rename, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, references, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, rangeFormatting, textDocument], true).
stored_json_value(client_capabilities, [versionSupport, publishDiagnostics, textDocument], true).
stored_json_value(client_capabilities, [valueSet, tagSupport, publishDiagnostics, textDocument], [1, 2]).
stored_json_value(client_capabilities, [relatedInformation, publishDiagnostics, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, onTypeFormatting, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, linkedEditingRange, textDocument], true).
stored_json_value(client_capabilities, [linkSupport, implementation, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, implementation, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, hover, textDocument], true).
stored_json_value(client_capabilities, [contentFormat, hover, textDocument], ["markdown", "plaintext"]).
stored_json_value(client_capabilities, [dynamicRegistration, formatting, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, foldingRange, textDocument], true).
stored_json_value(client_capabilities, [valueSet, symbolKind, documentSymbol, textDocument], [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26]).
stored_json_value(client_capabilities, [hierarchicalDocumentSymbolSupport, documentSymbol, textDocument], true).
stored_json_value(client_capabilities, [tooltipSupport, documentLink, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, documentLink, textDocument], true).
stored_json_value(client_capabilities, [relatedDocumentSupport, diagnostic, textDocument], false).
stored_json_value(client_capabilities, [dynamicRegistration, diagnostic, textDocument], false).
stored_json_value(client_capabilities, [linkSupport, definition, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, definition, textDocument], true).
stored_json_value(client_capabilities, [linkSupport, declaration, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, declaration, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, completion, textDocument], true).
stored_json_value(client_capabilities, [contextSupport, completion, textDocument], true).
stored_json_value(client_capabilities, [snippetSupport, completionItem, completion, textDocument], false).
stored_json_value(client_capabilities, [properties, resolveSupport, completionItem, completion, textDocument], ["documentation", "detail", "additionalTextEdits", "command"]).
stored_json_value(client_capabilities, [resolveAdditionalTextEditsSupport, completionItem, completion, textDocument], true).
stored_json_value(client_capabilities, [valueSet, insertTextModeSupport, completionItem, completion, textDocument], [1, 2]).
stored_json_value(client_capabilities, [insertReplaceSupport, completionItem, completion, textDocument], true).
stored_json_value(client_capabilities, [documentationFormat, completionItem, completion, textDocument], ["markdown", "plaintext"]).
stored_json_value(client_capabilities, [deprecatedSupport, completionItem, completion, textDocument], true).
stored_json_value(client_capabilities, [properties, resolveSupport, codeAction, textDocument], ["edit", "command"]).
stored_json_value(client_capabilities, [isPreferredSupport, codeAction, textDocument], true).
stored_json_value(client_capabilities, [dynamicRegistration, codeAction, textDocument], true).
stored_json_value(client_capabilities, [dataSupport, codeAction, textDocument], true).
stored_json_value(client_capabilities, [valueSet, codeActionKind, codeActionLiteralSupport, codeAction, textDocument], ["", "quickfix", "refactor", "refactor.extract", "refactor.inline", "refactor.rewrite", "source", "source.organizeImports"]).
stored_json_value(client_capabilities, [dynamicRegistration, callHierarchy, textDocument], false).
stored_json_value(client_capabilities, [positionEncodings, general], ["utf-32", "utf-16"]).

true.

2 ?-


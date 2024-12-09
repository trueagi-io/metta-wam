%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lsp_metta_save_actions.pl
%
% Purpose:
%
% A Learning Experiment: Add a Date adding hook in `willSaveWaitUntil`
%
% This module mimics the behavior of CVS (Concurrent Versions System) by managing and
% updating a file header with a CVS-style date comment. Specifically, it updates or
% inserts a line at the top of each file with the format:
%
%   ; $Date: YYYY-MM-DD HH:MM:SS $
%
% This date comment records the last saved date and time of the file. When the file
% is about to be saved, the system checks the top of the file for this comment. If
% it is found, the date is updated. If not, a new comment with the current date and
% time is inserted at the top. This approach ensures that every file maintains a
% record of its most recent modification, in the same way that CVS used to maintain
% metadata in file headers.
%
% Key Responsibilities:
% - **CVS-Style Date Comment Management**: Tracks and updates a date comment formatted
%   as `; $Date: YYYY-MM-DD HH:MM:SS $` at the top of each file in the workspace.
% - **Date Update or Insertion**: On each save, the date comment is either updated
%   with the current date and time or added if it is not found.
% - **Integration with LSP**: This module hooks into the `textDocument/willSaveWaitUntil`
%   request in the Language Server Protocol (LSP) to ensure that the date comment is
%   correctly updated before the file is saved.
%
% How It Works:
% - **Date Comment Detection**: When a file is saved, the module checks the first line
%   of the file for a valid CVS-style date comment. This comment must match the format
%   `; $Date: YYYY-MM-DD HH:MM:SS $`.
% - **Date Update**: If the comment is found, it is updated with the current date and
%   time.
% - **Date Insertion**: If the comment is not found, a new date comment is inserted at
%   the top of the file.
%
% Example:
% - Before saving:
%   ```
%   ; $Date: 2023-09-15 10:45:12 $
%   print("Hello, World!")
%   ```
%
% - After saving (with current date):
%   ```
%   ; $Date: 2024-10-01 12:34:56 $
%   print("Hello, World!")
%   ```
%
% Integration with LSP:
% - The module integrates with the LSP `willSaveWaitUntil` request, ensuring that the
%   date comment is updated or added just before the file is saved.
%
% Author: Douglas Miles
% Date: 10-21-2024
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(lsp_metta_include).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle the 'textDocument/willSaveWaitUntil' Request and Execute All Save Actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lsp_hooks:handle_msg_hook("textDocument/willSaveWaitUntil", Msg, _{id: Id, result: FinalEdits}) :-
    _{id: Id, params: Params} :< Msg,
    _{textDocument: _{uri: Uri}} :< Params,
    find_and_execute_save_action_hooks(Uri, [], FinalEdits).  % Start with an empty initial edit list

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Centralized Save Action Handler - Loops through All Save Actions by Retrieving Bodies
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_and_execute_save_action_hooks(Uri, CurrentEdits, FinalEdits) :-
    find_and_execute_save_actions(Uri, CurrentEdits, FinalEdits).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper: Loop Through and Execute Save Actions Using clause/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
find_and_execute_save_actions(Uri, CurrentEdits, FinalEdits) :-
    (   clause(lsp_hooks:handle_save_actions(Uri, CurrentEdits), Body)  % Retrieve the body of the action
    ->  call(Body, Uri, CurrentEdits, NewEdits),  % Call the body dynamically and get updated edits
        find_and_execute_save_actions(Uri, NewEdits, FinalEdits)  % Continue to the next action
    ;   FinalEdits = CurrentEdits  % If no more actions, return the final edits
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example Save Action: Update or Add Date Comment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lsp_hooks:handle_save_actions(Uri, CurrentEdits, NewEdits) :-
    update_or_add_date_comment(Uri, CurrentEdits, NewEdits).

% Actual logic for updating or adding a date comment
update_or_add_date_comment(Uri, CurrentEdits, NewEdits) :-
    get_current_date_comment(NewDateComment),
    source_file_text(Uri, Text),
    split_string(Text, "\n", "", [FirstLine | _Rest]),  % Split the document into lines
    (   valid_date_comment(FirstLine)
    ->  NewEdits = [_{
            range: _{
                start: _{line: 0, character: 0},
                end: _{line: 0, character: _}
            },
            newText: NewDateComment
        } | CurrentEdits]
    ;   NewEdits = [_{
            range: _{
                start: _{line: 0, character: 0},
                end: _{line: 0, character: 0}
            },
            newText: NewDateComment
        } | CurrentEdits]
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Another Save Action: Log the Save Event
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lsp_hooks:handle_save_actions(Uri, CurrentEdits, CurrentEdits) :-
    log_save_event(Uri).

% Helper for logging the save event
log_save_event(Uri) :-
    format("Save action executed for: ~w~n", [Uri]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Placeholder for Additional Save Actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lsp_hooks:handle_save_actions(Uri, CurrentEdits, CurrentEdits) :-
    log_additional_action(Uri).

% Helper for additional save actions
log_additional_action(Uri) :-
    format("Another save action executed for: ~w~n", [Uri]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper to get the current date comment in string format (; $Date: YYYY-MM-DD HH:MM:SS $)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_current_date_comment(CommentText) :-
    get_time(Stamp),
    format_time(atom(Date), "%Y-%m-%d %H:%M:%S", Stamp),
    format(atom(CommentText), '; $Date: ~w $\n', [Date]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper to determine if a string is a valid date comment (; $Date: YYYY-MM-DD HH:MM:SS $)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
valid_date_comment(Line) :-
    sub_atom(Line, 0, 8, _, "; $Date:"),   % Check if the line starts with the CVS-style date comment format
    sub_atom(Line, 9, 4, _, Year),         % Extract the year part
    sub_atom(Line, 14, 2, _, Month),       % Extract the month part
    sub_atom(Line, 17, 2, _, Day),         % Extract the day part
    sub_atom(Line, 20, 2, _, Hour),        % Extract the hour part
    sub_atom(Line, 23, 2, _, Minute),      % Extract the minute part
    sub_atom(Line, 26, 2, _, Second),      % Extract the second part
    atom_number(Year, _),                  % Check if year is a number
    atom_number(Month, _),                 % Check if month is a number
    atom_number(Day, _),                   % Check if day is a number
    atom_number(Hour, _),                  % Check if hour is a number
    atom_number(Minute, _),                % Check if minute is a number
    atom_number(Second, _),                % Check if second is a number,
    sub_atom(Line, 13, 1, _, '-'),         % Check if there's a dash after the year
    sub_atom(Line, 16, 1, _, '-'),         % Check if there's a dash after the month
    sub_atom(Line, 19, 1, _, ' '),         % Check if there's a space after the day
    sub_atom(Line, 22, 1, _, ':'),         % Check if there's a colon after the hour
    sub_atom(Line, 25, 1, _, ':').         % Check if there's a colon after the minute


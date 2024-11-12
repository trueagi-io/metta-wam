/* 
   current_column/1 retrieves the current column position of the output stream.
   This predicate uses two methods: checking line_position and stream_position_data.
*/

% Retrieve the current column position from the current output stream
% @param Column - The column number to be returned
current_column(Column) :- 
    current_output(Stream), % Get the current output stream
    line_position(Stream, Column), % Get the column from the stream using line_position
    !. % Cut to ensure no backtracking occurs

% Fallback to stream_property if line_position is not available
% @param Column - The column number to be returned
current_column(Column) :- 
    stream_property(current_output, position(Position)), % Get the stream position properties
    stream_position_data(column, Position, Column). % Extract the column number

/* 
   min_indent/1 ensures the output is indented by a specified number of spaces.
   It uses current_column to decide whether to add a new line before indenting.
*/

% Ensure that the output has at least Sz spaces of indentation
% @param Sz - The required indentation size
min_indent(Sz) :- 
    current_column(Col), % Get the current column position
    Col > Sz, % Check if current column is already indented enough
    nl, % If not, start a new line
    indent_len(Sz). % Indent to the desired length

% If the current column is less than the required size, pad with spaces
% @param Sz - The required indentation size
min_indent(Sz) :- 
    current_column(Col), % Get the current column position
    Need is Sz - Col, % Calculate how many spaces are needed
    indent_len(Need), % Add the required number of spaces
    !. % Prevent backtracking

% Always ensure indentation by starting a new line if necessary
% @param Sz - The required indentation size
min_indent(Sz) :- 
    nl, % Start a new line
    indent_len(Sz). % Add the required indentation

% Helper predicate to print 'Need' spaces
% @param Need - The number of spaces to print
indent_len(Need) :- 
    forall(between(1, Need, _), write(' ')). % Write a space for each number in the range

/*
   w_proper_indent/2 ensures proper indentation before executing a given goal G.
   It calculates the indentation level based on the flag value and the given N.
*/

% Write with proper indentation based on a flag
% @param N - The base indentation size
% @param G - The goal to execute
w_proper_indent(N, G) :- 
    flag(w_in_p, X, X), % Get the current flag value for indentation level
    XX is (X * 2) + N, % Calculate the indentation size
    setup_call_cleanup(min_indent(XX), G, true). % Ensure proper indentation and execute the goal

/* 
   w_in_p/1 increments the indentation flag for nested structures,
   ensuring proper indentation inside complex expressions.
*/

% Temporarily increment the indentation flag and execute a goal
% @param G - The goal to execute
w_in_p(G) :- 
    setup_call_cleanup(flag(w_in_p, X, X + 1), G, flag(w_in_p, _, X)). % Increment flag, execute G, then reset flag

/*
   always_dash_functor/2 ensures that a functor is replaced with a 'dashed' version
   if it doesn't match the second argument (used for transforming functor names).
*/

% Ensure functors are replaced with dashed versions if necessary
% @param A - The original functor
% @param B - The transformed functor
always_dash_functor(A, B) :- 
    once(dash_functor(A, B)), % Perform the dash transformation once
    A \=@= B, % Ensure the original and transformed functors are not syntactically equal
    !. % Cut to prevent backtracking

% If A and B are already equal, no transformation is needed
% @param A - The original functor
always_dash_functor(A, A).

/*
   dash_functor/2 transforms a functor into a 'dashed' version unless it's a symbol.
   The transformation is skipped if it's already a symbol.
*/

dash_functor(A,C):- \+ symbol(A),!,C=A.
% dash_functor(A,C):- p2m(A,B),A\==B,!,always_dash_functor(B,C).
dash_functor(ASymbolProc,O):- fail, symbol_contains(ASymbolProc,'_'),
    symbol_contains(ASymbolProc,'atom'),
    current_predicate(system:ASymbolProc/_),
    symbolic_list_concat(LS,'atom',ASymbolProc),
    symbolic_list_concat(LS,'symbol',SymbolProc),
    always_dash_functor(SymbolProc,O),!.
dash_functor(ASymbolProc,O):- symbol_concat('$',LS,ASymbolProc),!,
    symbol_concat('%',LS,SymbolProc),
    always_dash_functor(SymbolProc,O).

dash_functor(Functor,DFunctor):- fail,
   symbolic_list_concat(L,'_',Functor), L\=[_],
   symbolic_list_concat(L,'-',DFunctor).



% Print the arguments of a compound term in S-expression format
% @param Args - A list of arguments to print
write_args_as_sexpression([]). % Base case: empty argument list
write_args_as_sexpression([H|T]) :- 
    write(' '), % Print a space between arguments
    pp_sex(H), % Pretty-print the first argument
    write_args_as_sexpression(T). % Recursively print the rest

% Print the elements of a list in S-expression format
% @param List - The list to print
print_list_as_sexpression([]). % Base case: empty list
print_list_as_sexpression([H]) :- 
    pp_sex(H). % Print the only element if it's a singleton list

% Print the elements of a list with spaces between them
% @param List - The list to print
print_list_as_sexpression([H|T]) :- 
    pp_sex(H), % Print the first element
    write(' '), % Print a space between elements
    print_list_as_sexpression(T). % Recursively print the rest

/*
   with_indents/2 modifies the src_indents option for the duration of a goal's execution.
   This allows temporarily changing the indentation settings during specific operations.
*/

% Modify src_indents option for the execution of a goal
% @param TF - The new value for the src_indents option
% @param Goal - The goal to execute with the new setting
with_indents(TF, Goal) :- 
    as_tf(TF, Value), % Convert TF to a true/false value
    with_option(src_indents, Value, Goal). % Temporarily set src_indents and execute Goal

% Check if source indentation is disabled
no_src_indents :- 
    option_else(src_indents, TF, true), % Get the src_indents option value
    !, 
    TF == 'False'. % Check if indentation is disabled

% Check if quoting symbols are disabled
no_quoting_symbols :- 
    option_else(no_quoting_symbols, TF, true), % Get the no_quoting_symbols option value
    !, 
    TF == 'True'. % Check if quoting symbols are disabled

/*
   with_no_quoting_symbols/2 modifies the no_quoting_symbols option for the duration of a goal's execution.
*/

% Modify no_quoting_symbols option for the execution of a goal
% @param TF - The new value for the no_quoting_symbols option
% @param Goal - The goal to execute with the new setting
with_no_quoting_symbols(TF, Goal) :- 
    with_option(no_quoting_symbols, TF, Goal). % Temporarily set no_quoting_symbols and execute Goal

/*
   allow_concepts/0 checks if the use of concepts is allowed by examining the concepts option.
   This predicate is currently disabled due to the 'fail' at the start of the clause.
*/

% Check if the use of concepts is allowed
allow_concepts :- 
    !, 
    fail, % This predicate is currently disabled
    option_else(concepts, TF, 'False'), % Get the value of the concepts option
    \+ TF == 'False'. % Ensure that concepts are not explicitly disabled

% The following commented-out code used to handle concept-based logic but was removed:
% previously: dash_functor(ASymbolProc,O):- fail, symbol_contains(ASymbolProc,'_').
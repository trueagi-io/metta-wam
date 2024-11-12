/* PLDoc header for translate_to_metta/2
   @param Module The module to which the translation is applied.
   @param Input The Prolog input to be translated into Metta.
   @example 
     translate_to_metta(my_module, Input).
*/
% The main predicate to translate Prolog clauses to Metta format.
% The clause is read using read_clause_with_info/1. If successful (!), 
% it recursively translates the next clause.
translate_to_metta(Module, Input):-
  % Reads a clause and captures additional information.
  read_clause_with_info(Input),!,
  % Recursively translate the next clause.
  translate_to_metta(Module, Input).


/* PLDoc header for is_reprint_char/1
   @param Char The character being checked.
   @desc This predicate checks if a character (space or period) should be reprinted as-is.
   @example 
     is_reprint_char('.').
*/
% Predicate to determine if the character is a space or a period, which should be retained in output.
is_reprint_char(Char):- char_type(Char, space).
is_reprint_char(Char):- Char == '.'.


/* PLDoc header for translate_comment/2
   @param Cmt The original Prolog comment.
   @param Str The translated Metta comment.
   @desc Translates comments from Prolog to Metta by replacing specific strings in the comment.
   @example 
     translate_comment("% This is a comment", "; This is a comment").
*/
% Translates Prolog comments into Metta comments by applying specific string replacements.
translate_comment(Cmt, Str):- replace_in_string(["%"=";",
                                                 "prolog"="MeTTa",
                                                 "PROLOG"="MeTTa",
                                                 "Prolog"="MeTTa"], Cmt, Str).


/* PLDoc header for read_clause_with_info/1
   @param Stream The input stream from which the clause is read.
   @desc This predicate reads a clause and captures relevant metadata like variable bindings and comments.
   @example 
     read_clause_with_info(Stream).
*/
% Reads a clause from the input stream and captures metadata. 
% Stops reading if end of stream is reached.
read_clause_with_info(Stream) :- at_end_of_stream(Stream),!.
read_clause_with_info(Stream):- 
  % Uses a catch block to handle exceptions when reading the clause.
  catch(read_clause_with_info_0(Stream), E,
  % Outputs the error using user_io if an exception occurs.
  ((user_io(write_src_cmt(E)), write_src_cmt(E)))).


/* PLDoc header for read_clause_with_info_0/1
   @param Stream The input stream from which the clause is read.
   @desc This predicate reads a clause and captures detailed information like term positions, comments, and bindings.
   @example 
     read_clause_with_info_0(Stream).
*/
% Reads a clause from the input stream with detailed options, capturing variable names, 
% term positions, syntax errors, and comments. 
read_clause_with_info_0(Stream) :-
    Options = [ variable_names(Bindings),            % Captures variable bindings.
                term_position(Pos),                 % Captures the term position.
                subterm_positions(RawLayout),       % Captures the layout of the term.
                syntax_errors(error),               % Handles syntax errors.
                comments(Comments),                 % Captures comments associated with the term.
                module(trans_mod)],                 % Sets the module for term processing.
    read_term(Stream, Term, Options),               % Reads the term with the given options.
    % If end of file is reached, terminate the process.
    (   (fail, Term == end_of_file)
    ->  true
    ;   % Sets global variables for term position and variable names.
        b_setval('$term_position', Pos),
        b_setval('$variable_names', Bindings),
        % Displays the term information and processes comments.
        display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments)).


/* PLDoc header for display_term_info/6
   @param Stream The input stream.
   @param Term The Prolog term being processed.
   @param Bindings The variable bindings for the term.
   @param Pos The term position.
   @param RawLayout The raw layout of the term.
   @param Comments The comments associated with the term.
   @desc This predicate processes the term and displays its metadata, including comments.
   @example 
     display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments).
*/
% Displays the term's information such as variable bindings, term positions, and comments.
display_term_info(Stream, Term, Bindings, Pos, RawLayout, Comments):-
   % Processes each variable binding into a human-readable format.
   maplist(into_namings, Bindings),
   % Processes the term while ignoring failure if term processing fails.
   ignore(process_term(Stream, Term)),
   % Prints comments in Metta format.
   print_metta_comments(Comments), !.


/* PLDoc header for print_metta_comments/1
   @param Comments The list of comments to be printed.
   @desc This predicate prints all the comments in Metta format.
   @example 
     print_metta_comments(["% Prolog comment"]).
*/
% Prints Metta comments by processing the list of comments.
print_metta_comments(Comments):- print_metta_comment(Comments).


/* PLDoc header for print_metta_comment/1
   @param Comments A comment or list of comments to be printed.
   @desc This predicate processes and prints individual comments recursively.
   @example 
     print_metta_comment("% This is a comment").
*/
% Base case for empty comment list.
print_metta_comment([]):-!.
% Recursively processes and prints a list of comments.
print_metta_comment(_TP-Cmt):- !, print_metta_comment(Cmt).
print_metta_comment([Cmt|Cs]):- !, print_metta_comment(Cmt),!, print_metta_comment(Cs).
% Processes a single comment by translating and printing it.
print_metta_comment(Cmt):- translate_comment(Cmt, String), print_cmt_lines(String).


/* PLDoc header for print_cmt_lines/1
   @param String The string representing the comment.
   @desc This predicate prints each line of a multi-line comment.
   @example 
     print_cmt_lines("This is a comment\nThis is another line").
*/
% Normalizes spaces and splits the comment into individual lines before printing.
print_cmt_lines(String):-
    normalize_space(string(TaxM), String),
    atomics_to_string(List, '\n', TaxM), !,
    % Prints each line of the comment.
    maplist(print_cmt_line, List).


/* PLDoc header for print_cmt_line/1
   @param Str The comment string to be printed.
   @desc This predicate prints a single comment line in the Metta format.
   @example 
     print_cmt_line("This is a comment").
*/
% Formats and prints a single comment line.
print_cmt_line(Str):- format('~N; ~w', [Str]).


/* PLDoc header for echo_as_commnents_until_eof/1
   @param Stream The input stream.
   @desc This predicate echoes comments from the input stream until end of file.
   @example 
     echo_as_commnents_until_eof(Stream).
*/
% Reads and echoes comments from the stream until the end of the file.
echo_as_commnents_until_eof(Stream):-
    repeat,
    % If end of the stream is reached, stop the process.
    (at_end_of_stream(Stream)-> !;
     % Reads the next line and echoes it as a comment.
     (read_line_to_string(Stream, Cmt),
       ignore((print_metta_comments(Cmt))),
        fail)).


/* PLDoc header for process_term/2
   @param Stream The input stream.
   @param Term The Prolog term being processed.
   @desc This predicate processes the term based on its type (directive or clause).
   @example 
     process_term(Stream, end_of_file).
*/
% Processes each term, checking if it is a directive or clause.
process_term(Stream, end_of_file):- !, echo_as_commnents_until_eof(Stream).
process_term(Stream, Term):-
    % If the term is a directive, call it and print it.
    is_directive(Term),
    ignore(maybe_call_directive(Stream, Term)),
    !, ignore(print_directive(Term)).
% Otherwise, expand the term and translate it to Metta.
process_term(_, Term):-
  expand_to_hb(Term, H, B),
  p2m((H:-B), STerm),
  push_term_ctx(Term),
  write_pl_metta(STerm).


/* PLDoc header for maybe_call_directive/2
   @param Stream The input stream.
   @param Directive The directive term.
   @desc This predicate calls Prolog directives when appropriate.
   @example 
     maybe_call_directive(Stream, op(500, xfx, '=>')).
*/
% Handles Prolog directives, potentially calling them if applicable.
maybe_call_directive(Stream, (:- X)):- !, maybe_call_directive(Stream, X).
maybe_call_directive(_Stream, op(X, F, Y)):- trans_mod:op(X, F, Y).
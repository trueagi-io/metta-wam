/* 
    @predicate file_name_extension/3
    @desc Generates a new filename by replacing the old extension with `.metta`.
    @param Base The base name of the file without extension.
    @param _OldExt Placeholder for the old file extension.
    @param Filename The original filename with the old extension.
    @example file_name_extension('myfile', txt, Filename). 
        % Generates Filename as 'myfile.txt'
*/
file_name_extension(Base, _OldExt, Filename),

/* 
    @predicate file_name_extension/3 
    @desc Generates a new filename with the extension 'metta'.
    @param Base The base name of the file.
    @param metta The new extension to be added.
    @param NewFilename The generated filename with the '.metta' extension.
    @example file_name_extension('myfile', _, NewFilename).
        % Generates NewFilename as 'myfile.metta'
*/
file_name_extension(Base, metta, NewFilename),

/* 
    @predicate file_base_name/2
    @desc Extracts the base name of the file to use as the module name.
    @param Base The base name of the file.
    @param Module The module name derived from the file base name.
    @example file_base_name('myfile.metta', Module).
        % Module is set to 'myfile'
*/
file_base_name(Base, Module),

/* 
    @desc Setup: Prepare to open both the input and output files for reading and writing.
    The format statement was previously printing the action being performed but is now commented out.
    It can be re-enabled for debugging or logging purposes.
    previously: format('~N~n~w~n', [convert_to_metta(Filename,NewFilename)])
*/
% format('~N~n~w~n', [convert_to_metta(Filename, NewFilename)]),

/* 
    @predicate convert_to_metta_file/4
    @desc Handles the process of converting the file into the Metta format by opening 
    the input and output files and calling the necessary translation steps.
    @param Module The module name derived from the file.
    @param OutputIn The initial output stream, which can be a variable.
    @param Filename The original file to be converted.
    @param NewFilename The newly generated file with the .metta extension.
    @example convert_to_metta_file(myfile, _, 'myfile.txt', 'myfile.metta').
        % Converts the content of 'myfile.txt' to 'myfile.metta'
*/
convert_to_metta_file(Module, OutputIn, Filename, NewFilename).

/* 
    @predicate write_src_cmt/1
    @desc Writes the source of the goal G as a string and wraps it in a comment.
    @param G The goal whose source is written.
    @example write_src_cmt(my_goal).
        % Outputs the Prolog source of my_goal within comments.
*/
write_src_cmt(G) :- ignore((with_output_to(string(S), write_src(G)), in_cmt(write(S)))).

/* 
    @predicate convert_to_metta_file/4 
    @desc Main logic for converting a file from one format to another, handling input/output operations 
    and cleanup.
    @param Module The module name extracted from the filename.
    @param OutputIn The output stream for the new file (may be variable).
    @param Filename The original filename.
    @param NewFilename The new filename with the .metta extension.
    @example convert_to_metta_file(my_module, _, 'input.txt', 'output.metta').
        % Converts input.txt into output.metta.
*/
convert_to_metta_file(Module, OutputIn, Filename, NewFilename):-

    /* Copy the term OutputIn to Output to handle the conversion operation */
    copy_term(OutputIn, Output),

    /* 
        If OutputIn is unbound (a variable), print the conversion action being performed.
        previously: This part was used for tracing/debugging purposes and is kept for potential future use.
    */
    if_t(var(OutputIn),
       user_io(write_src_cmt(convert_to_metta_file(Module, OutputIn, Filename, NewFilename)))),

    /* previously: Output = user_output */
    
    /* 
        Use setup_call_cleanup to ensure proper handling of file streams. 
        Open the input file in read mode and apply ISO Latin-1 encoding. 
    */
    setup_call_cleanup(
        open(Filename, read, Input, [encoding(iso_latin_1)]),

        /* 
            Call: Open the output file if Output is a variable, and set its encoding to UTF-8. 
            Then, translate the content from the input file to the output file.
        */
        setup_call_cleanup(
            (if_t(var(Output), open(NewFilename, write, Output, [encoding(utf8)]))),
            with_output_to(Output,
                (
                    write_src_cmt(convert_to_metta_file(Module, OutputIn, Filename, NewFilename)),
                    translate_to_metta(Module, Input)
                )),
            /* 
                Cleanup: Ensure the output stream is closed after writing. 
            */
            close(Output)
        ),

        /* 
            Cleanup: Ensure the input stream is closed after reading. 
        */
        close(Input)
    ).

/* 
    @predicate into_namings/1
    @desc Helper predicate to handle variable naming in Prolog terms, ignoring if the variable is an unbound variable.
    @param N=V The term in the format of a variable name and its value.
    @example into_namings(X = '$VAR'(name)).
        % Ensures the variable name is handled appropriately.
*/
into_namings(N=V) :- ignore(V='$VAR'(N)).

/* 
    @predicate translate_to_metta/2
    @desc Recursively translates content from the input file to the Metta format.
    Terminates when the end of the input stream is reached.
    @param Module The module name for context.
    @param Input The input stream from the file being read.
    @example translate_to_metta(my_module, Input).
        % Processes the content from the input stream until the end of the file.
*/
translate_to_metta(Module, Input) :-
    at_end_of_stream(Input),  % Check if we've reached the end of the input file.
    !, nl.  % Stop processing and insert a newline.

/* 
    @predicate translate_to_metta/2
    @desc Handles the translation of reprintable characters (e.g., whitespace), ensuring they are preserved.
    @param Module The module name for context.
    @param Input The input stream from the file being read.
    @example translate_to_metta(my_module, Input).
        % Processes and prints any reprintable characters such as whitespace.
*/
translate_to_metta(Module, Input) :-
    peek_char(Input, Char),  % Peek at the next character in the input without consuming it.
    is_reprint_char(Char), !,  % If it's a reprintable character, proceed.
    get_char(Input, _),  % Consume the character.
    put_char(Char),  % Output the character as-is.
    translate_to_metta(Module, Input).  % Continue translating.

/* 
    @predicate translate_to_metta/2
    @desc Translates Prolog comments (starting with %) to Metta-style comments (starting with ;).
    @param Module The module name for context.
    @param Input The input stream from the file being read.
    @example translate_to_metta(my_module, Input).
        % Converts Prolog comments to Metta comments and processes the rest of the file.
*/
translate_to_metta(Module, Input) :-
    peek_char(Input, Char),  % Peek at the next character.
    Char == '%',  % Check if the character is a Prolog comment (%).
    get_char(Input, _), put_char(';'),  % Replace % with ; for Metta comment style.
    read_line_to_string(Input, Cmt),  % Read the rest of the comment line.
    print_metta_comments(Cmt), nl,  % Output the Metta-style comment and insert a newline.
    translate_to_metta(Module, Input).  % Continue processing.

/* 
    @predicate translate_to_metta/2
    @desc Translates alternative comment syntax (starting with #) to Metta-style comments.
    @param Module The module name for context.
    @param Input The input stream from the file being read.
    @example translate_to_metta(my_module, Input).
        % Converts # comments into Metta-style comments.
*/
translate_to_metta(Module, Input) :-
    peek_char(Input, Char),  % Peek at the next character.
    Char == '#',  % Check if the character is a comment starting with #.
    get_char(Input, _), put_char(';'),  % Replace # with ; for Metta comment style.
    read_line_to_string(Input, Cmt),  % Read the rest of the comment line.
    print_metta_comments(Cmt), nl,  % Output the Metta-style comment and insert a newline.
    translate_to_metta(Module, Input).  % Continue processing.

/* previously: The following code block for reading clauses with metadata was removed 
due to redundancy in the newer logic handling comments and file translation directly. 
It can be restored if metadata extraction becomes necessary in the future. */
% Reads a clause along with its metadata, then continues translation.
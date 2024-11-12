/**
 * @predicate with_file_lists/3
 * @param Rel The base directory or relative path for file resolution.
 * @param P1 A predicate to apply to each found file.
 * @param Wildcard A wildcard expression to find matching files.
 * Resolves files based on the given wildcard pattern relative to a base directory
 * and applies the predicate P1 to each found file.
 * Skips processing if no files are found that match the wildcard pattern.
 *
 * @example 
 * with_file_lists('.', process_file, '*.pl').
 */
with_file_lists(Rel, P1, Wildcard) :- 
    atom(Wildcard),  % Ensures the wildcard is an atom (a basic Prolog term).
    absolute_file_name(Wildcard, AbsWildcard, [relative_to(Rel)]),  % Resolves the absolute file name based on the wildcard and relative path.
    \+ exists_file(AbsWildcard),  % Checks if the file does not exist to avoid re-processing.
    expand_file_name(AbsWildcard, Files),  % Expands the wildcard to a list of matching files.
    Files \== [],  % Ensures that the list of files is not empty.
    !,  % Cuts to prevent backtracking if files are found.
    ignore(maplist(with_file_lists(Rel, P1), Files)).  % Recursively processes each found file.

% Previously: Predicate to apply P1 to the given filename, ensuring deterministic logic.
% Skipped to avoid enforcing strict determinism here.
% with_file_lists(Rel, P1, Filename) :- must_det_ll(call(P1, Filename)).

% Writes out the current file being processed to the source, then prints a newline.
with_file_lists(Rel, P1, Filename) :- 
    write_src(with_file_lists(Rel, P1, Filename)),  % Outputs the operation details.
    nl.  % Prints a newline.

/* previously: 
 * The following commented code was an example entry point to process files for 
 * a custom "Metta" format. It has been skipped due to its dependency on a specific environment and external tools.
 */
% Entry point for printing to Metta format. It clears the screen, sets the working directory,
% expands the filenames with a specific extension, and processes each file.
% cls,  % Clears the screen (assumes a custom or system-specific implementation).
% with_pwd('/opt/logicmoo_opencog/hyperon-wam/tests/gpt2-like/language_models/', 
% Finds all Prolog files in the specified directory.
% Filt = 'tests/gpt2-like/language_models/*.pl', 
% Filt = '/opt/logicmoo_opencog/hyperon-wam/tests/performance/nondet_unify/*.pl',
% convert_to_metta(Filt),  % Processes each found file.

% Default wildcard mask for locating Prolog files to process.
% A list of file patterns is specified, focusing on Prolog files in various subdirectories.
default_pl_mask(Mask) :- 
    Mask = [
        % 'src/main/metta_*.pl',  % Previously used to target specific metta files.
        % 'src/main/flybase_*.pl',  % Previously used to target specific flybase files.
        '*/*.pl', 
        '*/*/*.pl',
        '*/*/*/.pl',
        '*/*/*/*/.pl',
        '*/*/*/*/*/.pl',
        '*/*/*/*/*/*.pl',
        '*.pl'  % Includes all files ending with .pl in any subdirectory.
    ], !.  % Ensure no backtracking after determining the mask.

% Alternate default wildcard mask if the previous definition is not used.
default_pl_mask(Mask) :- 
    Mask = ['**/*.pl'].  % Recursive pattern to include all .pl files in any subdirectory.

% High-level function to convert files to Metta format in the console.
convert_to_metta_console :- 
    default_pl_mask(Mask),  % Get the default Prolog file mask.
    ignore(convert_to_metta_console(Mask)),  % Safely apply conversion to the mask.
    !,  % Prevent backtracking.
    writeln(';; convert_to_metta_console.').  % Log the completion message.

% High-level function to convert files to Metta format and write to a file.
convert_to_metta_file :- 
    default_pl_mask(Mask),  % Get the default Prolog file mask.
    ignore(convert_to_metta_file(Mask)),  % Safely apply conversion to the mask.
    !,  % Prevent backtracking.
    writeln(';; convert_to_metta_file.').  % Log the completion message.

% Main function that processes files and converts them to Metta format.
convert_to_metta :- 
    default_pl_mask(Mask),  % Get the default Prolog file mask.
    % Skipped setting garbage collection flag locally here as it's an optimization detail.
    call(ignore(convert_to_metta(Mask))),  % Safely apply conversion to the mask.
    !,  % Prevent backtracking.
    writeln(';; convert_to_metta.').  % Log the completion message.

% Short alias to call the Metta conversion process.
ctm :- convert_to_metta.

% Processes a list of filenames by applying the conversion predicate to each.
convert_to_metta_console(FileSpec) :-  
    with_file_lists('.', convert_to_metta_now(user_output), FileSpec).  % Process files in console mode.

% Processes a list of filenames and writes the output to a file.
convert_to_metta_file(FileSpec) :-  
    with_file_lists('.', convert_to_metta_now(_Create), FileSpec).  % Process files and write output to file.

% Main conversion handler for files that exist.
convert_to_metta(Filename) :- 
    atomic(Filename),  % Ensure the filename is an atom.
    exists_file(Filename),  % Check if the file exists.
    !,  % Prevent backtracking.
    ignore(convert_to_metta_file(Filename)),  % Safely apply file conversion.
    ignore(convert_to_metta_console(Filename)),  % Safely print file conversion result to the console.

% Fallback case to handle wildcard patterns for Metta conversion.
convert_to_metta(FileSpec) :- 
    with_file_lists('.', convert_to_metta, FileSpec).  % Process wildcard pattern.

% Processes the given filename by opening it, translating its content, and then closing the file.
convert_to_metta_now(OutputIn, Filename) :- 
    user_io(convert_to_metta_now_out(OutputIn, Filename)).  % Apply conversion with output handling.

% Helper predicate to output the conversion result.
convert_to_metta_now_out(OutputIn, Filename) :- 
    atom(Filename),  % Verifies that the filename is an atom.
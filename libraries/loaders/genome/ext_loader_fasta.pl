/*
 * Project: MeTTaLog - A MeTTa to Prolog Transpiler/Interpreter
 * Description: This file is part of the source code for a transpiler designed to convert
 *              MeTTa language programs into Prolog, utilizing the SWI-Prolog compiler for
 *              optimizing and transforming function/logic programs. It handles different
 *              logical constructs and performs conversions between functions and predicates.
 *
 * Author: Douglas R. Miles
 * Contact: logicmoo@gmail.com / dmiles@logicmoo.org
 * License: LGPL
 * Repository: https://github.com/trueagi-io/metta-wam
 *             https://github.com/logicmoo/hyperon-wam
 * Created Date: 8/23/2023
 * Last Modified: $LastChangedDate$  # You will replace this with Git automation
 *
 * Usage: This file is a part of the transpiler that transforms MeTTa programs into Prolog. For details
 *        on how to contribute or use this project, please refer to the repository README or the project documentation.
 *
 * Contribution: Contributions are welcome! For contributing guidelines, please check the CONTRIBUTING.md
 *               file in the repository.
 *
 * Notes:
 * - Ensure you have SWI-Prolog installed and properly configured to use this transpiler.
 * - This project is under active development, and we welcome feedback and contributions.
 *
 * Acknowledgments: Special thanks to all contributors and the open source community for their support and contributions.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

%********************************************************************************************* 
% PROGRAM FUNCTION: to parse and load biological sequence data from a FASTA-formatted file,
% which is is a text-based format for representing either nucleotide sequences or amino acid (protein) 
% sequences, in which nucleotides or amino acids are represented using single-letter codes.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ==============================
% FA/FASTA Reader
% ==============================

% Provides parse_and_store_attributes/2 for parsing and storing attributes.
:- ensure_loaded(ext_loader_gff).  

%!  load_fb_fa(+Fn, +Filename) is det.
%
%   Loads a FASTA file and processes its contents into an internal representation.
%   This predicate handles the file loading, metadata extraction, and parsing of sequences.
%
%   @arg Fn       The function or identifier associated with the file being processed.
%   @arg Filename The path to the FASTA file to be loaded.
%
%   @example
%     % Load a FASTA file and process its contents.
%     ?- load_fb_fa(my_function, 'example.fasta').
%
load_fb_fa(Fn, Filename):-
    track_load_into_file(Filename,
        must_det_ll((
            % Log the loading operation.
            fbug(load_fb_fa(Fn, Filename)),
            % Extract directory and basename information from the file path.
            directory_file_path(Directory, BaseName, Filename),
            file_name_extension(Id, _, BaseName),
            % Set the type of file being loaded.
            Type = 'SequenceFile',
            % Assert metadata about the file into the OBO knowledge base.
            assert_OBO(id_type(Id, Type)),
            assert_OBO(pathname(Id, Filename)),!,
            assert_OBO(basename(Id, BaseName)),!,
            assert_OBO(directory(Id, Directory)),!,
            % Open the file and process its contents.
            setup_call_cleanup(
                open(Filename, read, In, [encoding(utf8)]),
                load_fb_fa_read(Id, In, _, 0),
                close(In))))).

%!  load_fb_fa_read(+Fn, +In, -FBTe, +At) is det.
%
%   Reads the contents of a FASTA file stream and processes it line by line.
%   Stops processing when the end of the stream is reached or a file size limit is hit.
%
%   @arg Fn   The function or identifier associated with the file being processed.
%   @arg In   The input stream for reading the file.
%   @arg FBTe The current sequence identifier being processed (if applicable).
%   @arg At   The current position within the sequence.
%
load_fb_fa_read(_Fn, In, _, _) :-
    (at_end_of_stream(In); reached_file_max), !.
load_fb_fa_read(Fn, In, FBTe, At) :-
    % Read a line from the stream and process it.
    read_line_to_string(In, Str),
    load_fb_fa_read_str(Fn, In, FBTe, Str, At).

%!  load_fb_fa_read_str(+Fn, +In, -FBTe, +Str, +From) is det.
%
%   Processes a single line from the FASTA file.
%   If the line contains a header (starts with '>'), it extracts the sequence identifier
%   and properties, storing them using parse_and_store_attributes/2.
%   Otherwise, it treats the line as part of the sequence data.
%
%   @arg Fn   The function or identifier associated with the file being processed.
%   @arg In   The input stream for reading the file.
%   @arg FBTe The current sequence identifier being processed (if applicable).
%   @arg Str  The line from the FASTA file being processed.
%   @arg From The current position within the sequence.
%
load_fb_fa_read_str(Fn, In, _, Str, _) :-
    % If the line is a header, process the identifier and attributes.
    string_concat('>', Line, Str), !,
    split_string(Line, " \t", " \t", [FBTe|Props]), !,
    parse_and_store_attributes(FBTe, Props),
    load_fb_fa_read(Fn, In, FBTe, 0).
load_fb_fa_read_str(Fn, In, FBTe, Str, From) :-
    % If the line is sequence data, assert it as a fact.
    atom_chars(Str, Chars),
    Data =.. [fasta_sequence, Fn, FBTe, From, Chars],
    assert_MeTTa(Data), !,
    % Update the position based on the length of the current line.
    length(Chars, Plus),
    At is From + Plus,
    load_fb_fa_read(Fn, In, FBTe, At).
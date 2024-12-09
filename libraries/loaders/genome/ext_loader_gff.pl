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

% ==============================
% GFF/GTF/GFF3 Reader
% ==============================

%********************************************************************************************* 
% PROGRAM FUNCTION: GFF parser and database loader, transforming lines of a GFF file into structured 
% Prolog facts. It supports extracting both metadata (file-specific details) and genomic feature 
% information (e.g., sequences, features, and attributes). The parsed data can then be queried 
% in Prolog for downstream bioinformatics applications.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%!  load_fb_gff(+Fn, +Filename) is det.
%
%   Loads a GFF file and processes its contents to extract genomic feature information.
%   The predicate asserts metadata about the file (e.g., directory, basename) and parses
%   its lines into structured Prolog facts using `load_fb_gff_read/2`.
%
%   @arg Fn       A unique identifier for the file or dataset being loaded.
%   @arg Filename The path to the GFF file to be read and processed.
%
%   The process includes:
%   - Asserting metadata about the file (e.g., its ID, type, pathname, basename, and directory).
%   - Iterating through each line in the file and delegating line processing to `load_fb_gff_read/2`.
%   - Ensuring proper resource cleanup using `setup_call_cleanup/3`.
%
%   Example:
%       ?- load_fb_gff(my_dataset, '/path/to/file.gff').
%       % Parses and stores genomic feature facts from the specified GFF file.
%
load_fb_gff(Fn, Filename) :-
    % Track the file being loaded for logging and debugging.
    track_load_into_file(Filename,
        % Perform the file processing within a "must-det" logical block (ensures no silent failures).
        must_det_ll((
            % Debugging step: Log the start of the file load process.
            fbug(load_fb_gff(Fn, Filename)),

            % Extract file metadata: directory, base name, and file ID.
            directory_file_path(Directory, BaseName, Filename),
            file_name_extension(Id, _, BaseName),

            % Set the file type as 'SequenceFile' and assert metadata facts.
            Type = 'SequenceFile',
            assert_OBO(id_type(Id, Type)),
            assert_OBO(pathname(Id, Filename)), !,
            assert_OBO(basename(Id, BaseName)), !,
            assert_OBO(directory(Id, Directory)), !,

            % Open the file for reading, process its lines, and ensure proper cleanup.
            setup_call_cleanup(
                open(Filename, read, In), 
                (repeat, load_fb_gff_read(Id, In)), 
                close(In))))).

%!  load_fb_gff_read(+Fn, +In) is det.
%
%   Reads lines from the input stream `In` of a GFF file and processes them.
%   The predicate stops reading when the end of the stream is reached or a file size limit is encountered.
%
%   @arg Fn A unique identifier for the file being read.
%   @arg In Input stream of the GFF file.
%
%   Example:
%       ?- load_fb_gff_read(my_file_id, In).
%       % Processes all lines in the GFF file, storing relevant facts.
%
load_fb_gff_read(_Fn, In) :-
    % Stop reading if at the end of the file or a file size limit is reached.
    (at_end_of_stream(In); reached_file_max), !.
load_fb_gff_read(Fn, In) :-
    % Read the next line from the file.
    read_line_to_string(In, Line),
    % Process the line using `load_fb_gff_line/2`.
    load_fb_gff_line(Fn, Line),
    % Fail to force backtracking and process the next line.
    !, fail.

%!  load_fb_gff_line(+Fn, +Line) is det.
%
%   Processes a single line from the GFF file and extracts relevant information
%   into structured Prolog facts.
%
%   The predicate handles different types of lines:
%   - Sequence region definitions (`##sequence-region`).
%   - GFF version headers (`##gff-version`).
%   - Comments (`#`).
%   - Feature lines (tab-separated fields with attributes).
%   - Logs unexpected line formats for debugging.
%
%   @arg Fn   A unique identifier for the file being processed.
%   @arg Line A string representing the current line from the GFF file.
%
%   Example:
%       ?- load_fb_gff_line(my_file_id, "##sequence-region chr1 1 248956422").
%       % Asserts a genomic sequence region fact.
%
load_fb_gff_line(Fn, Line) :-
    % Handle lines defining sequence regions (e.g., "##sequence-region chr1 1 248956422").
    split_string(Line, " \t", " \t", ['##sequence-region', SeqID, StartStr, EndStr]),
    % Convert start and end positions from strings to numbers.
    atom_number(StartStr, Start),
    atom_number(EndStr, End),!,
    % Assert a fact for the sequence region.
    assert_MeTTa(genomic_sequence_region(Fn, SeqID, Start, End)).
load_fb_gff_line(_Fn, Line) :-
    % Ignore lines starting with "##gff-version" (e.g., "##gff-version 3").
    split_string(Line, " \t", " \t", ['##gff-version'|_]), !.
load_fb_gff_line(_Fn, Line) :-
    % Ignore comment lines (e.g., "# This is a comment").
    string_concat('#', _, Line), !.
load_fb_gff_line(Fn, Line) :-
    % Process feature lines with tab-separated fields.
    % Example: "chr1\tsource\tgene\t11869\t14409\t.\t+\t.\tID=gene0;Name=OR4F5".
    split_string(Line, "\t", "", [SeqID, Source, Type, StartStr, EndStr, ScoreStr, Strand, Phase | MoreProps]),
    % Convert start and end positions from strings to numbers.
    atom_number(StartStr, Start),
    atom_number(EndStr, End),
    % Store individual feature attributes as facts.
    store_gff_fact(Fn, SeqID, "source", Source),
    store_gff_fact(Fn, SeqID, "type", Type),
    store_gff_fact(Fn, SeqID, "start", Start),
    store_gff_fact(Fn, SeqID, "end", End),
    store_gff_fact(Fn, SeqID, "score", ScoreStr),
    store_gff_fact(Fn, SeqID, "strand", Strand),
    store_gff_fact(Fn, SeqID, "phase", Phase),
    % Parse and store any additional attributes.
    parse_and_store_attributes(SeqID, MoreProps).
load_fb_gff_line(Fn, Line) :-
    % Log unexpected lines for debugging purposes.
    fbug(load_fb_gff_line(Fn, Line)).

%!  store_gff_fact(+Fn, +SeqID, +Key, +Value) is det.
%
%   Stores a key-value pair from a GFF feature as a Prolog fact in the database.
%   The predicate ensures that only meaningful values (not `"."`) are stored.
%   Each fact represents a genomic sequence feature with its associated attributes.
%
%   @arg Fn     A unique identifier for the file being processed.
%   @arg SeqID  The sequence ID to which this feature belongs.
%   @arg Key    The attribute key (e.g., "source", "type", "start", "end").
%   @arg Value  The attribute value corresponding to the key.
%
%   Example:
%       ?- store_gff_fact(my_file_id, "chr1", "type", "gene").
%       % Asserts the fact: genomic_sequence_feature(my_file_id, "chr1", "type", "gene").
%
store_gff_fact(Fn, SeqID, Key, Value) :-
    % Skip storing the attribute if the value is "." (indicating missing or irrelevant data).
    Value \= ".",
    % Assert the key-value pair as a fact in the database, linking it to the sequence ID and file ID.
    assert_MeTTa(genomic_sequence_feature(Fn, SeqID, Key, Value)).

%!  parse_and_store_attributes(+Fn, +SeqID, +Attributes) is det.
%
%   Processes the "attributes" field of a GFF feature line.
%   The attributes field contains a semicolon-separated list of key-value pairs
%   (e.g., `ID=gene123;Name=ExampleGene`). This predicate splits the attributes
%   into individual pairs and delegates each pair to `parse_and_store_attribute/3`.
%
%   @arg Fn        A unique identifier for the file being processed.
%   @arg SeqID     The sequence ID to which these attributes belong.
%   @arg Attributes A list where the first element contains the attributes string to be parsed.
%
%   Example:
%       ?- parse_and_store_attributes(my_file_id, chr1, ["ID=gene123;Name=ExampleGene"]).
%       % Asserts the attributes as facts: ID=gene123, Name=ExampleGene.
%
parse_and_store_attributes(Fn, SeqID, [AttributesStr | _]) :-
    % Split the attributes string into individual key-value pairs.
    % Example: "ID=gene123;Name=ExampleGene" -> ["ID=gene123", "Name=ExampleGene"]
    split_string(AttributesStr, ";", "", AttrList),
    % Process each key-value pair using `parse_and_store_attribute/3`.
    maplist(parse_and_store_attribute(Fn, SeqID), AttrList).

%!  parse_and_store_attribute(+Fn, +SeqID, +AttrStr) is det.
%
%   Parses a single key-value pair from the attributes field and stores it as a fact.
%   Key-value pairs are typically separated by "=" (e.g., `ID=gene123`). If "=" is not
%   present, space-separated values are processed instead.
%
%   @arg Fn      A unique identifier for the file being processed.
%   @arg SeqID   The sequence ID to which this attribute belongs.
%   @arg AttrStr The string representing a single key-value pair (e.g., `ID=gene123`).
%
%   Example:
%       ?- parse_and_store_attribute(my_file_id, chr1, "ID=gene123").
%       % Asserts the fact: genomic_sequence_feature(my_file_id, chr1, "ID", "gene123").
%
parse_and_store_attribute(Fn, SeqID, AttrStr) :-
    % Split the attribute into Key and Value.
    % Primary format: "Key=Value" (e.g., "ID=gene123").
    % Alternate format (fallback): Space-separated values (e.g., "Key Value").
    (split_string(AttrStr, "=", "\"", [Key, Value])
        -> true
        ; split_string(AttrStr, " ", "\"", [Key | Value])),
    % Store the parsed Key-Value pair as a fact.
    store_gff_fact(Fn, SeqID, Key, Value).

/*

find . \( -name "*.fa" -o -name "*.gff" -o -name "*.json" \) -execdir bash -c 'for file; do metta_pattern="${file%.*}"*metta*; full_path="$(pwd)/$file"; if compgen -G "$metta_pattern" > /dev/null; then true; else echo "Metta file does not exist for $full_path"; fi; done' bash {} \; | sort -r

find .  ! -name "*.metta" - -execdir bash -c 'for file; do metta_pattern="${file%.*}"*metta*; full_path="$(pwd)/$file"; if compgen -G "$metta_pattern" > /dev/null; then true; else echo "Metta file does not exist for $full_path"; fi; done' bash {} \; | sort -r

find . \( -name "*.fa" -o -name "*.gff" -o -name "*.json" \) -execdir bash -c 'for file; do metta_pattern="${file%.*}"*datalog*; full_path="$(pwd)/$file"; if compgen -G "$metta_pattern" > /dev/null; then true; else echo "Datalog file does not exist for $full_path"; fi; done' bash {} \; | sort -r

*/



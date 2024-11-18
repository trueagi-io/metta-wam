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
% PROGRAM FUNCTION: processes JSON data from FlyBase, particularly focusing on genetic information 
% like transposons and genes, by extracting and transforming it into a structured format using 
% Prolog predicates that handle nested JSON objects and arrays while maintaining relationships 
% between different genetic elements.
%*********************************************************************************************

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IMPORTANT:  DO NOT DELETE COMMENTED-OUT CODE AS IT MAY BE UN-COMMENTED AND USED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ```prolog

% use_module() is equivalent to ensure_loaded/1, except that it raises an error if Files are not module files.
:- use_module(library(logicmoo_utils)).

%!  extracted_predicate(+Entity, +Attributes) is det.
%
%   Defines both facts and rules used to specify the structure of data for different entities
%   such as transposons, genes, exons, sequences, alleles, and metadata.
%   The second argument, `Attributes`, lists the relevant attributes or
%   relationships associated with the given `Entity`. In some cases, a rule 
%   dynamically generates attributes based on the context.
%
%   @arg Entity The name of the entity being defined. This represents a specific
%        type of biological or informational object (e.g., `transposon`, `gene`).
%   @arg Attributes A list of attributes or relationships associated with the
%        entity. This may include IDs, symbols, sequences, metadata, or
%        related entities.
%
%   Defined Entities (facts):
%   - `transposon`: Attributes include primary ID, symbol, sequence, URL, taxon ID, and SO term ID.
%   - `transposon_publications`: Links transposons with publication IDs.
%   - `transposon_synonyms`: Lists synonyms for transposons.
%   - `transposon_cross_references`: References external data sources for transposons.
%   - `gene`: Attributes include primary ID, gene ID, symbol, URL, locus tag, and name.
%   - `exon_locations`: Genome assembly and exon location details such as chromosome, strand, start/end positions.
%   - `relatedSequences`: Sequence relationships for a primary ID.
%   - `gene_synonyms`: Synonyms for gene IDs.
%   - `metadata`: General information about the dataset, including provider, schema version, and production date.
%   - `allele_image`: Detailed information about allele-associated images.
%   - `allele_image2` and `allele_image3`: Variations on allele image details.
%
%   Dynamic attribute generation for `gene` and `allele`: Based on specific attribute types (e.g., name, synonyms).
%
%   Note: The dynamic predicates generate specific attributes for entities
%   using constructs like `atom_concat` and `member` to define relationships.
%
%   @example
%   % Query dynamically generated attributes:
%   ?- extracted_predicate(N, [gene_geneId, N]).
%   
extracted_predicate(transposon, [primaryId, symbol, sequence, url, taxonId, soTermId]).
extracted_predicate(transposon_publications, [primaryId, publications]).
extracted_predicate(transposon_synonyms, [primaryId, symbolSynonyms]).
extracted_predicate(transposon_cross_references, [primaryId, crossReferenceIds]).
extracted_predicate(gene, [primaryId,gene_geneId, gene_symbol, gene_url, gene_locusTag, gene_name]).
extracted_predicate(N,[gene_geneId,N]):- member(M,[symbol, url, locusTag, name, synonyms]), atom_concat('gene_',M,N).
extracted_predicate(exon_locations,
 [primaryId,
   genomeLocations_assembly,
   genomeLocations_gca_accession,
   exons_INSDC_accession, exons_chromosome, exons_strand,
   exons_startPosition,
   exons_endPosition]).
extracted_predicate(relatedSequences,[primaryId,relatedSequences_sequenceId,
   relatedSequences_relationship]).
extracted_predicate(gene_synonyms, [gene_geneId, gene_synonyms]).
%extracted_predicate(exon, [primaryId, geneId, 'INSDC_accession', chromosome, strand, startPosition, endPosition]).
extracted_predicate(metadata, [dataProvider, schemaVersion, release, genomicCoordinateSystem, dateProduced]).
%extracted_predicate(exon_locations, [ assembly, chromosome, strand, startPosition, endPosition]).
extracted_predicate(N,[fbid,M]):-
  member(M,[transposons, common_terms, major_stages, major_tissues, name,
           pubs, rex_gene, stocks, expression_desc_text, images]), atom_concat('allele_',M,N).
extracted_predicate(allele_image,[fbid,images,images_imageDescription,
            images_publicationId, images_pubFigure, images_permission]).
extracted_predicate(allele_image2,
  [fbid,images,
            imageDescription, publicationId,
            pubFigure, permission]).
extracted_predicate(allele_image3,
  [fbid,    images_imageDescription,
            images_publicationId,
            images_pubFigure, images_permission]).

%!  wdmsg_json(+Message) is det.
%
%   Logs a debug message in JSON format.
%
%   This predicate is used to display a debug message encapsulated in a JSON 
%   structure. It calls `nop/1` with `fbug/1` to handle the debug information.
%
%   @arg Message The message or data to be logged in JSON format.
%
%   @example:
%   % Log a message for debugging:
%   ?- wdmsg_json("Processing data").
%   % Output will depend on the implementation of `nop/1` and `fbug/1`.
wdmsg_json(G) :- nop(fbug(G)).

%!  note_doing(+Action) is det.
%
%   Logs and executes a user-defined action.
%
%   This predicate logs the action using `wdmsg_json/1` and then attempts to
%   execute it by calling `user:Action`. It ensures that the action is noted 
%   before execution.
%
%   @arg Action The action to be logged and executed. It should be a callable
%        term that represents a user-defined goal.
%
%   @example:
%   % Log and execute an action:
%   ?- note_doing(my_custom_action).
%   % Output depends on the implementation of `wdmsg_json/1` and the execution
%   % result of `my_custom_action/0`.
note_doing(P) :- wdmsg_json(P), !, call(user:P).

%!  assert_JSON(+Fact) is det.
%
%   Asserts a fact after logging its creation.
%
%   This predicate logs the action of asserting the fact using `note_doing/1`
%   and then asserts it by calling `assert_OBO/1`. It ensures the operation is
%   traceable in debug logs.
%
%   @arg Fact The fact to be asserted. It should be a Prolog term that represents
%        the fact to be added to the database.
%
%   @example:
%   % Log and assert a fact:
%   ?- assert_JSON(my_fact(attribute, value)).
%   % Output depends on the implementation of `note_doing/1` and `assert_OBO/1`.
assert_JSON(P) :- note_doing(assert_OBO(P)).

:- ensure_loaded(flybase_main).
:- ensure_loaded(ext_loader_obo).

%:- listing(assert_OBO/1).

% dynamic enables adding, removing, or modifying clauses for a predicate while the program is running
:- dynamic json_kv/2.

%!  mapcurly_or_list(+Pred, +Structure) is det.
%
%   Recursively applies a predicate to elements of a list or curly-braced structure.
%
%   This predicate handles lists (`[H|T]`) and Prolog curly-braced structures 
%   (`{H}`, `{H, T}`). It applies the given predicate `Pred` to each element
%   within the structure. The base cases handle empty structures (`{}` and `[]`).
%
%   @arg Pred The predicate to apply to each element of the structure. 
%        `Pred` should be callable and accept a single argument.
%   @arg Structure A list, a curly-braced structure, or an empty structure.
%
%   @example
%     % Apply a predicate to a list:
%     ?- mapcurly_or_list(writeln, [1, 2, 3]).
%     1
%     2
%     3
mapcurly_or_list(_, {}) :- 
    % Base case: empty curly structure, succeed without action.
    !.
mapcurly_or_list(_, []) :- 
    % Base case: empty list, succeed without action.
    !.
mapcurly_or_list(Pred, [H|T]) :- 
    % Apply the predicate to the head of the list, then recurse on the tail.
    !,call(Pred, H), 
    mapcurly_or_list(Pred, T).
mapcurly_or_list(Pred, {H, T}) :- 
    % Apply the predicate to the first element of the curly structure, 
    % then recurse on the rest of the structure.
    !,call(Pred, H), 
    mapcurly_or_list(Pred, T).
mapcurly_or_list(Pred, {H}) :- 
    % Apply the predicate to the single element in the curly structure.
    !,call(Pred, H).

%!  simple_first(-Result, +Term1, +Term2) is det.
%
%   Compares two terms with special handling for `json(_)` terms.
%
%   This predicate determines the order of two terms. If one term is `json(_)`
%   and the other is not, it prioritizes the non-`json(_)` term. Otherwise,
%   it falls back to Prolog's built-in `compare/3` for standard term comparison.
%
%   @arg Result The comparison result: '<', '>', or '='.
%   @arg Term1 The first term to compare.
%   @arg Term2 The second term to compare.
%
%   @example
%     % Compare terms with one being a `json(_)`:
%     ?- simple_first('<', A, json(B)).
simple_first(R, _=AA, _=BB) :- 
    % Decompose key-value pairs and compare their values.
    !, simple_first(R, AA, BB).
simple_first('<', AA, BB) :- 
    % Prioritize non-json terms over json terms.
    BB = json(_), AA \= json(_), !.
simple_first('>', AA, BB) :- 
    % Prioritize json terms over non-json terms.
    AA = json(_), BB \= json(_), !.
simple_first(R, AA, BB) :- 
    % Use standard comparison for terms without special rules.
    !, compare(R, AA, BB).

%!  prefix_key(+Prefix, +KeyIn, -KeyOut) is det.
%
%   Generates a prefixed key if a prefix is valid and not equal to certain reserved terms.
%
%   This predicate checks if the prefix is an atom and not equal to reserved
%   terms like `Kee` or `data`. If valid, it concatenates the prefix and the
%   key using an underscore. Otherwise, it returns the original key.
%
%   @arg Prefix The prefix, which can be a list or atom.
%   @arg KeyIn The original key.
%   @arg KeyOut The resulting prefixed key, or the original key if no prefix is used.
%
%   @example
%     % Add a prefix to a key:
%     ?- prefix_key('my_prefix', 'key', Result).
%     Result = 'my_prefix_key'.
prefix_key([O|_], Kee, Key) :- 
    % Handle a list prefix by using its first element.
    atom(O), 
    !, prefix_key(O, Kee, Key).
prefix_key(O, Kee, Key) :-
    % Concatenate prefix and key if the prefix is valid and not reserved.
    atom(O),
    O \== Kee,
    O \== data, 
    symbolic_list_concat([O, '_', Kee], Key), 
    !.
prefix_key(_, Key, Key). % If no valid prefix is found, return the original key.
    
:- use_module(library(http/json)).

%!  load_flybase_json(+Function, +File) is det.
%
%   Loads and processes a FlyBase JSON file.
%
%   This predicate processes a FlyBase JSON file by invoking the appropriate 
%   procedures to handle the file, potentially converting it and managing the 
%   resulting data.
%
%   @arg Function The function to associate with the file processing (currently unused).
%   @arg File The path to the JSON file to be processed.
%
load_flybase_json(_Fn, File) :-
    process_json_file(File).

%!  process_json_file(+File) is det.
%
%   Processes a JSON file by creating a corresponding `.metta_x` file or
%   handling the JSON directly if conversion fails.
%
%   @arg File The path to the JSON file to be processed.
%
process_json_file(File) :-
    % Generate a .metta_x filename based on the input file and process it.
    atom_concat(File, '.metta_x', MXFile),
    process_json_file(File, MXFile).
process_json_file(_File, MXFile) :-
    % Attempt to process the .metta_x file if it exists.
    fail, exists_file(MXFile), !,
    process_metta_x_file(MXFile).
process_json_file(File, MXFile) :-
    % Convert the file to .metta_x format and process it.
    fail, exists_file(File), !,
    setup_call_cleanup(
        open(MXFile, write, Strm, []),
        setup_call_cleanup(
            set_stream(Strm, alias(metta_x_output)),
            with_option(make_metta_x, 'True', process_json_file_direct(File)),
            set_stream(current_output, alias(metta_x_output))),
        close(Strm)),
    remove_duplicates(MXFile),
    process_metta_x_file(MXFile).
process_json_file(File, _) :-
    % Process the file directly if other methods fail.
    process_json_file_direct(File), !.
process_json_file(File, MXFile) :-
    % Throw an error if all attempts fail.
    throw(process_json_file(File, MXFile)).

%!  process_json_file_direct(+File) is det.
%
%   Directly processes a JSON file by reading it and passing the data to `process_json/2`.
%
%   @arg File The path to the JSON file to be processed.
%
process_json_file_direct(File) :-
    setup_call_cleanup(
        open(File, read, Stream, [encoding(utf8)]),
        json_read(Stream, JSONDict),
        close(Stream)),
    process_json([], JSONDict).

%!  process_json(+Parent, +JSONData) is det.
%
%   Processes JSON data recursively, handling structures like objects, lists,
%   and key-value pairs.
%
%   @arg Parent A list of parent keys leading to the current JSON data.
%   @arg JSONData The JSON data to process (can be an atomic value, a list, or an object).
%
process_json(JsonString) :-
    % Entry point for JSON data processing.
    process_json([], JsonString), !.
process_json(O, JsonString) :-
    % Parse a JSON string and process its contents.
    atomic(JsonString), !,
    atom_json_term(JsonString, Json, []),
    process_json(O, Json).
process_json(O, json(Values)) :-
    % Process a JSON object represented as `json/1`.
    !, process_json(O, Values).
process_json(O, K=json(Values)) :-
    % Handle key-value pairs where the value is a JSON object.
    !, process_json([K|O], Values).
process_json(O, Values) :-
    % Handle lists by processing each element.
    is_list(Values), !,
    maplist(with_json1(O), Values).
process_json(O, Values) :-
    % Handle other values.
    with_json1(O, Values), !.

%!  with_json1(+Parent, +Data) is det.
%
%   Processes a single JSON entry based on its structure or content.
%
%   @arg Parent A list of parent keys leading to the current JSON entry.
%   @arg Data The JSON entry to process.
%
with_json1(O, K=Values) :-
    % Special case for driver entries.
    K == driver, !,
    with_json1(O, Values).
with_json1(O, K=Values) :-
    % Handle key-value pairs recursively.
    !, with_json1([K|O], Values).
with_json1(O, Values) :-
    % Handle lists of JSON data.
    is_list(Values), !,
    maplist(with_json1(O), Values).
with_json1(O, json([driver=json(Values0)])) :-
    % Special case for nested driver entries.
    !, with_json2(O, Values0), !.
with_json1(O, json(Values0)) :-
    % Handle JSON objects stored as lists.
    is_list(Values0), !,
    with_json2(O, Values0), !.
with_json1(O, Val) :-
    % Log errors for unhandled cases.
    fbug(error(O=Val)), !.

%!  with_json2(+Parent, +Values) is det.
%
%   Processes a list of JSON key-value pairs while tracking seen arguments.
%
%   @arg Parent A list of parent keys leading to the current JSON entry.
%   @arg Values The JSON key-value pairs to process.
%
with_json2([metaData], _Values0) :- 
    % Skip processing for metaData entries.
    !.
with_json2(O, Values) :-
    % Process values and log arguments that have been seen.
    retractall(seen_arg(_, _)),
    with_json3(O, Values),
    ignore((
        seen_arg(_, _),
        fbug(Values),
        listing(seen_arg/2))).

%!  with_json3(+Parent, +Values) is det.
%
%   Prepares the environment and processes JSON values recursively.
%
%   @arg Parent A list of parent keys leading to the current JSON entry.
%   @arg Values The JSON values to process.
%
with_json3(O, Values0) :-
    retractall(json_kv(_, _)),
    with_json4(O, Values0),
    retractall(json_kv(_, _)), !.

%!  with_json4(+Parent, +Values) is det.
%
%   Processes sorted JSON values, ensuring consistent order and processing.
%
%   @arg Parent A list of parent keys leading to the current JSON entry.
%   @arg Values The JSON values to process.
%
with_json4(O, [json(Values)]) :-
    % Handle single JSON objects wrapped in a list.
    !, with_json4(O, Values).
with_json4(O, json(Values)) :-
    % Handle a JSON object.
    !, with_json4(O, Values).
with_json4(O, Values0) :-
    % Sort values and process each entry.
    predsort(simple_first, Values0, Values),
    wdmsg_json(O==Values),
    ignore(maplist(with_entry(O, assert), Values)).


%!  with_entry(+Parent, +Action, +Data) is det.
%
%   Processes a JSON entry by invoking the appropriate handling based on its structure.
%
%   If the entry is a key-value pair, it delegates to `with_kv/4` for further processing.
%   If the entry does not match a known pattern, an error is logged.
%
%   @arg Parent A list of parent keys leading to the current JSON entry.
%   @arg Action The action to be performed, typically `assert`.
%   @arg Data The JSON entry to process, which can be a key-value pair or another structure.
%
%   @example
%     % Process a key-value entry:
%     ?- with_entry([], assert, key=value).
with_entry(O, AR, Key=Value) :- 
    % Handle key-value pairs by delegating to `with_kv/4`.
    !, with_kv([Key|O], AR, Key, Value).
%with_entry(O, assert, JSON) :- 
%    % Uncomment this line to enable processing JSON objects directly.
%    !, process_json(O, JSON).
with_entry(O, AR, JSON) :- 
    % Log an error for unhandled entries.
    fbug(error_with_entry(O, AR, JSON)).

%!  uses_id_subprops(+Key) is nondet.
%
%   Determines if the given key is a subproperty related to identifiers.
%
%   @arg Key The key to check.
%
%   @example:
%     % Check if `images` is a subproperty:
%     ?- uses_id_subprops(images).
%     true.
uses_id_subprops(images).

%!  key_can_nv(+Key) is nondet.
%
%   Checks if a key is allowed for key-value processing based on a predefined list.
%
%   @arg Key The key to check.
%
%   @example:
%     % Verify if a key is allowed:
%     ?- key_can_nv(name).
%     true.
key_can_nv(M) :-
    member(M, [major_stages, major_tissues, name, rex_gene, insertions, transposons,
               % Uncomment the line below if expression descriptions and images are valid keys.
               % expression_desc_text, images,
               pubs, stocks]).

%!  is_field(+Field) is nondet.
%
%   Determines if a given field is part of an extracted predicate's attributes.
%
%   @arg Field The field to check.
%
%   @example:
%     % Check if `symbol` is a valid field:
%     ?- is_field(symbol).
%     true.
is_field(Field) :-
    extracted_predicate(_, List), 
    % Check if the field is present in the attribute list of any predicate.
    \+ \+ member(Field, List), !.

%!  with_kv_maybe_more(+Parent, +Action, +Key, +Value) is det.
%
%   Processes a key-value pair, optionally handling empty JSON objects.
%
%   This predicate delegates processing to `with_kv/4` for standard key-value pairs.
%
%   @arg Parent A list of parent keys leading to the current JSON entry.
%   @arg Action The action to be performed, typically `assert`.
%   @arg Key The key in the key-value pair.
%   @arg Value The value in the key-value pair.
%
%   @example
%     % Process a key-value pair:
%     ?- with_kv_maybe_more([], assert, key, value).
with_kv_maybe_more(_O, _AR, _Key, json([])) :- 
    % Skip processing for empty JSON objects.
    !.
with_kv_maybe_more(O, AR, Key, Do) :- 
    % Delegate processing to `with_kv/4`.
    with_kv(O, AR, Key, Do), !.



%!  assert_id_about(+Parent, +Key, +ID, +NVAboutID) is det.
%
%   Asserts information about an ID based on its key and associated data.
%
%   This predicate processes a JSON structure where the key is associated with
%   an ID, and additional data (`NVAboutID`) is provided. It delegates to 
%   `with_json4/2` for further processing.
%
%   @arg Parent A list of parent keys leading to the current context.
%   @arg Key The key associated with the ID.
%   @arg ID The identifier to be processed.
%   @arg NVAboutID A list of additional key-value pairs associated with the ID.
%
%   @example
%     % Assert information about an ID:
%     ?- assert_id_about([], key, id_value, [key1=value1, key2=value2]).
assert_id_about(O, Key, ID, NVAboutID) :-
    % Pass the key, ID, and associated data to `with_json4/2` for processing.
    with_json4([Key|O], [Key=ID|NVAboutID]).

  %images= json( [ 'FBal0040476_1.jpg'= json( [ imageDescription='GAL4[Bx-MS1096].jpg',

%!  with_kv(+Parent, +Action, +Key, +Value) is det.
%
%   Processes key-value pairs in a JSON structure, handling nested structures
%   and specific patterns like `json/1` objects or lists of values.
%
%   This predicate processes keys and their associated values, supporting cases
%   where the value is a nested JSON structure, a list, or a single value. It
%   also handles special cases where subproperties or key-value constraints are used.
%
%   @arg Parent A list of parent keys leading to the current JSON entry.
%   @arg Action The action to perform, such as `assert`.
%   @arg Key The key in the key-value pair.
%   @arg Value The value associated with the key, which can be a JSON object, list, or atomic value.
%
%   @example:
%     % Process a key-value pair with a JSON object:
%     ?- with_kv([], assert, key, json([id=json([subkey=subvalue])])).
%
with_kv(O, AR, Key, json([ID=json(NVAboutID)|More])) :- 
    % Handle cases where the value is a JSON object with a nested ID.
    uses_id_subprops(Key),
    % Log the key and ID (commented out).
    % wdmsg_json(cr1(Key)=ID),
    atom(ID), !,
    % Declare the type of the ID based on the key.
    decl_type(ID, Key),
    % Process the ID as a regular key-value pair.
    with_kv(O, AR, Key, ID),
    % Assert additional information about the ID and its attributes.
    assert_id_about(O, Key, ID, NVAboutID),
    % Handle any remaining entries in the JSON object.
    with_kv_maybe_more(O, AR, Key, json(More)).
with_kv(O, AR, Key, json([ID=Value|More])) :- 
    % Handle cases where the value is an atomic value and the key allows key-value processing.
    key_can_nv(Key),
    atom(Value),
    % Prefix the key and value fields, ensuring they do not overlap with existing fields (commented out).
    % prefix_key(O,ID,Field), \+ is_field(Field),!,
    % prefix_key(O,Value,VField), \+ is_field(VField),!,
    % Declare the type of the ID based on the key.
    decl_type(ID, Key),
    % Process the ID as a regular key-value pair.
    with_kv(O, AR, Key, ID),
    % Assert a JSON representation of the name and value.
    % atom_concat(Key,'_name',Pred),
    % Pred = object_name,
    assert_JSON([name, ID, Value]),
    % Handle any remaining entries in the JSON object.
    with_kv_maybe_more(O, AR, Key, json(More)).
with_kv(O, AR, OK, Key=Values) :- 
    % Handle nested key-value pairs by appending the parent key to the hierarchy.
    !, with_kv([OK|O], AR, Key, Values).
with_kv(O, AR, Key, json(Values)) :- 
    % Handle a JSON object by delegating to the value processor.
    !, with_kv(O, AR, Key, Values).
with_kv(O, AR, Key, Value) :- 
    % Handle lists of values by mapping the key-value processor over the list.
    is_list(Value), Value \== [], !,
    maplist(with_kv(O, AR, Key), Value).
with_kv(O, AR, Kee, Value) :- 
    % Handle individual key-value pairs, applying the action and tracking the key.
    prefix_key(O, Kee, Key),
    % Remove any previous knowledge of the key.
    retractall(json_kv(Key, _)),
    % Create a key-value pair for processing.
    KV = json_kv(Key, Value),
    % Declare the key as seen.
    decl_seen(Value, Key),
    % Prepare the action for execution.
    Do =.. [AR, KV],
    % Execute the action.
    call(Do),
    % Check readiness if the action is `assert`.
    ignore((AR == assert,
    % Log the key and value (commented out).
    % wdmsg_json(cr(Key)=Value),
    check_ready(Key))).

%!  check_ready(+Key) is det.
%
%   Validates whether a key is ready for processing by checking its presence
%   in extracted predicates and ensuring all associated arguments are available.
%
%   This predicate iterates over all extracted predicates that include the given
%   key in their attribute list. For each predicate, it collects arguments, verifies
%   the argument count matches the expected length, asserts the resulting fact,
%   and declares types for the arguments.
%
%   @arg Key The key to check for readiness and process.
%
%   @example:
%     % Check readiness for a key and process associated predicates:
%     ?- check_ready(symbol).
check_ready(Key) :-
    % Iterate over extracted predicates where Key is in the attribute list.
    forall((extracted_predicate(P, List), memberchk(Key, List)),
        (
            % Get the expected number of arguments.
            length(List, Len),
            % Collect arguments for all keys in the predicate.
            ignore((
                findall(Arg, (member(K, List), json_kv(K, Arg)), ArgList),
                % Ensure the number of collected arguments matches the expected length.
                length(ArgList, Len),
                % Form the fact as a list with the predicate name as the head.
                Fact = [P | ArgList],
                % Assert the fact in JSON format.
                assert_JSON(Fact),
                % Declare the types for the arguments.
                maplist(decl_type, ArgList, List)
            ))
        )
    ).
% Rows 937,381,148

% Dynamic predicates for tracking argument types and seen arguments.
:- dynamic(arg_typed/2).
:- dynamic(seen_arg/2).

%!  decl_type(+Argument, +Type) is det.
%
%   Declares the type of an argument by asserting it as `arg_typed/2`. If the
%   type already exists, it is updated. This predicate also logs the typing
%   information and asserts the argument in JSON format.
%
%   @arg Argument The argument whose type is being declared.
%   @arg Type The type to associate with the argument.
%
%   @example:
%     % Declare the type of an argument:
%     ?- decl_type(my_arg, my_type).
decl_type(Arg, Type) :-
    % Remove any existing "seen" record for the argument.
    retractall(seen_arg(Arg, _)),
    % Check if the argument type already exists and log it.
    arg_typed(Arg, Type), 
    wdmsg_json(arg_typed(Arg, Type)), 
    !.
decl_type(Arg, Type) :-
    % Assert the new argument type.
    assert(arg_typed(Arg, Type)),
    !,
    % Assert the argument in JSON format.
    assert_JSON([Type, Arg]).

%!  decl_seen(+Argument, +Type) is det.
%
%   Marks an argument as "seen" if it has not already been seen or typed. This
%   prevents redundant declarations for the same argument.
%
%   @arg Argument The argument to mark as seen.
%   @arg Type The type of the argument being marked.
%
%   @example:
%     % Mark an argument as seen:
%     ?- decl_seen(my_arg, my_type).
decl_seen(Arg, _) :-
    % Check if the argument has already been seen.
    seen_arg(Arg, _), 
    !.
decl_seen(Arg, _) :-
    % Check if the argument type already exists.
    arg_typed(Arg, _), 
    !.
decl_seen(Arg, Type) :-
    % Mark the argument as seen with the given type.
    assert(seen_arg(Arg, Type)), 
    !.

%!  err is det.
%
%   Runs test cases for processing JSON data with FlyBase metadata and structured content.
%
%   This predicate includes various examples of FlyBase JSON structures to validate 
%   the processing pipeline. It tests the handling of metadata, gene data, transcripts, 
%   drivers, and associated information like publications, sequences, and genome locations.
%
%   The test cases ensure that:
%   - Metadata is parsed correctly.
%   - Nested structures, such as genome locations and exons, are processed accurately.
%   - Key-value pairs within the JSON data are handled appropriately.
%
%   @example:
%     % Run the test cases for JSON processing:
%     ?- err.
err
:- process_json(json([metaData= json( [ dataProvider='FlyBase',
                    publications=['PMID:35266522'], schemaVersion='0.4.0',release=fb_2023_04,
                    genomicCoordinateSystem='1-start, fully-closed',
                    dateProduced='2023-07-25T03:20:14+00:00']),
  data= [ json( [ primaryId='FBtr0070001',
                  symbol='tRNA:Pro-CGG-1-1-RA',
                  sequence='GGCTCGTTGGTCTAGGGGTATGATTCTCGCTTCGGGTGCGAGAGGTCCCGGGTTCAAATCCCGGACGAGCCC',
                  url='http://flybase.org/reports/FBtr0070001.html',
                  symbolSynonyms=['CR32826-RA','tRNA:CR32826-RA'], taxonId='NCBITaxon:7227',soTermId='SO:0000253',
                  genomeLocations= [ json( [ assembly='R6',
                                             gca_accession='GCA_000001215.4',
                                             exons= [ json( [ 'INSDC_accession'='AE014298.5', chromosome='X',strand=(+),
                                                              startPosition=20025099,endPosition=20025170])]])],
                  gene= json( [ geneId='FBgn0052826',
                                symbol='tRNA:Pro-CGG-1-1',
                                url='http://flybase.org/reports/FBgn0052826.html',
                                locusTag='Dmel_CR32826',
                                synonyms=['tRNA:P:CGG:AE002611'],
                                name='transfer RNA:Proline-CGG 1-1']),
                  publications=['PMID:26673694']]),
          json( [ primaryId='FBtr0070292',
                  symbol='snoRNA:M-RA',
                  sequence='AATTCAATGATTTCAACTTATTCTAATACACAC',
                  url='http://flybase.org/reports/FBtr0070292.html', taxonId='NCBITaxon:7227',soTermId='SO:0000275',
                  crossReferenceIds=['REFSEQ:NR_002093.1'],
                  genomeLocations= [ json( [ assembly='R6',
                                             gca_accession='GCA_000001215.4',
                                             exons= [ json( [ 'INSDC_accession'='AE014298.5', chromosome='X',strand=(-),
                                                              startPosition=1482492,endPosition=1482590])]])],
                  gene= json( [ geneId='FBgn0044508',
                                symbol='snoRNA:M',
                                url='http://flybase.org/reports/FBgn0044508.html',
                                locusTag='Dmel_CR32807', synonyms=['CR32807'],name='snoRNA:M'])]),
          json( [ primaryId='FBtr0308931',
                  symbol='lncRNA:CR33218-RC',
                  sequence='ACGAAATCAATAAACATTTGTACCTTT',
                  url='http://flybase.org/reports/FBtr0308931.html',
                  symbolSynonyms=['CR33218-RC'], taxonId='NCBITaxon:7227',soTermId='SO:0001877',
                  crossReferenceIds=['REFSEQ:NR_047742.1'],
                  genomeLocations= [ json( [ assembly='R6',
                                             gca_accession='GCA_000001215.4',
                                             exons= [ json( [ 'INSDC_accession'='AE014298.5', chromosome='X',strand=(+),
                                                              startPosition=2330159,endPosition=2330355]),
                                                      json( [ 'INSDC_accession'='AE014298.5', chromosome='X',strand=(+),
                                                              startPosition=2330413,endPosition=2330826])]])],
                  gene= json( [ geneId='FBgn0053218',
                                symbol='lncRNA:CR33218',
                                url='http://flybase.org/reports/FBgn0053218.html',
                                locusTag='Dmel_CR33218',
                                synonyms=['CG2854','CG33218','CR33218','CT9762'],
                                name='long non-coding RNA:CR33218'])]),
          json( [ primaryId='FBtr0070634', symbol='lncRNA:roX1-RA',sequence='TTGTAGAACAATTACTATA',
                  url='http://flybase.org/reports/FBtr0070634.html',
                  symbolSynonyms=['roX1-RA','roX1-RA....'], taxonId='NCBITaxon:7227',soTermId='SO:0001877',
                  crossReferenceIds=['REFSEQ:NR_002098.2'],
                  genomeLocations= [ json( [ assembly='R6',
                                             gca_accession='GCA_000001215.4',
                                             exons= [ json( [ 'INSDC_accession'='AE014298.5', chromosome='X',strand=(-),
                                                              startPosition=3858940,endPosition=3862697])]])],
                  gene= json( [ geneId='FBgn0019661',
                                symbol='lncRNA:roX1',
                                url='http://flybase.org/reports/FBgn0019661.html',
                                locusTag='Dmel_CR32777',
                                synonyms= [ 'BcDNA:GH10432', 'CR32777','EG:EG0002.2','RoX1',
                                            'chrX:3706836..3706970',rox1],
                                name='long non-coding RNA on the X 1']),
                  publications=['PMID:10445033','PMID:12446910','PMID:9038336','PMID:9038337']])]   ])).




err
:- process_json([],'
{
     "metaData": {
          "dataProvider": "FlyBase",
          "publications": [
               "PMID:35266522"
          ],
          "schemaVersion": "0.4.0",
          "release": "fb_2023_04",
          "genomicCoordinateSystem": "1-start, fully-closed",
          "dateProduced": "2023-07-25T03:20:14+00:00"
     },
     "data": [
          {
               "primaryId": "FBtr0070001",
               "symbol": "tRNA:Pro-CGG-1-1-RA",
               "sequence": "GGCTCGTTGGTCTAGGGGTATGATTCTCGCTTCGGGTGCGAGAGGTCCCGGGTTCAAATCCCGGACGAGCCC",
               "url": "http://flybase.org/reports/FBtr0070001.html",
               "symbolSynonyms": [
                    "CR32826-RA",
                    "tRNA:CR32826-RA"
               ],
               "taxonId": "NCBITaxon:7227",
               "soTermId": "SO:0000253",
               "genomeLocations": [
                    {
                         "assembly": "R6",
                         "gca_accession": "GCA_000001215.4",
                         "exons": [
                              {
                                   "INSDC_accession": "AE014298.5",
                                   "chromosome": "X",
                                   "strand": "+",
                                   "startPosition": 20025099,
                                   "endPosition": 20025170
                              }
                         ]
                    }
               ],
               "gene": {
                    "geneId": "FBgn0052826",
                    "symbol": "tRNA:Pro-CGG-1-1",
                    "url": "http://flybase.org/reports/FBgn0052826.html",
                    "locusTag": "Dmel_CR32826",
                    "synonyms": [

                         "tRNA:P:CGG:AE002611"
                    ],
                    "name": "transfer RNA:Proline-CGG 1-1"
               },
               "publications": [
                    "PMID:26673694"
               ]
          },
          {
               "primaryId": "FBtr0070292",
               "symbol": "snoRNA:M-RA",
               "sequence": "AATTCAATGATTTCAACTTATTCTAATACACAC",
               "url": "http://flybase.org/reports/FBtr0070292.html",
               "taxonId": "NCBITaxon:7227",
               "soTermId": "SO:0000275",
               "crossReferenceIds": [
                    "REFSEQ:NR_002093.1"
               ],
               "genomeLocations": [
                    {
                         "assembly": "R6",
                         "gca_accession": "GCA_000001215.4",
                         "exons": [
                              {
                                   "INSDC_accession": "AE014298.5",
                                   "chromosome": "X",
                                   "strand": "-",
                                   "startPosition": 1482492,
                                   "endPosition": 1482590
                              }
                         ]
                    }
               ],
               "gene": {
                    "geneId": "FBgn0044508",
                    "symbol": "snoRNA:M",
                    "url": "http://flybase.org/reports/FBgn0044508.html",
                    "locusTag": "Dmel_CR32807",
                    "synonyms": [
                         "CR32807"
                    ],
                    "name": "snoRNA:M"
               }
          },
          {
               "primaryId": "FBtr0308931",
               "symbol": "lncRNA:CR33218-RC",
               "sequence": "ACGAAATCAATAAACATTTGTACCTTT",
               "url": "http://flybase.org/reports/FBtr0308931.html",
               "symbolSynonyms": [
                    "CR33218-RC"
               ],
               "taxonId": "NCBITaxon:7227",
               "soTermId": "SO:0001877",
               "crossReferenceIds": [
                    "REFSEQ:NR_047742.1"
               ],
               "genomeLocations": [
                    {
                         "assembly": "R6",
                         "gca_accession": "GCA_000001215.4",
                         "exons": [
                              {
                                   "INSDC_accession": "AE014298.5",
                                   "chromosome": "X",
                                   "strand": "+",
                                   "startPosition": 2330159,
                                   "endPosition": 2330355
                              },
                              {
                                   "INSDC_accession": "AE014298.5",
                                   "chromosome": "X",
                                   "strand": "+",
                                   "startPosition": 2330413,
                                   "endPosition": 2330826
                              }
                         ]
                    }
               ],
               "gene": {
                    "geneId": "FBgn0053218",
                    "symbol": "lncRNA:CR33218",
                    "url": "http://flybase.org/reports/FBgn0053218.html",
                    "locusTag": "Dmel_CR33218",
                    "synonyms": [
                         "CG2854",
                         "CG33218",
                         "CR33218",
                         "CT9762"
                    ],
                    "name": "long non-coding RNA:CR33218"
               }
          },
          {
               "primaryId": "FBtr0070634",
               "symbol": "lncRNA:roX1-RA",
               "sequence": "TTGTAGAACAATTACTATA",
               "url": "http://flybase.org/reports/FBtr0070634.html",
               "symbolSynonyms": [
                    "roX1-RA"
               ],
               "taxonId": "NCBITaxon:7227",
               "soTermId": "SO:0001877",
               "crossReferenceIds": [
                    "REFSEQ:NR_002098.2"
               ],
               "genomeLocations": [
                    {
                         "assembly": "R6",
                         "gca_accession": "GCA_000001215.4",
                         "exons": [
                              {
                                   "INSDC_accession": "AE014298.5",
                                   "chromosome": "X",
                                   "strand": "-",
                                   "startPosition": 3858940,
                                   "endPosition": 3862697
                              }
                         ]
                    }
               ],
               "gene": {
                    "geneId": "FBgn0019661",
                    "symbol": "lncRNA:roX1",
                    "url": "http://flybase.org/reports/FBgn0019661.html",
                    "locusTag": "Dmel_CR32777",
                    "synonyms": [
                         "BcDNA:GH10432",
                         "CR32777",
                         "EG:EG0002.2",
                         "RoX1",
                         "chrX:3706836..3706970",
                         "rox1"
                    ],
                    "name": "long non-coding RNA on the X 1"
               },
               "publications": [
                    "PMID:10445033",
                    "PMID:12446910",
                    "PMID:9038336",
                    "PMID:9038337"
               ]
          }
     ]
  }
}').

err
:- process_json([],'
{
  "metaData": {
    "dataProvider": "FlyBase",
    "title": "Frequently Used GAL4 Table",
    "dateProduced": "2023-07-24T23:20:12-04:00",
    "databaseRelease": "2023_04",
    "annotationRelease": "R6.53"
  },
  "data": [

   {"driver": {
        "name": "Scer\\GAL4<up>dpp.blk1</up>",
        "fbid": "FBal0040480",
        "images": {
          "FBal0040480_1.png": {
            "imageDescription": "GAL4[dpp.blk1].png",
            "publicationId": "FBrf0218242",
            "pubFigure": "Figure 5A",
            "permission": ""
          }
        },
        "pubs": {
          "FBrf0074522": "Staehling-Hampton et al., 1994, Cell Growth Diffn 5(6): 585--593",
          "FBrf0076140": "Wilder and Perrimon, 1995, Development 121(2): 477--488",
          "FBrf0084454": "Treisman and Rubin, 1995, Development 121(11): 3519--3527",
          "FBrf0086426": "Grimm and Pflugfelder, 1996, Science 271(5255): 1601--1604",
          "FBrf0087557": "Lecuit et al., 1996, Nature 381(6581): 387--393",
          "FBrf0087626": "Nellen et al., 1996, Cell 85(3): 357--368",
          "FBrf0087630": "Ng et al., 1996, Nature 381(6580): 316--318",
          "FBrf0088035": "Burke and Basler, 1996, Development 122(7): 2261--2269",
          "FBrf0088295": "Kim et al., 1996, Nature 382(6587): 133--138",
          "FBrf0089604": "Brook and Cohen, 1996, Science 273(5280): 1373--1377",
          "FBrf0089753": "Morimura et al., 1996, Dev. Biol. 177(1): 136--151",
          "FBrf0091093": "Johnston and Schubiger, 1996, Development 122(11): 3519--3529",
          "FBrf0091111": "Leevers et al., 1996, EMBO J. 15(23): 6584--6594",
          "FBrf0091167": "Shen and Mardon, 1997, Development 124(1): 45--52",
          "FBrf0091298": "Gonzalez-Crespo and Morata, 1996, Development 122(12): 3921--3928",
          "FBrf0091412": "Theisen et al., 1996, Development 122(12): 3939--3948",
          "FBrf0092493": "Chanut and Heberlein, 1997, Development 124(2): 559--567",
          "FBrf0092641": "Neumann and Cohen, 1997, Development 124(4): 871--880",
          "FBrf0093060": "Aplin and Kaufman, 1997, Mech. Dev. 62(1): 51--60",
          "FBrf0251844": "Matsuda et al., 2021, Nat. Commun. 12(1): 6435",
          "FBrf0252066": "Kinsey et al., 2021, G3 (Bethesda) 11(12): jkab350",
          "FBrf0253453": "Lu et al., 2022, Int. J. Mol. Sci. 23(9): 4543",
          "FBrf0253792": "Akiyama et al., 2022, Dev. Biol. 488: 91--103",
          "FBrf0255082": "Inoshita et al., 2022, iScience 25(12): 105476",
          "FBrf0255366": "Bharti et al., 2023, Proc. Natl. Acad. Sci. U.S.A. 120(2): e2211189119",
          "FBrf0256654": "He et al., 2023, Development 150(11): dev201297"
        },
        "rex_gene": {
          "FBgn0000490": "dpp"
        },
        "common_terms": "A/P boundary",
        "major_stages": {
          "FBdv00005336": "larval stage"
        },
        "major_tissues": {
          "FBbt00111520": "anterior-posterior compartment boundary of imaginal disc",
          "FBbt00001769": "eye disc morphogenetic furrow"
        },
        "transposons": {
          "FBtp0000365": "P{GAL4-dpp.blk1}"
        },
        "expression_desc_text": "Drives expression at the anterior/posterior compartment boundary of imaginal discs, and at the morphogenetic furrow of the eye disc.",
        "stocks": {
          "FBst0305049": "106380",
          "FBst0001553": "1553",
          "FBst0067066": "67066",
          "FBst0084296": "84296",
          "FBst0084316": "84316",
          "FBst0084337": "84337",
          "FBst0093385": "93385"
        }
      }
    },
    {
      "driver": {
        "name": "Scer\\GAL4<up>Ir25a.PA</up>",
        "fbid": "FBal0249373",
        "images": null,
        "pubs": {
          "FBrf0212725": "Abuin et al., 2011, Neuron 69(1): 44--60",
          "FBrf0215822": "Silbering et al., 2011, J. Neurosci. 31(38): 13357--13375",
          "FBrf0221182": "Min et al., 2013, Proc. Natl. Acad. Sci. U.S.A. 110(14): E1321--E1329",
          "FBrf0230271": "Chen et al., 2015, Nature 527(7579): 516--520",
          "FBrf0232388": "Enjin et al., 2016, Curr. Biol. 26(10): 1352--1358",
          "FBrf0236232": "Frank et al., 2017, Curr. Biol. 27(15): 2381--2388.e4",
          "FBrf0236716": "Chen and Amrein, 2017, Curr. Biol. 27(18): 2741--2750.e4",
          "FBrf0236934": "Rist and Thum, 2017, J. Comp. Neurol. 525(18): 3865--3889",
          "FBrf0237619": "Lee et al., 2018, Neuron 97(1): 67--74.e4",
          "FBrf0237676": "Ahn et al., 2017, eLife 6: e30115",
          "FBrf0238151": "Steck et al., 2018, eLife 7: e31625",
          "FBrf0240321": "Sánchez-Alcañiz et al., 2018, Nat. Commun. 9(1): 4252",
          "FBrf0240352": "Sun et al., 2018, eLife 7: e39249",
          "FBrf0241429": "Chai et al., 2019, Nat. Commun. 10(1): 643",
          "FBrf0242503": "Lei et al., 2019, Front. Physiol. 10: 556",
          "FBrf0246007": "Alpert et al., 2020, Curr. Biol. 30(12): 2275--2288.e5",
          "FBrf0247159": "Weaver et al., 2020, G3 (Bethesda) 10(11): 4147--4158",
          "FBrf0251811": "Dhakal et al., 2021, Commun. Biol. 4(1): 1281",
          "FBrf0253093": "Trisal et al., 2022, J. Neurosci. 42(14): 2930--2941",
          "FBrf0253272": "Task et al., 2022, eLife 11: e72599",
          "FBrf0255204": "Omelchenko et al., 2022, Front. Mol. Neurosci. 15: 1023492",
          "FBrf0256676": "Shrestha et al., 2023, EMBO Rep. 24(6): e56319",
          "unattributed": null
        },
        "rex_gene": {
          "FBgn0031634": "Ir25a"
        },
        "common_terms": "anterior cold cell, ACc, bitter sensing GRN,",
        "major_stages": {
          "FBdv00005336": "larval stage",
          "FBdv00005369": "adult stage"
        },
        "major_tissues": {
          "FBbt00049613": "bitter-sensing neuron",
          "FBbt00005923": "hygrosensory neuron",
          "FBbt00047485": "calcium-sensing neuron of labellar S-type taste bristle",
          "FBbt00048209": "bitter-sensing neuron of the leg",
          "FBbt00049720": "non-aristal sensory neuron VP3",
          "FBbt00051293": "adult thermosensory neuron",
          "FBbt00110990": "adult hygrosensory neuron Ir40a"
        },
        "transposons": {
          "FBtp0057158": "P{Ir25a-GAL4.A}"
        },
        "expression_desc_text": "Drives expression in a subset of bitter-sensing, hygrosensory, and cold-sensing thermosensory neurons in the labellum, legs, arista, sacculus, antenna, labrum, and larval head sensory organs",
        "stocks": {
          "FBst0041728": "41728"
        }
      }
    }
  ]
}').

%!  json1 is det.
%
%   Processes the FlyBase ncRNA genes JSON file.
%
%   This predicate handles the JSON file containing ncRNA gene data from FlyBase.
%   It processes the data to extract relevant information such as gene identifiers,
%   symbols, and associated metadata.
%
%   @example:
%     % Process the ncRNA genes JSON file:
%     ?- json1.
%   % 51,290,751 inferences, 8.285 CPU in 8.289 seconds (100% CPU, 6190948 Lips)
json1 :-
    process_json('/opt/logicmoo_opencog/hyperon-wam/data/ftp.flybase.org/releases/FB2023_04/precomputed_files/genes/ncRNA_genes_fb_2023_04.json').

%!  json2 is det.
%
%   Processes the FlyBase GAL4 insertions JSON file.
%
%   This predicate handles the JSON file containing GAL4 insertion data from FlyBase.
%   It processes the data to extract relevant information such as insertion identifiers,
%   driver names, and associated metadata.
%
%   @example:
%     % Process the GAL4 insertions JSON file:
%     ?- json2.
%   % 27,108,104 inferences, 4.454 CPU in 4.456 seconds (100% CPU, 6085908 Lips)
json2 :-
    process_json('/opt/logicmoo_opencog/hyperon-wam/data/ftp.flybase.org/releases/FB2023_04/precomputed_files/insertions/fu_gal4_table_fb_2023_04.json').




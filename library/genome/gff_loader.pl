
% ==============================
% GFF/GTF/GFF3 Reader
% ==============================

load_fb_gff(Fn,Filename):-
 track_load_into_file(Filename,
  must_det_ll((
    fbug(load_fb_gff(Fn,Filename)),
    directory_file_path(Directory, BaseName, Filename),
    file_name_extension(Id, _, BaseName),
    Type = 'SequenceFile',
    assert_OBO(id_type(Id,Type)),
    assert_OBO(pathname(Id,Filename)),!,
    assert_OBO(basename(Id,BaseName)),!,
    assert_OBO(directory(Id,Directory)),!,
    setup_call_cleanup(open(Filename,read,In), (repeat,load_fb_gff_read(Id,In)), close(In))))).
 % Main predicate to parse a GFF line and store it as facts
load_fb_gff_read(_Fn,In):- (at_end_of_stream(In);reached_file_max),!.
load_fb_gff_read(Fn,In):- read_line_to_string(In,Line), load_fb_gff_line(Fn,Line),!,fail.

load_fb_gff_line(Fn,Line) :- % Predicate to process a line starting with ##sequence-region
    split_string(Line, " \t", " \t", ['##sequence-region', SeqID, StartStr, EndStr]),
    atom_number(StartStr, Start), atom_number(EndStr, End),!,
    assert_MeTTa(genomic_sequence_region(Fn,SeqID,Start,End)).
load_fb_gff_line(_Fn,Line) :- split_string(Line, " \t", " \t", ['##gff-version'|_]),!.
load_fb_gff_line(_Fn,Line) :- string_concat('#', _, Line),!.
load_fb_gff_line(Fn,Line) :-
    split_string(Line, "\t", "", [SeqID, Source, Type, StartStr, EndStr, ScoreStr, Strand, Phase | MoreProps]),
    atom_number(StartStr, Start),
    atom_number(EndStr, End),
    store_gff_fact(Fn,SeqID, "source", Source),
    store_gff_fact(Fn,SeqID, "type", Type),
    store_gff_fact(Fn,SeqID, "start", Start),
    store_gff_fact(Fn,SeqID, "end", End),
    store_gff_fact(Fn,SeqID, "score", ScoreStr),
    store_gff_fact(Fn,SeqID, "strand", Strand),
    store_gff_fact(Fn,SeqID, "phase", Phase),
    parse_and_store_attributes(SeqID, MoreProps).
load_fb_gff_line(Fn,Line):- fbug(load_fb_gff_line(Fn,Line)).
% Predicate to store each field as a fact
store_gff_fact(Fn,SeqID, Key, Value) :-
    Value \= ".",
    assert_MeTTa(genomic_sequence_feature(Fn, SeqID, Key, Value)).

% Predicate to handle the attributes field
parse_and_store_attributes(Fn, SeqID, [AttributesStr | _]) :-
    split_string(AttributesStr, ";", "", AttrList),
    maplist(parse_and_store_attribute(Fn, SeqID), AttrList).
% Parse individual attribute and store it
parse_and_store_attribute(Fn, SeqID, AttrStr) :-
    (split_string(AttrStr, "=", "\"", [Key, Value])->true;split_string(AttrStr, " ", "\"", [Key | Value])),
    store_gff_fact(Fn,SeqID, Key, Value).

/*

find . \( -name "*.fa" -o -name "*.gff" -o -name "*.json" \) -execdir bash -c 'for file; do metta_pattern="${file%.*}"*metta*; full_path="$(pwd)/$file"; if compgen -G "$metta_pattern" > /dev/null; then true; else echo "Metta file does not exist for $full_path"; fi; done' bash {} \; | sort -r

find .  ! -name "*.metta" - -execdir bash -c 'for file; do metta_pattern="${file%.*}"*metta*; full_path="$(pwd)/$file"; if compgen -G "$metta_pattern" > /dev/null; then true; else echo "Metta file does not exist for $full_path"; fi; done' bash {} \; | sort -r

find . \( -name "*.fa" -o -name "*.gff" -o -name "*.json" \) -execdir bash -c 'for file; do metta_pattern="${file%.*}"*datalog*; full_path="$(pwd)/$file"; if compgen -G "$metta_pattern" > /dev/null; then true; else echo "Datalog file does not exist for $full_path"; fi; done' bash {} \; | sort -r

*/



% ==============================
% FA/FASTA Reader
% ==============================
:- ensure_loaded(gff_loader).  % parse_and_store_attributes/2

load_fb_fa(Fn,Filename):-
 track_load_into_file(Filename,
  must_det_ll((
    fbug(load_fb_fa(Fn,Filename)),
    directory_file_path(Directory, BaseName, Filename),
    file_name_extension(Id, _, BaseName),
    Type = 'SequenceFile',
    assert_OBO(id_type(Id,Type)),
    assert_OBO(pathname(Id,Filename)),!,
    assert_OBO(basename(Id,BaseName)),!,
    assert_OBO(directory(Id,Directory)),!,
    setup_call_cleanup(open(Filename,read,In,[encoding(utf8)]), load_fb_fa_read(Id,In,_,0), close(In))))).
load_fb_fa_read(_Fn,In, _, _):- (at_end_of_stream(In);reached_file_max),!.
load_fb_fa_read(Fn,In,FBTe,At):- read_line_to_string(In,Str), load_fb_fa_read_str(Fn,In,FBTe,Str,At).

load_fb_fa_read_str(Fn,In,_,Str,_):- string_concat('>',Line,Str),!,
     split_string(Line, " \t", " \t", [FBTe|Props]),!,
     parse_and_store_attributes(FBTe, Props),
     load_fb_fa_read(Fn,In,FBTe,0).

load_fb_fa_read_str(Fn,In,FBTe,Str,From):-
   atom_chars(Str,Chars),
   Data =..[fasta_sequence,Fn,FBTe,From,Chars],
   assert_MeTTa(Data),!,
   length(Chars,Plus),
   At is From+Plus,
   load_fb_fa_read(Fn,In,FBTe,At).


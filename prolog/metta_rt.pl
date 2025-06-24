
:- set_prolog_flag(mettalog_rt,true).

:- (prolog_load_context(directory, Value);Value='.'), absolute_file_name('../libraries/',Dir,[relative_to(Value),file_type(directory)]),
    absolute_file_name('.',Here,[relative_to(Value),file_type(directory)]),
    asserta(metta_rt_src_file(Here)),
    atom_concat(Dir,'predicate_streams',PS),
    atom_concat(Dir,'logicmoo_utils',LU),
    atom_concat(Dir,'lsp_server_metta',LSP),
    attach_packs(Dir,[duplicate(replace),search(first)]),
    pack_attach(PS,[duplicate(replace),search(first)]),
    pack_attach(LU,[duplicate(replace),search(first)]),
    pack_attach(LSP,[duplicate(replace),search(first)]).


interp_src_dir(Dir):- getenv('INTERP_SRC_DIR',Dir),exists_directory(Dir),!.
interp_src_dir(Dir):- metta_rt_src_file(Here), absolute_file_name('metta_lang',Dir,[relative_to(Here),file_type(directory)]).

normal_IO:- stream_property(I,file_no(0)),
   stream_property(O,file_no(1)),
   stream_property(E,file_no(2)),
   set_system_IO(I,O,E),
   set_prolog_IO(I,O,E),!.


ii:- interp_src_dir(Dir),
   absolute_file_name('metta_interp',File,[relative_to(Dir)]),
   load_files(File, [qcompile(auto)]).


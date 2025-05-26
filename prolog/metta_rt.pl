

:- (prolog_load_context(directory, Value);Value='.'), absolute_file_name('../libraries/',Dir,[relative_to(Value)]),
    atom_concat(Dir,'predicate_streams',PS),
    atom_concat(Dir,'logicmoo_utils',LU),
    atom_concat(Dir,'lsp_server_metta',LSP),
    attach_packs(Dir,[duplicate(replace),search(first)]),
    pack_attach(PS,[duplicate(replace),search(first)]),
    pack_attach(LU,[duplicate(replace),search(first)]),
    pack_attach(LSP,[duplicate(replace),search(first)]).

%:- ensure_loaded(library(metta_lang/metta_interp)).


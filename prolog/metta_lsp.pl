:- module(metta_lsp, 
    [ is_metta_lsp_file/0 ]).

unused:- (prolog_load_context(directory, Value);Value='.'),
    absolute_file_name('../../libraries/',Dir,[relative_to(Value)]),
    absolute_file_name('../../',MettaDir,[relative_to(Value)]),
    atom_concat(Dir,'predicate_streams',PS),
    atom_concat(Dir,'logicmoo_utils',LU),
    atom_concat(Dir,'lsp_server_metta',LSP),
    % attach_packs(Dir,[duplicate(replace),search(first)]),
    pack_attach(PS,[duplicate(replace),search(first)]),
    pack_attach(LU,[duplicate(replace),search(first)]),
    pack_attach(MettaDir,[duplicate(replace),search(first)]),
    pack_attach(LSP,[duplicate(replace),search(first)]).


is_metta_lsp_file.

:- use_module(library(lsp_server_metta)).


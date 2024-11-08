
:- discontiguous(lsp_hooks:handle_msg_hook/3).
    :- multifile(lsp_hooks:handle_msg_hook/3).
    :-   dynamic(lsp_hooks:handle_msg_hook/3).
:- discontiguous(lsp_hooks:exec_code_action/3).
    :- multifile(lsp_hooks:exec_code_action/3).
    :-   dynamic(lsp_hooks:exec_code_action/3).
:- discontiguous(lsp_hooks:compute_code_lens/3).
    :- multifile(lsp_hooks:compute_code_lens/3).
    :-   dynamic(lsp_hooks:compute_code_lens/3).
:- discontiguous(lsp_hooks:compute_code_action/3).
    :- multifile(lsp_hooks:compute_code_action/3).
    :-   dynamic(lsp_hooks:compute_code_action/3).
:- discontiguous(lsp_hooks:handle_save_actions/3).
    :- multifile(lsp_hooks:handle_save_actions/3).
    :-   dynamic(lsp_hooks:handle_save_actions/3).


:- multifile(user:predicate_help_hook/5).
:-   dynamic(user:predicate_help_hook/5).

:- multifile(lsp_hooks:hover_hook/5).
:-   dynamic(lsp_hooks:hover_hook/5).


:-encoding(iso_latin_1).
pllm:is_word(_) :-
    dumpST,
    break.

:- dynamic so_convert:is_word/1.
:- multifile so_convert:is_word/1.
:- module_transparent so_convert:is_word/1.



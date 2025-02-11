:- use_module(library(jpl)).
:- ensure_loaded(minecraft_bot_driver).
% :- ensure_loaded(library(metta_lang/metta_interp)).
% :- ensure_loaded(library(mettalog)).

on_main:- writeq(login('MeTTaPrologBot', '', 'localhost', 25565)),nl.

:- writeln(loaded(minecraft_bot_hello)).

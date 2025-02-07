:- use_module(library(jpl)).
:- ensure_loaded(minecraft_bot_driver).

on_main:- writeq(login('MeTTaPrologBot', '', 'localhost', 25565)),nl.

:- writeln(loaded(minecraft_bot_hello)).
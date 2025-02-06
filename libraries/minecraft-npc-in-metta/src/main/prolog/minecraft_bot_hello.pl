:- use_module(library(jpl)).

% Event Handlers
on_chat_message(Message) :-
    format("Chat received: ~w~n", [Message]),
    (Message = "hello bot" -> chat("Hello, player!");
     Message = "who are you" -> chat("I am a Prolog-powered bot!");
     format("Ignoring chat: ~w~n", [Message])).

on_bot_disconnected(Reason) :-
    format("Bot disconnected: ~w~n", [Reason]).

% Sending chat messages to Java
chat(Message) :-
    retract(command_queue(Q)),
    append(Q, [chat(Message)], NewQ),
    assert(command_queue(NewQ)).

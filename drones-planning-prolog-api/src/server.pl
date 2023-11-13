:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- use_module('route_handler').
:- use_module('logic').

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).

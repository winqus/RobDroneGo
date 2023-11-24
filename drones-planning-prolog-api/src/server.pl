:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_log)).

:- use_module('route_handler').

:- debug(http(request)).

start_server(Port) :-
    http_server(http_dispatch, [port(Port)]).
    

server(Port) :-
    http_server(http_dispatch,
                [ port(Port),
                    workers(16)
                ]),
    server_loop.

server_loop :-
    repeat,
    thread_get_message(Message),
    handle_message(Message),
    Message == stop_server,
    !.

handle_message(stop_server, Port) :-
    http_stop_server(Port, []),
    writeln('Server stopped.').

handle_message(Message) :-
    % Add handling for other messages as needed
    writeln('Received unknown message: '), writeln(Message).

start :-
    start_server(4400),
    write_ln('Starting server...').
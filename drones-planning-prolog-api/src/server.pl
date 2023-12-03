:- include('../config.pl').

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_log)).
:- use_module(library(date)).

:- use_module('route_handler').

:- debug(http(request)).

set_log_file :-
    get_current_date(DateStr),
    format(string(LogFileNameString), 'server-~w.log', [DateStr]),
    atom_string(LogFileName, LogFileNameString),
    write('Log file: '), writeln(LogFileName),
    set_setting(http:logfile, LogFileName).
get_current_date(DateStr) :- get_time(Stamp), format_time(string(DateStr), '%Y-%m-%d', Stamp).

start :-
    set_log_file,
    port(Port),
    format('Starting server on port ~d~n', [Port]),
    log_message('Starting server on port'), log_message_ln(Port),
    start_server(Port).

start_server(Port) :-
    set_log_file,
    format('Starting server on port ~d~n', [Port]),
    log_message('Starting server on port'), log_message_ln(Port),
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

:- module(taskPlanningRouteHandler, []).

:- include('../config.pl').
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(library(thread)).


:- dynamic planning_state/1.

:- http_handler('/planning-api/planTasks', post_planTasks_handler, [method(post)]).
:- http_handler('/planning-api/getPlanResults', get_planResults_handler, [method(get)]).

% Initialize the planning state to "unstarted".
:- assert(planning_state(unstarted)).

post_planTasks_handler(Request) :-
    catch(
        post_planTasks(Request),
        Error,
        handle_error(Error)
    ).

handle_error(Error) :-
    format('Status: 500~n'),
    format('Content-type: application/json~n~n'),
    format('{"error": "An error occurred: ~w"}', [Error]).

post_planTasks(Request) :-
  planning_state(State),
  (   (State = unstarted; State = planned)
  ->  retract(planning_state(State)),
      assert(planning_state(planning)),
      http_read_data(Request, Data, [content_type(text/plain), to(atom)]),
      taskPanningDataFile(TaskDataFile),
      save_data_to_file(Data, TaskDataFile),
      thread_create(perform_planning, _, [detached(true)]),
      format('Content-type: application/json~n~n'),
      format('{"message": "Planning started", "state": "started"}')
  ;   State = planning
  ->  format('Content-type: application/json~n~n'),
      format('{"message": "Planning is currently in progress", "state": "planning"}')
  ).

get_planResults_handler(_Request) :-
    taskPlanningOutputFile(OutputFile),
(   exists_file(OutputFile)
->  open(OutputFile, read, Stream),
    read_file_to_string(Stream, Results, []),
    close(Stream),
    format('Content-type: application/json~n~n'),
    format('~w', [Results])
;   format('Content-type: application/json~n~n'),
    format('{"error": "No planning results available"}')
).

process_data(Data) :-
  split_string(Data, "\n", "", Lines),
  maplist(assert_fact, Lines).

assert_fact(Line) :-
  string(Line),
  Line \= "",
  atom_to_term(Line, Fact, []),
  assert(Fact).


perform_planning :-
  % Get the path for task data and output file
  taskPlanningOutputFile(OutputFile),
  taskPanningDataFile(TaskDataFile),

  % Ensure the directory exists, create if necessary
  ensure_directory_exists(TaskDataFile),
  ensure_directory_exists(OutputFile),

  % Read task data from file
  open(TaskDataFile, read, TaskDataStream),
  read_stream_to_terms(TaskDataStream, TaskData),
  close(TaskDataStream),

  % Process task data (placeholder logic)
  sleep(5),
  PlanningResult = "Some task planning result based on the data: ",

  % Write the results to the output file
  open(OutputFile, write, StreamResults),
  write(StreamResults, PlanningResult),
  write(StreamResults, TaskData),
  close(StreamResults),

  % Update the planning state
  retract(planning_state(planning)),
  assert(planning_state(planned)).



read_stream_to_terms(Stream, TaskData) :-
  read_file_to_terms(Stream, TaskData, []).

read_file_to_terms(Stream, TaskData, Options) :-
  read_term(Stream, Term, Options),
  (   Term == end_of_file
  ->  TaskData = []
  ;   TaskData = [Term|TaskDataRest],
      read_file_to_terms(Stream, TaskDataRest, Options)
  ).

save_data_to_file(Data, FileName) :-
  ensure_directory_exists(FileName),
  open(FileName, write, Stream),
  write(Stream, Data),
  close(Stream).

save_stream_to_file(Stream, FileName) :-
  ensure_directory_exists(FileName),
  open(FileName, write, Stream),
  close(Stream).

ensure_directory_exists(FileName) :-
  file_directory_name(FileName, Dir),
  (   exists_directory(Dir)
  ->  true
  ;   make_directory_path(Dir)
  ).
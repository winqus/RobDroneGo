:- module(taskPlanningRouteHandler, []).

:- include('../config.pl').
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(library(thread)).


:- dynamic planning_state/1.

:- http_handler('/planning-api/planTasks', post_planTasks_handler, [method(post)]).
:- http_handler('/planning-api/getPlanResults', get_planResults_handler, [method(get)]).
:- http_handler('/planning-api/getPlanningStatus', get_planningStatus_handler, [method(get)]).

log_message_ln(Message) :-
  http_log('~p~n', [Message]).

log_message(Message) :-
  http_log('~p ', [Message]).

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
      % http_read_data(Request, Data, []), if previous line stops working, try this
      taskPanningDataFile(TaskDataFile),
      save_data_to_file(Data, TaskDataFile), % might or might not need some kind of processing
      thread_create(perform_planning, _, [detached(true)]),
      format('Content-type: application/json~n~n'),
      format('{"message": "Planning started", "state": "started"}')
  ;   State = planning
  ->  format('Content-type: application/json~n~n'),
      format('{"message": "Planning is currently in progress", "state": "planning"}')
  ).

get_planningStatus_handler(_Request) :-
  planning_state(State),
  (   State = unstarted
  ->  format('Content-type: application/json~n~n'),
      format('{"message": "Planning has not yet started", "state": "unstarted"}')
  ;   State = planning
  ->  format('Content-type: application/json~n~n'),
      format('{"message": "Planning is currently in progress", "state": "planning"}')
  ;   State = planned
  ->  format('Content-type: application/json~n~n'),
      format('{"message": "Planning has completed", "state": "planned"}')
  ).

get_planResults_handler(_Request) :-
  planning_state(State),
  (   State = planned
  ->  taskPlanningOutputFile(OutputFile),
      (   exists_file(OutputFile)
      ->  
          log_message('Reading planning results from file: '), log_message_ln(OutputFile),
          read_file_to_string(OutputFile, PlanningResultsListOfTermsAsString, []),
          atom_to_term(PlanningResultsListOfTermsAsString, PlanningResultsListOfTerms, []),
          log_message('Planning results read from file:'),
          log_message_ln(PlanningResultsListOfTerms),

          % TODO: modify this later to suit the new format
          create_json_response(PlanningResultsListOfTerms, JsonResponse),


          format('Content-type: application/json~n~n'),
          json_write(current_output, JsonResponse)
      ;   format('Content-type: application/json~n~n'),
          format('{"error": "No planning results available"}')
      )
  ;   
    format('Status: 400~n'),
    format('Content-type: application/json~n~n'),
    format('{"error": "Planning not yet completed"}')
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
  read_stream_to_terms(TaskDataStream, _TaskData),
  close(TaskDataStream),

  % Process task data (placeholder logic)
  sleep(10),
  PlanningResult = "[taskToTask('0001-0001', '0001-0002', 1), taskToTask('0001-0001', '0001-0003', 2), taskToTask('0001-0002', '0001-0003', 3), taskToTask('0001-0002', '0001-0001', 4), taskToTask('0001-0003', '0001-0001', 5), taskToTask('0001-0003', '0001-0002', 6), robotToTask('0000-0001', '0001-0001', 1), robotToTask('0000-0001', '0001-0002', 2), robotToTask('0000-0001', '0001-0003', 3)].",

  % Write the results to the output file
  open(OutputFile, write, StreamResults),
  write(StreamResults, PlanningResult),
  % write(StreamResults, TaskData),
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


% Add more JSON stuff if needed later
taskToTask_to_json(taskToTask(A, B, C), _{type: "taskToTask", fromTaskId: A, toTaskId: B, cost: C}).
robotToTask_to_json(robotToTask(A, B, C), _{type: "robotToTask", fromRobotId: A, toTaskId: B, cost: C}).

terms_to_json_list([], []).
terms_to_json_list([H|T], [JH|JT]) :-
    (   H = taskToTask(_, _, _)
    ->  taskToTask_to_json(H, JH)
    ;   robotToTask_to_json(H, JH)
    ),
    terms_to_json_list(T, JT).

create_json_response(ListOfTerms, JsonResponse) :-
    terms_to_json_list(ListOfTerms, JsonList),
    JsonResponse = _{data: JsonList}.
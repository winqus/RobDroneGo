:- module(taskPlanningRouteHandler, []).

:- include('../config.pl').

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(library(thread)).

:- use_module(pathPlanningRouteHandler).
:- use_module(logic).
:- use_module(taskDataPreparation).
:- use_module(geneticRunner).

:- dynamic planning_state/1.

:- http_handler('/planning-api/planTasks', post_planTasks_handler, [method(post)]).
:- http_handler('/planning-api/getPlanResults', get_planResults_handler, [method(get)]).
:- http_handler('/planning-api/getPlanningStatus', get_planningStatus_handler, [method(get)]).

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
  (   (State = unstarted; State = planned; State = error)
  ->  retract(planning_state(State)),
      assert(planning_state(planning)),
      http_read_data(Request, Data, [content_type(text/plain), to(atom)]),
      % http_read_data(Request, Data, []), if previous line stops working, try this
      taskPanningDataFile(TaskDataFile),
      save_data_to_file(Data, TaskDataFile), % might or might not need some kind of processing
      thread_create(perform_planning_wrapper, _, [detached(true)]),
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
          open(OutputFile, read, Stream),
          json_read_dict(Stream, PlanningResultsJSON),
          close(Stream),
          
          log_message('Planning results read from file:'), log_message_ln(PlanningResultsJSON),

          JsonResponse = PlanningResultsJSON,
          log_message('Planning results converted to JSON, preparing response;'),

          format('Content-type: application/json~n~n'),
          json_write(current_output, JsonResponse)
      ;   format('Content-type: application/json~n~n'),
          format('{"error": "No planning results available"}')
      )
  ;   
    State = error,
    taskPlanningErrorFile(ErrorFile),
    (   exists_file(ErrorFile),
        read_file_to_string(ErrorFile, Error, [])
    ->  format('Content-type: application/json~n~n'),
        format('{"error": "~w"}', [Error])
    ;   format('Content-type: application/json~n~n'),
        format('{"error": "No planning results available due to failure"}')
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

perform_planning_wrapper :-
  catch(
    perform_planning,
    error(Error, _Context),
    handle_planning_error(Error)
  ).

handle_planning_error(Error) :-
  log_message('Planning failed with error: '), log_message_ln(Error),
  taskPlanningErrorFile(ErrorFile),
  ensure_directory_exists(ErrorFile),
  open(ErrorFile, write, Stream),
  write(Stream, Error),
  close(Stream),
  planning_state(State),
  retract(planning_state(State)),
  assert(planning_state(error)).

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
  log_message('Task data read from file: '), log_message_ln(TaskData),

  % sleep(1),
  taskDataPreparation:extract_robots(TaskData, Robots),
  log_message('Extracted robots: '), log_message_ln(Robots),

  logic:load_info(),
  log_message('Loaded info; '),

  geneticRunner:runGeneticForRobots(Robots, TaskData, Results),
  log_message('Genetic planning completed; '), log_message_ln(Results),

  taskDataPreparation:create_json_response(Results, JsonResponse),
  PlanningResults = JsonResponse,
  log_message('Planning results converted to JSON, preparing response;'),

  % Write the results to the output file
  open(OutputFile, write, StreamResults),
  % write(StreamResults, PlanningResults),
  json_write(StreamResults, PlanningResults),
  % write(StreamResults, TaskData),
  close(StreamResults),

  % Update the planning state
  retract(planning_state(planning)),
  assert(planning_state(planned)).

% Process each robot and apply genetic planning
% process_robots([], _, []).
% process_robots([Robot|Rest], TaskData, [Result|Results]) :-
%     Robot = robot(RobotId, _),
%     log_message('Planning for robot: '), log_message_ln(RobotId),
%     extract_tasks_for_robot(RobotId, TaskData, RobotTasks),
%     geneticPlanning(Robot, RobotTasks, Result),
%     process_robots(Rest, TaskData, Results).

% geneticPlanning(Robot, Tasks, Result) :- !,
%   log_message('Performing genetic planning for: '), log_message_ln(Robot),
%   log_message('Tasks: '), log_message_ln(Tasks),
%   % TODO: modify for actual genetic planning
%   Result = [planningResult(Robot, Tasks)].

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

% save_stream_to_file(Stream, FileName) :-
%   ensure_directory_exists(FileName),
%   open(FileName, write, Stream),
%   close(Stream).

ensure_directory_exists(FileName) :-
  file_directory_name(FileName, Dir),
  (   exists_directory(Dir)
  ->  true
  ;   make_directory_path(Dir)
  ).

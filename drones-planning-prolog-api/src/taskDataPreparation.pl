:- module(taskDataPreparation, [
  generate_data/5,
  extract_all_tasks/2, extract_robots/2, extract_tasks_for_robot/3, 
  find_robot_to_task/3, find_task_to_task/2, find_all_task_to_task/3, 
  calculateTaskToTaskCosts/3, calculateRobotToTaskCosts/4,
  create_json_response/2
  ]).

:- use_module(library(http/json)).

:- use_module(pathPlanningRouteHandler).
:- use_module(logic).
:- use_module(navigationBuildingsFloors).
:- use_module(graph_creation_for_maze_diagonal).
:- use_module(astar_maze_diagonal_algorithm).

/*
 generate_data/5
  - Generates all required data for the GA algorithm (also calls logic:load_info() for path planning).
  - Input:
    - TaskData: list of task/5 and robot/2 terms
  - Output:
    - Tasks: list of task/5 terms, [task(RobotId, TaskId, Origin, Destination, Type) | Rest].
    - Robots: list of robot/2 terms, [robot(RobotId, Origin) | Rest].
    - TaskToTaskCombinations: list of taskToTask/3 terms, [taskToTask(TaskId1, TaskId2, Cost) | Rest].
    - RobotToTaskCombinations: list of robotToTask/3 terms, [robotToTask(RobotId, TaskId, Cost) | Rest].

 General order of operations:
  1. Get the TaskData from file or elsewhere
  2. extract_robots(TaskData, Robots), extracts Robots from TaskData
  3. extract_all_tasks(TaskData, Tasks), extracts Tasks from TaskData
  4. find_robot_to_task(Robots, Tasks, RobotToTaskCombinations), generates RobotToTask combinations
  5. find_all_task_to_task(TaskData, Robots, AllTaskToTaskCombinations), generates TaskToTask combinations for each robot and combines into single list
  6. calculateTaskToTaskCosts(AllTaskToTaskCombinations, Tasks, AllTaskToTaskCombinationsWithCostsList), calculates costs for each TaskToTask combination
  7. calculateRobotToTaskCosts(RobotToTaskCombinations, Tasks, Robots, AllRobotToTaskCombinationsWithCostsList), calculates costs for each RobotToTask combination
  8. Data prepared for use in the GA algorithm, use Robots, Tasks, AllTaskToTaskCombinationsWithCostsList, AllRobotToTaskCombinationsWithCostsList

 Example TaskData:
    TaskData = [
            robot('robo-0001',origin('B',2,cel(1,9))),
            robot('robo-0002',origin('B',1,cel(2,3))),
            task('robo-0001','task1-r1',origin('B',2,cel(2,9)),destination('B',2,cel(6,8)),'PickUpAndDelivery'),
            task('robo-0001','task2-r1',origin('A',2,cel(2,3)),destination('B',2,cel(3,9)),'PickUpAndDelivery'),
            task('robo-0001','task3-r1',origin('C',2,cel(2,3)),destination('A',2,cel(2,5)),'PickUpAndDelivery'),
            task('robo-0002','task3-r2',origin('C',2,cel(2,5)),destination('D',3,cel(4,3)),'PickUpAndDelivery'),
            task('robo-0002','task4-r2',origin('C',2,cel(2,3)),destination('D',3,cel(4,5)),'PickUpAndDelivery')
        ]
*/

%%% Extracts all tasks from the task data, skipping non-task items
extract_all_tasks([], []).
extract_all_tasks([task(RobotId, TaskId, Origin, Destination, Type)|Rest], [task(RobotId, TaskId, Origin, Destination, Type)|Tasks]) :-
    extract_all_tasks(Rest, Tasks).
extract_all_tasks([_|Rest], Tasks) :-
    extract_all_tasks(Rest, Tasks).

%%% Extracts all robots from the task data, skipping non-robot items
extract_robots([], []).
extract_robots([robot(RobotId, Origin)|Rest], [robot(RobotId, Origin)|Robots]) :- !,
    extract_robots(Rest, Robots).
extract_robots([_|Rest], Robots) :- !,
    extract_robots(Rest, Robots).

%%% Extracts tasks for a specific robot 
extract_tasks_for_robot(_, [], []).
extract_tasks_for_robot(RobotId, [task(RobotId, TaskId, Origin, Destination, Type)|Rest], [task(RobotId, TaskId, Origin, Destination, Type)|Tasks]) :- !,
    extract_tasks_for_robot(RobotId, Rest, Tasks).
extract_tasks_for_robot(RobotId, [_|Rest], Tasks) :- !,
    extract_tasks_for_robot(RobotId, Rest, Tasks).

generate_data(TaskData, Tasks, Robots, TaskToTaskCombinations, RobotToTaskCombinations) :-
    % write('beginning generate_data'), nl,
    extract_all_tasks(TaskData, Tasks),
    % write('extract_all_tasks done'), nl,
    extract_robots(TaskData, Robots),
    % write('extract_robots done'), nl,
    find_robot_to_task(Robots, Tasks, RobotToTaskCombinationsNoCosts),
    % write('find_robot_to_task done'), nl,
    find_all_task_to_task(TaskData, Robots, AllTaskToTaskCombinationsNoCosts),
    % write('find_all_task_to_task done'), nl,
    logic:load_info(),
    % write('load_info done'), nl,
    calculateTaskToTaskCosts(AllTaskToTaskCombinationsNoCosts, Tasks, TaskToTaskCombinations),!,
    % write('calculateTaskToTaskCosts done'), nl,
    calculateRobotToTaskCosts(RobotToTaskCombinationsNoCosts, Tasks, Robots, RobotToTaskCombinations),!.
    % write('calculateRobotToTaskCosts done'), !.

%%% Generates all robotToTask combinations
find_robot_to_task(Robots, Tasks, RobotToTaskTerms) :-
    findall(RobotToTask, (
            member(robot(RobotId, _), Robots),
            findall(Task, member(task(RobotId, Task, _, _, _), Tasks), RobotTasks),
            % write('RobotId: '), write(RobotId), write(', RobotTasks: '), write(RobotTasks), nl,
            member(Task, RobotTasks),
            RobotToTask = robotToTask(RobotId, Task, 0)
        ), RobotToTaskTerms), !.

%%% Generates all taskToTask combinations for a given Tasks list
find_task_to_task(Tasks, TaskToTaskTerms) :-
    findall(TaskToTask, (
        member(task(_, TaskId1, _, _, _), Tasks),
        member(task(_, TaskId2, _, _, _), Tasks),
        TaskId1 \= TaskId2,
        TaskToTask = taskToTask(TaskId1, TaskId2, 0)
    ), TaskToTaskTerms), !.

%%% Generates all taskToTask combinations for each robot, and combines them into a single list
find_all_task_to_task(TaskData, Robots, AllTaskToTaskCombinationsFlattened) :-
    findall(TaskToTask, (
        member(robot(RobotId, _), Robots),
        extract_tasks_for_robot(RobotId, TaskData, RobotTasks),
        find_task_to_task(RobotTasks, TaskToTask)
    ), AllTaskToTaskCombinations),!,
    flatten(AllTaskToTaskCombinations, AllTaskToTaskCombinationsFlattened).

%%% For each taskToTask combination, find the path between taskToTask, and update costs (required logic:load_info() before calling this predicate
calculateTaskToTaskCosts(AllTaskToTaskCombinations, Tasks, AllTaskToTaskCombinationsWithCostsList) :-
    findall(AllTaskToTaskCombinationsWithCosts, (
        member(taskToTask(TaskId1, TaskId2, _), AllTaskToTaskCombinations),
        member(task(_, TaskId1, _, TaskEnd, _), Tasks),
        member(task(_, TaskId2, TaskStart, _, _), Tasks),
        pathAndTotalCostBetweenOriginDestination(TaskEnd, TaskStart, TotalCost, _),
        AllTaskToTaskCombinationsWithCosts = taskToTask(TaskId1, TaskId2, TotalCost)
    ), AllTaskToTaskCombinationsWithCostsList), !.

%%% For each robotToTask combination, find the path between robotToTask, and update costs (required logic:load_info() before calling this predicate
calculateRobotToTaskCosts(RobotToTaskCombinations, Tasks, Robots, AllRobotToTaskCombinationsWithCostsList) :-
    findall(AllRobotToTaskCombinationsWithCosts, (
        member(robotToTask(RobotId, TaskId, _), RobotToTaskCombinations),
        member(task(_, TaskId, TaskStart, _, _), Tasks),
        member(robot(RobotId, RobotOrigin), Robots),
        pathAndTotalCostBetweenOriginDestination(RobotOrigin, TaskStart, TotalCost, _),
        AllRobotToTaskCombinationsWithCosts = robotToTask(RobotId, TaskId, TotalCost)
    ), AllRobotToTaskCombinationsWithCostsList), !.
    



create_json_response(ListOfTerms, JsonResponse) :-
    terms_to_json_list(ListOfTerms, JsonList),
    JsonResponse = _{data: JsonList}.

taskToTask_to_json(taskToTask(A, B, C), _{type: "taskToTask", from: A, to: B, cost: C}).

robotToTask_to_json(robotToTask(A, B, C), _{type: "robotToTask", robot: A, task: B, cost: C}).

terms_to_json_list([], []).
terms_to_json_list([H|T], [JH|JT]) :-
    (   H = taskToTask(_, _, _)
    ->  taskToTask_to_json(H, JH)
    ;   robotToTask_to_json(H, JH)
    ),
    terms_to_json_list(T, JT).

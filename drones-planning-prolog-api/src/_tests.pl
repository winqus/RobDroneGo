% :- consult('server.pl').
% :- consult('taskPlanningRouteHandler.pl').

:- use_module(library(http/json)).

:- use_module(pathPlanningRouteHandler).
:- use_module(logic).
:- use_module(navigationBuildingsFloors).
:- use_module(graph_creation_for_maze_diagonal).
:- use_module(astar_maze_diagonal_algorithm).
/*
taskToTask_to_json(taskToTask(A, B, C), _{type: "taskToTask", from: A, to: B, cost: C}).

robotToTask_to_json(robotToTask(A, B, C), _{type: "robotToTask", robot: A, task: B, cost: C}).

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


:- ListOfTerms = [
  taskToTask('0001-0001', '0001-0002', 1),
  robotToTask('0000-0001', '0001-0001', 1)
],

% Convert the list of terms to JSON
create_json_response(ListOfTerms, JsonResponse),

% Print the JSON response
json_write(current_output, JsonResponse).
*/

extract_all_tasks([], []).
extract_all_tasks([task(RobotId, TaskId, Origin, Destination, Type)|Rest], [task(RobotId, TaskId, Origin, Destination, Type)|Tasks]) :-
    extract_all_tasks(Rest, Tasks).
extract_all_tasks([_|Rest], Tasks) :-
    extract_all_tasks(Rest, Tasks).

% Extracts all robots from the task data, skipping non-robot items
extract_robots([], []).
extract_robots([robot(RobotId, Origin)|Rest], [robot(RobotId, Origin)|Robots]) :- !,
    extract_robots(Rest, Robots).
extract_robots([_|Rest], Robots) :- !, % Skip non-robot items
    extract_robots(Rest, Robots).

% Extracts tasks for a specific robot
extract_tasks_for_robot(_, [], []).
extract_tasks_for_robot(RobotId, [task(RobotId, TaskId, Origin, Destination, Type)|Rest], [task(RobotId, TaskId, Origin, Destination, Type)|Tasks]) :- !,
    extract_tasks_for_robot(RobotId, Rest, Tasks).
extract_tasks_for_robot(RobotId, [_|Rest], Tasks) :- !,
    extract_tasks_for_robot(RobotId, Rest, Tasks).

% Generate taskToTask combinations
generate_taskToTask(Tasks, Combinations) :-
    findall(taskToTask(TaskId1, TaskId2, 0), (
        member(task(_, TaskId1, _, _, _), Tasks),
        member(task(_, TaskId2, _, _, _), Tasks),
        TaskId1 \= TaskId2
    ), Combinations).

% Generate robotToTask combinations
generate_robotToTask(RobotId, Tasks, RobotTasks) :-
    findall(robotToTask(RobotId, TaskId, 0), member(task(_, TaskId, _, _, _), Tasks), RobotTasks).

% Main predicate to generate required data
generate_data(TaskData, TaskToTaskCombinations, RobotToTaskCombinations) :-
    findall(Robot, member(robot(Robot, _), TaskData), Robots),
    findall(TaskPair, (
        member(robot(RobotId, _), Robots),
        extract_tasks_for_robot(RobotId, TaskData, RobotTasks),
        generate_taskToTask(RobotTasks, TaskPairs),
        member(TaskPair, TaskPairs)
    ), TaskToTaskCombinations),
    findall(RobotTask, (
        member(robot(RobotId, _), Robots),
        extract_tasks_for_robot(RobotId, TaskData, RobotTasks),
        generate_robotToTask(RobotId, RobotTasks, RobotTasksPairs),
        member(RobotTask, RobotTasksPairs)
    ), RobotToTaskCombinations).

find_robot_to_task(Robots, Tasks, RobotToTaskTerms) :-
    findall(RobotToTask, (
            member(robot(RobotId, _), Robots),
            findall(Task, member(task(RobotId, Task, _, _, _), Tasks), RobotTasks),
            % write('RobotId: '), write(RobotId), write(', RobotTasks: '), write(RobotTasks), nl,
            member(Task, RobotTasks),
            RobotToTask = robotToTask(RobotId, Task, 0)
        ), RobotToTaskTerms).

find_task_to_task(Tasks, TaskToTaskTerms) :-
    findall(TaskToTask, (
        member(task(_, TaskId1, _, _, _), Tasks),
        member(task(_, TaskId2, _, _, _), Tasks),
        TaskId1 \= TaskId2,
        TaskToTask = taskToTask(TaskId1, TaskId2, 0)
    ), TaskToTaskTerms).

find_all_task_to_task(TaskData, Robots, AllTaskToTaskCombinationsFlattened) :-
    findall(TaskToTask, (
        member(robot(RobotId, _), Robots),
        extract_tasks_for_robot(RobotId, TaskData, RobotTasks),
        find_task_to_task(RobotTasks, TaskToTask)
    ), AllTaskToTaskCombinations),!,
    flatten(AllTaskToTaskCombinations, AllTaskToTaskCombinationsFlattened).

% % Predicate to sum costs in the MapPaths list
% sum_costs(MapPaths, TotalCost) :-
%     sum_costs(MapPaths, 0, TotalCost).
% sum_costs([], Accumulator, Accumulator).
% sum_costs([json{buildingCode:_, cost:Cost, floorNumber:_, path:_}|Rest], Accumulator, TotalCost) :-
%     NewAccumulator is Accumulator + Cost,
%     sum_costs(Rest, NewAccumulator, TotalCost).

initialize :-
    TaskData = [
        robot('robo-0001',origin('B',2,cel(1,9))),
        robot('robo-0002',origin('B',1,cel(2,3))),
        task('robo-0001','task1-r1',origin('B',2,cel(2,9)),destination('B',2,cel(6,8)),'PickUpAndDelivery'),
        task('robo-0001','task2-r1',origin('A',2,cel(2,3)),destination('B',2,cel(3,9)),'PickUpAndDelivery'),
        task('robo-0001','task3-r1',origin('C',2,cel(2,3)),destination('A',2,cel(2,5)),'PickUpAndDelivery'),
        task('robo-0002','task3-r2',origin('C',2,cel(2,5)),destination('D',3,cel(4,3)),'PickUpAndDelivery'),
        task('robo-0002','task4-r2',origin('C',2,cel(2,3)),destination('D',3,cel(4,5)),'PickUpAndDelivery')
    ],
    % findall(Robot, member(robot(Robot, _), TaskData), Robots),
    extract_robots(TaskData, Robots),
    extract_all_tasks(TaskData, Tasks),

    %% finds all robot to task combinations
    find_robot_to_task(Robots, Tasks, RobotToTaskCombinations),
    length(RobotToTaskCombinations, RobotToTaskCombinationsCount),
    write('RobotToTaskCombinationsCount: '), write(RobotToTaskCombinationsCount), nl,
    write('RobotToTaskCombinations: '), write(RobotToTaskCombinations), nl, nl,

    %% finds all task to task combinations for a robot
    extract_tasks_for_robot('robo-0001', TaskData, RobotTasks),
    find_task_to_task(RobotTasks, TaskToTaskCombinations),
    length(TaskToTaskCombinations, TaskToTaskCombinationsCount),
    write('TaskToTaskCombinationsCount: '), write(TaskToTaskCombinationsCount), nl,
    write('TaskToTaskCombinations: '), write(TaskToTaskCombinations), nl, nl,

    %%% finds all task to task combinations of each robot and combines them
    % findall(TaskToTask, (
    %     member(robot(RobotId, _), Robots),
    %     extract_tasks_for_robot(RobotId, TaskData, RobotTasks),
    %     find_task_to_task(RobotTasks, TaskToTask)
    % ), AllTaskToTaskCombinations),
    find_all_task_to_task(TaskData, Robots, AllTaskToTaskCombinations),
    length(AllTaskToTaskCombinations, AllTaskToTaskCombinationsCount),
    write('AllTaskToTaskCombinationsCount: '), write(AllTaskToTaskCombinationsCount), nl,
    write('AllTaskToTaskCombinations: '), write(AllTaskToTaskCombinations), nl, nl,

    % iterate over all task to task combinations, find the path between taskToTask, and update costs
    logic:load_info(),
    findall(AllTaskToTaskCombinationsWithCosts, (
        member(taskToTask(TaskId1, TaskId2, _), AllTaskToTaskCombinations),
        % write('TaskId1: '), write(TaskId1), write(', TaskId2: '), write(TaskId2), nl,
        member(task(_, TaskId1, _, TaskEnd, _), Tasks),
        member(task(_, TaskId2, TaskStart, _, _), Tasks),
        % write('TaskStart: '), write(TaskStart), write(', TaskEnd: '), write(TaskEnd), nl,
        pathAndTotalCostBetweenOriginDestination(TaskEnd, TaskStart, TotalCost, _),
        % write('TotalCost: '), write(TotalCost), nl,
        AllTaskToTaskCombinationsWithCosts = taskToTask(TaskId1, TaskId2, TotalCost)
    ), AllTaskToTaskCombinationsWithCostsList),!,
    length(AllTaskToTaskCombinationsWithCostsList, AllTaskToTaskCombinationsWithCostsListCount),
    write('AllTaskToTaskCombinationsWithCostsListCount: '), write(AllTaskToTaskCombinationsWithCostsListCount), nl,
    write('AllTaskToTaskCombinationsWithCostsList: '), write(AllTaskToTaskCombinationsWithCostsList), nl, nl,

    % iterate over all robot to task combinations, find the path between robotToTask, and update costs
    findall(AllRobotToTaskCombinationsWithCosts, (
        member(robotToTask(RobotId, TaskId, _), RobotToTaskCombinations),
        % write('RobotId: '), write(RobotId), write(', TaskId: '), write(TaskId), nl,
        member(task(_, TaskId, TaskStart, _, _), Tasks),
        member(robot(RobotId, RobotOrigin), Robots),
        % write('RobotOrigin: '), write(RobotOrigin), write(', TaskStart: '), write(TaskStart), nl,
        pathAndTotalCostBetweenOriginDestination(RobotOrigin, TaskStart, TotalCost, _),
        % write('TotalCost: '), write(TotalCost), nl,
        AllRobotToTaskCombinationsWithCosts = robotToTask(RobotId, TaskId, TotalCost)
    ), AllRobotToTaskCombinationsWithCostsList),!,
    length(AllRobotToTaskCombinationsWithCostsList, AllRobotToTaskCombinationsWithCostsListCount),
    write('AllRobotToTaskCombinationsWithCostsListCount: '), write(AllRobotToTaskCombinationsWithCostsListCount), nl,
    write('AllRobotToTaskCombinationsWithCostsList: '), write(AllRobotToTaskCombinationsWithCostsList), nl, nl.


% findall(AllTaskToTaskCombinationsWithCosts, (
%     member(taskToTask(TaskId1, TaskId2, _), AllTaskToTaskCombinations),
%     write('TaskId1: '), write(TaskId1), write(', TaskId2: '), write(TaskId2), nl,

:- initialize.
% :- Tasks = [task('robo-0001','task1-r1',origin('B',2,cel(1,1)),destination('B',2,cel(4,3)),'PickUpAndDelivery')],
% logic:load_info(),
% member(task(_, _, TaskOrigin, TaskEnd, _), Tasks),!,
% pathAndTotalCostBetweenOriginDestination(TaskEnd, TaskOrigin, TotalCost, _),!,
% write('TotalCost: '), write(TotalCost), nl.

% usage example:
% pathAndTotalCostBetweenOriginDestination(origin('A', 1, cel(1, 1)), destination('A', 2, cel(5, 5)), TotalCost, _).
% pathAndTotalCostBetweenOriginDestination(origin('A', 1, cel(1, 1)), destination('A', 1, cel(5, 5)), TotalCost, _).
% pathAndTotalCostBetweenOriginDestination(origin('A', 2, cel(2, 3)), destination('B', 2, cel(6, 8)), TotalCost, _).
% A = 'A', B = 'B', pathAndTotalCostBetweenOriginDestination(origin(A, 2, cel(2, 3)), destination(B, 2, cel(6, 8)), TotalCost, _).
% pathAndTotalCostBetweenOriginDestination(origin('B', 2, cel(1, 1)), destination('B', 2, cel(4, 3)), TotalCost, _).
% pathAndTotalCostBetweenOriginDestination(origin('B', 2, cel(4, 3)), destination('B', 2, cel(1, 1)), TotalCost, _).
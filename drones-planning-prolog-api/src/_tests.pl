% :- consult('server.pl').
% :- consult('taskPlanningRouteHandler.pl').

:- use_module(library(http/json)).

:- use_module(pathPlanningRouteHandler).
:- use_module(logic).
:- use_module(taskDataPreparation).
:- use_module(navigationBuildingsFloors).
:- use_module(graph_creation_for_maze_diagonal).
:- use_module(astar_maze_diagonal_algorithm).

initialize :-
    TaskData = [
        robot('robo-0001',origin('B',2,cel(1,9))),
        % robot('robo-0001',origin('B',2,cel(1,2))),
        robot('robo-0002',origin('B',1,cel(2,3))),
        task('robo-0001','task1-r1',origin('B',2,cel(2,9)),destination('B',2,cel(6,8)),'PickUpAndDelivery'),
        task('robo-0001','task2-r1',origin('A',2,cel(2,3)),destination('B',2,cel(3,9)),'PickUpAndDelivery'),
        task('robo-0001','task3-r1',origin('C',2,cel(2,3)),destination('A',2,cel(2,5)),'PickUpAndDelivery'),
        task('robo-0002','task3-r2',origin('C',2,cel(2,5)),destination('D',3,cel(4,3)),'PickUpAndDelivery'),
        task('robo-0002','task4-r2',origin('C',2,cel(2,3)),destination('D',3,cel(4,5)),'PickUpAndDelivery')
    ],
    %%%%%%%%% findall(Robot, member(robot(Robot, _), TaskData), Robots),
    generate_data(TaskData, Tasks, Robots, TaskToTaskCombinations, RobotToTaskCombinations),
    length(Tasks, TasksCount), write('Tasks count: '), write(TasksCount), nl,
    length(Robots, RobotsCount), write('Robots count: '), write(RobotsCount), nl,
    
    length(TaskToTaskCombinations, TaskToTaskCombinationsCount),
    write('TaskToTaskCombinations count: '), write(TaskToTaskCombinationsCount), write(', TaskToTaskCombinations: '), write(TaskToTaskCombinations), nl, nl,
    length(RobotToTaskCombinations, RobotToTaskCombinationsCount),
    write('RobotToTaskCombinations count: '), write(RobotToTaskCombinationsCount), write(', RobotToTaskCombinations: '), write(RobotToTaskCombinations), nl, nl,
    nl.


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

% pathAndTotalCostBetweenOriginDestination(origin('B', 2, cel(1, 9)), destination('B', 2, cel(2, 9)), TotalCost, _).
% pathAndTotalCostBetweenOriginDestination(origin('B', 2, cel(2, 3)), destination('B', 2, cel(2, 9)), TotalCost, _).
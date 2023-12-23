% :- consult('server.pl').
% :- consult('taskPlanningRouteHandler.pl').

:- use_module(library(http/json)).

:- use_module(pathPlanningRouteHandler).
:- use_module(logic).
:- use_module(taskDataPreparation).
:- use_module(geneticRunner).

initialize :-
    % /* MODIFY TaskData FOR TESTING FOR COMPLEXITY ANALYSIS (use only one robot) */ %
    % TIME MEASUREMENT FOR COMPLEXITY ANALYSIS SHOULD BE DONE INSIDE geneticRunner:runGeneticForRobots (CHECK COMMENTS THERE) %
    % USE MORE THAN 2 TASKS %
    % USE ONLY ONE ROBOT %
    TaskData = [
        robot('robo-0001',origin('B',2,cel(1,9))),
        task('robo-0001','task1-r1',origin('B',2,cel(2,9)),destination('B',2,cel(6,8)),'PickUpAndDelivery'),
        task('robo-0001','task2-r1',origin('A',2,cel(2,3)),destination('B',2,cel(3,9)),'PickUpAndDelivery'),
        task('robo-0001','task3-r1',origin('C',2,cel(2,3)),destination('A',2,cel(2,5)),'PickUpAndDelivery')
    ],
    
    taskDataPreparation:extract_robots(TaskData, Robots),
    logic:load_info(),
    geneticRunner:runGeneticForRobots(Robots, TaskData, Results),
    nl, write('Results: '), write(Results), nl,
    !.

:- initialize.



% usage example of pathAndTotalCostBetweenOriginDestination:
% pathAndTotalCostBetweenOriginDestination(origin('A', 1, cel(1, 1)), destination('A', 2, cel(5, 5)), TotalCost, _).
% pathAndTotalCostBetweenOriginDestination(origin('A', 1, cel(1, 1)), destination('A', 1, cel(5, 5)), TotalCost, _).
% pathAndTotalCostBetweenOriginDestination(origin('A', 2, cel(2, 3)), destination('B', 2, cel(6, 8)), TotalCost, _).
% pathAndTotalCostBetweenOriginDestination(origin('B', 2, cel(1, 1)), destination('B', 2, cel(4, 3)), TotalCost, _).
% pathAndTotalCostBetweenOriginDestination(origin('B', 2, cel(4, 3)), destination('B', 2, cel(1, 1)), TotalCost, _).

% pathAndTotalCostBetweenOriginDestination(origin('B', 2, cel(1, 9)), destination('B', 2, cel(2, 9)), TotalCost, _).
% pathAndTotalCostBetweenOriginDestination(origin('B', 2, cel(2, 3)), destination('B', 2, cel(2, 9)), TotalCost, _).

/*
    EXAMPLE DATA:
    TaskData = [
            robot('robo-0001',origin('B',2,cel(1,9))),
            robot('robo-0002',origin('B',1,cel(2,3))),
            task('robo-0001','task1-r1',origin('B',2,cel(2,9)),destination('B',2,cel(6,8)),'PickUpAndDelivery'),
            task('robo-0001','task2-r1',origin('A',2,cel(2,3)),destination('B',2,cel(3,9)),'PickUpAndDelivery'),
            task('robo-0001','task3-r1',origin('C',2,cel(2,3)),destination('A',2,cel(2,5)),'PickUpAndDelivery'),
            task('robo-0002','task4-r2',origin('C',2,cel(2,5)),destination('D',3,cel(4,3)),'PickUpAndDelivery'),
            task('robo-0002','task5-r2',origin('C',2,cel(2,3)),destination('D',3,cel(4,5)),'PickUpAndDelivery'),
            task('robo-0002','task6-r2',origin('C',1,cel(2,3)),destination('D',2,cel(4,5)),'PickUpAndDelivery')
        ],
*/
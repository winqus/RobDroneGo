% :- consult('server.pl').
% :- consult('taskPlanningRouteHandler.pl').
:- consult('geneticTestData.pl').

:- use_module(library(http/json)).

:- use_module(pathPlanningRouteHandler).
:- use_module(logic).
:- use_module(taskDataPreparation).
:- use_module(geneticRunner).
:- use_module(genetic).
:- use_module(permutationRunner).

fake_find_task_to_task(Tasks, TaskToTaskTerms) :-
    findall(TaskToTask, (
        member(task(_, TaskId1, _, _, _), Tasks),
        member(task(_, TaskId2, _, _, _), Tasks),
        TaskId1 \= TaskId2,
        random_between(1, 1000, RandomValue),
        RandomValueDivided is RandomValue / 10,
        TaskToTask = taskToTask(TaskId1, TaskId2, RandomValueDivided)
    ), TaskToTaskTerms), !.

fake_find_robot_to_task(Robots, Tasks, RobotToTaskTerms) :-
    findall(RobotToTask, (
            member(robot(RobotId, _), Robots),
            findall(Task, member(task(RobotId, Task, _, _, _), Tasks), RobotTasks),
            member(Task, RobotTasks),
            random_between(1, 1000, RandomValue),
            RandomValueDivided is RandomValue / 10,
            RobotToTask = robotToTask(RobotId, Task, RandomValueDivided)
        ), RobotToTaskTerms), !.


test_genetic :-
    %  MODIFY TaskData (in geneticTestData.pl) FOR TESTING FOR COMPLEXITY ANALYSIS (use only one robot)  %
    % USE MORE THAN 2 TASKS %
    % USE ONLY ONE ROBOT %

    testTaskData(TaskData),
    taskDataPreparation:extract_robots(TaskData, Robots),
    logic:load_info(),
    Robots = [Robot|_],
    Robot = robot(RobotId, _),
    taskDataPreparation:extract_tasks_for_robot(RobotId, TaskData, Tasks),
    length(Tasks, TaskCount),
    write('> Task count: '), write(TaskCount), nl,
    fake_find_task_to_task(Tasks, TaskToTaskTerms),
    length(TaskToTaskTerms, TaskToTaskCount),
    write('> Task to task combinations: '), write(TaskToTaskCount), nl,
    fake_find_robot_to_task(Robots, Tasks, RobotToTaskTerms),!,

    /* TIME MEASUREMENT FOR GA COMPLEXITY ANALYSIS SHOULD START HERE */
    TaskToTaskCombinations = TaskToTaskTerms,
    RobotToTaskCombinations = RobotToTaskTerms,
    get_time(StartTime),
    genetic:runGeneticAlgorithm(Tasks, TaskToTaskCombinations, RobotToTaskCombinations, BestIndividual),
    get_time(EndTime),
    ExecutionTime is EndTime - StartTime,
    ExecutionTimeRounded is round(ExecutionTime * 10000) / 10000,
    % write('> Best Individual: '), write(BestIndividual), nl,
    with_output_to(string(BestIndividualString), write(BestIndividual)),
    split_string(BestIndividualString, "*", " ", [_, CostString]),
    write('> Best Individual: '), write('[task....]*'), write(CostString), nl,
    format('\e[1;32m> Execution Time: ~w seconds\e[0m', [ExecutionTimeRounded]), nl,
    /* TIME MEASUREMENT FOR GA COMPLEXITY ANALYSIS SHOULD END HERE */
    !.

test_perm :- 
    % max 8 tasks, after that it happens scack limit exceeded
    testTaskData(TaskData),
    taskDataPreparation:extract_robots(TaskData, Robots),
    logic:load_info(),
    Robots = [Robot|_],
    Robot = robot(RobotId, _),
    taskDataPreparation:extract_tasks_for_robot(RobotId, TaskData, Tasks),
    length(Tasks, TaskCount),
    write('> Task count: '), write(TaskCount), nl,
    fake_find_task_to_task(Tasks, TaskToTaskTerms),
    length(TaskToTaskTerms, TaskToTaskCount),
    write('> Task to task combinations: '), write(TaskToTaskCount), nl,
    fake_find_robot_to_task(Robots, Tasks, RobotToTaskTerms),!,

    /* TIME MEASUREMENT FOR GA COMPLEXITY ANALYSIS SHOULD START HERE */
    TaskToTaskCombinations = TaskToTaskTerms,
    RobotToTaskCombinations = RobotToTaskTerms,
    get_time(StartTime),
    permutationRunner:runPermutation(Tasks, TaskToTaskCombinations, RobotToTaskCombinations, BestIndividual),
    write(BestIndividual),
    get_time(EndTime),
    ExecutionTime is EndTime - StartTime,
    ExecutionTimeRounded is round(ExecutionTime * 10000) / 10000,
    % write('> Best Individual: '), write(BestIndividual), nl,
    with_output_to(string(BestIndividualString), write(BestIndividual)),
    split_string(BestIndividualString, "*", " ", [_, CostString]),
    write('> Best Individual: '), write('[task....]*'), write(CostString), nl,
    format('\e[1;32m> Execution Time: ~w seconds\e[0m', [ExecutionTimeRounded]), nl,
    /* TIME MEASUREMENT FOR GA COMPLEXITY ANALYSIS SHOULD END HERE */
    !.


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
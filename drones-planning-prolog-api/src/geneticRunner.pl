:- module(geneticRunner, [runGeneticForRobots/3]).


:- use_module(pathPlanningRouteHandler).
:- use_module(logic).
:- use_module(taskDataPreparation).
:- use_module(genetic).

/*
  Main predicate is runGeneticForRobots/3.
  It takes a list of robots, a list of tasks and returns a list of task plans.
  Each task plan is a term taskPlan(RobotId, TaskIds, CostOfChange).
*/

select_better_task_to_task(TaskToTaskCombinations, BetterTaskToTask) :-
    TaskToTaskCombinations = [taskToTask(Task1a, Task1b, Cost1), taskToTask(Task2a, Task2b, Cost2)],
    (   Cost1 < Cost2
    ->  BetterTaskToTask = taskToTask(Task1a, Task1b, Cost1)
    ;   BetterTaskToTask = taskToTask(Task2a, Task2b, Cost2)
    ).

runGeneticForRobots([], _, []) :- !.
runGeneticForRobots([Robot|Rest], TaskData, Results) :-
    runGeneticForARobot(Robot, TaskData, Result),
    runGeneticForRobots(Rest, TaskData, ResultsRest),
    Results = [Result|ResultsRest],
    !.

runGeneticForARobot(Robot, TaskData, Result) :-
    Robot = robot(RobotId, _),
    log_message('runGeneticForARobot:'), log_message_ln(RobotId),
    taskDataPreparation:extract_tasks_for_robot(RobotId, TaskData, Tasks),
    log_message('Robot tasks:'), log_message_ln(Tasks),

    taskDataPreparation:find_task_to_task(Tasks, TaskToTaskTerms),
    log_message('found task to task:'), log_message_ln(TaskToTaskTerms),
    taskDataPreparation:find_robot_to_task([Robot], Tasks, RobotToTaskTerms),
    log_message('found robot to task:'), log_message_ln(RobotToTaskTerms),

    calculateTaskToTaskCosts(TaskToTaskTerms, Tasks, TaskToTaskCombinations),
    log_message('calculated task to task costs:'), log_message_ln(TaskToTaskCombinations),
    calculateRobotToTaskCosts(RobotToTaskTerms, Tasks, [Robot], RobotToTaskCombinations),
    log_message('calculated robot to task costs:'), log_message_ln(RobotToTaskCombinations),

    length(Tasks, TasksCount),
    (TasksCount > 2,
        /* TIME MEASUREMENT FOR GA COMPLEXITY ANALYSIS SHOULD START HERE */
        genetic:runGeneticAlgorithm(Tasks, TaskToTaskCombinations, RobotToTaskCombinations, BestIndividual),
        /* TIME MEASUREMENT FOR GA COMPLEXITY ANALYSIS SHOULD END HERE */
        with_output_to(string(BestIndividualString), write(BestIndividual))
    ;
    TasksCount = 2, % FOR SOME REASON, GA DOES NOT WORK FOR 2 TASKS
        select_better_task_to_task(TaskToTaskCombinations, BetterTaskToTask),
        BetterTaskToTask = taskToTask(Task1Id, Task2Id, Cost),
        BestIndividual = [Task1Id, Task2Id]*Cost,
        with_output_to(string(BestIndividualString), write(BestIndividual))
    ;
    TasksCount = 1, % ONLY ONE TASK, NO PLANNING REQUIRED
        Tasks = [Task],
        Task = task(_, TaskId, _, _, _),
        RobotToTaskCombinations = [robotToTask(RobotId, TaskId, CostOfChange)],
        BestIndividual = [TaskId]*CostOfChange,
        
        with_output_to(string(BestIndividualString), write(BestIndividual))
    ; % 0 TASKS OR LESS PROVIDED
        CostOfChange = -111111,
        BestIndividual = []*CostOfChange,
        with_output_to(string(BestIndividualString), write(BestIndividual))
    ),

    log_message('BestIndividualString:'), log_message_ln(BestIndividualString),
    split_string(BestIndividualString, "*", " ", [IndividualString, CostString]),
    % write('IndividualString: '), write(IndividualString), nl,
    log_message('IndividualString:'), log_message_ln(IndividualString),
    % write('CostOfChangeString: '), write(CostString), nl,
    log_message('CostOfChangeString:'), log_message_ln(CostString),
    Result = taskPlan(RobotId, IndividualString, CostString),
    !.
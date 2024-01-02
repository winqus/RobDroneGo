:- module(permutationRunner, [runPermutationForRobots/3]).

:- use_module(pathPlanningRouteHandler).
:- use_module(logic).
:- use_module(taskDataPreparation).
:- use_module(genetic).

:- dynamic tasks/5.
:- dynamic population/1.
:- dynamic best_ind/1.
:- dynamic lastTaskId/1.
:- dynamic selectRobot/1. % to select the robot to be used in the simulation

runPermutationForRobots([], _, []) :- !.
runPermutationForRobots([Robot|Rest], TaskData, Results) :-
    runPermutationForARobot(Robot, TaskData, Result),
    runPermutationForRobots(Rest, TaskData, ResultsRest),
    Results = [Result|ResultsRest],
    !.

runPermutationForARobot(Robot, TaskData, Result) :-
    Robot = robot(RobotId, _),
    log_message('runPermutationForARobot:'), log_message_ln(RobotId),
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
        runPermutation(Tasks, TaskToTaskCombinations, RobotToTaskCombinations, BestIndividual),
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

load_data(TasksTerms, TaskToTaskTerms, RobotToTaskTerms):-
  retractall(task(_, _, _, _, _)),
  retractall(taskToTask(_, _, _)),
  retractall(robotToTask(_, _, _)),
  retractall(tasks(_)),
  % retractall(robot(_, _)),
  maplist(assert, TasksTerms),
  % maplist(assert, RobotsTerms),
  maplist(assert, TaskToTaskTerms),
  maplist(assert, RobotToTaskTerms),
  length(TasksTerms, NTasks),
  asserta(tasks(NTasks)).

evaluate_population([], []).
evaluate_population([Ind|Rest], [Ind*V|Rest1]):-
  retractall(lastTaskId(_)),
  evaluate(Ind, V),
  evaluate_population(Rest, Rest1).

  
evaluate([], 0).
evaluate([TaskID|Rest], V):-
  task(Robot, TaskID, _, _, _),
  (( clause(lastTaskId(_), _) , 
  lastTaskId(LastTaskID), taskToTask(LastTaskID, TaskID, Cost), 
  retract(lastTaskId(LastTaskID)));
  (robotToTask(Robot, TaskID, Cost))
  ),
  asserta(lastTaskId(TaskID)),
  evaluate(Rest, V1),
  V is V1+Cost.

sort_population(PopEval, PopEvalSorted):-
  bubble_sort(PopEval, PopEvalSorted).

bubble_sort([X], [X]) :- !.
bubble_sort([X|Xs], Ys) :-
    bubble_sort(Xs, Zs),
    bubble_swap([X|Zs], Ys).

bubble_swap([X], [X]) :- !.

bubble_swap([X*VX, Y*VY|L1], [Y*VY|L2]) :-
    VX > VY, !,
    bubble_swap([X*VX|L1], L2).

bubble_swap([X|L1], [X|L2]) :- bubble_swap(L1, L2).

runPermutation(Tasks, TaskToTaskCombinations, RobotToTaskCombinations, BestIndividual) :-
    load_data(Tasks, TaskToTaskCombinations, RobotToTaskCombinations),
    findall(TaskId, task(_, TaskId, _, _, _), TaskIds),
    findall(LTask, permutation(TaskIds, LTask), Combinations),
    evaluate_population(Combinations, EvaluatedCombinations),
    sort_population(EvaluatedCombinations, SortedCombinations),
    SortedCombinations = [BestIndividual | _].
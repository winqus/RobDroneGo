:- use_module(library(lists)).

% Dynamic declarations
:- dynamic generations/1.
:- dynamic population/1.
:- dynamic crossover_probability/1.
:- dynamic mutation_probability/1.

%% task(Id, ProcessingTime, CompletionTime, PenaltyWeight). AKA Individual
task(t1, 2, 5, 1).
task(t2, 4, 7, 6).
task(t3, 1, 11, 2).
task(t4, 3, 9, 3).
task(t5, 3, 8, 2).

%% tasks(NTasks).
tasks(5).

%%% Usage example %%%:
/*-----------------------------------------
  ?- generate.
  Number of new Generations: 10.
  Population Size: |: 20.
  Crossover Probability (%):|: 70.
  Mutation Probability (%):|: 30. 

  OR PASTE TO CONSOLE AS: generate. 10. 20. 70. 30.
-----------------------------------------*/
/*-----Smaller output example--------------
  ?- generate.
  Number of new Generations: 5.
  Population Size: |: 5.
  Crossover Probability (%):|: 70.
  Mutation Probability (%):|: 30. 

  OR PASTE TO CONSOLE AS: generate. 5. 5. 70. 30.
-----------------------------------------*/


% Parameterization
initialize:-
  write('Number of new Generations: '), read(NG), 
  (retract(generations(_)); true), asserta(generations(NG)),
  write('Population Size: '), read(PS),
  (retract(population(_)); true), asserta(population(PS)),
  write('Crossover Probability (%):'), read(P1),
  CP is P1/100, 
  (retract(crossover_probability(_)); true), asserta(crossover_probability(CP)),
  write('Mutation Probability (%):'), read(P2),
  MP is P2/100, 
  (retract(mutation_probability(_)); true), asserta(mutation_probability(MP)).

generate:-
  initialize,
  generate_population(Pop),
  write('Pop='), write(Pop), nl,
  evaluate_population(Pop, PopEval),
  write('PopEval='), write(PopEval), nl,
  sort_population(PopEval, PopSorted),
  generations(NG),
  generate_generation(0, NG, PopSorted).

generate_population(Pop):-
  population(PopSize),
  tasks(NumT),
  findall(Task, task(Task, _, _, _), TaskList),
  generate_population(PopSize, TaskList, NumT, Pop).

generate_population(0, _, _, []) :- !.

generate_population(PopSize, TaskList, NumT, [Ind|Rest]):-
  PopSize1 is PopSize-1,
  generate_population(PopSize1, TaskList, NumT, Rest),
  generate_individual(TaskList, NumT, Ind),
  not(member(Ind, Rest)).
generate_population(PopSize, TaskList, NumT, L):-
    generate_population(PopSize, TaskList, NumT, L).

generate_individual([G], 1, [G]) :- !.

generate_individual(TaskList, NumT, [G|Rest]):-
  NumTemp is NumT + 1, % To use with random
  random(1, NumTemp, N),
  remove(N, TaskList, G, NewList),
  NumT1 is NumT-1,
  generate_individual(NewList, NumT1, Rest).

evaluate_population([], []).
evaluate_population([Ind|Rest], [Ind*V|Rest1]):-
  evaluate(Ind, V),
  evaluate_population(Rest, Rest1).

evaluate(Seq, V):-
  evaluate(Seq, 0, V).

evaluate([], _, 0).
evaluate([T|Rest], Inst, V):-
  task(T, Dur, Deadline, Pen),
  EndInst is Inst+Dur,
  evaluate(Rest, EndInst, VRest),
  (
      (EndInst =< Deadline, !, VT is 0)
  ;
      (VT is (EndInst-Deadline)*Pen)
  ),
  V is VT+VRest.

sort_population(PopEval, PopEvalSorted):-
  bubble_sort(PopEval, PopEvalSorted).

generate_generation(G, G, Pop) :- !,
    write('Generation '), write(G), write(':'), nl, write(Pop), nl.

%%% Modified to include partial elitism %%%
generate_generation(N, G, Pop) :-
    write('Generation '), write(N), write(':'), nl, write(Pop), nl,
    elitism(Pop, Elites, 20), % Extract pertencage of elites (best evaluated individuals) from the current population
    % write('Elites: '), write(Elites), nl,
    population(PopSize),
    crossover(Pop, NPop1, PopSize),
    mutation(NPop1, NPop),
    evaluate_population(NPop, NPopEval),
    append(Elites, NPopEval, CombinedPop), % Combine elites with new population
    % sort_population(NPopEval, NPopSorted),
    sort_population(CombinedPop, CombinedPopSorted),
    select_new_population(CombinedPopSorted, NPopSorted), % Select new population (ensures population size is maintained)
    N1 is N + 1,
    generate_generation(N1, G, NPopSorted).

generate_crossover_points(P1, P2) :-
    generate_crossover_points1(P1, P2).

generate_crossover_points1(P1, P2) :-
    tasks(N),
    NTemp is N + 1,
    random(1, NTemp, P11),
    random(1, NTemp, P21),
    P11 \== P21, !,
    ((P11 < P21, !, P1 = P11, P2 = P21); (P1 = P21, P2 = P11)).
generate_crossover_points1(P1, P2) :-
    generate_crossover_points1(P1, P2).

% crossover([], []).
% crossover([Ind* _], [Ind]).
% crossover([Ind1* _, Ind2* _|Rest], [NInd1, NInd2|Rest1]) :-
%     generate_crossover_points(P1, P2),
%     crossover_probability(Pcross), random(0.0, 1.0, Pc),
%     ((Pc =< Pcross, !,
%         cross(Ind1, Ind2, P1, P2, NInd1),
%         cross(Ind2, Ind1, P1, P2, NInd2))
%     ;
%     (NInd1 = Ind1, NInd2 = Ind2)),
%     crossover(Rest, Rest1).

% crossover(Pop, NewPop, NumberIndv).
crossover(_, [], 0).
crossover(_, [], 1).
crossover(Pop, [NInd1, NInd2|Rest1], NumberIndv) :-
    generate_crossover_points(P1, P2),
    crossover_probability(Pcross), random(0.0, 1.0, Pc),
    random_members(Pop, Ind1, Ind2),
    ((Pc =< Pcross, !,
        cross(Ind1, Ind2, P1, P2, NInd1),
        cross(Ind2, Ind1, P1, P2, NInd2))
    ;
    (NInd1 = Ind1, NInd2 = Ind2)),
    NumberIndv1 is NumberIndv - 2,
    crossover(Pop, Rest1, NumberIndv1).

random_members(Pop, Ind1, Ind2) :-
    population(PopSize),
    random(1, PopSize, IndexInd1),
    random(1, PopSize, IndexInd2),
    ((IndexInd1 \= IndexInd2, !, 
    nth1(IndexInd1, Pop, Ind1* _),
    nth1(IndexInd2, Pop, Ind2* _));
    random_members(Pop, Ind1, Ind2)).

cross(Ind1, Ind2, P1, P2, NInd11) :-
  sublist(Ind1, P1, P2, Sub1),
  tasks(NumT),
  R is NumT - P2,
  rotate_right(Ind2, R, Ind21),
  eliminate(Ind21, Sub1, Sub2),
  P3 is P2 + 1,
  insert(Sub2, Sub1, P3, NInd1),
  eliminate_h(NInd1, NInd11).

mutation([], []).
mutation([Ind|Rest], [NInd|Rest1]) :-
    mutation_probability(Pmut),
    random(0.0, 1.0, Pm),
    ((Pm < Pmut, !, mutation1(Ind, NInd)); NInd = Ind),
    mutation(Rest, Rest1).

mutation1(Ind, NInd) :-
    generate_crossover_points(P1, P2),
    mutation22(Ind, P1, P2, NInd).

mutation22([G1|Ind], 1, P2, [G2|NInd]) :-
    !, P21 is P2 - 1,
    mutation23(G1, P21, Ind, G2, NInd).
mutation22([G|Ind], P1, P2, [G|NInd]) :-
    P11 is P1 - 1, P21 is P2 - 1,
    mutation22(Ind, P11, P21, NInd).

mutation23(G1, 1, [G2|Ind], G2, [G1|Ind]) :- !.
mutation23(G1, P, [G|Ind], G2, [G|NInd]) :-
    P1 is P - 1,
    mutation23(G1, P1, Ind, G2, NInd).

%%% Elitism predicate to extract the best individuals from the population %%%
elitism(Population, Res, Percentage) :-
  population(PopSize),
  MaxN is floor(PopSize * (Percentage / 100)),
  EliteCount is max(1, MaxN), % Top Percentage% or at least 1 individual
  % write('EliteCount: '), write(EliteCount), nl,
  Population = [BestInd|T],
  Elites = [BestInd],
  reverse(T, PopRev),
  Count is EliteCount - 1,
  PopulationSize is PopSize - 1,
  take(Count, PopRev, PopulationSize, Elites, Res).

take(0, _, _, R, R).
take(N, [H|T], PopSize, R, Elites) :-
    N > 0,
    Percentage is N / PopSize,
    random(0.0, 1.0, P),
    %write('P: '), write(P), nl,
    %write('Percentage: '), write(Percentage), nl,
    (( 
    P < Percentage, 
    %write('P < Percentage'), nl,
    N1 is N - 1,
    %write('N1: '), write(N1), nl,
    %write('H: '), write(H), nl,
    %write('R: '), write(R), nl,
    Result = [H|R]
    %write('Result: '), write(Result), nl
    );
    N1 is N, 
    Result = R),
    PopSize1 is PopSize - 1,
    take(N1, T, PopSize1, Result, Elites).

%%% Ensures population size is maintained %%%
select_new_population(Population, Selected) :-
  population(PopSize),
  length(Selected, PopSize),
  append(Selected, _, Population).

bubble_sort([X], [X]) :- !.
bubble_sort([X|Xs], Ys) :-
    bubble_sort(Xs, Zs),
    bubble_swap([X|Zs], Ys).

bubble_swap([X], [X]) :- !.

bubble_swap([X*VX, Y*VY|L1], [Y*VY|L2]) :-
    VX > VY, !,
    bubble_swap([X*VX|L1], L2).

bubble_swap([X|L1], [X|L2]) :- bubble_swap(L1, L2).

%----- Other helper predicates ----------------------------------------------------%
remove(1, [G|Rest], G, Rest).
remove(N, [G1|Rest], G, [G1|Rest1]):-
  N1 is N-1,
  remove(N1, Rest, G, Rest1).

fill_h([], []).

fill_h([_|R1], [h|R2]) :-
    fill_h(R1, R2).

sublist(L1, I1, I2, L) :-
    I1 < I2, !,
    sublist1(L1, I1, I2, L).

sublist(L1, I1, I2, L) :-
    sublist1(L1, I2, I1, L).

sublist1([X|R1], 1, 1, [X|H]) :- !,
    fill_h(R1, H).

sublist1([X|R1], 1, N2, [X|R2]) :- !,
    N3 is N2 - 1,
    sublist1(R1, 1, N3, R2).

sublist1([_|R1], N1, N2, [h|R2]) :-
    N3 is N1 - 1,
    N4 is N2 - 1,
    sublist1(R1, N3, N4, R2).

rotate_right(L, K, L1) :-
    tasks(N),
    T is N - K,
    rr(T, L, L1).

rr(0, L, L) :- !.

rr(N, [X|R], R2) :-
    N1 is N - 1,
    append(R, [X], R1),
    rr(N1, R1, R2).


eliminate([], _, []) :- !.

eliminate([X|R1], L, [X|R2]) :-
    not(member(X, L)), !,
    eliminate(R1, L, R2).

eliminate([_|R1], L, R2) :-
    eliminate(R1, L, R2).

insert([], L, _, L) :- !.
insert([X|R], L, N, L2) :-
    tasks(T),
    ((N > T, !, N1 is N mod T); N1 = N),
    insert1(X, N1, L, L1),
    N2 is N + 1,
    insert(R, L1, N2, L2).

insert1(X, 1, L, [X|L]) :- !.
insert1(X, N, [Y|L], [Y|L1]) :-
    N1 is N - 1,
    insert1(X, N1, L, L1).

eliminate_h([], []).

eliminate_h([h|R1], R2) :- !,
    eliminate_h(R1, R2).

eliminate_h([X|R1], [X|R2]) :-
    eliminate_h(R1, R2).

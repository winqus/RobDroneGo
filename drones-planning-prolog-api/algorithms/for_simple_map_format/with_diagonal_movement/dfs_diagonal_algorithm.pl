:- module(dfs_diagonal_algorithm, [dfs/4, better_dfs/4, all_dfs/3]).

% ALGORITHM IS THE SAME AT STANDARD DFS ALGORITHM
% THE ONLY DIFFERENCE HERE IS THAT THIS MODULE USES THE DIAGONAL GRAPH CREATION
:- use_module(graph_creation_diagonal).

% graph_creation_diagonal:create_graph(8,7).
% dfs(cel(1,2), cel(7,7), Path), writeln(Path).
% better_dfs(cel(1,2), cel(7,7), Path), writeln(Path).


dfs(Start, End, Path, Cost) :- dfs2(Start, End, [Start], Path, 0, Cost).

dfs2(End, End, LA, Path, Cost, Cost) :- reverse(LA, Path).

dfs2(Act, End, LA, Path, CostSoFar, TotalCost) :-
    connectCell(Act, X, MoveCost),  % MoveCost is either 1 or 1.41
    \+ member(X, LA),
    NewCost is CostSoFar + MoveCost,
    dfs2(X, End, [X | LA], Path, NewCost, TotalCost).

all_dfs(Start, End, LPathCost) :- findall((Path, Cost), dfs(Start, End, Path, Cost), LPathCost).

better_dfs(Start, End, Path, Cost) :-
    all_dfs(Start, End, LPathCost),
    shortlist(LPathCost, Path, Cost).

shortlist([(L, N)], L, N) :- !.

shortlist([(L, NL) | LL], Lm, Nm) :-
    shortlist(LL, Lm1, Nm1),
    ((NL < Nm1, !, Lm = L, Nm = NL); (Lm = Lm1, Nm = Nm1)).

time_dfs(Start, End, Path, Cost, Time) :-
    get_time(Ti),
    dfs(Start, End, Path, Cost),
    get_time(Tf),
    Time is Tf - Ti.

time_better_dfs(Start, End, Path, Cost, Time) :-
    get_time(Ti),
    better_dfs(Start, End, Path, Cost),
    get_time(Tf),
    Time is Tf - Ti.

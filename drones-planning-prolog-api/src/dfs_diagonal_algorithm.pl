:- module(dfs_diagonal_algorithm, [dfs/4, better_dfs/4, all_dfs/3]).

% DFS algorithm with Cost for diagonal moves
:- use_module(graph_creation_for_maze_diagonal).

% graph_creation_for_maze_diagonal:create_graph(4,4).
% dfs(cel(1,1), cel(4,4), Path, Cost), writeln(Path).
% better_dfs(cel(1,1), cel(4,4), Path, Cost), writeln(Path).


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

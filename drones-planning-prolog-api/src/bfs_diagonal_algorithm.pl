:- module(bfs_diagonal_algorithm, [bfs/4, shortest_bfs/4, all_bfs/3]).

% BFS algorithm with Cost for diagonal moves
:- use_module(graph_creation_for_maze_diagonal).

% graph_creation_for_maze_diagonal:create_graph(4,4).
% bfs(cel(1,1), cel(4,4), Path, Cost), writeln(Path).
% shortest_bfs(cel(1,2), cel(4,4), Path, Cost), writeln(Path).


bfs(Start, End, Path, Cost) :- bfs2(End, [([Start], 0)], Path, Cost).

bfs2(End, [([End | T], Cost) | _], Path, Cost) :- reverse([End | T], Path).

bfs2(End, [([Act | T], CostSoFar) | Others], Path, FinalCost) :-
    findall(([X, Act | T], NewCost), (
        End \== Act,
        connectCell(Act, X, MoveCost),  % MoveCost is either 1 or 1.41
        \+ member(X, [Act | T]),
        NewCost is CostSoFar + MoveCost
    ), New),
    append(Others, New, All),
    bfs2(End, All, Path, FinalCost).

all_bfs(Start, End, LPathCost) :- findall((Path, Cost), bfs(Start, End, Path, Cost), LPathCost).

shortest_bfs(Start, End, Path, Cost) :-
    all_bfs(Start, End, LPathCost),
    shortlist(LPathCost, Path, Cost).

shortlist([(L, N)], L, N) :- !.

shortlist([(L, NL) | LL], Lm, Nm) :-
    shortlist(LL, Lm1, Nm1),
    ((NL < Nm1, !, Lm = L, Nm = NL); (Lm = Lm1, Nm = Nm1)).

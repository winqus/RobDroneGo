:- module(bfs_diagonal_algorithm, [bfs/4, shortest_bfs/4, all_bfs/3]).

% ALGORITHM IS THE SAME AT STANDARD BFS ALGORITHM
% THE ONLY DIFFERENCE HERE IS THAT THIS MODULE USES THE DIAGONAL GRAPH CREATION
:- use_module(graph_creation_diagonal).

% graph_creation_diagonal:create_graph(8,7).
% bfs(cel(1,2), cel(7,7), Path), writeln(Path).
% shortest_bfs(cel(1,2), cel(7,7), Path), writeln(Path).


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

time_bfs(Start, End, Path, Cost, Time) :-
    get_time(Ti),
    bfs(Start, End, Path, Cost),
    get_time(Tf),
    Time is Tf - Ti.

time_shortest_bfs(Start, End, Path, Cost, Time) :-
    get_time(Ti),
    shortest_bfs(Start, End, Path, Cost),
    get_time(Tf),
    Time is Tf - Ti.
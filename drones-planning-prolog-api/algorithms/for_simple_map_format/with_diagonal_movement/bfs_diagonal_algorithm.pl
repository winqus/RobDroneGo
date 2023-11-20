:- module(bfs_diagonal_algorithm, [bfs/3, shortest_bfs/3, all_bfs/3]).

% ALGORITHM IS THE SAME AT STANDARD BFS ALGORITHM
% THE ONLY DIFFERENCE HERE IS THAT THIS MODULE USES THE DIAGONAL GRAPH CREATION
:- use_module(graph_creation_diagonal).

% graph_creation_diagonal:create_graph(8,7).
% bfs(cel(1,2), cel(7,7), Path), writeln(Path).
% shortest_bfs(cel(1,2), cel(7,7), Path), writeln(Path).


bfs(Start, End, Path) :- bfs2(End, [[Start]], Path).

bfs2(End, [[End | T] | _], Path) :- reverse([End | T], Path).

bfs2(End, [LA | Others], Path) :-
    LA = [Act | _],
    findall([X | LA], (End \== Act, connectCell(Act, X), \+ member(X, LA)), New),
    append(Others, New, All),
    bfs2(End, All, Path).

all_bfs(Start, End, LPath) :- findall(Path, bfs(Start, End, Path), LPath).

shortest_bfs(Start, End, ShortestPath) :-
    all_bfs(Start, End, AllPaths),
    shortlist(AllPaths, ShortestPath, _).

shortlist([L], L, N) :- !, length(L, N).

shortlist([L | LL], Lm, Nm) :-
    shortlist(LL, Lm1, Nm1),
    length(L, NL),
    ((NL < Nm1, !, Lm = L, Nm = NL); (Lm = Lm1, Nm = Nm1)).

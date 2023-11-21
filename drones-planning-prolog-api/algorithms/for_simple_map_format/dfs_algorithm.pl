:- module(dfs_algorithm, [dfs/3, better_dfs/3, all_dfs/3]).

:- use_module(graph_creation).

% graph_creation:create_graph(8,7).
% dfs(cel(1,2), cel(7,7), Path), writeln(Path). % can take long time
% better_dfs(cel(1,2), cel(7,7), Path), writeln(Path). % takes long time


dfs(Start, End, Path) :- dfs2(Start, End, [Start], Path).

dfs2(End, End, LA, Path) :- reverse(LA, Path).

dfs2(Act, End, LA, Path) :-
    connectCell(Act, X),
    \+ member(X, LA),
    dfs2(X, End, [X | LA], Path).

all_dfs(Start, End, LPath) :- findall(Path, dfs(Start, End, Path), LPath).

better_dfs(Start, End, Path) :-
    all_dfs(Start, End, LPath),
    shortlist(LPath, Path, _).

shortlist([L], L, N) :- !, length(L, N).

shortlist([L | LL], Lm, Nm) :-
    shortlist(LL, Lm1, Nm1),
    length(L, NL),
    ((NL < Nm1, !, Lm = L, Nm = NL); (Lm = Lm1, Nm = Nm1)).

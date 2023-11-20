:- module(astar_maze_diagonal_algorithm, [aStar/4]).

% Uses graph_creation_diagonal and replaced estimate to support diagonal movement, other than that it's the same as astar_algorithm
:- use_module(graph_creation_for_maze_diagonal).

% graph_creation_for_maze_diagonal:create_graph(4,4).
% aStar(cel(1, 1), cel(4, 4), Path, Cost), writeln(Path). % quick
% aStar(cel(3, 2), cel(2, 3), Path, Cost), writeln(Path).


% Estimate function (Euclidean distance, supports diagonal)
estimate(cel(Col1, Row1), cel(Col2, Row2), Estimate) :-
	DCol is Col1 - Col2,
	DRow is Row1 - Row2,
	Estimate is sqrt(DCol * DCol + DRow * DRow).

% A* Algorithm Implementation
aStar(Start, End, Path, Cost) :-
	aStar2(End, [(_, 0, [Start])], Path, Cost).

aStar2(End, [(_, Cost, [End | T]) | _], Path, Cost) :-
	reverse([End | T], Path).

aStar2(End, [(_, Ca, LA) | Others], Path, Cost) :-
	LA = [Act | _],
	findall((CEX, CaX, [X | LA]),
					(End \== Act, connectCell(Act, X, CostX), \+ member(X, LA),
					CaX is CostX + Ca, estimate(X, End, EstX),
					CEX is CaX + EstX), New),
	append(Others, New, All),
	sort(All, AllOrd),
	aStar2(End, AllOrd, Path, Cost).

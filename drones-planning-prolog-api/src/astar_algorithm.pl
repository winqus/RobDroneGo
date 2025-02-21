:- module(astar_algorithm, [aStar/4]).

:- use_module(graph_creation_for_maze).

% graph_creation_for_maze:create_graph(4,4).
% aStar(cel(1, 2), cel(4, 4), Path, Cost), writeln(Path). % quick
% aStar(cel(1, 1), cel(3, 3), Path, Cost), writeln(Path).


% Node definition (using cells in the maze)
node(cel(Col, Row)) :-
	m(Col, Row, 0).

% Edge definition (using connectCell facts)
edge(cel(Col1, Row1), cel(Col2, Row2), Cost) :-
	connectCell(cel(Col1, Row1), cel(Col2, Row2)),
	Cost = 1. % Assuming uniform cost for simplicity

% Estimate function (Manhattan distance)
estimate(cel(Col1, Row1), cel(Col2, Row2), Estimate) :-
	Estimate is abs(Col1 - Col2) + abs(Row1 - Row2).

% A* Algorithm Implementation
aStar(Start, End, Path, Cost) :-
	aStar2(End, [(_, 0, [Start])], Path, Cost).

aStar2(End, [(_, Cost, [End | T]) | _], Path, Cost) :-
	reverse([End | T], Path).

aStar2(End, [(_, Ca, LA) | Others], Path, Cost) :-
	LA = [Act | _],
	findall((CEX, CaX, [X | LA]),
			(End \== Act, edge(Act, X, CostX), \+ member(X, LA),
			CaX is CostX + Ca, estimate(X, End, EstX),
			CEX is CaX + EstX), New),
	append(Others, New, All),
	sort(All, AllOrd),
	aStar2(End, AllOrd, Path, Cost).

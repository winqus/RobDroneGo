:- module(astar_diagonal_algorithm, [aStar/4]).

% Uses graph_creation_diagonal and replaced estimate to support diagonal movement, 
% other than that it's the same as astar_algorithm
:- use_module(graph_creation_diagonal).

% graph_creation_diagonal:create_graph(8,7).
% aStar(cel(1, 2), cel(7, 7), Path, Cost), writeln(Path). % quick


% Node definition (using cells in the maze)
node(cel(Col, Row)) :-
	graph_creation_diagonal:m(Col, Row, 0).

% Edge definition (using connectCell facts)
edge(cel(Col1, Row1), cel(Col2, Row2), Cost) :-
	connectCell(cel(Col1, Row1), cel(Col2, Row2),Cost).

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
					(End \== Act, edge(Act, X, CostX), \+ member(X, LA),
					CaX is CostX + Ca, estimate(X, End, EstX),
					CEX is CaX + EstX), New),
	append(Others, New, All),
	sort(All, AllOrd),
	aStar2(End, AllOrd, Path, Cost).

time_aStar(Start, End, Path, Cost, T) :-
	get_time(Ti),
	aStar(Start, End, Path, Cost),
	get_time(Tf),
	T is Tf - Ti.

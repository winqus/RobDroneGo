:- module(astar_algorithm, [aStar/4]).

% :- use_module(graph_creation).

% graph_creation:create_graph(8,7).
% aStar(cel(1, 2), cel(7, 7), Path, Cost), writeln(Path). % quick

create_graph(_,0):-!.

create_graph(Col,Lin):-create_graph_lin(Col,Lin),Lin1 is Lin-1,create_graph(Col,Lin1).

create_graph_lin(0,_):-!.

create_graph_lin(Col,Lin):- m(Col,Lin,0),!,ColS is Col+1, ColA is Col-1, LinS is Lin+1,LinA is Lin-1,((m(ColS,Lin,0),
assertz(connectCell(cel(Col,Lin),cel(ColS,Lin)));true)),
((m(ColA,Lin,0),assertz(connectCell(cel(Col,Lin),cel(ColA,Lin)));true)),
((m(Col,LinS,0),assertz(connectCell(cel(Col,Lin),cel(Col,LinS)));true)),
((m(Col,LinA,0),assertz(connectCell(cel(Col,Lin),cel(Col,LinA)));true)),
Col1 is Col-1, create_graph_lin(Col1,Lin).

create_graph_lin(Col,Lin):-Col1 is Col-1,create_graph_lin(Col1,Lin).

:-dynamic m/3.
:-dynamic nlin/1.
:-dynamic connectCell/2.

cria_matriz:-
	retractall(m(_,_,_)),
	retractall(connectCell(_,_)),
	write('Numero de Colunas: '),read(NCol),nl,
	write('Numero de Linhas: '),read(NLin),nl,asserta(nlin(NLin)),
	cria_matriz_0(NCol,NLin),create_graph(NCol,NLin),retract(nlin(_)).
	
cria_matriz_0(1,1):-!,asserta(m(1,1,0)).
cria_matriz_0(NCol,1):-!,asserta(m(NCol,1,0)),NCol1 is NCol-1,nlin(NLin),cria_matriz_0(NCol1,NLin).
cria_matriz_0(NCol,NLin):-asserta(m(NCol,NLin,0)),NLin1 is NLin-1,cria_matriz_0(NCol,NLin1).

count_m(Count) :-
    predicate_property(m(_,_,_), number_of_clauses(Count)).

count_connect(Count) :-
    predicate_property(connectCell(_,_), number_of_clauses(Count)).

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

time_aStar(Start, End, Path, Cost, T) :-
	get_time(Ti),
	aStar(Start, End, Path, Cost),
	get_time(Tf),
	T is Tf - Ti.
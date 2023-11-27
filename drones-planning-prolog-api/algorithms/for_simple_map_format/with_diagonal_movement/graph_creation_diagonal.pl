:- module(graph_creation_diagonal, [create_graph/2, connectCell/3, m/3, cria_matriz/0, count_m/1, count_connect/1]).
% m/3 rules are used for testing purposes
%column :1,2,3,4,5,6,7,8
%line  1:1,1,1,1,1,1,1,1
%line  2:0,0,0,0,0,0,0,1
%line  3:0,0,0,0,0,0,0,1
%line  4:0,0,0,0,0,0,0,1
%line  5:1,1,1,1,0,0,0,1
%line  6:1,1,1,1,0,0,0,1
%line  7:1,1,1,1,0,0,0,1
%column :1,2,3,4,5,6,7,8

% create_graph(numberOfCols,numberofRows).
% create_graph(8,7).

% m(col, row, value)
% m(Column, Row, 0) indicates a passable cell; 1 indicates an impassable cell

:-dynamic connectCell/3.
:-dynamic m/3.

:-dynamic nlin/1.

cria_matriz:-
	retractall(m(_,_,_)),
	retractall(connectCell(_,_,_)),
	write('Numero de Colunas: '),read(NCol),nl,
	write('Numero de Linhas: '),read(NLin),nl,asserta(nlin(NLin)),
	cria_matriz_0(NCol,NLin),create_graph(NCol,NLin),retract(nlin(_)).
	
cria_matriz_0(1,1):-!,asserta(m(1,1,0)).
cria_matriz_0(NCol,1):-!,asserta(m(NCol,1,0)),NCol1 is NCol-1,nlin(NLin),cria_matriz_0(NCol1,NLin).
cria_matriz_0(NCol,NLin):-asserta(m(NCol,NLin,0)),NLin1 is NLin-1,cria_matriz_0(NCol,NLin1).

count_m(Count) :-
    predicate_property(m(_,_,_), number_of_clauses(Count)).

count_connect(Count) :-
    predicate_property(connectCell(_,_,_), number_of_clauses(Count)).


create_graph(_,0):-!.

create_graph(Col, Lin) :-
    create_graph_lin(Col, Lin),
    Lin1 is Lin - 1,
    create_graph(Col, Lin1).

create_graph_lin(0, _) :-!.

create_graph_lin(Col, Lin) :-
    m(Col, Lin, 0), !,
    ColS is Col + 1, ColA is Col - 1, LinS is Lin + 1, LinA is Lin - 1,
    % Horizontal and vertical connections
    (m(ColS, Lin, 0), assertz(connectCell(cel(Col, Lin), cel(ColS, Lin), 1)); true),
    (m(ColA, Lin, 0), assertz(connectCell(cel(Col, Lin), cel(ColA, Lin), 1)); true),
    (m(Col, LinS, 0), assertz(connectCell(cel(Col, Lin), cel(Col, LinS), 1)); true),
    (m(Col, LinA, 0), assertz(connectCell(cel(Col, Lin), cel(Col, LinA), 1)); true),
    % Diagonal connections
    (m(ColS, LinS, 0), m(Col, LinS, 0), m(ColS, Lin, 0), assertz(connectCell(cel(Col, Lin), cel(ColS, LinS), 1.41)); true),
    (m(ColA, LinS, 0), m(Col, LinS, 0), m(ColA, Lin, 0), assertz(connectCell(cel(Col, Lin), cel(ColA, LinS), 1.41)); true),
    (m(ColS, LinA, 0), m(Col, LinA, 0), m(ColS, Lin, 0), assertz(connectCell(cel(Col, Lin), cel(ColS, LinA), 1.41)); true),
    (m(ColA, LinA, 0), m(Col, LinA, 0), m(ColA, Lin, 0), assertz(connectCell(cel(Col, Lin), cel(ColA, LinA), 1.41)); true),
    Col1 is Col - 1,
    create_graph_lin(Col1, Lin).

create_graph_lin(Col, Lin) :-
    Col1 is Col - 1,
    create_graph_lin(Col1, Lin).

% TRANSLATED AND MODIFIED EXAMPLE FROM MOODLE

% m/3 rules are used for testing purposes
%coluna :1,2,3,4,5,6,7,8
%linha 1:1,1,1,1,1,1,1,1
%linha 2:0,0,0,0,0,0,0,1
%linha 3:0,0,0,0,0,0,0,1
%linha 4:0,0,0,0,0,0,0,1
%linha 5:1,1,1,1,0,0,0,1
%linha 6:1,1,1,1,0,0,0,1
%linha 7:1,1,1,1,0,0,0,1
%coluna :1,2,3,4,5,6,7,8

% create_graph(numberOfCols,numberofRows).
% create_graph(8,7).
% better_dfs(cel(1,2), cel(7,7), Path), writeln(Path). % ok
% bfs(cel(1,2), cel(7,7), Path), writeln(Path). % ok
% aStar(cel(1, 2), cel(7, 7), Path, Cost), writeln(Path). % ok

%m(col,row,value)
m(1,1,1).
m(2,1,1).
m(3,1,1).
m(4,1,1).
m(5,1,1).
m(6,1,1).
m(7,1,1).
m(8,1,1).
m(1,2,0).
m(2,2,0).
m(3,2,0).
m(4,2,0).
m(5,2,0).
m(6,2,0).
m(7,2,0).
m(8,2,1).
m(1,3,0).
m(2,3,0).
m(3,3,0).
m(4,3,0).
m(5,3,0).
m(6,3,0).
m(7,3,0).
m(8,3,1).
m(1,4,0).
m(2,4,0).
m(3,4,0).
m(4,4,0).
m(5,4,0).
m(6,4,0).
m(7,4,0).
m(8,4,1).
m(1,5,1).
m(2,5,1).
m(3,5,1).
m(4,5,1).
m(5,5,0).
m(6,5,0).
m(7,5,0).
m(8,5,1).
m(1,6,1).
m(2,6,1).
m(3,6,1).
m(4,6,1).
m(5,6,0).
m(6,6,0).
m(7,6,0).
m(8,6,1).
m(1,7,1).
m(2,7,1).
m(3,7,1).
m(4,7,1).
m(5,7,0).
m(6,7,0).
m(7,7,0).
m(8,7,1).

:-dynamic connectCell/2.

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
	

% Old stuff

% replace the call edge(Act,X,CostX)
% with (edge(Act,X,CostX);edge(X,Act,CostX))
% if you want bidirectional connections


% estimate(Node1,Node2,Estimate):-
% 	node(Node1,X1,Y1),
% 	node(Node2,X2,Y2),
% 	Estimate is sqrt((X1-X2)^2+(Y1-Y2)^2).

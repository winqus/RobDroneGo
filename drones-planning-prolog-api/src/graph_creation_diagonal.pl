:- module(graph_creation_diagonal, [create_graph/2, connectCell/2]).
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
    (m(ColS, Lin, 0), assertz(connectCell(cel(Col, Lin), cel(ColS, Lin))); true),
    (m(ColA, Lin, 0), assertz(connectCell(cel(Col, Lin), cel(ColA, Lin))); true),
    (m(Col, LinS, 0), assertz(connectCell(cel(Col, Lin), cel(Col, LinS))); true),
    (m(Col, LinA, 0), assertz(connectCell(cel(Col, Lin), cel(Col, LinA))); true),
    % Diagonal connections
    (m(ColS, LinS, 0), assertz(connectCell(cel(Col, Lin), cel(ColS, LinS))); true),
    (m(ColA, LinS, 0), assertz(connectCell(cel(Col, Lin), cel(ColA, LinS))); true),
    (m(ColS, LinA, 0), assertz(connectCell(cel(Col, Lin), cel(ColS, LinA))); true),
    (m(ColA, LinA, 0), assertz(connectCell(cel(Col, Lin), cel(ColA, LinA))); true),
    Col1 is Col - 1,
    create_graph_lin(Col1, Lin).

create_graph_lin(Col, Lin) :-
    Col1 is Col - 1,
    create_graph_lin(Col1, Lin).
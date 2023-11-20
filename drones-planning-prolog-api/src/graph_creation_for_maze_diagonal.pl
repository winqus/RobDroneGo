:- module(graph_creation_for_maze_diagonal, [create_graph/2, connectCell/3]).
/* 
  NoNorthWallNoWestWall = 0,
  NoNorthWallYesWestWall = 1,
  YesNorthWallNoWestWall = 2,
  YesNorthWallYesWestWall = 3,
  DoorNorth = 4,
  DoorWest = 5,
  PassageNorth = 6,
  PassageWest = 7,
  ElevatorNorth = 8,
  ElevatorSouth = 9,
  ElevatorWest = 10,
  ElevatorEast = 11,
*/

%column :1,2,3,4
%line  1:3,2,6,3
%line  2:1,0,0,0
%line  3:1,0,1,0
%line  4:1,0,1,10
%


% m(col, row, value)
m(1,1,3).
m(2,1,2).
m(3,1,6).
m(4,1,3).

m(1,2,1).
m(2,2,0).
m(3,2,0).
m(4,2,0).

m(1,3,1).
m(2,3,0).
m(3,3,1).
m(4,3,0).

m(1,4,1).
m(2,4,0).
m(3,4,1).
m(4,4,10).

% create_graph(numberOfCols,numberofRows).
% create_graph(4,4).

:-dynamic connectCell/3. % connectCell(cel(Col, Row), cel(Col, Row), Cost).

% Base case for recursion: do nothing when Row is 0
create_graph(_, 0) :- !.

% Main predicate to create the graph: process each row, then move to the next
create_graph(Col, Row) :-
    create_graph_lin(Col, Row),  % Process the current row
    PreviousRow is Row - 1,            % Decrement row number
    create_graph(Col, PreviousRow).    % Recursively call for the next row

% Base case for recursion in row: do nothing when Col is 0
create_graph_lin(0, _) :- !.


% Process each cell in the row, then move to the next cell
create_graph_lin(Col, Row) :-
  m(Col, Row, Value),          % Get the current cell value
  EastCol is Col + 1, WestCol is Col - 1, SouthRow is Row + 1, NorthRow is Row - 1,  % Calculate neighboring cell coordinates
  StraightMoveCost is 1, DiagonalMoveCost is 1.41, % Define move costs
  % Check and assert connections for each direction based on cell value
  % Horizontal and vertical connections
  (can_move_to(Value, east), m(EastCol, Row, EastValue), can_enter_from(EastValue, west),
    assertz(connectCell(cel(Col, Row), cel(EastCol, Row), StraightMoveCost)); true),
  (can_move_to(Value, west), m(WestCol, Row, WestValue), can_enter_from(WestValue, east),
    assertz(connectCell(cel(Col, Row), cel(WestCol, Row), StraightMoveCost)); true),
  (can_move_to(Value, south), m(Col, SouthRow, SouthValue), can_enter_from(SouthValue, north),
    assertz(connectCell(cel(Col, Row), cel(Col, SouthRow), StraightMoveCost)); true),
  (can_move_to(Value, north), m(Col, NorthRow, NorthValue), can_enter_from(NorthValue, south),
    assertz(connectCell(cel(Col, Row), cel(Col, NorthRow), StraightMoveCost)); true),
  % Diagonal connections
  (can_move_to(Value, ne), m(EastCol, NorthRow, NEValue), can_enter_from(NEValue, sw),
    m(Col, NorthRow, NorthValue), can_enter_from(NorthValue, se),
    m(EastCol, Row, EastValue), can_enter_from(EastValue, nw),
    assertz(connectCell(cel(Col, Row), cel(EastCol, NorthRow), DiagonalMoveCost)); true),
  (can_move_to(Value, nw), m(WestCol, NorthRow, NWValue), can_enter_from(NWValue, se),
    m(Col, NorthRow, NorthValue), can_enter_from(NorthValue, sw),
    m(WestCol, Row, WestValue), can_enter_from(WestValue, ne),
    assertz(connectCell(cel(Col, Row), cel(WestCol, NorthRow), DiagonalMoveCost)); true),
  (can_move_to(Value, se), m(EastCol, SouthRow, SEValue), can_enter_from(SEValue, nw),
    m(Col, SouthRow, SouthValue), can_enter_from(SouthValue, ne),
    m(EastCol, Row, EastValue), can_enter_from(EastValue, sw),
    assertz(connectCell(cel(Col, Row), cel(EastCol, SouthRow), DiagonalMoveCost)); true),
  (can_move_to(Value, sw), m(WestCol, SouthRow, SWValue), can_enter_from(SWValue, ne),
    m(Col, SouthRow, SouthValue), can_enter_from(SouthValue, nw),
    m(WestCol, Row, WestValue), can_enter_from(WestValue, se),
    assertz(connectCell(cel(Col, Row), cel(WestCol, SouthRow), DiagonalMoveCost)); true),
  PreviousCol is Col - 1,
  create_graph_lin(PreviousCol, Row).

% nw for northwest, ne for northeast, sw for southwest, and se for southeast
% Predicate definitions to determine if movement is possible based on cell value and direction
% Aka can the player move from current cell to the next cell in the given direction
can_move_to(0, _).
can_move_to(1, Direction) :- Direction \= west, Direction \= nw, Direction \= sw.
can_move_to(2, Direction) :- Direction \= north, Direction \= ne; Direction \= nw.
can_move_to(3, Direction) :- Direction = east; Direction = se; Direction = south.
can_move_to(4, Direction) :- Direction \= north, Direction \= ne; Direction \= nw.
can_move_to(5, Direction) :- Direction \= west, Direction \= nw, Direction \= sw.
can_move_to(6, Direction) :- Direction \= north, Direction \= ne; Direction \= nw.
can_move_to(7, Direction) :- Direction \= west, Direction \= nw, Direction \= sw.
can_move_to(8, north).
can_move_to(9, south).
can_move_to(10, west).
can_move_to(11, east).

% Predicate definitions to determine if entering a cell is possible based on cell value and direction
% AKA can the cell be entered from the given direction
can_enter_from(Value, Direction) :- can_move_to(Value, Direction).
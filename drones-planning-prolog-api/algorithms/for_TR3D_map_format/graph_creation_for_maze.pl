:- module(graph_creation_for_maze, [create_graph/2, connectCell/2]).
/* 
Cell value 0 means that cell is passable.
Cell value 1 means there's a wall on the west side of that cell, meaning that one can move to that cell from 
north, east, south (if not blocked from those sides by other cells) but not from west.
Cell value 2 means there's a wall on the north side of that cell, meaning that one can move to that cell from
east, south, west (if not blocked from those sides by other cells) but not from north.
Cell value 3 means there's a wall on the west and north sides of that cell, meaning that one can move to that cell
from south, east (if not blocked from those sides by other cells) but not from north and west.
Cell value 4 means there's a door on the north side of that cell, but its always open, 
meaning its passable from north and south sides.
Cell value 5 means there's a door on the west side of that cell, but its always open, 
meaning its passable from west and east sides.
Cell value 6 means there's a passage (coridor) on the north side of that cell, 
meaning that one can move to that cell from the east, south, west sides (if not blocked by other cells).
Cell value 7 means there's a passage (coridor) on the west side of that cell,
meaning that one can move to that cell from the north, east, south sides (if not blocked by other cells).
Cell value 8 means there's an elevator entrance facing the north and is entered to from the north.
Cell value 9 means there's an elevator entrance facing the south and is entered to entered from the south.
Cell value 10 means there's an elevator entrance facing the west and is entered to from the west.
Cell value 11 means there's an elevator entrance facing the east and is entered to from the east.

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

% m/3 rules are used for testing purposes
%column :1,2,3,4
%line  1:3,2,6,3
%line  2:1,0,0,0
%line  3:1,0,1,0
%line  4:1,0,1,10
%

% create_graph(numberOfCols,numberofRows).
% create_graph(4,4).

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


:-dynamic connectCell/2.


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
    % Check and assert connections for each direction based on cell value
    (can_move_to(Value, east), m(EastCol, Row, EastValue), can_enter_from(EastValue, west), assertz(connectCell(cel(Col, Row), cel(EastCol, Row))); true),
    (can_move_to(Value, west), m(WestCol, Row, WestValue), can_enter_from(WestValue, east), assertz(connectCell(cel(Col, Row), cel(WestCol, Row))); true),
    (can_move_to(Value, south), m(Col, SouthRow, SouthValue), can_enter_from(SouthValue, north), assertz(connectCell(cel(Col, Row), cel(Col, SouthRow))); true),
    (can_move_to(Value, north), m(Col, NorthRow, NorthValue), can_enter_from(NorthValue, south), assertz(connectCell(cel(Col, Row), cel(Col, NorthRow))); true),
    PreviousCol is Col - 1,
    create_graph_lin(PreviousCol, Row).

% Predicate definitions to determine if movement is possible based on cell value and direction
% Aka can the player move from current cell to the next cell in the given direction
can_move_to(0, _).
can_move_to(1, Direction) :- Direction \= west.
can_move_to(2, Direction) :- Direction \= north.
can_move_to(3, Direction) :- Direction = east; Direction = south.
can_move_to(4, Direction) :- Direction \= north.
can_move_to(5, Direction) :- Direction \= west.
can_move_to(6, Direction) :- Direction \= north.
can_move_to(7, Direction) :- Direction \= west.
can_move_to(8, north).
can_move_to(9, south).
can_move_to(10, west).
can_move_to(11, east).

% Predicate definitions to determine if entering a cell is possible based on cell value and direction
% AKA can the cell be entered from the given direction
can_enter_from(Value, Direction) :- can_move_to(Value, Direction).

:- module(route_handler, [get_route_handler/1]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(logic).
:- use_module(navigationBuildingsFloors).
:- use_module(graph_creation_for_maze_diagonal).
:- use_module(astar_maze_diagonal_algorithm).
% :- use_module(graph_creation_for_maze).
% :- use_module(astar_algorithm).


:- http_handler('/planning-api/route', get_route_handler, []).

:- http_handler('/planning-api/test', get_test_handler, []).
:- http_handler('/planning-api/', get_test_handler, []).
:- http_handler('/', get_test_handler, []).

log_message_ln(Message) :-
  http_log('~p~n', [Message]).

log_message(Message) :-
  http_log('~p ', [Message]).

%  Route route with parameters handler
get_route_handler(Request) :-
  http_log('>>>get_route_handler<<< received request: ~p~n', [Request]),
  http_parameters(Request,
    [ origin_building_code(OriginBuildingCode_Raw, []),
      origin_floor_number(OriginFloorNumber_Raw, []),
      origin_map_cell_x(OriginMapCellX_Raw, []),
      origin_map_cell_y(OriginMapCellY_Raw, []),
      destination_building_code(DestinationBuildingCode_Raw, []),
      destination_floor_number(DestinationFloorNumber_Raw, []),
      destination_map_cell_x(DestinationMapCellX_Raw, []),
      destination_map_cell_y(DestinationMapCellY_Raw, []),
      minimize_elevator_uses(_, []), % MinimizeElevatorUses
      minimize_building_count(_, []) % MinimizeBuildingCount
    ]),
      log_message('received parameters:'),
  % Convert parameters to atoms
  atom_string(OriginBuildingCode, OriginBuildingCode_Raw),
  atom_number(OriginFloorNumber_Raw, OriginFloorNumber),
  atom_number(OriginMapCellX_Raw, TempOriginMapCellX),
  OriginMapCellX is TempOriginMapCellX + 1,
  atom_number(OriginMapCellY_Raw, TempOriginMapCellY),
  OriginMapCellY is TempOriginMapCellY + 1,
  atom_string(DestinationBuildingCode, DestinationBuildingCode_Raw),
  atom_number(DestinationFloorNumber_Raw, DestinationFloorNumber),
  atom_number(DestinationMapCellX_Raw, TempDestinationMapCellX),
  DestinationMapCellX is TempDestinationMapCellX + 1,
  atom_number(DestinationMapCellY_Raw, TempDestinationMapCellY),
  DestinationMapCellY is TempDestinationMapCellY + 1,
      log_message_ln([OriginBuildingCode, OriginFloorNumber, OriginMapCellX, OriginMapCellY, DestinationBuildingCode, DestinationFloorNumber, DestinationMapCellX, DestinationMapCellY]),
  logic:load_info(), % GETs building, floor, elevator, passage info from backend API
      log_message('loaded info;'),
  format(atom(Origin), '~w::~w', [OriginBuildingCode, OriginFloorNumber]), % Format origin as Building::FloorFloorNumber
  format(atom(Destination), '~w::~w', [DestinationBuildingCode, DestinationFloorNumber]), % Format destination as Building::FloorNumber
  
  %%% This block of code is temporary until the path finding algorithms is fixed (either graph_creation_for_maze_diagonal or astar_maze_diagonal_algorithm)
  %%% There are cases when aStar gets stuck and doesn't return anything, problem might be in graph creation (how Cost or perhaps other values are asigned, as it seems
  %%% the algorithm struggles on walking on cells like '1') 
  % better_path_floors(Origin, Destination, Connections), % temporary
  % format_connections(Connections, JsonConnections), % temporary
  % JsonResponse = json{floors_paths: JsonConnections, map_paths: []}, % temporary
  % format('Content-type: application/json~n~n'), % temporary
  % json_write(current_output, JsonResponse),   % temporary
  %     log_message_ln('finished with JsonResponse'). % temporary

  %%% UNCOMMENT WHEN PATH FINDING IS FIXED
  (OriginBuildingCode = DestinationBuildingCode, OriginFloorNumber = DestinationFloorNumber ->
    % If origin and destination are on the same floor
    same_floor_path(OriginFloorNumber, OriginBuildingCode, OriginMapCellX, OriginMapCellY, DestinationMapCellX, DestinationMapCellY, SameFloorPath),
          log_message('found same floor path;'),
    JsonResponse = json{floors_paths: [], map_paths: [SameFloorPath]}
    ;
    % Different floors
          log_message('finding better path floors...;'),
    better_path_floors(Origin, Destination, Connections),
          log_message_ln('found better path floors'),
    format_connections(Connections, JsonConnections),
    find_map_paths(Connections, OriginMapCellX, OriginMapCellY, DestinationMapCellX, DestinationMapCellY, MapPaths),
          log_message_ln('found map paths'),
    JsonResponse = json{floors_paths: JsonConnections, map_paths: MapPaths}
  ),
      log_message('finished paths finding;'),
  format('Content-type: application/json~n~n'),
  json_write(current_output, JsonResponse),
      log_message_ln('finished with JsonResponse').


same_floor_path(OriginFloorNumber, OriginBuildingCode, OriginCol, OriginRow, DestCol, DestRow, MapPathJson) :-
  logic:load_map(OriginFloorNumber, OriginBuildingCode, MapWidth, MapHeight),
  % graph_creation_for_maze_diagonal:create_graph(26, 16),
  create_graph(MapWidth, MapHeight),
  Start = cel(OriginCol, OriginRow),
  End = cel(DestCol, DestRow),
  % astar_maze_diagonal_algorithm:aStar(Start, End, Path, Cost),
  % aStar(Start, End, Path, Cost),
  bestFirst(Start, End, Path, Cost),
  % graph_creation_for_maze_diagonal:remove_graph(),
  remove_graph(),
  format_path_json(Path, Cost, MapPathJson).

printConnections([]).
printConnections([H|T]) :-
  format('~w~n', [H]),
  printConnections(T).



find_map_paths([], _, _, _, _, []).
% fails to process connection destination part (elevator or passage), e.g. elev('A::1', 'A::2') - fails to process (do path finding on) 'A::2'
% Something like NextFloor should be added to process_connection predicate perhaps
find_map_paths([Connection|Rest], OriginCol, OriginRow, DestCol, DestRow, [MapPath|MapPathsRest]) :-
      log_message('find_map_paths with data [Connection, Rest, OriginCol, OriginRow, DestCol, DestRow]:'),
      log_message_ln([Connection, Rest, OriginCol, OriginRow, DestCol, DestRow]),
      log_message_ln('calling process_connection'),
  process_connection(Connection, OriginCol, OriginRow, IntermediateCol, IntermediateRow, MapPath),
      log_message_ln('finished process_connection'),
      % Check if there is a next segment in the path and set the next origin accordingly
  (Rest = [] -> 
      NextOriginCol = DestCol, NextOriginRow = DestRow,
          log_message_ln('Rest is empty')
  ; 
      NextOriginCol = IntermediateCol, NextOriginRow = IntermediateRow,
          log_message('NextOriginCol='), log_message(NextOriginCol), log_message('NextOriginRow='), log_message_ln(NextOriginRow)
  ),
      log_message_ln('calling find_map_paths'),
  find_map_paths(Rest, NextOriginCol, NextOriginRow, DestCol, DestRow, MapPathsRest),
      log_message_ln('finished find_map_paths').

process_connection(Connection, OriginCol, OriginRow, DestCol, DestRow, MapPathJson) :-
      log_message('started process_connection;'),
  connection_floor_building(Connection, FromFloor, FromBuilding, ToFloor, ToBuilding),
      log_message('finished connection_floor_building;'),
  logic:load_map(FromFloor, FromBuilding, MapWidth, MapHeight),
      log_message('finished load_map;'),
  % graph_creation_for_maze_diagonal:create_graph(26, 16),
  create_graph(MapWidth, MapHeight),
      log_message('finished create_graph;'),

  % Set StartCel based on the current connection type
  (Connection = elev(_, _) ->
        log_message('Connection is elev;'),
      StartCel = cel(OriginCol, OriginRow),
      logic:elevator_pos(DestCol, DestRow)
  ; Connection = cor(_, _) ->
      StartCel = cel(OriginCol, OriginRow),
      logic:passage(DestCol, DestRow, ToBuilding, ToFloor)
  ),

  EndCel = cel(DestCol, DestRow),
      log_message('StartCel='), log_message(StartCel), log_message('EndCel='), log_message(EndCel),

  % astar_maze_diagonal_algorithm:aStar(StartCel, EndCel, Path, Cost),
  % aStar(StartCel, EndCel, Path, Cost),
  bestFirst(StartCel, EndCel, Path, Cost),
      log_message('finished bestFirst aStar;'),
  % graph_creation_for_maze_diagonal:remove_graph(),
  remove_graph(),
      log_message('finished remove_graph;'),
  format_path_json(Path, Cost, MapPathJson),
      log_message_ln('finished format_path_json').

connection_floor_building(Connection, FromFloor, FromBuilding, ToFloor, ToBuilding) :-
  % Split the 'From' and 'To' parts of the connection
  (Connection = elev(From, To) ; Connection = cor(From, To)),
  split_building_floor(From, FromBuilding, FromFloor),
  split_building_floor(To, ToBuilding, ToFloor).

find_start_end(OriginMapCellCol, OriginMapCellRow, DestinationMapCellCol, DestinationMapCellRow, Start, End) :-
  Start = cel(OriginMapCellCol, OriginMapCellRow),
  End = cel(DestinationMapCellCol, DestinationMapCellRow).

cel_to_json(cel(Col, Row), json{col: Col, row: Row}).

format_path_json(Path, Cost, MapPathJson) :-
  maplist(cel_to_json, Path, JsonPath),
  MapPathJson = json{path: JsonPath, cost: Cost}.

% Helper predicate to format a single connection as a JSON-like term
format_connection(elev(From, To), json{fromBuilding: FromBuilding, fromFloorNumber: FromFloor, toBuilding: ToBuilding, toFloorNumber: ToFloor, type:"elevator"}) :-
  split_building_floor(From, FromBuilding, FromFloor),
  split_building_floor(To, ToBuilding, ToFloor).

format_connection(cor(From, To), json{fromBuilding: FromBuilding, fromFloorNumber: FromFloor, toBuilding: ToBuilding, toFloorNumber: ToFloor, type:"corridor"}) :-
  split_building_floor(From, FromBuilding, FromFloor),
  split_building_floor(To, ToBuilding, ToFloor).

% Predicate to transform the entire connection list
format_connections([], []).
format_connections([H|T], [JsonH|JsonT]) :-
    format_connection(H, JsonH),
    format_connections(T, JsonT).

% Splits a string of the form 'Building::Floor' into separate components
split_building_floor(Compound, Building, Floor) :-
  split_string(Compound, "::", "", [BuildingStr, _, FloorStr]),
  atom_string(Building, BuildingStr),
  atom_string(Floor, FloorStr).








% Test route handler that returns a simple JSON response
get_test_handler(_Request) :-
  format('Content-type: application/json~n~n'),
  format('{"message": "This is a simple JSON response from Planning API"}').

% Test route handler that returns request query parameters
get_route_test_handler(Request) :-
  http_parameters(Request,
      [ origin_building_code(OriginBuildingCode, []),
        origin_floor_number(OriginFloorNumber, []),
        origin_map_cell_x(OriginMapCellX, []),
        origin_map_cell_y(OriginMapCellY, []),
        destination_building_code(DestinationBuildingCode, []),
        destination_floor_number(DestinationFloorNumber, []),
        destination_map_cell_x(DestinationMapCellX, []),
        destination_map_cell_y(DestinationMapCellY, []),
        minimize_elevator_uses(MinimizeElevatorUses, []),
        minimize_building_count(MinimizeBuildingCount, [])
      ]),
  format('Content-type: text/plain~n~n'),
  format('Origin Building Code: ~w~n', [OriginBuildingCode]),
  format('Origin Floor Number: ~w~n', [OriginFloorNumber]),
  format('Origin Map Cell X: ~w~n', [OriginMapCellX]),
  format('Origin Map Cell Y: ~w~n', [OriginMapCellY]),
  format('Destination Building Code: ~w~n', [DestinationBuildingCode]),
  format('Destination Floor Number: ~w~n', [DestinationFloorNumber]),
  format('Destination Map Cell X: ~w~n', [DestinationMapCellX]),
  format('Destination Map Cell Y: ~w~n', [DestinationMapCellY]),
  format('Minimize Elevator Uses: ~w~n', [MinimizeElevatorUses]),
  format('Minimize Building Count: ~w~n', [MinimizeBuildingCount]).



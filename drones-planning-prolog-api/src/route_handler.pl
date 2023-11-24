:- module(route_handler, [get_route_handler/1]).

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(logic).
:- use_module(navigationBuildingsFloors).
:- use_module(graph_creation_for_maze_diagonal).
:- use_module(astar_maze_diagonal_algorithm).


:- http_handler('/planning-api/route', get_route_handler, []).

:- http_handler('/planning-api/test', get_route_test_handler, []).
:- http_handler('/planning-api/', get_test_handler, []).
:- http_handler('/', get_test_handler, []).

%  Route route with parameters handler
get_route_handler(Request) :-
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
  logic:load_info(), % GETs building, floor, elevator, passage info from backend API
  format(atom(Origin), '~w::~w', [OriginBuildingCode, OriginFloorNumber]),
  format(atom(Destination), '~w::~w', [DestinationBuildingCode, DestinationFloorNumber]),
  % path_floors(Origin, Destination, Path, Connections),
  better_path_floors(Origin, Destination, Connections),
  format_connections(Connections, JsonConnections),
  find_map_paths(Connections, MapPaths),
  JsonResponse = json{floors_paths: JsonConnections, map_paths: MapPaths},
  format('Content-type: application/json~n~n'),
  json_write(current_output, JsonResponse),
  write([OriginMapCellX,OriginMapCellY,DestinationMapCellX,DestinationMapCellY,MinimizeElevatorUses,MinimizeBuildingCount]).

find_map_paths([], []).
find_map_paths([Connection|Rest], [MapPath|MapPathsRest]) :-
    process_connection(Connection, MapPath),
    find_map_paths(Rest, MapPathsRest).

process_connection(Connection, MapPathJson) :-
  connection_floor_building(Connection, FromFloor, FromBuilding, ToFloor, ToBuilding),
  logic:load_map(FromFloor, FromBuilding),
  graph_creation_for_maze_diagonal:create_graph(26, 16),
  find_start_end(FromFloor, FromBuilding, ToFloor, ToBuilding, Start, End),
  astar_maze_diagonal_algorithm:aStar(Start, End, Path, Cost),
  graph_creation_for_maze_diagonal:remove_graph(),
  format_path_json(Path, Cost, MapPathJson).

connection_floor_building(Connection, FromFloor, FromBuilding, ToFloor, ToBuilding) :-
  % Split the 'From' and 'To' parts of the connection
  (Connection = elev(From, To) ; Connection = cor(From, To)),
  split_building_floor(From, FromBuilding, FromFloor),
  split_building_floor(To, ToBuilding, ToFloor).

find_start_end(FromFloor, FromBuilding, ToFloor, ToBuilding, Start, End) :-
  % Define Start based on FromFloor and FromBuilding
  % You need to determine how to convert these into cell coordinates
  % For simplicity, let's assume there's a predicate start_cell that does this
  start_cell(FromFloor, FromBuilding, Start),
  end_cell(ToFloor, ToBuilding, End).

cel_to_json(cel(Col, Row), json{col: Col, row: Row}).

format_path_json(Path, Cost, MapPathJson) :-
  maplist(cel_to_json, Path, JsonPath),
  MapPathJson = json{path: JsonPath, cost: Cost}.

% Dummy predicates for start_cell and end_cell. Replace with actual logic
start_cell(_, _, cel(1,1)). % Replace with actual logic
end_cell(_, _, cel(10,10)). % Replace with actual logic

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



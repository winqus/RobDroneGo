:- module(pathPlanningRouteHandler, [get_route_handler/1, log_message/1, log_message_ln/1,
    same_floor_path/5, find_map_paths/7, pathAndTotalCostBetweenOriginDestination/4
  ]).

:- include('../config.pl').

:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_log)).

:- use_module(logic).
:- use_module(navigationBuildingsFloors).
:- use_module(graph_creation_for_maze_diagonal).
:- use_module(astar_maze_diagonal_algorithm).

:- http_handler('/planning-api/route', get_route_handler, []).

:- http_handler('/planning-api/test', get_test_handler, []).
:- http_handler('/planning-api/', get_test_handler, []).
:- http_handler('/planning-api/clear-paths-cache', clear_cache_handler, []).
:- http_handler('/', get_test_handler, []).

log_message_ln(Message) :-
  http_log('~p~n', [Message]).

log_message(Message) :-
  http_log('~p ', [Message]).

:- dynamic navigation_cache/3.
:- dynamic pathCache/5.


%%  Route route with parameters handler
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
%% Convert parameters to atoms
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
%% Find the route
  OriginCell = cel(OriginMapCellX, OriginMapCellY),
  DestinationCell = cel(DestinationMapCellX, DestinationMapCellY),
  pathAndTotalCostBetweenOriginDestination(origin(OriginBuildingCode, OriginFloorNumber, OriginCell), destination(DestinationBuildingCode, DestinationFloorNumber, DestinationCell), _, JsonResponse),
      log_message('finished paths finding;'),
  format('Content-type: application/json~n~n'),
  json_write(current_output, JsonResponse),
      log_message_ln('finished with JsonResponse').




%%% Try to find a path in the cache
pathAndTotalCostBetweenOriginDestination(Origin, Destination, NavigationTotalCost, JsonNavigationData) :-
  % log_message_ln('Trying to find a cached path...'),
  get_cached_navigation(Origin, Destination, JsonData),
  JsonData = json{estimatedTotalCost: TotalCost, floorsPaths: _, floorsConnectionsCost: _, mapPathCount: _, mapPaths: _},
  TotalCost >= 0,
  NavigationTotalCost = TotalCost,
  JsonNavigationData = JsonData,
  log_message_ln('CACHED navigation data found;'), !.


%%% Origin and destination on same floor
pathAndTotalCostBetweenOriginDestination(Origin, Destination, TotalCost, JsonNavigationData) :-
  Origin = origin(BuildingCode, FloorNumber, OriginCell),
  Destination = destination(BuildingCode, FloorNumber, DestinationCell), !,
  log_message('starting path finding on same floor...;'),
  same_floor_path(FloorNumber, BuildingCode, OriginCell, DestinationCell, Path),
  log_message('found same floor path;'),
  Path = json{buildingCode:_, cost:Cost, floorNumber:_, path:_},
  TotalCost is Cost,
  JsonNavigationData = json{estimatedTotalCost: TotalCost, floorsPaths: [], floorsConnectionsCost: 0, mapPathCount: 1, mapPaths: [Path]}, 
  add_navigationResponse_to_cache(Origin, Destination, JsonNavigationData), !.

%%% Origin and destination on different floors
pathAndTotalCostBetweenOriginDestination(Origin, Destination, TotalCost, JsonNavigationData) :-
  % Extract information from the Origin and Destination arguments
  Origin = origin(OriginBuildingCode, OriginFloorNumber, OriginCell),
  Destination = destination(DestinationBuildingCode, DestinationFloorNumber, DestinationCell),
  % Compose the BuildingFloor identifiers
  format(atom(OriginBuildingFloor), '~w::~w', [OriginBuildingCode, OriginFloorNumber]), % Format origin as Building::FloorFloorNumber
  format(atom(DestinationBuildingFloor), '~w::~w', [DestinationBuildingCode, DestinationFloorNumber]), % Format destination as Building::FloorNumber
  % Find the best path between floors
  log_message('starting better_path_floors...;'),!,
  better_path_floors(OriginBuildingFloor, DestinationBuildingFloor, Connections),
% Calculate the cost of the connections (elevators and passages)
  connection_list_cost(Connections, ConnectionsCost),
  log_message('found better path floors; '),
  % Find the map paths
  log_message('starting find_map_paths...;'),
  find_map_paths(0, Connections, OriginBuildingCode, OriginFloorNumber, OriginCell, DestinationCell, MapPaths),
  log_message('found floor map path(s);'),
  length(MapPaths, MapPathsCount),
% Calculate the total cost
  sum_costs(MapPaths, PathsCost),
  TotalCost is ConnectionsCost + PathsCost,
% Format the JSON response
  format_connections(Connections, JsonConnections),
  JsonNavigationData = json{estimatedTotalCost: TotalCost, floorsPaths: JsonConnections, floorsConnectionsCost: ConnectionsCost, mapPathCount: MapPathsCount, mapPaths: MapPaths}, 
  add_navigationResponse_to_cache(Origin, Destination, JsonNavigationData), !.

% When arguments origin and destination are swapped
pathAndTotalCostBetweenOriginDestination(Destination, Origin, TotalCost, JsonNavigationData) :-
  Origin = origin(_, _, _),
  Destination = destination(_, _, _),!,
  log_message('detected swapped origin and destination;'),
  pathAndTotalCostBetweenOriginDestination(Origin, Destination, TotalCost, JsonNavigationData).
% When arguments are origin and origin
pathAndTotalCostBetweenOriginDestination(Origin, Destination, TotalCost, JsonNavigationData) :-
  Origin = origin(_, _, _),
  Destination = origin(A, B, C),!,
  log_message('detected origin and origin (instead of destination);'),
  pathAndTotalCostBetweenOriginDestination(Origin, destination(A, B, C), TotalCost, JsonNavigationData).
% When arguments are destination and destination
pathAndTotalCostBetweenOriginDestination(Origin, Destination, TotalCost, JsonNavigationData) :-
  Origin = destination(A, B, C),
  Destination = destination(_, _, _),
  log_message('detected destination (instead of origin) and destination;'),
  pathAndTotalCostBetweenOriginDestination(origin(A, B, C), Destination, TotalCost, JsonNavigationData).

same_floor_path(OriginFloorNumber, OriginBuildingCode, OriginCell, DestinationCell, MapPathJson) :-
  logic:load_map(OriginFloorNumber, OriginBuildingCode, MapWidth, MapHeight),
  create_graph(MapWidth, MapHeight),
  find_and_format_best_path(OriginCell, DestinationCell, OriginBuildingCode, OriginFloorNumber, MapPathJson).
  % bestFirst(OriginCell, DestinationCell, Path, Cost),
  % beamSearch(OriginCell, DestinationCell, Path, Cost),
  % remove_graph(),
  % format_path_json(Path, Cost, OriginBuildingCode, OriginFloorNumber, MapPathJson).

printConnections([]).
printConnections([H|T]) :-
  format('~w~n', [H]),
  printConnections(T).


find_map_paths(EntranceConnection, [], _, _, MainOriginCell, MainDestinationCell, [MapPath]) :-
  isConnection(EntranceConnection),
  set_intermediate_building_floor(EntranceConnection, [], IntermediateBuildingCode, IntermediateFloorNumber),
  log_message_ln(''),log_message('FIND1 find_map_paths with data [EntranceConnection, IntermediateBuildingCode, IntermediateFloorNumber, MainOriginCell, MainDestinationCell]:'),
  log_message_ln([EntranceConnection, IntermediateBuildingCode, IntermediateFloorNumber, MainOriginCell, MainDestinationCell]),
  load_map_for_floor(IntermediateBuildingCode, IntermediateFloorNumber, _, _),
  set_intermediate_points(EntranceConnection, [], MainOriginCell, MainDestinationCell, IntermediateOrigin, IntermediateDestination),
  log_message('IntermediateOrigin: '), log_message(IntermediateOrigin), log_message('IntermediateDestination: '), log_message_ln(IntermediateDestination),
  find_and_format_best_path(IntermediateOrigin, IntermediateDestination, IntermediateBuildingCode, IntermediateFloorNumber, MapPath),
  log_message('LAST MapPath: '), log_message_ln(MapPath).


find_map_paths(EntranceConnection, [ExitConnection|RemainingConnections], BuildingCode, FloorNumber, MainOriginCell, MainDestinationCell, MapPaths) :-
  log_message_ln(''),log_message('FIND2'),
  log_message('find_map_paths with data [EntranceConnection, ExitConnection, RemainingConnections, BuildingCode, FloorNumber, MainOriginCell, MainDestinationCell]:'),
  log_message_ln([EntranceConnection, ExitConnection, RemainingConnections, BuildingCode, FloorNumber, MainOriginCell, MainDestinationCell]),

  set_intermediate_building_floor(EntranceConnection, ExitConnection, IntermediateBuildingCode, IntermediateFloorNumber),
  load_map_for_floor(IntermediateBuildingCode, IntermediateFloorNumber, _, _),
  set_intermediate_points(EntranceConnection, ExitConnection, MainOriginCell, MainDestinationCell, IntermediateOrigin, IntermediateDestination),
    log_message('IntermediateOrigin: '), log_message(IntermediateOrigin), log_message('IntermediateDestination: '), log_message_ln(IntermediateDestination),
  
  find_and_format_best_path(IntermediateOrigin, IntermediateDestination, IntermediateBuildingCode, IntermediateFloorNumber, MapPath),
  append([MapPath], [], NewMapPaths),
  log_message('MapPath: '), log_message_ln(MapPath),
  log_message('NewMapPaths: '), log_message_ln(NewMapPaths),

  find_map_paths(ExitConnection, RemainingConnections, BuildingCode, FloorNumber, MainOriginCell, MainDestinationCell, RecursedMapPaths),
  append([MapPath], RecursedMapPaths, MapPaths),
  log_message('MapPaths: '), log_message_ln(MapPaths).

find_map_paths(EntranceConnection, [], _, _, _, _, _) :- log_message_ln(''),log_message('FIND3 OF find_map_paths'), log_message('EntranceConnection: '), log_message_ln(EntranceConnection), !.

isConnection(Connection) :- (Connection = elev(_, _) ; Connection = cor(_, _)).

load_map_for_floor(BuildingCode, FloorNumber, MapWidth, MapHeight) :-
  logic:load_map(FloorNumber, BuildingCode, MapWidth, MapHeight),
    log_message('loaded floor map of size'), log_message(MapWidth), log_message('x'), log_message(MapHeight), log_message(';'),
  create_graph(MapWidth, MapHeight),
    log_message('created floor map graph;'), !.

find_and_format_best_path(IntermediateOrigin, IntermediateDestination, BuildingCode, FloorNumber, MapPath) :-
  log_message('Finding best path with data [IntermediateOrigin, IntermediateDestination, BuildingCode, FloorNumber]:'),
  log_message_ln([IntermediateOrigin, IntermediateDestination, BuildingCode, FloorNumber]),
  % Check cache first
  (   check_path_cache(IntermediateOrigin, IntermediateDestination, BuildingCode, FloorNumber, Path, Cost)
  ->  log_message('Loaded path from pathCache;')
  ;   (beamSearch(IntermediateOrigin, IntermediateDestination, Path, Cost) -> true; (Path = [], Cost = -40404)),
      (   Path \= [], Cost \= -40404
      ->  add_to_path_cache(IntermediateOrigin, IntermediateDestination, BuildingCode, FloorNumber, (Path, Cost)),
          log_message('found floor map path with beamSearch and updated pathCache;')
      ;   log_message('No path found; not updating pathCache;')
      )
  ),
  remove_graph(),
  log_message('removed floor map graph;'),
  format_path_json(Path, Cost, BuildingCode, FloorNumber, MapPath),
  log_message_ln('finished format_path_json'), !.
  
  
set_intermediate_points(EntranceConnection, ExitConnection, MainOriginCell, MainDestinationCell, IntermediateOrigin, IntermediateDestination) :-
  %% Set Intermediate Origin
  (isConnection(EntranceConnection) ->
      log_message('EntranceConnection is not empty; '),!,
      connection_floor_building(EntranceConnection, RawPreviousFloorNumber, PreviousBuildingCode, RawCurrentFloorNumber, CurrentBuildingCode),
      atom_number(RawPreviousFloorNumber, PreviousFloorNumber),
      atom_number(RawCurrentFloorNumber, CurrentFloorNumber),
      (EntranceConnection = elev(_, _) ->
          log_message_ln('EntranceConnection is elev'),
          logic:elevator_pos(IntermediateCol, IntermediateRow)
      ;
      EntranceConnection = cor(_, _) ->
          log_message_ln('EntranceConnection is passage'),
          logic:passage(IntermediateCol, IntermediateRow, PreviousBuildingCode, PreviousFloorNumber)
      ),
      IntermediateOrigin = cel(IntermediateCol, IntermediateRow)
  ;
      log_message_ln('EntranceConnection is empty'),
      IntermediateOrigin = MainOriginCell
  ),

  %% Set Intermediate Destination
  (isConnection(ExitConnection) ->
      log_message('ExitConnection is not empty; '),!,
      connection_floor_building(ExitConnection, RawCurrentFloorNumber, CurrentBuildingCode, RawNextFloorNumber, NextBuildingCode),
      atom_number(RawCurrentFloorNumber, CurrentFloorNumber),
      atom_number(RawNextFloorNumber, NextFloorNumber),
      (ExitConnection = elev(_, _) ->
          log_message_ln('ExitConnection is elev'),
          logic:elevator_pos(DestCol, DestRow)
      ;
      ExitConnection = cor(_, _) ->
          log_message_ln('ExitConnection is passage'),
          log_message('DestCol: '), log_message(DestCol), log_message('DestRow: '), log_message(DestRow), log_message('NextBuildingCode: '), log_message(NextBuildingCode), log_message('NextFloorNumber: '), log_message_ln(NextFloorNumber),
          logic:passage(DestCol, DestRow, NextBuildingCode, NextFloorNumber)
      ),!,
      IntermediateDestination = cel(DestCol, DestRow)
  ;
      log_message_ln('ExitConnection is empty'),!,
      IntermediateDestination = MainDestinationCell
  ), !.

set_intermediate_building_floor(EntranceConnection, ExitConnection, IntermediateBuildingCode, IntermediateFloorNumber) :-
  %% Determine Intermediate Building and Floor based on Entrance Connection
  (isConnection(EntranceConnection) ->
      connection_floor_building(EntranceConnection, _, _, RawCurrentFloorNumber, CurrentBuildingCode),
      atom_number(RawCurrentFloorNumber, CurrentFloorNumber),
      IntermediateBuildingCode = CurrentBuildingCode,
      IntermediateFloorNumber = CurrentFloorNumber
  ;
      true % Do nothing if EntranceConnection is not a connection
  ),

  %% Determine Intermediate Building and Floor based on Exit Connection
  (isConnection(ExitConnection) ->
      connection_floor_building(ExitConnection, RawCurrentFloorNumber, CurrentBuildingCode, _, _),
      atom_number(RawCurrentFloorNumber, CurrentFloorNumber),
      IntermediateBuildingCode = CurrentBuildingCode,
      IntermediateFloorNumber = CurrentFloorNumber
  ;
      true % Do nothing if ExitConnection is not a connection
  ).



connection_floor_building(Connection, FromFloor, FromBuilding, ToFloor, ToBuilding) :-
  % Split the 'From' and 'To' parts of the connection
  (Connection = elev(From, To) ; Connection = cor(From, To)),
  split_building_floor(From, FromBuilding, FromFloor),
  split_building_floor(To, ToBuilding, ToFloor).

find_start_end(OriginMapCellCol, OriginMapCellRow, DestinationMapCellCol, DestinationMapCellRow, Start, End) :-
  Start = cel(OriginMapCellCol, OriginMapCellRow),
  End = cel(DestinationMapCellCol, DestinationMapCellRow).

cel_to_json(cel(Col, Row), json{col: ColMinusOne, row: RowMinusOne}) :-
  ColMinusOne is Col - 1,
  RowMinusOne is Row - 1.

format_path_json(Path, Cost, BuildingCode, FloorNumber, MapPathJson) :-
  maplist(cel_to_json, Path, JsonPath),
  MapPathJson = json{path: JsonPath, cost: Cost, buildingCode: BuildingCode, floorNumber: FloorNumber}.

% Helper predicate to format a single connection as a JSON-like term
format_connection(elev(From, To), json{fromBuilding: FromBuilding, fromFloorNumber: FromFloor, toBuilding: ToBuilding, toFloorNumber: ToFloor, type:"elevator"}) :-
  split_building_floor(From, FromBuilding, FromFloor),
  split_building_floor(To, ToBuilding, ToFloor).

format_connection(cor(From, To), json{fromBuilding: FromBuilding, fromFloorNumber: FromFloor, toBuilding: ToBuilding, toFloorNumber: ToFloor, type:"passage"}) :-
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

% Predicate to sum costs in the MapPaths list
sum_costs(MapPaths, TotalCost) :-
  sum_costs(MapPaths, 0, TotalCost).
sum_costs([], Accumulator, Accumulator).
sum_costs([json{buildingCode:_, cost:Cost, floorNumber:_, path:_}|Rest], Accumulator, TotalCost) :-
  NewAccumulator is Accumulator + Cost,
  sum_costs(Rest, NewAccumulator, TotalCost).

% Navigation data cache predicates
get_cached_navigation(Origin, Destination, JsonResponse) :-
  navigation_cache(Origin, Destination, JsonResponse).

is_not_cached(Origin, Destination) :-
  \+ navigation_cache(Origin, Destination, _).

add_navigationResponse_to_cache(Origin, Destination, JsonResponse) :-
  useNavigationCache(UseNavigationCache),
  (is_not_cached(Origin, Destination),
    UseNavigationCache,
    assertz(navigation_cache(Origin, Destination, JsonResponse))
  ; true).
  
remove_navigation_from_cache(Origin, Destination) :-
  retractall(navigation_cache(Origin, Destination, _)).

remove_all_navigationData_from_cache :-
  retractall(navigation_cache(_, _, _)).

% Path cache predicates
add_to_path_cache(Origin, Destination, Building, Floor, Result) :-
  usePathCache(UsePathCache),
  (UsePathCache,
    retractall(pathCache(Origin, Destination, Building, Floor, _)),
    assert(pathCache(Origin, Destination, Building, Floor, Result))
  ; true).

check_path_cache(Origin, Destination, Building, Floor, Path, Cost) :-
  pathCache(Origin, Destination, Building, Floor, Result),
  Result = (Path, Cost).

remove_from_path_cache(Origin, Destination, Building, Floor) :-
  retractall(pathCache(Origin, Destination, Building, Floor, _)).

clear_path_cache :-
  retractall(pathCache(_, _, _, _, _)).

% Cache clearing route handler
clear_cache_handler(_Request) :-
  remove_all_navigationData_from_cache,
  clear_path_cache,
  format('Content-type: application/json~n~n'),
  format('{"message": "Cache cleared"}').

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


